// This supervisor program keeps track of a process (normally the parent
// process) and receives process IDs (called children) on standard input. If
// the supervisor process receives a SIGINT (Ctrl-C) or SIGTERM, or if it
// detects that the parent process has died, it will kill all the child
// processes.
//
// Every 0.2 seconds, it does the following:
// * Checks for any new process IDs on standard input, and adds them to the list
//   of child processes to track. If the PID is negative, as in "-1234", then
//   that value will be negated and removed from the list of processes to track.
// * Checks if any child processes have died. If so, remove them from the list
//   of child processes to track.
// * Checks if the parent process has died. If so, kill all children and exit.
//
// To test it out in verbose mode, run:
//   gcc supervisor.c -o supervisor
//   ./supervisor -v -p [parent_pid]
//
// The [parent_pid] is optional. If not supplied, the supervisor will auto-
// detect the parent process.
//
// After it is started, you can enter pids for child processes. Then you can
// do any of the following to test it out:
// * Press Ctrl-C.
// * Send a SIGTERM to the supervisor with `killall supervisor`.
// * Kill the parent processes.
// * Kill a child process.

#ifdef __INTEL_COMPILER
#define _BSD_SOURCE 1
#define _POSIX_C_SOURCE  200809L
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/types.h>
#include <time.h>
#include <signal.h>
#include <unistd.h>

#ifdef WIN32
#include "windows.h"
#endif

#include "utils.h"

// Constants ------------------------------------------------------------------

// Size of stdin input buffer
#define INPUT_BUF_LEN 1024
// Maximum number of children to keep track of
#define MAX_CHILDREN 1024
// Milliseconds to sleep in polling loop
#define POLL_MS 200

// Globals --------------------------------------------------------------------

// Child processes to track
int children[MAX_CHILDREN];
int n_children = 0;

int sigint_received  = false;
int sigterm_received = false;

// Utility functions ----------------------------------------------------------

// Cross-platform sleep function
#ifdef WIN32
#include <windows.h>
#elif _POSIX_C_SOURCE >= 199309L
#include <time.h>   // for nanosleep
#else
#include <unistd.h> // for usleep
#endif

void sleep_ms(int milliseconds) {
#ifdef WIN32
    Sleep(milliseconds);
#elif _POSIX_C_SOURCE >= 199309L
    struct timespec ts;
    ts.tv_sec = milliseconds / 1000;
    ts.tv_nsec = (milliseconds % 1000) * 1000000;
    nanosleep(&ts, NULL);
#else
    usleep(milliseconds * 1000);
#endif
}


// Given a string of format "102", return 102. If conversion fails because it
// is out of range, or because the string can't be parsed, return 0.
int extract_pid(char* buf, int len) {
    long pid = strtol(buf, NULL, 10);

    // Out of range: errno is ERANGE if it's out of range for a long. We're
    // going to cast to int, so we also need to make sure that it's within
    // range for int.
    if (errno == ERANGE || pid > INT_MAX || pid < INT_MIN) {
        return 0;
    }

    return (int)pid;
}


// Check if a process is running. Returns 1 if yes, 0 if no.
bool pid_is_running(pid_t pid) {
    #ifdef WIN32
    HANDLE h_process = OpenProcess(PROCESS_QUERY_INFORMATION, false, pid);
    if (h_process == NULL) {
        printf("Unable to check if process %d is running.\n", (int)pid);
        return false;
    }

    DWORD exit_code;
    if (!GetExitCodeProcess(h_process, &exit_code)) {
        printf("Unable to check if process %d is running.\n", (int)pid);
        return false;
    }

    if (exit_code == STILL_ACTIVE) {
        return true;
    } else {
        return false;
    }

    #else
    int res = kill(pid, 0);
    if (res == -1 && errno == ESRCH) {
        return false;
    }
    return true;
    #endif
}

// Send a soft kill signal to all children, wait 5 seconds, then hard kill any
// remaining processes.
void kill_children(void) {
    if (n_children == 0)
        return;

    verbose_printf("Sending close signal to children: ");
    for (int i=0; i<n_children; i++) {
        verbose_printf("%d ", children[i]);

        #ifdef WIN32
        sendCtrlC(children[i]);
        sendWmClose(children[i]);
        #else
        kill(children[i], SIGTERM);
        #endif
    }
    verbose_printf("\n");


    // Poll, checking that child processes have exited. Using `time()` isn't
    // the most accurate way to get time, since it only has a resolution of 1
    // second, but it is cross-platform and good enough for this purpose.
    time_t stop_time = time(NULL) + 5;

    do {
        sleep_ms(POLL_MS);

        verbose_printf("Checking status of children: ");
        for (int i=0; i<n_children; i++) {
            if (pid_is_running(children[i])) {
                verbose_printf("%d ", children[i]);
            } else {
                verbose_printf("%d(stopped) ", children[i]);
                n_children = remove_element(children, n_children, i);
            }
        }
        verbose_printf("\n");

        if (n_children == 0) {
            return;
        }
    } while(time(NULL) < stop_time);


    // Hard-kill any remaining processes
    bool kill_message_shown = false;

    for (int i=0; i<n_children; i++) {
        if (pid_is_running(children[i])) {

            if (!kill_message_shown) {
                verbose_printf("Sending kill signal to children: ");
                kill_message_shown = true;
            }

            verbose_printf("%d ", children[i]);

            #ifdef WIN32
            kill_pid(children[i]);
            #else
            kill(children[i], SIGKILL);
            #endif
        }
    }

    if (kill_message_shown)
        verbose_printf("\n");
}


static void sig_handler(int signum) {
    char* signame;
    if (signum == SIGTERM) {
        signame = "SIGTERM";
        sigterm_received = true;

    } else if (signum == SIGINT) {
        signame = "SIGINT";
        sigint_received = true;

    } else {
        signame = "Unknown signal";
    }

    verbose_printf("%s received.\n", signame);
}


int main(int argc, char **argv) {

    int parent_pid;
    int parent_pid_arg = 0;
    int parent_pid_detected;
    char* input_pipe_name = NULL;

    // Process arguments ------------------------------------------------------
    if (argc >= 2) {
        for (int i=1; i<argc; i++) {
            if (strcmp(argv[i], "-v") == 0) {
                verbose_mode = true;

            } else if (strcmp(argv[i], "-p") == 0) {
                i++;
                if (i >= argc) {
                    printf("-p must be followed with a process ID.");
                    exit(1);
                }

                parent_pid_arg = extract_pid(argv[i], (int) strlen(argv[i]));
                if (parent_pid_arg == 0) {
                    printf("Invalid parent process ID: %s\n", argv[i]);
                    exit(1);
                }

            } else if (strcmp(argv[i], "-i") == 0) {
                i++;
                if (i >= argc) {
                    printf("-i must be followed with the name of a pipe.");
                    exit(1);
                }

                input_pipe_name = argv[i];

            } else {
                printf("Unknown argument: %s\n", argv[i]);
                exit(1);
            }
        }
    }

    printf("PID: %d\n", getpid());
    fflush(stdout);

    parent_pid_detected = getppid();
    verbose_printf("Parent PID (detected): %d\n", parent_pid_detected);

    if (parent_pid_arg != 0) {
        verbose_printf("Parent PID (argument): %d\n", parent_pid_arg);
        parent_pid = parent_pid_arg;

        // This check is really only useful for testing.
        if (parent_pid_arg != parent_pid_detected) {
            verbose_printf("Note: detected parent PID differs from argument parent PID.\n");
            verbose_printf("Using parent PID from argument (%d).\n", parent_pid_arg);
        }
    } else {
        parent_pid = parent_pid_detected;
    }

    if (input_pipe_name != NULL) {
        verbose_printf("Reading input from %s.\n", input_pipe_name);
    }


    // Open and configure input source ----------------------------------------

    // Input buffer for messages from the R process
    char readbuf[INPUT_BUF_LEN];

    #ifdef WIN32

    HANDLE h_input;

    if (input_pipe_name == NULL) {
        h_input = open_stdin();
    } else {
        h_input = open_named_pipe(input_pipe_name);
    }

    configure_input_handle(h_input);

    #else

    FILE* fp_input;

    if (input_pipe_name == NULL) {
        fp_input = stdin;

    } else {
        printf("fopen.\n");

        fp_input = fopen(input_pipe_name, "r");
        printf("fopened.\n");
        if (fp_input == NULL) {
            printf("Unable to open %s for reading.\n", input_pipe_name);
            exit(1);
        }
    }

    if (fcntl(fileno(fp_input), F_SETFL, O_NONBLOCK) == -1) {
        printf("Error setting input to non-blocking mode.\n");
        exit(1);
    }

    #endif

    printf("Ready\n");
    fflush(stdout);


    // Register signal handler ------------------------------------------------
    #ifdef WIN32

    signal(SIGINT, sig_handler);
    signal(SIGTERM, sig_handler);

    #else

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sig_handler;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGINT, &sa, NULL)  == -1 ||
        sigaction(SIGTERM, &sa, NULL) == -1) {
        printf("Error setting up signal handler.\n");
        exit(1);
    }

    #endif


    // Poll -------------------------------------------------------------------
    while(1) {

        // Check if a sigint or sigterm has been received. If so, then kill
        // the child processes and quit. Do the work here instead of in the
        // signal handler, because the signal handler can itself be
        // interrupted by another call to the same handler if another signal
        // is received, and that could result in some unsafe operations.
        if (sigint_received || sigterm_received) {
            kill_children();
            verbose_printf("\nExiting.\n");
            exit(0);
        }


        // Look for any new processes IDs from the input
        char* res = NULL;

        // Read in the input buffer. There could be multiple lines so we'll
        // keep reading lines until there's no more content.
        while(1) {
            #ifdef WIN32
            res = get_line_nonblock(readbuf, INPUT_BUF_LEN, h_input);
            #else
            res = fgets(readbuf, INPUT_BUF_LEN, fp_input);
            #endif

            if (res == NULL)
                break;

            if (strncmp(readbuf, "kill", 4) == 0) {
                verbose_printf("\'kill' command received.\n");
                kill_children();
                verbose_printf("\nExiting.\n");
                return 0;
            }
            int pid = extract_pid(readbuf, INPUT_BUF_LEN);
            if (pid > 0) {
                if (n_children == MAX_CHILDREN) {
                    printf(
                        "Number of child processes to watch has exceeded limit of %d.",
                        MAX_CHILDREN
                    );

                } else if (array_contains(children, n_children, pid)) {
                    verbose_printf("Not adding (already present):%d\n", pid);

                } else {
                    verbose_printf("Adding:%d\n", pid);
                    children[n_children] = pid;
                    n_children++;
                }

            } else if (pid < 0) {
                // Remove pids that start with '-'
                pid = -pid;
                for (int i=0; i<n_children; i++) {
                    if (children[i] == pid) {
                        verbose_printf("Removing:%d\n", pid);
                        n_children = remove_element(children, n_children, i);
                        break;
                    }
                }
            }
        }

        // Remove any children from list that are no longer running.
        verbose_printf("Children: ");
        for (int i=0; i<n_children; i++) {
            if (pid_is_running(children[i])) {
                verbose_printf("%d ", children[i]);
            } else {
                verbose_printf("%d(stopped) ", children[i]);
                n_children = remove_element(children, n_children, i);
            }
        }
        verbose_printf("\n");

        // Check that parent is still running. If not, kill children.
        if (!pid_is_running(parent_pid)) {
            verbose_printf("Parent (%d) is no longer running.\n", parent_pid);
            kill_children();
            verbose_printf("\nExiting.\n");
            return 0;
        }

        sleep_ms(POLL_MS);
    }

    return 0;
}
