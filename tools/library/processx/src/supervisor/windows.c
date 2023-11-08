
#ifdef __INTEL_COMPILER
#define _BSD_SOURCE 1
#define _POSIX_C_SOURCE  200809L
#endif

#include <stdio.h>
#include <stdbool.h>
#include "windows.h"
#include "utils.h"

#define MIN(a,b) ((a<b)?a:b)
#define MAX(a,b) ((a>b)?a:b)


int getppid(void) {
    int pid = GetCurrentProcessId();

    HANDLE hProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    PROCESSENTRY32 pe;
    // Set the size of the structure before using it.
    pe.dwSize = sizeof(PROCESSENTRY32);

    // Get info about first process.
    if(!Process32First(hProcessSnap, &pe)) {
        printf("Unable to get parent pid");
        exit(1);
    }

    // Walk the snapshot of processes to find the parent.
    do {
        if (pe.th32ProcessID == pid) {
            return pe.th32ParentProcessID;
        }
    } while(Process32Next(hProcessSnap, &pe));

    CloseHandle(hProcessSnap);
    printf("Unable to get parent pid");
    exit(1);
}


HANDLE open_stdin(void) {
    HANDLE h_input = GetStdHandle(STD_INPUT_HANDLE);

    if (h_input == INVALID_HANDLE_VALUE) {
        printf("Unable to get stdin handle.");
        exit(1);
    }

    return h_input;
}


HANDLE open_named_pipe(const char* pipe_name) {
    HANDLE h_input = CreateFile(pipe_name,
        GENERIC_READ,
        0,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL
    );

    if (h_input == INVALID_HANDLE_VALUE) {
        printf("CreateFile failed with error %u\n", (unsigned)GetLastError());
        exit(1);
    }

    return h_input;
}


void configure_input_handle(HANDLE h_input) {
    DWORD handle_type = GetFileType(h_input);

    if (handle_type == FILE_TYPE_CHAR) {

        DWORD lpmode;
        GetConsoleMode(h_input, &lpmode);

        // Disable line input
        lpmode = lpmode &
                 ~ENABLE_LINE_INPUT &
                 ~ENABLE_ECHO_INPUT;

        // Only listen for character input events
        if (!SetConsoleMode(h_input, lpmode)) {
            printf("Unable to set console mode. %d", (int)GetLastError());
            exit(1);
        }

    } else if (handle_type == FILE_TYPE_PIPE) {
        // No need to do anything
    } else if (handle_type == FILE_TYPE_DISK) {
        printf("Don't know how to handle FILE_TYPE_DISK.");
        exit(1);
    } else {
        printf("Unknown input type.");
        exit(1);
    }
}


// If there's a complete line of text, put that line in buffer, and return the
// number of characters. Otherwise, return NULL.
char* get_line_nonblock(char* buf, int max_chars, HANDLE h_input) {
    // Check what type of thing we're reading from
    DWORD input_type = GetFileType(h_input);

    // Debugging info
    char* input_type_name;
    switch(input_type) {
        case FILE_TYPE_CHAR:
            input_type_name = "FILE_TYPE_CHAR (console)";
            break;
        case FILE_TYPE_DISK:
            input_type_name = "FILE_TYPE_DISK";
            break;
        case FILE_TYPE_PIPE:
            input_type_name = "FILE_TYPE_PIPE";
            break;
        default:
            input_type_name = "Unknown";
    }


    if (input_type == FILE_TYPE_CHAR) {
        // Attempt to read enough to fill the buffer
        DWORD num_peeked;
        INPUT_RECORD in_record_buf[WIN_INPUT_BUF_LEN];
        char input_char_buf[WIN_INPUT_BUF_LEN];
        int input_char_buf_n = 0;

        // First use PeekConsoleInput to make sure some char is available,
        // because ReadConsoleInput will block if there's no input.
        if (!PeekConsoleInput(h_input, in_record_buf, WIN_INPUT_BUF_LEN, &num_peeked)) {
            printf("Error peeking at console input.\n");
            return NULL;
        };

        if (num_peeked == 0) {
            return NULL;
        }

        bool found_newline = false;

        int i;
        for (i=0; i<num_peeked; i++) {
            // We're looking for key down events where the value is not 0.
            // (Special keys like Shift will have AsciiChar value of 0.)
            if (in_record_buf[i].EventType == KEY_EVENT &&
                in_record_buf[i].Event.KeyEvent.bKeyDown &&
                in_record_buf[i].Event.KeyEvent.uChar.AsciiChar != 0)
            {
                // Store the character in input_char_buf. If there's a \n, then
                // copy in_record_buf (up to the \n) to buf.
                char c = in_record_buf[i].Event.KeyEvent.uChar.AsciiChar;

                if (c == '\r') {
                    found_newline = true;
                    input_char_buf[input_char_buf_n] = '\n';
                    input_char_buf_n++;
                    break;
                } else {
                    input_char_buf[input_char_buf_n] = c;
                    input_char_buf_n++;
                }
            }
        }


        if (found_newline) {
            // This is the number of events up to and including the '\n'
            DWORD num_events_read = i+1;
            DWORD num_events_read2;
            // Clear out console buffer up to the '\n' event
            if (!ReadConsoleInput(h_input, in_record_buf, num_events_read , &num_events_read2)) {
                printf("Error reading console input.\n");
                return NULL;
            }

            // Place the content in buf
            snprintf(buf, MIN(input_char_buf_n, max_chars), "%s", input_char_buf);
            return buf;

        } else {
            return NULL;
        }

    } else if (input_type == FILE_TYPE_PIPE) {
        DWORD num_peeked;
        char input_char_buf[WIN_INPUT_BUF_LEN];
        int input_char_buf_n = 0;

        if (!PeekNamedPipe(h_input, input_char_buf, WIN_INPUT_BUF_LEN, &num_peeked, NULL, NULL)) {
            printf("Error peeking at pipe input. Error %d.\n", (unsigned)GetLastError());
            return NULL;
        };

        bool found_newline = false;
        for (int i=0; i<num_peeked; i++) {
            if (input_char_buf[i] == '\r' || input_char_buf[i] == '\n') {
                found_newline = true;
            }
            input_char_buf_n++;
        }

        DWORD num_read;
        if (found_newline) {
            // Clear out pipe
            if (!ReadFile(h_input, input_char_buf, input_char_buf_n, &num_read, NULL)) {
                printf("Error reading pipe input.\n");
                return NULL;
            }

            // Place the content in buf
            snprintf(buf, MIN(input_char_buf_n, max_chars), "%s", input_char_buf);
            return buf;

        } else {
            return NULL;
        }

    } else {
        printf("Unsupported input type: %s\n", input_type_name);
        exit(1);
    }

    return buf;
}


// Send Ctrl-C to a program if it has a console.
void sendCtrlC(int pid) {
    verbose_printf("Sending ctrl+c to pid %d", pid);
    FreeConsole();

    if (AttachConsole(pid)) {
        GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
    } else {
        verbose_printf("Error attaching to console for PID: %d\n", pid);
    }
}

// Callback function that closes a window if the PID matches the value passed
// in to lParam.
BOOL CALLBACK enumCloseWindowProc(_In_ HWND hwnd, LPARAM lParam) {
    DWORD current_pid = 0;

    GetWindowThreadProcessId(hwnd, &current_pid);

    if (current_pid == (DWORD) lParam) {
        PostMessage(hwnd, WM_CLOSE, 0, 0);
    }

    return true;
}

void sendWmClose(int pid) {
    EnumWindows(enumCloseWindowProc, (LPARAM)pid);
}

// Terminate process by pid.
BOOL kill_pid(DWORD dwProcessId) {
    HANDLE hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, dwProcessId);

    if (hProcess == NULL)
        return FALSE;

    BOOL result = TerminateProcess(hProcess, 1);

    CloseHandle(hProcess);

    return result;
}
