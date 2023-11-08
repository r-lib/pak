
#include <stdio.h>
#include <string.h>
#include <windows.h>

int main(int argc, const char **argv) {

  int ctrlbreak = 1;
  int pid;
  int ret;
  BOOL bret;

  if (argc == 1) return 1;
  ret = sscanf(argv[1], "%d", &pid);
  if (ret != 1) return 1;
  printf("Pid: %d\n", pid);

  if (argc == 3 && !strcmp(argv[2], "c")) ctrlbreak = 0;
  printf("Event: %s\n", ctrlbreak ? "ctrl+break" : "ctrl+c");

  printf("Free console\n");
  bret = FreeConsole();
  if (!bret) return GetLastError();

  printf("Attach console\n");
  bret = AttachConsole(pid);
  if (!bret) return GetLastError();

  printf("Set console ctrl handler\n");
  SetConsoleCtrlHandler(NULL, TRUE);

  printf("Send event\n");
  bret = GenerateConsoleCtrlEvent(
    ctrlbreak ? CTRL_BREAK_EVENT : CTRL_C_EVENT,
    0);
  if (!bret) return GetLastError();

  printf("Done\n");
  return 0;
}
