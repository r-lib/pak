
#include <stdio.h>
#include <stdlib.h>

#include <processx/unix-sockets.c>

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <socket-file>", argv[0]);
  }

  processx_socket_t sock;
#ifdef _WIN32
  const char *prefix = "\\\\?\\pipe\\";
  char name[1024] = {0};
  strncpy(name, prefix, sizeof(name) - 1);
  strncat(name, argv[1], sizeof(name) - 1);
#else
  const char *name = argv[1];
#endif
  int ret = processx_socket_connect(name, &sock);
  if (ret == -1) {
    fprintf(
      stderr,
      "Failed to connect to socket at '%s': %s\n",
      argv[1],
      processx_socket_error_message()
    );
    exit(1);
  }
  fprintf(stderr, "Connected\n");

  char buffer[1024];
  ssize_t nbytes = processx_socket_read(&sock, buffer, sizeof(buffer));
  if (nbytes == -1) {
    fprintf(
      stderr,
      "Failed to read from server socket: %s\n",
      processx_socket_error_message()
    );
    exit(2);
  }

  buffer[nbytes] = '\0';
  printf("Message from server: %s\n", buffer);

  const char *msg = "hello there!";
  nbytes = processx_socket_write(&sock, (void*) msg, strlen(msg));
  if (nbytes == -1) {
    fprintf(
      stderr,
      "Failed to write to server socket: %s\n",
      processx_socket_error_message()
    );
    exit(3);
  }

  ret = processx_socket_close(&sock);
  if (ret == -1) {
    fprintf(
      stderr,
      "Failed to close client socket: %s\n",
      processx_socket_error_message()
    );
    exit(4);
  }

  return 0;
}
