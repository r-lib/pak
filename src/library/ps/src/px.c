
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <stdlib.h>

void usage(void) {
  fprintf(stderr, "Usage: px [command arg] [command arg] ...\n\n");
  fprintf(stderr, "Commands:\n");
  fprintf(stderr, "  sleep  <seconds>           -- "
	  "sleep for a number os seconds\n");
  fprintf(stderr, "  out    <string>            -- "
	  "print string to stdout\n");
  fprintf(stderr, "  err    <string>            -- "
	  "print string to stderr\n");
  fprintf(stderr, "  outln  <string>            -- "
	  "print string to stdout, add newline\n");
  fprintf(stderr, "  errln  <string>            -- "
	  "print string to stderr, add newline\n");
  fprintf(stderr, "  cat    <filename>          -- "
	  "print file to stdout\n");
  fprintf(stderr, "  return <exitcode>          -- "
	  "return with exitcode\n");
  fprintf(stderr, "  write <fd> <string>        -- "
	  "write to file descriptor\n");
  fprintf(stderr, "  echo <fd1> <fd2> <nbytes>  -- "
	  "echo from fd to another fd\n");
  fprintf(stderr, "  getenv <var>               -- "
	  "environment variable to stdout\n");
}

void cat2(int f, const char *s) {
  char buf[8192];
  long n;

  while ((n = read(f, buf, (long) sizeof buf)) > 0) {
    if (write(1, buf, n) != n){
      fprintf(stderr, "write error copying %s", s);
      exit(6);
    }
  }

  if (n < 0) fprintf(stderr, "error reading %s", s);
}

void cat(const char* filename) {
  int f = open(filename, O_RDONLY);

  if (f < 0) {
    fprintf(stderr, "can't open %s", filename);
    exit(6);
  }

  cat2(f, filename);
  close(f);
}

int write_to_fd(int fd, const char *s) {
  size_t len = strlen(s);
  ssize_t ret = write(fd, s, len);
  if (ret != len) {
    fprintf(stderr, "Cannot write to fd '%d'\n", fd);
    return 1;
  }
  return 0;
}

int echo_from_fd(int fd1, int fd2, int nbytes) {
  char buffer[nbytes + 1];
  ssize_t ret;
  buffer[nbytes] = '\0';
  ret = read(fd1, buffer, nbytes);
  if (ret == -1) {
    fprintf(stderr, "Cannot read from fd '%d', %s\n", fd1, strerror(errno));
    return 1;
  }
  if (ret != nbytes) {
    fprintf(stderr, "Cannot read from fd '%d' (%d bytes)\n", fd1, (int) ret);
    return 1;
  }
  if (write_to_fd(fd2, buffer)) return 1;
  fflush(stdout);
  fflush(stderr);
  return 0;
}

int main(int argc, const char **argv) {

  int num, idx, ret, fd, fd2, nbytes;
  double fnum;

  if (argc == 2 && !strcmp("--help", argv[1])) { usage(); return 0; }

  for (idx = 1; idx < argc; idx++) {
    const char *cmd = argv[idx];

    if (idx + 1 == argc) {
      fprintf(stderr, "Missing argument for '%s'\n", argv[idx]);
      return 5;
    }

    if (!strcmp("sleep", cmd)) {
      ret = sscanf(argv[++idx], "%lf", &fnum);
      if (ret != 1) {
	fprintf(stderr, "Invalid seconds for px sleep: '%s'\n", argv[idx]);
	return 3;
      }
      num = (int) fnum;
      sleep(num);
      fnum = fnum - num;
      if (fnum > 0) usleep((useconds_t) (fnum * 1000.0 * 1000.0));

    } else if (!strcmp("out", cmd)) {
      printf("%s", argv[++idx]);
      fflush(stdout);

    } else if (!strcmp("err", cmd)) {
      fprintf(stderr, "%s", argv[++idx]);

    } else if (!strcmp("outln", cmd)) {
      printf("%s\n", argv[++idx]);
      fflush(stdout);

    } else if (!strcmp("errln", cmd)) {
      fprintf(stderr, "%s\n", argv[++idx]);

    } else if (!strcmp("cat", cmd)) {
      cat(argv[++idx]);

    } else if (!strcmp("return", cmd)) {
      ret = sscanf(argv[++idx], "%d", &num);
      if (ret != 1) {
	fprintf(stderr, "Invalid exit code for px return: '%s'\n", argv[idx]);
	return 4;
      }
      return num;

    } else if (!strcmp("write", cmd)) {
      if (idx + 2 >= argc) {
	fprintf(stderr, "Missing argument(s) for 'write'\n");
	return 5;
      }
      ret = sscanf(argv[++idx], "%d", &fd);
      if (ret != 1) {
	fprintf(stderr, "Invalid fd for write: '%s'\n", argv[idx]);
	return 6;
      }
      if (write_to_fd(fd, argv[++idx])) return 7;

    } else if (!strcmp("echo", cmd)) {
      if (idx + 3 >= argc) {
	fprintf(stderr, "Missing argument(s) for 'read'\n");
	return 7;
      }
      ret = sscanf(argv[++idx], "%d", &fd);
      ret = ret + sscanf(argv[++idx], "%d", &fd2);
      ret = ret + sscanf(argv[++idx], "%d", &nbytes);
      if (ret != 3) {
	fprintf(stderr, "Invalid fd1, fd2 or nbytes for read: '%s', '%s', '%s'\n",
		argv[idx-2],  argv[idx-1], argv[idx]);
	return 8;
      }
      if (echo_from_fd(fd, fd2, nbytes)) return 9;

    } else if (!strcmp("getenv", cmd)) {
      printf("%s\n", getenv(argv[++idx]));
      fflush(stdout);

    } else {
      fprintf(stderr, "Unknown px command: '%s'\n", cmd);
      return 2;
    }
  }

  return 0;
}
