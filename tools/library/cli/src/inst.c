#include <Rinternals.h>
#include <cli/progress.h>

// This file only tests that `inst/cli/progress.h` can be compiled
// without issues

void test(void) {
  cli_progress_bar(0, R_NilValue);
}
