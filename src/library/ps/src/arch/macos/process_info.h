
#include "../../common.h"

typedef struct kinfo_proc kinfo_proc;

int ps__get_argmax(void);
int ps__get_kinfo_proc(long pid, struct kinfo_proc *kp);
int ps__get_proc_list(kinfo_proc **procList, size_t *procCount);
int ps__proc_pidinfo(
    long pid, int flavor, uint64_t arg, void *pti, int size);
SEXP ps__get_cmdline(long pid);
SEXP ps__get_environ(long pid);
