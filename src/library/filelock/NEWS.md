# filelock 1.0.3

* No user visible changes.

# filelock 1.0.2

* `lock()` now removes the timer on Unix, to avoid undefined behavior in
  non-interactive R sessions, when a SIGALRM is delivered after the process
  acquired the lock.

# filelock 1.0.1

First public release.
