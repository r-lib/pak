# check_keyring_backend, file

    Code
      check_keyring_backend(keyring::default_backend())
    Condition
      Error in `check_keyring_backend()`:
      ! The default keyring (<tempfile>) does not exist, call `repo_auth_unlock()` to create it.

---

    Code
      repo_auth_unlock_internal(password = "secret")
    Message
      i Using keyring backend: file.
      i Creating default keyring.
      v Created default keyring: <tempfile>.

---

    Code
      check_keyring_backend(keyring::default_backend())
    Condition
      Error in `check_keyring_backend()`:
      ! The default keyring (<tempfile>) is locked. Call `repo_auth_unlock() to unlock it.

# check_keyring_backend, secret service

    Code
      check_keyring_backend(keyring::default_backend())
    Condition
      Error in `check_keyring_backend()`:
      ! The 'secret service' keyring backend only works on Linux.

---

    Code
      check_keyring_backend(keyring::default_backend())
    Condition
      Error in `check_keyring_backend()`:
      ! This pak build does not support the secret service keyring backend.

# repo_auth_key_get_internal, repo_auth_key_set_internal

    Code
      repo_auth_key_set_internal(url, "secret")
    Message
      i Using keyring backend: env.
      v Set keyring password for <https://username@cloud.r-project.org>.

---

    Code
      pw <- repo_auth_key_get_internal(url)
    Message
      i Using keyring backend: env.

# repo_auth_unlock_internal

    Code
      repo_auth_unlock_internal("pass")
    Message
      i Using keyring backend: env.
      v Keyring is already unlocked.

---

    Code
      repo_auth_unlock_internal("pass")
    Message
      i Using keyring backend: file.
      i Creating default keyring.
      v Created default keyring: <tempfile>.

---

    Code
      repo_auth_unlock_internal("pass")
    Message
      i Using keyring backend: file.

---

    Code
      repo_auth_unlock_internal("pass2")
    Message
      i Using keyring backend: file.
    Condition
      Error:
      ! keyring error (file-based keyring), cannot unlock keyring: The supplied password does not work.

