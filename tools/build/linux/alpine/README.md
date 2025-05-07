
# (Re)building Alpine packages

This is needed because some packages are built without `-fPIC` and this is
a problem when the static libs are included in shared libraries.

For Alpine 3.19 I need to rebuilt these packages:

- gettext-static
- libgpg-error-static
- libffi-dev
- libgcrypt-static

## Steps

1. Start an Alpine container
   ```
   docker run -it -v gaborcsardi-673e2275.rsa.pub:/etc/apk/keys/gaborcsardi-673e2275.rsa.pub sh
   ```
2. Create a user
   ```
   adduser gaborcsardi
   ```
   
