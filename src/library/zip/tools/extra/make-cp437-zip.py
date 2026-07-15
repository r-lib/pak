#!/usr/bin/env python3
# Creates tests/testthat/fixtures/cp437.zip: a ZIP with one entry whose
# filename is encoded in IBM CP437 (bit 11 of the general purpose flag is
# NOT set).  The byte 0xa4 in CP437 maps to U+00F1 (n with tilde, ñ),
# so the filename decodes to "catalñn.txt".

import struct
import zlib
import os

filename = b"catal\xa4n.txt"  # CP437: 0xa4 = ñ
content = b"CP437 filename test\n"
crc = zlib.crc32(content) & 0xFFFFFFFF

# Local file header
lfh = struct.pack(
    "<IHHHHHIIIHH",
    0x04034B50,  # signature
    20,          # version needed to extract
    0x0000,      # general purpose bit flag: bit 11 NOT set → CP437
    0,           # compression method: stored
    0, 0,        # last mod time, date
    crc,
    len(content),
    len(content),
    len(filename),
    0,           # extra field length
)

# Central directory file header
cdh = struct.pack(
    "<IHHHHHHIIIHHHHHII",
    0x02014B50,  # signature
    20,          # version made by
    20,          # version needed
    0x0000,      # flags
    0,           # compression
    0, 0,        # mod time, date
    crc,
    len(content),
    len(content),
    len(filename),
    0, 0, 0,     # extra, comment, disk start
    0,           # internal attributes
    0,           # external attributes
    0,           # relative offset of local header
)

lf_size = len(lfh) + len(filename) + len(content)
cd_size = len(cdh) + len(filename)

# End of central directory record
eocd = struct.pack(
    "<IHHHHIIH",
    0x06054B50,  # signature
    0, 0,        # disk number, disk with start of CD
    1, 1,        # entries on this disk, total entries
    cd_size,
    lf_size,
    0,           # comment length
)

data = lfh + filename + content + cdh + filename + eocd

out = os.path.join(
    os.path.dirname(__file__),
    "../../tests/testthat/fixtures/cp437.zip",
)
out = os.path.normpath(out)
with open(out, "wb") as f:
    f.write(data)

print(f"Wrote {len(data)} bytes to {out}")
print(f"Filename bytes: {filename.hex()}  (0xa4 = ñ in CP437)")
