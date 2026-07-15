"""
Build a zip with the STORED-comp_size=0 quirk:
  - A directory entry (method=STORED, comp_size=0, uncomp_size=non-zero)
  - A regular deflated file

This replicates what old Windows zip tools produced, e.g. PENSE_2009_microdados.zip.
Run from the repo root: python3 tools/make_stored_zero_compsize.py
"""
import struct, zlib, time

def le16(v): return struct.pack('<H', v)
def le32(v): return struct.pack('<I', v)

t = time.localtime(1391500718)  # 2014-02-04 ~08:58 UTC
mod_time = (t.tm_hour << 11) | (t.tm_min << 5) | (t.tm_sec // 2)
mod_date = ((t.tm_year - 1980) << 9) | (t.tm_mon << 5) | t.tm_mday

out = bytearray()
entries = []  # (fname_bytes, lhdr_offset, comp_size, uncomp_size, crc32, method)

# Entry 1: directory "subdir/" — comp_size=0, uncomp_size=20 (the quirk)
fname1 = b'subdir/'
lhdr1_ofs = len(out)
out += b'PK\x03\x04'
out += le16(20); out += le16(0); out += le16(0)   # ver, flags, method=STORED
out += le16(mod_time); out += le16(mod_date)
out += le32(0)    # crc32
out += le32(0)    # comp_size = 0  (the quirk)
out += le32(20)   # uncomp_size = 20 (non-zero, the quirk)
out += le16(len(fname1)); out += le16(0)
out += fname1
entries.append((fname1, lhdr1_ofs, 0, 20, 0, 0))

# Entry 2: file "subdir/hello.txt" with DEFLATE
fname2 = b'subdir/hello.txt'
content2 = b'Hello from a quirky zip file!\n'
crc2 = zlib.crc32(content2) & 0xFFFFFFFF
compressed2 = zlib.compress(content2)[2:-4]  # raw deflate
lhdr2_ofs = len(out)
out += b'PK\x03\x04'
out += le16(20); out += le16(0); out += le16(8)   # ver, flags, method=DEFLATE
out += le16(mod_time); out += le16(mod_date)
out += le32(crc2); out += le32(len(compressed2)); out += le32(len(content2))
out += le16(len(fname2)); out += le16(0)
out += fname2; out += compressed2
entries.append((fname2, lhdr2_ofs, len(compressed2), len(content2), crc2, 8))

# Central directory
cdir_ofs = len(out)
for fname, lhdr_ofs, comp_sz, uncomp_sz, crc32, method in entries:
    out += b'PK\x01\x02'
    out += le16(0); out += le16(20)               # ver made by, ver needed
    out += le16(0); out += le16(method)           # flags, method
    out += le16(mod_time); out += le16(mod_date)
    out += le32(crc32); out += le32(comp_sz); out += le32(uncomp_sz)
    out += le16(len(fname)); out += le16(0); out += le16(0)  # fname, extra, comment lens
    out += le16(0); out += le16(0); out += le32(0)           # disk, int attr, ext attr
    out += le32(lhdr_ofs)
    out += fname
cdir_size = len(out) - cdir_ofs

# End of central directory
out += b'PK\x05\x06'
out += le16(0); out += le16(0)                   # disk num, disk with cdir
out += le16(len(entries)); out += le16(len(entries))
out += le32(cdir_size); out += le32(cdir_ofs)
out += le16(0)                                   # comment len

dest = 'tests/testthat/fixtures/stored-zero-compsize.zip'
with open(dest, 'wb') as f:
    f.write(out)
print('Wrote', len(out), 'bytes to', dest)
