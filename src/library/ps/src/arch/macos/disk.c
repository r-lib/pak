#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/storage/IOBlockStorageDriver.h>
#include <IOKit/storage/IOMedia.h>
#include <IOKit/IOBSD.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "../../ps-internal.h"

SEXP ps__disk_io_counters(void) {
  CFDictionaryRef parent_dict;
  CFDictionaryRef props_dict;
  CFDictionaryRef stats_dict;
  io_registry_entry_t parent;
  io_registry_entry_t disk;
  io_iterator_t disk_list;

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 20));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, 20));
  int idx = 0;

  // Get list of disks
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  if (IOServiceGetMatchingServices(kIOMasterPortDefault,
                                   IOServiceMatching(kIOMediaClass),
                                   &disk_list) != kIOReturnSuccess) {
    ps__set_error("IOKIT unable to get the list of disks");
    goto error;
  }
# pragma GCC diagnostic pop

// Iterate over disks
  while ((disk = IOIteratorNext(disk_list)) != 0) {
    if (IORegistryEntryGetParentEntry(disk, kIOServicePlane, &parent)
        != kIOReturnSuccess) {
      ps__set_error("IOKIT: unable to get the disk's parent.");
      IOObjectRelease(disk);
      goto error;
    }

    if (IOObjectConformsTo(parent, "IOBlockStorageDriver")) {
      if (IORegistryEntryCreateCFProperties(
            disk,
            (CFMutableDictionaryRef *) &parent_dict,
            kCFAllocatorDefault,
            kNilOptions
          ) != kIOReturnSuccess) {
        ps__set_error("IOKIT: unable to get the parent's properties.");
        IOObjectRelease(disk);
        IOObjectRelease(parent);
        goto error;
      }

      if (IORegistryEntryCreateCFProperties(
            parent,
            (CFMutableDictionaryRef *) &props_dict,
            kCFAllocatorDefault,
            kNilOptions
          ) != kIOReturnSuccess) {
        ps__set_error("IOKIT: unable to get the disk properties.");
        CFRelease(props_dict);
        IOObjectRelease(disk);
        IOObjectRelease(parent);
        goto error;
      }

      const int kMaxDiskNameSize = 64;
      CFStringRef disk_name_ref = (CFStringRef)CFDictionaryGetValue(
        parent_dict, CFSTR(kIOBSDNameKey)
      );
      char disk_name[kMaxDiskNameSize];

      CFStringGetCString(
        disk_name_ref,
        disk_name,
        kMaxDiskNameSize,
        CFStringGetSystemEncoding()
      );

      stats_dict = (CFDictionaryRef)CFDictionaryGetValue(
        props_dict,
        CFSTR(kIOBlockStorageDriverStatisticsKey)
      );

      if (stats_dict == NULL) {
        ps__set_error("IOKIT: unable to get disk stats.");
        goto error;
      }

      CFNumberRef number;
      int64_t reads = 0;
      int64_t writes = 0;
      int64_t read_bytes = 0;
      int64_t write_bytes = 0;
      int64_t read_time = 0;
      int64_t write_time = 0;

      // Get disk reads/writes
      if ((number = (CFNumberRef)CFDictionaryGetValue(
           stats_dict,
           CFSTR(kIOBlockStorageDriverStatisticsReadsKey)))) {
        CFNumberGetValue(number, kCFNumberSInt64Type, &reads);
      }
      if ((number = (CFNumberRef)CFDictionaryGetValue(
           stats_dict,
           CFSTR(kIOBlockStorageDriverStatisticsWritesKey)))) {
        CFNumberGetValue(number, kCFNumberSInt64Type, &writes);
      }

      // Get disk bytes read/written
      if ((number = (CFNumberRef)CFDictionaryGetValue(
           stats_dict,
           CFSTR(kIOBlockStorageDriverStatisticsBytesReadKey)))) {
        CFNumberGetValue(number, kCFNumberSInt64Type, &read_bytes);
      }
      if ((number = (CFNumberRef)CFDictionaryGetValue(
           stats_dict,
           CFSTR(kIOBlockStorageDriverStatisticsBytesWrittenKey)))) {
        CFNumberGetValue(number, kCFNumberSInt64Type, &write_bytes);
      }

      // Get disk time spent reading/writing (nanoseconds)
      if ((number = (CFNumberRef)CFDictionaryGetValue(
           stats_dict,
           CFSTR(kIOBlockStorageDriverStatisticsTotalReadTimeKey)))) {
        CFNumberGetValue(number, kCFNumberSInt64Type, &read_time);
      }
      if ((number = (CFNumberRef)CFDictionaryGetValue(
           stats_dict,
           CFSTR(kIOBlockStorageDriverStatisticsTotalWriteTimeKey)))) {
        CFNumberGetValue(number, kCFNumberSInt64Type, &write_time);
      }

      SEXP di = ps__build_list(
        "KKKKKK", reads, writes, read_bytes, write_bytes,
        read_time / 1000 / 1000, write_time / 1000 / 1000
      );
      SET_VECTOR_ELT(res, idx, di);
      SET_STRING_ELT(nms, idx, Rf_mkCharCE(disk_name, CE_UTF8));
      idx++;

      CFRelease(parent_dict);
      IOObjectRelease(parent);
      CFRelease(props_dict);
      IOObjectRelease(disk);
    }
  }

  IOObjectRelease (disk_list);

  SET_NAMES(res, nms);

  UNPROTECT(2);
  return res;

error:
  ps__throw_error();
  return R_NilValue;
}
