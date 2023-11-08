
// Fixes clash between winsock2.h and windows.h
#define WIN32_LEAN_AND_MEAN

#include "common.h"
#include "windows.h"
#include <winsock2.h>
#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
#include <ws2tcpip.h>
#endif
#include <wincrypt.h>
#include <iphlpapi.h>
#include <stdlib.h>

#define BYTESWAP_USHORT(x) ((((USHORT)(x) << 8) | ((USHORT)(x) >> 8)) & 0xffff)
#ifndef AF_INET6
#define AF_INET6 23
#endif

static DWORD MyGetExtendedTcpTable(ULONG address_family,
                                   PVOID * data, DWORD * size) {

  // Due to other processes being active on the machine, it's possible
  // that the size of the table increases between the moment where we
  // query the size and the moment where we query the data.  Therefore, it's
  // important to call this in a loop to retry if that happens.
  //
  // Also, since we may loop a theoretically unbounded number of times here,
  // release the GIL while we're doing this.
  DWORD error = ERROR_INSUFFICIENT_BUFFER;
  *size = 0;
  *data = NULL;
  error = GetExtendedTcpTable(NULL, size, FALSE, address_family,
			      TCP_TABLE_OWNER_PID_ALL, 0);
  while (error == ERROR_INSUFFICIENT_BUFFER) {
    *data = malloc(*size);
    if (*data == NULL) {
      error = ERROR_NOT_ENOUGH_MEMORY;
      continue;
    }
    error = GetExtendedTcpTable(*data, size, FALSE, address_family,
				TCP_TABLE_OWNER_PID_ALL, 0);
    if (error != NO_ERROR) {
      free(*data);
      *data = NULL;
    }
  }
  return error;
}

static DWORD MyGetExtendedUdpTable(ULONG address_family,
                                   PVOID * data, DWORD * size) {

  // Due to other processes being active on the machine, it's possible
  // that the size of the table increases between the moment where we
  // query the size and the moment where we query the data.  Therefore, it's
  // important to call this in a loop to retry if that happens.
  //
  // Also, since we may loop a theoretically unbounded number of times here,
  // release the GIL while we're doing this.
  DWORD error = ERROR_INSUFFICIENT_BUFFER;
  *size = 0;
  *data = NULL;
  error = GetExtendedUdpTable(NULL, size, FALSE, address_family,
			      UDP_TABLE_OWNER_PID, 0);
  while (error == ERROR_INSUFFICIENT_BUFFER) {
    *data = malloc(*size);
    if (*data == NULL) {
      error = ERROR_NOT_ENOUGH_MEMORY;
      continue;
    }
    error = GetExtendedUdpTable(*data, size, FALSE, address_family,
				UDP_TABLE_OWNER_PID, 0);
    if (error != NO_ERROR) {
      free(*data);
      *data = NULL;
    }
  }
  return error;
}

SEXP psll_connections(SEXP p) {

  static long null_address[4] = { 0, 0, 0, 0 };
  unsigned long pid;
  typedef PSTR (NTAPI * _RtlIpv4AddressToStringA)(struct in_addr *, PSTR);
  _RtlIpv4AddressToStringA rtlIpv4AddressToStringA;
  typedef PSTR (NTAPI * _RtlIpv6AddressToStringA)(struct in6_addr *, PSTR);
  _RtlIpv6AddressToStringA rtlIpv6AddressToStringA;
  PVOID table = NULL;
  DWORD tableSize;
  DWORD err;
  PMIB_TCPTABLE_OWNER_PID tcp4Table;
  PMIB_UDPTABLE_OWNER_PID udp4Table;
  PMIB_TCP6TABLE_OWNER_PID tcp6Table;
  PMIB_UDP6TABLE_OWNER_PID udp6Table;
  ULONG i;
  CHAR addressBufferLocal[65];
  CHAR addressBufferRemote[65];

  SEXP retlist;
  PROTECT_INDEX ret_idx;
  int ret_len = 10, ret_num = -1;

  SEXP conn;
  char *addr_local = NULL, *addr_remote = NULL;
  int port_local = 0, port_remote = 0;
  char *empty_string = "";

  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");
  pid = handle->pid;

  // Import some functions.
  {
    HMODULE ntdll;

    ntdll = LoadLibrary(TEXT("ntdll.dll"));
    rtlIpv4AddressToStringA = (_RtlIpv4AddressToStringA)GetProcAddress(
      ntdll, "RtlIpv4AddressToStringA");
    rtlIpv6AddressToStringA = (_RtlIpv6AddressToStringA)GetProcAddress(
      ntdll, "RtlIpv6AddressToStringA");
    /* TODO: Check these two function pointers */
    FreeLibrary(ntdll);
  }

  PROTECT_WITH_INDEX(retlist = allocVector(VECSXP, ret_len), &ret_idx);

  // TCP IPv4

  table = NULL;
  conn = R_NilValue;
  addr_local = empty_string;
  addr_remote = empty_string;
  port_local = NA_INTEGER;
  port_remote = NA_INTEGER;
  tableSize = 0;

  err = MyGetExtendedTcpTable(AF_INET, &table, &tableSize);
  if (err == ERROR_NOT_ENOUGH_MEMORY) {
    ps__no_memory("");
    ps__throw_error();
  }

  if (err == NO_ERROR) {
    tcp4Table = table;

    for (i = 0; i < tcp4Table->dwNumEntries; i++) {

      if (tcp4Table->table[i].dwOwningPid != pid) continue;

      if (tcp4Table->table[i].dwLocalAddr != 0 ||
	  tcp4Table->table[i].dwLocalPort != 0) {
	struct in_addr addr;

	addr.S_un.S_addr = tcp4Table->table[i].dwLocalAddr;
	rtlIpv4AddressToStringA(&addr, addressBufferLocal);
	addr_local = addressBufferLocal;
	port_local = BYTESWAP_USHORT(tcp4Table->table[i].dwLocalPort);
      }

      // On Windows <= XP, remote addr is filled even if socket
      // is in LISTEN mode in which case we just ignore it.
      if ((tcp4Table->table[i].dwRemoteAddr != 0 ||
	   tcp4Table->table[i].dwRemotePort != 0) &&
	  (tcp4Table->table[i].dwState != MIB_TCP_STATE_LISTEN)) {
	struct in_addr addr;

	addr.S_un.S_addr = tcp4Table->table[i].dwRemoteAddr;
	rtlIpv4AddressToStringA(&addr, addressBufferRemote);
	addr_remote = addressBufferRemote;
	port_remote = BYTESWAP_USHORT(tcp4Table->table[i].dwRemotePort);
      }

      PROTECT(conn = ps__build_list("iiisisii",
        NA_INTEGER, AF_INET, SOCK_STREAM, addr_local, port_local, addr_remote,
        port_remote, tcp4Table->table[i].dwState));

      if (++ret_num == ret_len) {
	ret_len *= 2;
	REPROTECT(retlist = Rf_lengthgets(retlist, ret_len), ret_idx);
      }
      SET_VECTOR_ELT(retlist, ret_num, conn);
      UNPROTECT(1);
    }
  } else {
    ps__set_error_from_windows_error(err);
    free(table);
    ps__throw_error();
  }

  free(table);
  table = NULL;
  tableSize = 0;

  // TCP IPv6

  table = NULL;
  conn = R_NilValue;
  addr_local = empty_string;
  addr_remote = empty_string;
  port_local = NA_INTEGER;
  port_remote = NA_INTEGER;
  tableSize = 0;

  err = MyGetExtendedTcpTable(AF_INET6, &table, &tableSize);
  if (err == ERROR_NOT_ENOUGH_MEMORY) {
    ps__no_memory("");
    ps__throw_error();
  }

  if (err == NO_ERROR) {
    tcp6Table = table;

    for (i = 0; i < tcp6Table->dwNumEntries; i++) {

      if (tcp6Table->table[i].dwOwningPid != pid) continue;

      if (memcmp(tcp6Table->table[i].ucLocalAddr, null_address, 16) != 0 ||
	  tcp6Table->table[i].dwLocalPort != 0) {
	struct in6_addr addr;

	memcpy(&addr, tcp6Table->table[i].ucLocalAddr, 16);
	rtlIpv6AddressToStringA(&addr, addressBufferLocal);
	addr_local = addressBufferLocal;
	port_local = BYTESWAP_USHORT(tcp6Table->table[i].dwLocalPort);
      }

      // On Windows <= XP, remote addr is filled even if socket
      // is in LISTEN mode in which case we just ignore it.
      if ((memcmp(tcp6Table->table[i].ucRemoteAddr, null_address, 16) != 0 ||
	   tcp6Table->table[i].dwRemotePort != 0) &&
	  (tcp6Table->table[i].dwState != MIB_TCP_STATE_LISTEN)) {
	struct in6_addr addr;

	memcpy(&addr, tcp6Table->table[i].ucRemoteAddr, 16);
	rtlIpv6AddressToStringA(&addr, addressBufferRemote);
	addr_remote = addressBufferRemote;
	port_remote = BYTESWAP_USHORT(tcp6Table->table[i].dwRemotePort);
      }

      PROTECT(conn = ps__build_list("iiisisii",
        NA_INTEGER, AF_INET6, SOCK_STREAM, addr_local, port_local, addr_remote,
        port_remote, tcp6Table->table[i].dwState));

      if (++ret_num == ret_len) {
	ret_len *= 2;
	REPROTECT(retlist = Rf_lengthgets(retlist, ret_len), ret_idx);
      }
      SET_VECTOR_ELT(retlist, ret_num, conn);
      UNPROTECT(1);
    }
  } else {
    ps__set_error_from_windows_error(err);
    free(table);
    ps__throw_error();
  }

  free(table);
  table = NULL;
  tableSize = 0;

  // UDP IPv4

  table = NULL;
  conn = R_NilValue;
  addr_local = empty_string;
  addr_remote = empty_string;
  port_local = NA_INTEGER;
  port_remote = NA_INTEGER;
  tableSize = 0;

  err = MyGetExtendedUdpTable(AF_INET, &table, &tableSize);
  if (err == ERROR_NOT_ENOUGH_MEMORY) {
    ps__no_memory("");
    ps__throw_error();
  }

  if (err == NO_ERROR) {
    udp4Table = table;

    for (i = 0; i < udp4Table->dwNumEntries; i++) {
      if (udp4Table->table[i].dwOwningPid != pid) continue;

      if (udp4Table->table[i].dwLocalAddr != 0 ||
	  udp4Table->table[i].dwLocalPort != 0) {
	struct in_addr addr;

	addr.S_un.S_addr = udp4Table->table[i].dwLocalAddr;
	rtlIpv4AddressToStringA(&addr, addressBufferLocal);
	addr_local = addressBufferLocal;
	port_local = BYTESWAP_USHORT(udp4Table->table[i].dwLocalPort);
      }

      PROTECT(conn = ps__build_list("iiisisii",
        NA_INTEGER, AF_INET, SOCK_DGRAM, addr_local, port_local, addr_remote,
        port_remote, PS__CONN_NONE));

      if (++ret_num == ret_len) {
	ret_len *= 2;
	REPROTECT(retlist = Rf_lengthgets(retlist, ret_len), ret_idx);
      }
      SET_VECTOR_ELT(retlist, ret_num, conn);
      UNPROTECT(1);
    }
  } else {
    ps__set_error_from_windows_error(err);
    free(table);
    ps__throw_error();
  }

  free(table);
  table = NULL;
  tableSize = 0;

  // UDP IPv6

  table = NULL;
  conn = R_NilValue;
  addr_local = empty_string;
  addr_remote = empty_string;
  port_local = NA_INTEGER;
  port_remote = NA_INTEGER;
  tableSize = 0;

  err = MyGetExtendedUdpTable(AF_INET6, &table, &tableSize);
  if (err == ERROR_NOT_ENOUGH_MEMORY) {
    ps__no_memory("");
    ps__throw_error();
  }

  if (err == NO_ERROR) {
    udp6Table = table;

    for (i = 0; i < udp6Table->dwNumEntries; i++) {
      if (udp6Table->table[i].dwOwningPid != pid) continue;

      if (memcmp(udp6Table->table[i].ucLocalAddr, null_address, 16) != 0 ||
	  udp6Table->table[i].dwLocalPort != 0) {
	struct in6_addr addr;

	memcpy(&addr, udp6Table->table[i].ucLocalAddr, 16);
	rtlIpv6AddressToStringA(&addr, addressBufferLocal);
	addr_local = addressBufferLocal;
	port_local = BYTESWAP_USHORT(udp6Table->table[i].dwLocalPort);
      }

      PROTECT(conn = ps__build_list("iiisisii",
        NA_INTEGER, AF_INET6, SOCK_DGRAM, addr_local, port_local, addr_remote,
        port_remote, PS__CONN_NONE));

      if (++ret_num == ret_len) {
	ret_len *= 2;
	REPROTECT(retlist = Rf_lengthgets(retlist, ret_len), ret_idx);
      }
      SET_VECTOR_ELT(retlist, ret_num, conn);
      UNPROTECT(1);
    }
  } else {
    ps__set_error_from_windows_error(err);
    free(table);
    ps__throw_error();
  }

  free(table);
  table = NULL;
  tableSize = 0;

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return retlist;
}
