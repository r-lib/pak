/*
 * Copyright (c) 2009, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *
 * Functions related to the Windows Management Instrumentation API.
 */

#include "../../common.h"
#include "../../windows.h"

#include <windows.h>
#include <pdh.h>

// We use an exponentially weighted moving average, just like Unix systems do
// https://en.wikipedia.org/wiki/Load_(computing)#Unix-style_load_calculation
//
// These constants serve as the damping factor and are calculated with
// 1 / exp(sampling interval in seconds / window size in seconds)
//
// This formula comes from linux's include/linux/sched/loadavg.h
// https://github.com/torvalds/linux/blob/345671ea0f9258f410eb057b9ced9cefbbe5dc78/include/linux/sched/loadavg.h#L20-L23
#define LOADAVG_FACTOR_1F  0.9200444146293232478931553241
#define LOADAVG_FACTOR_5F  0.9834714538216174894737477501
#define LOADAVG_FACTOR_15F 0.9944598480048967508795473394
// The time interval in seconds between taking load counts, same as Linux
#define SAMPLING_INTERVAL 5

double load_avg_1m = 0;
double load_avg_5m = 0;
double load_avg_15m = 0;
int load_avg_inited = 0;

VOID CALLBACK LoadAvgCallback(PVOID hCounter, BOOLEAN timedOut) {
  PDH_FMT_COUNTERVALUE displayValue;
  double currentLoad;
  PDH_STATUS err;

  err = PdhGetFormattedCounterValue(
    (PDH_HCOUNTER)hCounter, PDH_FMT_DOUBLE, 0, &displayValue);
  // Skip updating the load if we can't get the value successfully
  if (err != ERROR_SUCCESS) {
    return;
  }
  currentLoad = displayValue.doubleValue;

  load_avg_1m = load_avg_1m * LOADAVG_FACTOR_1F + currentLoad * \
    (1.0 - LOADAVG_FACTOR_1F);
  load_avg_5m = load_avg_5m * LOADAVG_FACTOR_5F + currentLoad * \
    (1.0 - LOADAVG_FACTOR_5F);
  load_avg_15m = load_avg_15m * LOADAVG_FACTOR_15F + currentLoad * \
    (1.0 - LOADAVG_FACTOR_15F);
}


void ps__init_loadavg_counter(SEXP counter_name) {
  WCHAR *szCounterPath = NULL;
  PDH_STATUS s;
  BOOL ret;
  HQUERY hQuery;
  HCOUNTER hCounter;
  HANDLE event;
  HANDLE waitHandle;

  ps__utf8_to_utf16(CHAR(STRING_ELT(counter_name, 0)), &szCounterPath);

  if ((PdhOpenQueryW(NULL, 0, &hQuery)) != ERROR_SUCCESS) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  s = PdhAddCounterW(hQuery, szCounterPath, 0, &hCounter);
  if (s != ERROR_SUCCESS) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  event = CreateEventW(NULL, FALSE, FALSE, L"LoadUpdateEvent");
  if (event == NULL) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  s = PdhCollectQueryDataEx(hQuery, SAMPLING_INTERVAL, event);
  if (s != ERROR_SUCCESS) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  ret = RegisterWaitForSingleObject(
    &waitHandle,
    event,
    (WAITORTIMERCALLBACK)LoadAvgCallback,
    (PVOID)
    hCounter,
    INFINITE,
    WT_EXECUTEDEFAULT);

  if (ret == 0) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  load_avg_inited = 1;
}


/*
 * Gets the emulated 1 minute, 5 minute and 15 minute load averages
 * (processor queue length) for the system.
 * `init_loadavg_counter` must be called before this function to engage the
 * mechanism that records load values.
 */

void ps__get_loadavg(double avg[3], SEXP counter_name) {
  if (!load_avg_inited) ps__init_loadavg_counter(counter_name);
  avg[0] = load_avg_1m;
  avg[1] = load_avg_5m;
  avg[2] = load_avg_15m;
}
