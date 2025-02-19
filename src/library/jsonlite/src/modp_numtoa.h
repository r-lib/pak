/* -*- mode: c++; c-basic-offset: 4; indent-tabs-mode: nil; tab-width: 4 -*- */
/* vi: set expandtab shiftwidth=4 tabstop=4: */

/**
 * \file modp_numtoa.h
 * \brief Fast integer and floating-point numbers to string conversion
 *
 * This defines signed/unsigned integer, and 'double' to char buffer
 * converters.  The standard way of doing this is with "sprintf", however
 * these functions are
 *   * guarenteed maximum size output
 *   * 5-20x faster!
 *   * Won't core-dump
 *
 * Note: this file is under and MIT license
 */


/**
 * <pre>
 * Copyright &copy; 2007, Nick Galbreath -- nickg [at] client9 [dot] com
 * All rights reserved.
 * http://code.google.com/p/stringencoders/
 * Released under the MIT license.
 * </pre>
 *
 */

#ifndef COM_MODP_STRINGENCODERS_NUMTOA_H
#define COM_MODP_STRINGENCODERS_NUMTOA_H

//#include "extern_c_begin.h"

#include <string.h>
#include <stdint.h>
#include <stdbool.h>

/** \brief convert an signed integer to char buffer
 *
 * \param[in] value
 * \param[out] buf the output buffer.  Should be 16 chars or more.
 */
size_t modp_itoa10(int32_t value, char* buf);

/** \brief convert an unsigned integer to char buffer
 *
 * \param[in] value
 * \param[out] buf The output buffer, should be 16 chars or more.
 */
size_t modp_uitoa10(uint32_t value, char* buf);

/** \brief convert an signed long integer to char buffer
 *
 * \param[in] value
 * \param[out] buf the output buffer.  Should be 24 chars or more.
 */
size_t modp_litoa10(int64_t value, char* buf);

/** \brief convert an unsigned long integer to char buffer
 *
 * \param[in] value
 * \param[out] buf The output buffer, should be 24 chars or more.
 */
size_t modp_ulitoa10(uint64_t value, char* buf);

/** \brief convert a floating point number to char buffer with
 *         fixed-precision format
 *
 * This is similar to "%.[0-9]f" in the printf style.  It will include
 * trailing zeros
 *
 * If the input value is greater than 1<<31, then the output format
 * will be switched exponential format.
 *
 * \param[in] value
 * \param[out] buf  The allocated output buffer.  Should be 32 chars or more.
 * \param[in] precision  Number of digits to the right of the decimal point.
 *    Can only be 0-9.
 */
size_t modp_dtoa(double value, char* buf, int precision);

/** \brief convert a floating point number to char buffer with a
 *         variable-precision format, and no trailing zeros
 *
 * This is similar to "%.[0-9]f" in the printf style, except it will
 * NOT include trailing zeros after the decimal point.  This type
 * of format oddly does not exists with printf.
 *
 * If the input value is greater than 1<<31, then the output format
 * will be switched exponential format.
 *
 * \param[in] value
 * \param[out] buf  The allocated output buffer.  Should be 32 chars or more.
 * \param[in] precision  Number of digits to the right of the decimal point.
 *    Can only be 0-9.
 */
size_t modp_dtoa2(double value, char* buf, int precision);

/**
 * adds a 8-character hexadecimal representation of value
 *
 */
char* modp_uitoa16(uint32_t value, char* buf, int final);

//#include "extern_c_end.h"

#endif
