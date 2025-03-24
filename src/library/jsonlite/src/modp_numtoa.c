/* -*- mode: c; c-basic-offset: 4; indent-tabs-mode: nil; tab-width: 4 -*- */
/* vi: set expandtab shiftwidth=4 tabstop=4: */



#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

/*
 * other interesting references on num to string convesion
 * http://www.jb.man.ac.uk/~slowe/cpp/itoa.html
 * and http://www.ddj.com/dept/cpp/184401596?pgno=6
 *
 * Version 19-Nov-2007
 * Fixed round-to-even rules to match printf
 *   thanks to Johannes Otepka
 * Version 22-Sep-2016
 *  Fixed rounding error with decimals ending in 5 and low precision
 */

/**
 * Powers of 10
 * 10^0 to 10^9
 */
static const double powers_of_10[] = {1, 10, 100, 1000, 10000, 100000, 1000000,
	10000000, 100000000, 1000000000};


static void strreverse(char* begin, char* end)
{
	char aux;
	while (end > begin)
		aux = *end, *end-- = *begin, *begin++ = aux;
}

size_t modp_itoa10(int32_t value, char* str)
{
	char* wstr=str;
	/* Take care of sign */
	uint32_t uvalue = (value < 0) ? (uint32_t)(-value) : (uint32_t)(value);
	/* Conversion. Number is reversed. */
	do *wstr++ = (char)(48 + (uvalue % 10)); while(uvalue /= 10);
	if (value < 0) *wstr++ = '-';
	*wstr='\0';

	/* Reverse string */
	strreverse(str,wstr-1);
	return (size_t)(wstr - str);
}

size_t modp_uitoa10(uint32_t value, char* str)
{
	char* wstr=str;
	/* Conversion. Number is reversed. */
	do *wstr++ = (char)(48 + (value % 10)); while (value /= 10);
	*wstr='\0';
	/* Reverse string */
	strreverse(str, wstr-1);
	return (size_t)(wstr - str);
}

size_t modp_litoa10(int64_t value, char* str)
{
	char* wstr=str;
	uint64_t uvalue = (value < 0) ? (uint64_t)(-value) : (uint64_t)(value);

	/* Conversion. Number is reversed. */
	do *wstr++ = (char)(48 + (uvalue % 10)); while(uvalue /= 10);
	if (value < 0) *wstr++ = '-';
	*wstr='\0';

	/* Reverse string */
	strreverse(str,wstr-1);
	return (size_t)(wstr - str);
}

size_t modp_ulitoa10(uint64_t value, char* str)
{
	char* wstr=str;
	/* Conversion. Number is reversed. */
	do *wstr++ = (char)(48 + (value % 10)); while (value /= 10);
	*wstr='\0';
	/* Reverse string */
	strreverse(str, wstr-1);
	return (size_t)(wstr - str);
}

size_t modp_dtoa(double value, char* str, int prec)
{
	/* Hacky test for NaN
	 * under -fast-math this won't work, but then you also won't
	 * have correct nan values anyways.  The alternative is
	 * to link with libmath (bad) or hack IEEE double bits (bad)
	 */
	if (! (value == value)) {
		str[0] = 'n'; str[1] = 'a'; str[2] = 'n'; str[3] = '\0';
		return (size_t)3;
	}
	/* if input is larger than thres_max, revert to exponential */
	const double thres_max = (double)(0x7FFFFFFF);

	double diff = 0.0;
	char* wstr = str;

	if (prec < 0) {
		prec = 0;
	} else if (prec > 9) {
		/* precision of >= 10 can lead to overflow errors */
		prec = 9;
	}


	/* we'll work in positive values and deal with the
	   negative sign issue later */
	int neg = 0;
	if (value < 0) {
		neg = 1;
		value = -value;
	}

	// given 0.05, prec=1
	// whole = 0
	// tmp = (0.05)* 10 = 0.5
	// frac = 0
	// diff = tmp -frac == 0.5 - 0.0 = 0.5
	//
	int whole = (int) value;
	double tmp = (value - whole) * powers_of_10[prec];
	uint32_t frac = (uint32_t)(tmp);
	diff = tmp - frac;

	if (diff > 0.5) {
		++frac;
		/* handle rollover, e.g.  case 0.99 with prec 1 is 1.0  */
		if (frac >= powers_of_10[prec]) {
			frac = 0;
			++whole;
		}
	} else if (diff == 0.5 && prec > 0 && (frac & 1)) {
		/* if halfway, round up if odd, OR
		   if last digit is 0.  That last part is strange */
		++frac;
		if (frac >= powers_of_10[prec]) {
			frac = 0;
			++whole;
		}
	} else if (diff == 0.5 && prec == 0 && (whole & 1)) {
		++frac;
		if (frac >= powers_of_10[prec]) {
			frac = 0;
			++whole;
		}
	}

	/* for very large numbers switch back to native sprintf for exponentials.
	   anyone want to write code to replace this? */
	/*
	   normal printf behavior is to print EVERY whole number digit
	   which can be 100s of characters overflowing your buffers == bad
	   */
	if (value > thres_max) {
		snprintf(str, 13, "%e", neg ? -value : value);
		return strlen(str);
	}

	int count = prec;
	while (count > 0) {
		--count;
		*wstr++ = (char)(48 + (frac % 10));
		frac /= 10;
	}
	if (frac > 0) {
		++whole;
	}

	/* add decimal */
	if (prec > 0) {
		*wstr++ = '.';
	}

	/* do whole part
	 * Take care of sign conversion
	 * Number is reversed.
	 */
	do *wstr++ = (char)(48 + (whole % 10)); while (whole /= 10);
	if (neg) {
		*wstr++ = '-';
	}
	*wstr='\0';
	strreverse(str, wstr-1);
	return (size_t)(wstr - str);
}


/* This is near identical to modp_dtoa above
 *   The differnce is noted below
 */
size_t modp_dtoa2(double value, char* str, int prec)
{
	/* Hacky test for NaN
	 * under -fast-math this won't work, but then you also won't
	 * have correct nan values anyways.  The alternative is
	 * to link with libmath (bad) or hack IEEE double bits (bad)
	 */
	if (! (value == value)) {
		str[0] = 'n'; str[1] = 'a'; str[2] = 'n'; str[3] = '\0';
		return (size_t) 3;
	}

	/* if input is larger than thres_max, revert to exponential */
	const double thres_max = (double)(0x7FFFFFFF);

	double diff = 0.0;
	char* wstr = str;

	if (prec < 0) {
		prec = 0;
	} else if (prec > 9) {
		/* precision of >= 10 can lead to overflow errors */
		prec = 9;
	}

	/* we'll work in positive values and deal with the
	   negative sign issue later */
	int neg = 0;
	if (value < 0) {
		neg = 1;
		value = -value;
	}

	int whole = (int) value;
	double tmp = (value - whole) * powers_of_10[prec];
	uint32_t frac = (uint32_t)(tmp);
	diff = tmp - frac;

	if (diff > 0.5) {
		++frac;
		/* handle rollover, e.g.  case 0.99 with prec 1 is 1.0  */
		if (frac >= powers_of_10[prec]) {
			frac = 0;
			++whole;
		}
	} else if (diff == 0.5 && prec > 0 && (frac & 1)) {
		/* if halfway, round up if odd, OR
		   if last digit is 0.  That last part is strange */
		++frac;
		if (frac >= powers_of_10[prec]) {
			frac = 0;
			++whole;
		}
	} else if (diff == 0.5 && prec == 0 && (whole & 1)) {
		++frac;
		if (frac >= powers_of_10[prec]) {
			frac = 0;
			++whole;
		}
	}

	/* for very large numbers switch back to native sprintf for exponentials.
	   anyone want to write code to replace this? */
	/*
	   normal printf behavior is to print EVERY whole number digit
	   which can be 100s of characters overflowing your buffers == bad
	   */
	if (value > thres_max) {
	  snprintf(str, 13, "%e", neg ? -value : value);
		return strlen(str);
	}

	int has_decimal = 0;
	int count = prec;

	/* Remove ending zeros */
	if (prec > 0) {
		while (count > 0 && ((frac % 10) == 0)) {
			count--;
			frac /= 10;
		}
	}

	while (count > 0) {
		--count;
		*wstr++ = (char)(48 + (frac % 10));
		frac /= 10;
		has_decimal = 1;
	}

	if (frac > 0) {
		++whole;
	}

	/* add decimal */
	if (has_decimal) {
		*wstr++ = '.';
	}
	/* do whole part
	 * Take care of sign conversion
	 * Number is reversed.
	 */
	do *wstr++ = (char)(48 + (whole % 10)); while (whole /= 10);
	if (neg) {
		*wstr++ = '-';
	}
	*wstr='\0';
	strreverse(str, wstr-1);
	return (size_t)(wstr - str);
}


/* You can get rid of the include, but adding... */
/* if on motoral, sun, ibm; uncomment this */
/* #define WORDS_BIGENDIAN 1 */
/* else for Intel, Amd; uncomment this */
/* #undef WORDS_BIGENDIAN */

char* modp_uitoa16(uint32_t value, char* str, int isfinal)
{
	static const char* hexchars = "0123456789ABCDEF";

	/**
	 * Implementation note:
	 *  No re-assignment of "value"
	 *  Each line is independent than the previous, so
	 *    even dumb compilers can pipeline without loop unrolling
	 */
#ifndef WORDS_BIGENDIAN
	/* x86 */
	str[0] = hexchars[(value >> 28) & 0x0000000F];
	str[1] = hexchars[(value >> 24) & 0x0000000F];
	str[2] = hexchars[(value >> 20) & 0x0000000F];
	str[3] = hexchars[(value >> 16) & 0x0000000F];
	str[4] = hexchars[(value >> 12) & 0x0000000F];
	str[5] = hexchars[(value >>  8) & 0x0000000F];
	str[6] = hexchars[(value >>  4) & 0x0000000F];
	str[7] = hexchars[(value ) & 0x0000000F];
#else
	/* sun, motorola, ibm */
	str[0] = hexchars[(value ) & 0x0000000F];
	str[1] = hexchars[(value >>  4) & 0x0000000F];
	str[2] = hexchars[(value >>  8) & 0x0000000F];
	str[3] = hexchars[(value >> 12) & 0x0000000F];
	str[4] = hexchars[(value >> 16) & 0x0000000F];
	str[5] = hexchars[(value >> 20) & 0x0000000F];
	str[6] = hexchars[(value >> 24) & 0x0000000F];
	str[7] = hexchars[(value >> 28) & 0x0000000F];
#endif

	if (isfinal) {
		str[8] = '\0';
		return str;
	} else {
		return str + 8;
	}
}
