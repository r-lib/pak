
/*  A Bison parser, made from lp_rlp.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	VAR	257
#define	CONS	258
#define	INTCONS	259
#define	VARIABLECOLON	260
#define	INF	261
#define	SEC_INT	262
#define	SEC_SEC	263
#define	SEC_SOS	264
#define	SOSDESCR	265
#define	SIGN	266
#define	AR_M_OP	267
#define	RE_OPLE	268
#define	RE_OPGE	269
#define	END_C	270
#define	COMMA	271
#define	COLON	272
#define	MINIMISE	273
#define	MAXIMISE	274
#define	UNDEFINED	275


#include <string.h>
#include <ctype.h>

#include "lpkit.h"
#include "yacc_read.h"

#ifdef FORTIFY
# include "lp_fortify.h"
#endif

static int HadVar0, HadVar1, HadVar2, HasAR_M_OP, do_add_row, Had_lineair_sum0, HadSign;
static char *Last_var = NULL, *Last_var0 = NULL;
static REAL f, f0, f1;
static int x;
static int state, state0;
static int Sign;
static int isign, isign0;      /* internal_sign variable to make sure nothing goes wrong */
                /* with lookahead */
static int make_neg;   /* is true after the relational operator is seen in order */
                /* to remember if lin_term stands before or after re_op */
static int Within_int_decl = FALSE; /* TRUE when we are within an int declaration */
static int Within_sec_decl = FALSE; /* TRUE when we are within an sec declaration */
static int Within_sos_decl = FALSE; /* TRUE when we are within an sos declaration */
static int Within_sos_decl1;
static short SOStype0; /* SOS type */
static short SOStype; /* SOS type */
static int SOSNr;
static int SOSweight = 0; /* SOS weight */

static int HadConstraint;
static int HadVar;
static int Had_lineair_sum;

extern FILE *lp_yyin;

#define YY_FATAL_ERROR lex_fatal_error

/* let's please C++ users */
#ifdef __cplusplus
extern "C" {
#endif

static int wrap(void)
{
  return(1);
}

static int __WINAPI lp_input_lp_yyin(void *fpin, char *buf, int max_size)
{
  int result;

  if ( (result = fread( (char*)buf, sizeof(char), max_size, (FILE *) fpin)) < 0)
    YY_FATAL_ERROR( "read() in flex scanner failed");

  return(result);
}

static read_modeldata_func *lp_input;

#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result = lp_input((void *) lp_yyin, buf, max_size);

#ifdef __cplusplus
};
#endif

#define lp_yywrap wrap
#define lp_yyerror read_error

#include "lp_rlp.h"

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		120
#define	YYFLAG		-32768
#define	YYNTBASE	22

#define YYTRANSLATE(x) ((unsigned)(x) <= 275 ? lp_yytranslate[x] : 77)

static const char lp_yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21
};

#if YYDEBUG != 0
static const short lp_yyprhs[] = {     0,
     0,     1,     2,     7,    10,    13,    15,    18,    20,    22,
    24,    26,    28,    31,    33,    34,    38,    39,    40,    41,
    50,    52,    53,    54,    60,    62,    64,    66,    67,    71,
    72,    75,    77,    80,    83,    85,    86,    90,    92,    94,
    97,    99,   101,   103,   105,   107,   109,   111,   113,   115,
   117,   119,   122,   124,   126,   128,   129,   133,   134,   140,
   142,   145,   147,   148,   152,   154,   155,   160,   162,   165,
   167,   169,   171,   175,   177,   179,   181,   182,   184,   186,
   189,   193,   196,   199,   202
};

static const short lp_yyrhs[] = {    -1,
     0,    24,    25,    28,    54,     0,    20,    26,     0,    19,
    26,     0,    26,     0,    27,    16,     0,    22,     0,    42,
     0,    22,     0,    29,     0,    30,     0,    29,    30,     0,
    32,     0,     0,     6,    31,    32,     0,     0,     0,     0,
    39,    33,    48,    34,    40,    35,    36,    16,     0,    22,
     0,     0,     0,    48,    37,    49,    38,    53,     0,    22,
     0,    40,     0,    42,     0,     0,     7,    41,    53,     0,
     0,    43,    44,     0,    45,     0,    44,    45,     0,    51,
    46,     0,    50,     0,     0,    52,    47,     3,     0,    14,
     0,    15,     0,    51,    50,     0,     7,     0,     5,     0,
     4,     0,    22,     0,    12,     0,    22,     0,    13,     0,
    22,     0,    22,     0,    55,     0,    57,     0,    55,    57,
     0,     8,     0,     9,     0,    10,     0,     0,    56,    58,
    61,     0,     0,    60,    62,    67,    64,    16,     0,    59,
     0,    61,    59,     0,    22,     0,     0,    11,    63,    73,
     0,    22,     0,     0,    14,     5,    65,    66,     0,    22,
     0,    18,     5,     0,    22,     0,    68,     0,    74,     0,
    68,    69,    74,     0,    22,     0,    17,     0,    22,     0,
     0,    22,     0,    22,     0,     3,    70,     0,     6,    71,
    75,     0,    50,    72,     0,    73,    76,     0,     3,    70,
     0,     6,    71,    50,    72,     0
};

#endif

#if YYDEBUG != 0
static const short lp_yyrline[] = { 0,
    86,    89,    98,   112,   116,   120,   123,   134,   135,   165,
   166,   169,   170,   174,   175,   182,   184,   190,   197,   221,
   237,   243,   254,   258,   279,   289,   295,   296,   301,   303,
   308,   322,   323,   327,   363,   367,   375,   380,   380,   383,
   385,   396,   396,   399,   404,   411,   415,   421,   438,   440,
   443,   444,   447,   447,   447,   450,   456,   458,   468,   480,
   482,   485,   486,   492,   494,   501,   515,   517,   521,   528,
   529,   532,   533,   538,   539,   542,   567,   586,   608,   622,
   625,   630,   632,   636,   639
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const lp_yytname[] = {   "$","error","$undefined.","VAR","CONS",
"INTCONS","VARIABLECOLON","INF","SEC_INT","SEC_SEC","SEC_SOS","SOSDESCR","SIGN",
"AR_M_OP","RE_OPLE","RE_OPGE","END_C","COMMA","COLON","MINIMISE","MAXIMISE",
"UNDEFINED","EMPTY","inputfile","@1","objective_function","real_of","lineair_sum",
"constraints","x_constraints","constraint","@2","real_constraint","@3","@4",
"@5","optionalrange","@6","@7","x_lineair_sum2","x_lineair_sum3","@8","x_lineair_sum",
"@9","x_lineair_sum1","x_lineair_term","x_lineair_term1","@10","RE_OP","cons_term",
"REALCONS","x_SIGN","optional_AR_M_OP","RHS_STORE","int_sec_sos_declarations",
"real_int_sec_sos_decls","SEC_INT_SEC_SOS","int_sec_sos_declaration","@11","xx_int_sec_sos_declaration",
"@12","x_int_sec_sos_declaration","optionalsos","@13","optionalsostype","@14",
"optionalSOSweight","vars","x_vars","optionalcomma","variable","variablecolon",
"sosweight","sosdescr","onevarwithoptionalweight","INTCONSorVARIABLE","x_onevarwithoptionalweight", NULL
};
#endif

static const short lp_yyr1[] = {     0,
    22,    24,    23,    25,    25,    25,    26,    27,    27,    28,
    28,    29,    29,    30,    31,    30,    33,    34,    35,    32,
    36,    37,    38,    36,    39,    39,    40,    41,    40,    43,
    42,    44,    44,    45,    46,    47,    46,    48,    48,    49,
    49,    50,    50,    51,    51,    52,    52,    53,    54,    54,
    55,    55,    56,    56,    56,    58,    57,    60,    59,    61,
    61,    62,    63,    62,    64,    65,    64,    66,    66,    67,
    67,    68,    68,    69,    69,    70,    71,    72,    73,    74,
    74,    75,    75,    76,    76
};

static const short lp_yyr2[] = {     0,
     0,     0,     4,     2,     2,     1,     2,     1,     1,     1,
     1,     1,     2,     1,     0,     3,     0,     0,     0,     8,
     1,     0,     0,     5,     1,     1,     1,     0,     3,     0,
     2,     1,     2,     2,     1,     0,     3,     1,     1,     2,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     2,     1,     1,     1,     0,     3,     0,     5,     1,
     2,     1,     0,     3,     1,     0,     4,     1,     2,     1,
     1,     1,     3,     1,     1,     1,     0,     1,     1,     2,
     3,     2,     2,     2,     4
};

static const short lp_yydefact[] = {     2,
    30,    30,    30,     8,     1,     6,     0,     9,     1,     5,
     4,    15,    28,    10,     1,    30,    12,    14,    17,    26,
    27,     7,    45,    44,     1,    32,     1,    30,     1,    53,
    54,    55,    49,     3,    50,    56,    51,    25,    13,     0,
    33,    43,    42,    47,    46,    34,    35,    36,    16,    48,
    29,    52,    58,    38,    39,    18,     0,    60,     1,    58,
    30,    37,    63,    62,     1,    61,    19,     1,     1,    77,
    70,     1,     1,    72,     1,    79,    64,    76,    80,     1,
     0,    65,     0,    75,    74,     0,    21,     0,    22,     1,
     0,    81,    66,    59,    73,    20,     1,    78,    82,     1,
    77,    83,     1,    41,    23,     0,    84,     0,     0,    68,
    67,     1,    40,     1,    69,    24,    85,     0,     0,     0
};

static const short lp_yydefgoto[] = {     4,
   118,     1,     5,     6,     7,    15,    16,    17,    28,    18,
    40,    61,    75,    88,    97,   112,    19,    20,    29,    21,
     9,    25,    26,    46,    57,    56,   105,    47,    27,    48,
    51,    34,    35,    36,    37,    53,    58,    59,    60,    65,
    68,    83,   103,   111,    72,    73,    86,    79,    80,    99,
    77,    74,    92,   102
};

static const short lp_yypact[] = {-32768,
    42,    10,    10,-32768,     2,-32768,    14,-32768,    32,-32768,
-32768,-32768,-32768,   -12,    64,    25,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,    41,-32768,     8,    34,-32768,-32768,
-32768,-32768,-32768,-32768,    64,-32768,-32768,-32768,-32768,     3,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,    56,-32768,    54,    28,
    73,-32768,-32768,-32768,    13,-32768,-32768,-32768,-32768,-32768,
-32768,    57,    29,-32768,     3,-32768,-32768,-32768,-32768,    72,
    77,-32768,    65,-32768,-32768,    13,-32768,    67,-32768,-32768,
    63,-32768,-32768,-32768,-32768,-32768,    35,-32768,-32768,-32768,
-32768,-32768,    66,-32768,-32768,    72,-32768,    72,    81,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,    87,    88,-32768
};

static const short lp_yypgoto[] = {    -5,
-32768,-32768,-32768,    76,-32768,-32768,-32768,    74,-32768,    61,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,    30,-32768,    49,
-32768,-32768,    68,-32768,-32768,    19,-32768,   -79,    -1,-32768,
   -15,-32768,-32768,-32768,    69,-32768,    39,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,     0,     1,   -13,
    23,    20,-32768,-32768
};


#define	YYLAST		109


static const short lp_yytable[] = {    14,
    90,   -25,   -25,    24,   -30,   -30,   -30,    12,    13,    33,
    38,    42,    43,   -30,   -30,    69,    54,    55,    70,    24,
    44,    45,    38,    50,   -11,    -1,   113,   -57,   114,    22,
    12,    13,   -11,   -11,   -11,   -57,   -57,   -57,    -1,    -1,
    13,   104,   -71,    23,   -71,    84,    23,    -1,    -1,     8,
     8,     8,    23,    64,   -31,   -31,   -31,    -1,    62,    71,
     2,     3,    76,    78,    63,   100,    82,    85,   101,    87,
    81,    30,    31,    32,    76,    42,    43,    10,    11,    13,
    94,    93,    96,   109,    98,   115,   119,   120,    49,    39,
    67,    24,    41,    89,    78,   106,   116,   110,    66,   107,
   117,   108,    91,    52,     0,    95,    50,     0,    98
};

static const short lp_yycheck[] = {     5,
    80,    14,    15,     9,     3,     4,     5,     6,     7,    15,
    16,     4,     5,    12,    13,     3,    14,    15,     6,    25,
    13,    27,    28,    29,     0,    16,   106,     0,   108,    16,
     6,     7,     8,     9,    10,     8,     9,    10,    14,    15,
     7,     7,    14,    12,    16,    17,    12,    14,    15,     1,
     2,     3,    12,    59,    14,    15,    16,    16,     3,    65,
    19,    20,    68,    69,    11,     3,    72,    73,     6,    75,
    14,     8,     9,    10,    80,     4,     5,     2,     3,     7,
    16,     5,    16,    18,    90,     5,     0,     0,    28,    16,
    61,    97,    25,    75,   100,    97,   112,   103,    60,   100,
   114,   101,    80,    35,    -1,    86,   112,    -1,   114
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */

/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define lp_yyerrok		(lp_yyerrstatus = 0)
#define lp_yyclearin	(lp_yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto lp_yyacceptlab
#define YYABORT 	goto lp_yyabortlab
#define YYERROR		goto lp_yyerrlab1
/* Like YYERROR except do call lp_yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto lp_yyerrlab
#define YYRECOVERING()  (!!lp_yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (lp_yychar == YYEMPTY && lp_yylen == 1)				\
    { lp_yychar = (token), lp_yylval = (value);			\
      lp_yychar1 = YYTRANSLATE (lp_yychar);				\
      YYPOPSTACK;						\
      goto lp_yybackup;						\
    }								\
  else								\
    { lp_yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		lp_yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		lp_yylex(&lp_yylval, &lp_yylloc, YYLEX_PARAM)
#else
#define YYLEX		lp_yylex(&lp_yylval, &lp_yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		lp_yylex(&lp_yylval, YYLEX_PARAM)
#else
#define YYLEX		lp_yylex(&lp_yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	lp_yychar;			/*  the lookahead symbol		*/
YYSTYPE	lp_yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE lp_yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int lp_yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int lp_yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __lp_yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __lp_yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__lp_yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__lp_yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif



/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into lp_yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG void
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int lp_yyparse (void *);
#else
int lp_yyparse (void);
#endif
#endif

int
lp_yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int lp_yystate;
  register int lp_yyn;
  register short *lp_yyssp;
  register YYSTYPE *lp_yyvsp;
  int lp_yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int lp_yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	lp_yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE lp_yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *lp_yyss = lp_yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *lp_yyvs = lp_yyvsa;	/*  to allow lp_yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE lp_yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *lp_yyls = lp_yylsa;
  YYLTYPE *lp_yylsp;

#define YYPOPSTACK   (lp_yyvsp--, lp_yyssp--, lp_yylsp--)
#else
#define YYPOPSTACK   (lp_yyvsp--, lp_yyssp--)
#endif

  int lp_yystacksize = YYINITDEPTH;
  int lp_yyfree_stacks = 0;

#ifdef YYPURE
  int lp_yychar;
  YYSTYPE lp_yylval;
  int lp_yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE lp_yylloc;
#endif
#endif

  YYSTYPE lp_yyval = 0;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int lp_yylen;


  lp_yystate = 0;
  lp_yyerrstatus = 0;
  lp_yynerrs = 0;
  lp_yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  lp_yyssp = lp_yyss - 1;
  lp_yyvsp = lp_yyvs;
#ifdef YYLSP_NEEDED
  lp_yylsp = lp_yyls;
#endif

/* Push a new state, which is found in  lp_yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
lp_yynewstate:

  *++lp_yyssp = lp_yystate;

  if (lp_yyssp >= lp_yyss + lp_yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *lp_yyvs1 = lp_yyvs;
      short *lp_yyss1 = lp_yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *lp_yyls1 = lp_yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = lp_yyssp - lp_yyss + 1;

#ifdef lp_yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if lp_yyoverflow is a macro.  */
      lp_yyoverflow("parser stack overflow",
		 &lp_yyss1, size * sizeof (*lp_yyssp),
		 &lp_yyvs1, size * sizeof (*lp_yyvsp),
		 &lp_yyls1, size * sizeof (*lp_yylsp),
		 &lp_yystacksize);
#else
      lp_yyoverflow("parser stack overflow",
		 &lp_yyss1, size * sizeof (*lp_yyssp),
		 &lp_yyvs1, size * sizeof (*lp_yyvsp),
		 &lp_yystacksize);
#endif

      lp_yyss = lp_yyss1; lp_yyvs = lp_yyvs1;
#ifdef YYLSP_NEEDED
      lp_yyls = lp_yyls1;
#endif
#else /* no lp_yyoverflow */
      /* Extend the stack our own way.  */
      if (lp_yystacksize >= YYMAXDEPTH)
	{
	  lp_yyerror("parser stack overflow");
	  if (lp_yyfree_stacks)
	    {
	      free (lp_yyss);
	      free (lp_yyvs);
#ifdef YYLSP_NEEDED
	      free (lp_yyls);
#endif
	    }
	  return 2;
	}
      lp_yystacksize *= 2;
      if (lp_yystacksize > YYMAXDEPTH)
	lp_yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      lp_yyfree_stacks = 1;
#endif
      lp_yyss = (short *) YYSTACK_ALLOC (lp_yystacksize * sizeof (*lp_yyssp));
      __lp_yy_memcpy ((char *)lp_yyss, (char *)lp_yyss1,
		   size * (unsigned int) sizeof (*lp_yyssp));
      lp_yyvs = (YYSTYPE *) YYSTACK_ALLOC (lp_yystacksize * sizeof (*lp_yyvsp));
      __lp_yy_memcpy ((char *)lp_yyvs, (char *)lp_yyvs1,
		   size * (unsigned int) sizeof (*lp_yyvsp));
#ifdef YYLSP_NEEDED
      lp_yyls = (YYLTYPE *) YYSTACK_ALLOC (lp_yystacksize * sizeof (*lp_yylsp));
      __lp_yy_memcpy ((char *)lp_yyls, (char *)lp_yyls1,
		   size * (unsigned int) sizeof (*lp_yylsp));
#endif
#endif /* no lp_yyoverflow */

      lp_yyssp = lp_yyss + size - 1;
      lp_yyvsp = lp_yyvs + size - 1;
#ifdef YYLSP_NEEDED
      lp_yylsp = lp_yyls + size - 1;
#endif


      if (lp_yyssp >= lp_yyss + lp_yystacksize - 1)
	YYABORT;
    }


  goto lp_yybackup;
 lp_yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* lp_yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  lp_yyn = lp_yypact[lp_yystate];
  if (lp_yyn == YYFLAG)
    goto lp_yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* lp_yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (lp_yychar == YYEMPTY)
    {
      lp_yychar = YYLEX;
    }

  /* Convert token to internal form (in lp_yychar1) for indexing tables with */

  if (lp_yychar <= 0)		/* This means end of input. */
    {
      lp_yychar1 = 0;
      lp_yychar = YYEOF;		/* Don't call YYLEX any more */

    }
  else
    {
      lp_yychar1 = YYTRANSLATE(lp_yychar);

    }

  lp_yyn += lp_yychar1;
  if (lp_yyn < 0 || lp_yyn > YYLAST || lp_yycheck[lp_yyn] != lp_yychar1)
    goto lp_yydefault;

  lp_yyn = lp_yytable[lp_yyn];

  /* lp_yyn is what to do for this token type in this state.
     Negative => reduce, -lp_yyn is rule number.
     Positive => shift, lp_yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (lp_yyn < 0)
    {
      if (lp_yyn == YYFLAG)
	goto lp_yyerrlab;
      lp_yyn = -lp_yyn;
      goto lp_yyreduce;
    }
  else if (lp_yyn == 0)
    goto lp_yyerrlab;

  if (lp_yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

  /* Discard the token being shifted unless it is eof.  */
  if (lp_yychar != YYEOF)
    lp_yychar = YYEMPTY;

  *++lp_yyvsp = lp_yylval;
#ifdef YYLSP_NEEDED
  *++lp_yylsp = lp_yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (lp_yyerrstatus) lp_yyerrstatus--;

  lp_yystate = lp_yyn;
  goto lp_yynewstate;

/* Do the default action for the current state.  */
lp_yydefault:

  lp_yyn = lp_yydefact[lp_yystate];
  if (lp_yyn == 0)
    goto lp_yyerrlab;

/* Do a reduction.  lp_yyn is the number of a rule to reduce with.  */
lp_yyreduce:
  lp_yylen = lp_yyr2[lp_yyn];
  if (lp_yylen > 0)
    lp_yyval = lp_yyvsp[1-lp_yylen]; /* implement default value of the action */

  switch (lp_yyn) {

case 2:
{
  isign = 0;
  make_neg = 0;
  Sign = 0;
  HadConstraint = FALSE;
  HadVar = HadVar0 = FALSE;
;
    break;}
case 4:
{
  set_obj_dir(TRUE);
;
    break;}
case 5:
{
  set_obj_dir(FALSE);
;
    break;}
case 7:
{
  add_row();
  HadConstraint = FALSE;
  HadVar = HadVar0 = FALSE;
  isign = 0;
  make_neg = 0;
;
    break;}
case 15:
{
  if(!add_constraint_name(Last_var))
    YYABORT;
  HadConstraint = TRUE;
;
    break;}
case 17:
{
  HadVar1 = HadVar0;
  HadVar0 = FALSE;
;
    break;}
case 18:
{
  if(!store_re_op((char *) lp_yytext, HadConstraint, HadVar, Had_lineair_sum))
    YYABORT;
  make_neg = 1;
  f1 = 0;
;
    break;}
case 19:
{
  Had_lineair_sum0 = Had_lineair_sum;
  Had_lineair_sum = TRUE;
  HadVar2 = HadVar0;
  HadVar0 = FALSE;
  do_add_row = FALSE;
  if(HadConstraint && !HadVar ) {
    /* it is a range */
    /* already handled */
  }
  else if(!HadConstraint && HadVar) {
    /* it is a bound */

    if(!store_bounds(TRUE))
      YYABORT;
  }
  else {
    /* it is a row restriction */
    if(HadConstraint && HadVar)
      store_re_op("", HadConstraint, HadVar, Had_lineair_sum); /* makes sure that data stored in temporary buffers is treated correctly */
    do_add_row = TRUE;
  }
;
    break;}
case 20:
{
  if((!HadVar) && (!HadConstraint)) {
    lp_yyerror("parse error");
    YYABORT;
  }
  if(do_add_row)
    add_row();
  HadConstraint = FALSE;
  HadVar = HadVar0 = FALSE;
  isign = 0;
  make_neg = 0;
  null_tmp_store(TRUE);
;
    break;}
case 21:
{
  if((!HadVar1) && (Had_lineair_sum0))
    if(!negate_constraint())
      YYABORT;
;
    break;}
case 22:
{
  make_neg = 0;
  isign = 0;
  if(HadConstraint)
    HadVar = Had_lineair_sum = FALSE;
  HadVar0 = FALSE;
  if(!store_re_op((char *) ((*lp_yytext == '<') ? ">" : (*lp_yytext == '>') ? "<" : lp_yytext), HadConstraint, HadVar, Had_lineair_sum))
    YYABORT;
;
    break;}
case 23:
{
  f -= f1;
;
    break;}
case 24:
{
  if((HadVar1) || (!HadVar2) || (HadVar0)) {
    lp_yyerror("parse error");
    YYABORT;
  }

  if(HadConstraint && !HadVar ) {
    /* it is a range */
    /* already handled */
    if(!negate_constraint())
      YYABORT;
  }
  else if(!HadConstraint && HadVar) {
    /* it is a bound */

    if(!store_bounds(TRUE))
      YYABORT;
  }
;
    break;}
case 25:
{
  /* to allow a range */
  /* constraint: < max */
  if(!HadConstraint) {
    lp_yyerror("parse error");
    YYABORT;
  }
  Had_lineair_sum = FALSE;
;
    break;}
case 26:
{
  Had_lineair_sum = TRUE;
;
    break;}
case 28:
{
  isign = Sign;
;
    break;}
case 30:
{
  state = state0 = 0;
;
    break;}
case 31:
{
  if (state == 1) {
    /* RHS_STORE */
    if (    (isign0 || !make_neg)
        && !(isign0 && !make_neg)) /* but not both! */
      f0 = -f0;
    if(make_neg)
      f1 += f0;
    if(!rhs_store(f0, HadConstraint, HadVar, Had_lineair_sum))
      YYABORT;
  }
;
    break;}
case 34:
{
  if ((HadSign || state == 1) && (state0 == 1)) {
    /* RHS_STORE */
    if (    (isign0 || !make_neg)
        && !(isign0 && !make_neg)) /* but not both! */
      f0 = -f0;
    if(make_neg)
      f1 += f0;
    if(!rhs_store(f0, HadConstraint, HadVar, Had_lineair_sum))
      YYABORT;
  }
  if (state == 1) {
    f0 = f;
    isign0 = isign;
  }
  if (state == 2) {
    if((HadSign) || (state0 != 1)) {
     isign0 = isign;
     f0 = 1.0;
    }
    if (    (isign0 || make_neg)
        && !(isign0 && make_neg)) /* but not both! */
      f0 = -f0;
    if(!var_store(Last_var, f0, HadConstraint, HadVar, Had_lineair_sum)) {
      lp_yyerror("var_store failed");
      YYABORT;
    }
    HadConstraint |= HadVar;
    HadVar = HadVar0 = TRUE;
  }
  state0 = state;
;
    break;}
case 35:
{
  state = 1;
;
    break;}
case 36:
{
  if ((HasAR_M_OP) && (state != 1)) {
    lp_yyerror("parse error");
    YYABORT;
  }
;
    break;}
case 37:
{
  state = 2;
;
    break;}
case 41:
{
  isign = Sign;
;
    break;}
case 44:
{
  isign = 0;
  HadSign = FALSE;
;
    break;}
case 45:
{
  isign = Sign;
  HadSign = TRUE;
;
    break;}
case 46:
{
  HasAR_M_OP = FALSE;
;
    break;}
case 47:
{
  HasAR_M_OP = TRUE;
;
    break;}
case 48:
{
  if (    (isign || !make_neg)
      && !(isign && !make_neg)) /* but not both! */
    f = -f;
  if(!rhs_store(f, HadConstraint, HadVar, Had_lineair_sum))
    YYABORT;
  isign = 0;
;
    break;}
case 56:
{
  Within_sos_decl1 = Within_sos_decl;
;
    break;}
case 58:
{
  if((!Within_int_decl) && (!Within_sec_decl) && (!Within_sos_decl1)) {
    lp_yyerror("parse error");
    YYABORT;
  }
  SOStype = SOStype0;
  check_int_sec_sos_decl(Within_int_decl, Within_sec_decl, Within_sos_decl1 = (Within_sos_decl1 ? 1 : 0));
;
    break;}
case 59:
{
  if((Within_sos_decl1) && (SOStype == 0))
  {
    lp_yyerror("Unsupported SOS type (0)");
    YYABORT;
  }
;
    break;}
case 63:
{
  FREE(Last_var0);
  Last_var0 = strdup(Last_var);
;
    break;}
case 65:
{
  if(Within_sos_decl1) {
    set_sos_type(SOStype);
    set_sos_weight((double) SOSweight, 1);
  }
;
    break;}
case 66:
{
  if((Within_sos_decl1) && (!SOStype))
  {
    set_sos_type(SOStype = (short) (f + .1));
  }
  else
  {
    lp_yyerror("SOS type not expected");
    YYABORT;
  }
;
    break;}
case 68:
{
  set_sos_weight((double) SOSweight, 1);
;
    break;}
case 69:
{
  set_sos_weight(f, 1);
;
    break;}
case 76:
{
  if(Within_sos_decl1 == 1)
  {
    char buf[16];

    SOSweight++;
    snprintf(buf, sizeof(buf), "SOS%d", SOSweight);
    storevarandweight(buf);

    check_int_sec_sos_decl(Within_int_decl, Within_sec_decl, 2);
    Within_sos_decl1 = 2;
    SOSNr = 0;
  }

  storevarandweight(Last_var);

  if(Within_sos_decl1 == 2)
  {
    SOSNr++;
    set_sos_weight((double) SOSNr, 2);
  }
;
    break;}
case 77:
{
  if(!Within_sos_decl1) {
    lp_yyerror("parse error");
    YYABORT;
  }
  if(Within_sos_decl1 == 1) {
    FREE(Last_var0);
    Last_var0 = strdup(Last_var);
  }
  if(Within_sos_decl1 == 2)
  {
    storevarandweight(Last_var);
    SOSNr++;
    set_sos_weight((double) SOSNr, 2);
  }
;
    break;}
case 78:
{
  if(Within_sos_decl1 == 1)
  {
    char buf[16];

    SOSweight++;
    snprintf(buf, sizeof(buf), "SOS%d", SOSweight);
    storevarandweight(buf);

    check_int_sec_sos_decl(Within_int_decl, Within_sec_decl, 2);
    Within_sos_decl1 = 2;
    SOSNr = 0;

    storevarandweight(Last_var0);
    SOSNr++;
  }

  set_sos_weight(f, 2);
;
    break;}
case 79:
{ /* SOS name */
  if(Within_sos_decl1 == 1)
  {
    storevarandweight(Last_var0);
    set_sos_type(SOStype);
    check_int_sec_sos_decl(Within_int_decl, Within_sec_decl, 2);
    Within_sos_decl1 = 2;
    SOSNr = 0;
    SOSweight++;
  }
;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */


  lp_yyvsp -= lp_yylen;
  lp_yyssp -= lp_yylen;
#ifdef YYLSP_NEEDED
  lp_yylsp -= lp_yylen;
#endif

  *++lp_yyvsp = lp_yyval;

#ifdef YYLSP_NEEDED
  lp_yylsp++;
  if (lp_yylen == 0)
    {
      lp_yylsp->first_line = lp_yylloc.first_line;
      lp_yylsp->first_column = lp_yylloc.first_column;
      lp_yylsp->last_line = (lp_yylsp-1)->last_line;
      lp_yylsp->last_column = (lp_yylsp-1)->last_column;
      lp_yylsp->text = 0;
    }
  else
    {
      lp_yylsp->last_line = (lp_yylsp+lp_yylen-1)->last_line;
      lp_yylsp->last_column = (lp_yylsp+lp_yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  lp_yyn = lp_yyr1[lp_yyn];

  lp_yystate = lp_yypgoto[lp_yyn - YYNTBASE] + *lp_yyssp;
  if (lp_yystate >= 0 && lp_yystate <= YYLAST && lp_yycheck[lp_yystate] == *lp_yyssp)
    lp_yystate = lp_yytable[lp_yystate];
  else
    lp_yystate = lp_yydefgoto[lp_yyn - YYNTBASE];

  goto lp_yynewstate;

lp_yyerrlab:   /* here on detecting error */

  if (! lp_yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++lp_yynerrs;

#ifdef YYERROR_VERBOSE
      lp_yyn = lp_yypact[lp_yystate];

      if (lp_yyn > YYFLAG && lp_yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -lp_yyn if nec to avoid negative indexes in lp_yycheck.  */
	  for (x = (lp_yyn < 0 ? -lp_yyn : 0);
	       x < (sizeof(lp_yytname) / sizeof(char *)); x++)
	    if (lp_yycheck[x + lp_yyn] == x)
	      size += strlen(lp_yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (lp_yyn < 0 ? -lp_yyn : 0);
		       x < (sizeof(lp_yytname) / sizeof(char *)); x++)
		    if (lp_yycheck[x + lp_yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, lp_yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      lp_yyerror(msg);
	      free(msg);
	    }
	  else
	    lp_yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	lp_yyerror("parse error");
    }

  goto lp_yyerrlab1;
lp_yyerrlab1:   /* here on error raised explicitly by an action */

  if (lp_yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (lp_yychar == YYEOF)
	YYABORT;

      lp_yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  lp_yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto lp_yyerrhandle;

lp_yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  lp_yyn = lp_yydefact[lp_yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (lp_yyn) goto lp_yydefault;
#endif

lp_yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (lp_yyssp == lp_yyss) YYABORT;
  lp_yyvsp--;
  lp_yystate = *--lp_yyssp;
#ifdef YYLSP_NEEDED
  lp_yylsp--;
#endif

lp_yyerrhandle:

  lp_yyn = lp_yypact[lp_yystate];
  if (lp_yyn == YYFLAG)
    goto lp_yyerrdefault;

  lp_yyn += YYTERROR;
  if (lp_yyn < 0 || lp_yyn > YYLAST || lp_yycheck[lp_yyn] != YYTERROR)
    goto lp_yyerrdefault;

  lp_yyn = lp_yytable[lp_yyn];
  if (lp_yyn < 0)
    {
      if (lp_yyn == YYFLAG)
	goto lp_yyerrpop;
      lp_yyn = -lp_yyn;
      goto lp_yyreduce;
    }
  else if (lp_yyn == 0)
    goto lp_yyerrpop;

  if (lp_yyn == YYFINAL)
    YYACCEPT;


  *++lp_yyvsp = lp_yylval;
#ifdef YYLSP_NEEDED
  *++lp_yylsp = lp_yylloc;
#endif

  lp_yystate = lp_yyn;
  goto lp_yynewstate;

 lp_yyacceptlab:
  /* YYACCEPT comes here.  */
  if (lp_yyfree_stacks)
    {
      free (lp_yyss);
      free (lp_yyvs);
#ifdef YYLSP_NEEDED
      free (lp_yyls);
#endif
    }
  return 0;

 lp_yyabortlab:
  /* YYABORT comes here.  */
  if (lp_yyfree_stacks)
    {
      free (lp_yyss);
      free (lp_yyvs);
#ifdef YYLSP_NEEDED
      free (lp_yyls);
#endif
    }
  return 1;
}


static void lp_yy_delete_allocated_memory(void)
{
  /* free memory allocated by flex. Otherwise some memory is not freed.
     This is a bit tricky. There is not much documentation about this, but a lot of
     reports of memory that keeps allocated */

  /* If you get errors on this function call, just comment it. This will only result
     in some memory that is not being freed. */

# if defined YY_CURRENT_BUFFER
    /* flex defines the macro YY_CURRENT_BUFFER, so you should only get here if lp_rlp.h is
       generated by flex */
    /* lex doesn't define this macro and thus should not come here, but lex doesn't has
       this memory leak also ...*/

    lp_yy_delete_buffer(YY_CURRENT_BUFFER); /* comment this line if you have problems with it */
    lp_yy_init = 1; /* make sure that the next time memory is allocated again */
    lp_yy_start = 0;
# endif

  FREE(Last_var);
  FREE(Last_var0);
}

static int parse(void)
{
  return(lp_yyparse());
}

lprec *read_lp1(lprec *lp, void *userhandle, read_modeldata_func read_modeldata, int verbose, char *lp_name)
{
  lp_yyin = (FILE *) userhandle;
  lp_yyout = NULL;
  lp_yylineno = 1;
  lp_input = read_modeldata;
  return(yacc_read(lp, verbose, lp_name, &lp_yylineno, parse, lp_yy_delete_allocated_memory));
}

lprec * __WINAPI read_lp(FILE *filename, int verbose, char *lp_name)
{
  return(read_lp1(NULL, filename, lp_input_lp_yyin, verbose, lp_name));
}

lprec * __WINAPI read_lpex(void *userhandle, read_modeldata_func read_modeldata, int verbose, char *lp_name)
{
  return(read_lp1(NULL, userhandle, read_modeldata, verbose, lp_name));
}

lprec *read_LP1(lprec *lp, char *filename, int verbose, char *lp_name)
{
  FILE *fpin;

  if((fpin = fopen(filename, "r")) != NULL) {
    lp = read_lp1(lp, fpin, lp_input_lp_yyin, verbose, lp_name);
    fclose(fpin);
  }
  else
    lp = NULL;
  return(lp);
}

lprec * __WINAPI read_LP(char *filename, int verbose, char *lp_name)
{
  return(read_LP1(NULL, filename, verbose, lp_name));
}

MYBOOL __WINAPI LP_readhandle(lprec **lp, FILE *filename, int verbose, char *lp_name)
{
  if(lp != NULL)
    *lp = read_lp1(*lp, filename, lp_input_lp_yyin, verbose, lp_name);

  return((lp != NULL) && (*lp != NULL));
}
