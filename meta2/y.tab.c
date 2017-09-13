/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 3 "jac.y" /* yacc.c:339  */

	#include <stdio.h>
	#include <string.h> 
	#include <stdlib.h> 

	


	void yyerror(const char *s);
	int yylex(void); 

    #include "definicoes.h"
	#include "arvores.h"

	int erros=0;

	extern char* yytext;
	extern int yyleng;

	extern int colunas; 
	extern int linhas;
	extern int flag;
	extern int t;
 


#line 93 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    BOOL = 258,
    BOOLLIT = 259,
    CLASS = 260,
    DO = 261,
    DOTLENGTH = 262,
    DOUBLE = 263,
    ELSE = 264,
    IF = 265,
    INT = 266,
    PARSEINT = 267,
    PRINT = 268,
    PUBLIC = 269,
    RETURN = 270,
    STATIC = 271,
    STRING = 272,
    VOID = 273,
    WHILE = 274,
    OCURV = 275,
    CCURV = 276,
    OBRACE = 277,
    CBRACE = 278,
    OSQUARE = 279,
    CSQUARE = 280,
    AND = 281,
    OR = 282,
    LT = 283,
    GT = 284,
    EQ = 285,
    NEQ = 286,
    LEQ = 287,
    GEQ = 288,
    PLUS = 289,
    MINUS = 290,
    STAR = 291,
    DIV = 292,
    MOD = 293,
    NOT = 294,
    ASSIGN = 295,
    SEMI = 296,
    COMMA = 297,
    RESERVED = 298,
    STRLIT = 299,
    DECLIT = 300,
    REALLIT = 301,
    ID = 302,
    UNARY = 303,
    IFX = 304
  };
#endif
/* Tokens.  */
#define BOOL 258
#define BOOLLIT 259
#define CLASS 260
#define DO 261
#define DOTLENGTH 262
#define DOUBLE 263
#define ELSE 264
#define IF 265
#define INT 266
#define PARSEINT 267
#define PRINT 268
#define PUBLIC 269
#define RETURN 270
#define STATIC 271
#define STRING 272
#define VOID 273
#define WHILE 274
#define OCURV 275
#define CCURV 276
#define OBRACE 277
#define CBRACE 278
#define OSQUARE 279
#define CSQUARE 280
#define AND 281
#define OR 282
#define LT 283
#define GT 284
#define EQ 285
#define NEQ 286
#define LEQ 287
#define GEQ 288
#define PLUS 289
#define MINUS 290
#define STAR 291
#define DIV 292
#define MOD 293
#define NOT 294
#define ASSIGN 295
#define SEMI 296
#define COMMA 297
#define RESERVED 298
#define STRLIT 299
#define DECLIT 300
#define REALLIT 301
#define ID 302
#define UNARY 303
#define IFX 304

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 30 "jac.y" /* yacc.c:355  */
 
	
	char* str; 

	no ptrNo;
	lista listaNos;

#line 239 "y.tab.c" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 254 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   344

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  50
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  21
/* YYNRULES -- Number of rules.  */
#define YYNRULES  78
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  168

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   304

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   127,   127,   129,   130,   131,   132,   136,   137,   140,
     141,   144,   148,   150,   152,   154,   158,   161,   162,   163,
     166,   168,   172,   174,   177,   180,   181,   182,   189,   190,
     192,   194,   195,   196,   197,   198,   199,   200,   201,   202,
     203,   204,   207,   208,   211,   215,   216,   218,   221,   222,
     225,   226,   228,   229,   230,   233,   234,   235,   236,   237,
     238,   239,   240,   241,   242,   243,   244,   245,   246,   247,
     248,   249,   250,   251,   252,   253,   254,   255,   256
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "BOOL", "BOOLLIT", "CLASS", "DO",
  "DOTLENGTH", "DOUBLE", "ELSE", "IF", "INT", "PARSEINT", "PRINT",
  "PUBLIC", "RETURN", "STATIC", "STRING", "VOID", "WHILE", "OCURV",
  "CCURV", "OBRACE", "CBRACE", "OSQUARE", "CSQUARE", "AND", "OR", "LT",
  "GT", "EQ", "NEQ", "LEQ", "GEQ", "PLUS", "MINUS", "STAR", "DIV", "MOD",
  "NOT", "ASSIGN", "SEMI", "COMMA", "RESERVED", "STRLIT", "DECLIT",
  "REALLIT", "ID", "UNARY", "IFX", "$accept", "Program", "Prog",
  "FieldDecl", "Var", "MethodDecl", "MethodHeader", "MethodBody", "Body",
  "FormalParams", "Formal", "VarDecl", "Type", "Statement", "Stat",
  "Assignment", "MethodInvocation", "Param", "ParseArgs", "Expr", "Expr2", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304
};
# endif

#define YYPACT_NINF -50

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-50)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
       9,   -13,    48,    -1,   -50,   -50,     6,     8,    55,   -50,
     -50,   -50,   -50,   -50,    25,   -50,   -50,   -50,    26,    53,
      33,    62,   -50,   -50,    63,    83,    66,   103,   -29,    60,
     -50,    69,    40,    52,   187,    77,    78,    79,   206,    85,
     -50,   -50,   -50,    -9,   -50,    61,   -50,    68,    71,    74,
     -50,    89,   -50,    72,    93,   -50,   -50,   -50,   107,   251,
      21,   223,   -50,   203,   260,   260,   260,   -50,   -50,   -50,
      30,   -50,   -50,   -50,    86,   282,   251,   129,   145,   251,
     -50,   -50,   -50,   -50,   -50,   -50,    81,    87,   112,   113,
     119,   109,   122,   124,   132,   135,   251,    12,   -50,   -50,
     -50,   -50,   -50,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   146,   -50,   -50,   147,
     -50,   -50,   -50,   -18,   -50,    27,   251,   187,   -50,   251,
     106,   128,   -50,   -50,   306,   295,   137,   137,   126,   126,
     137,   137,     3,     3,   -50,   -50,   -50,   187,   -50,   -16,
     -50,   130,   157,   141,   156,   -50,   -50,   -50,   -50,   251,
     -50,   142,   187,   161,   -50,   -50,   -50,   -50
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     1,     6,     0,     0,     0,     2,
       3,     4,     5,     8,     0,    25,    27,    26,     0,     0,
       0,     0,    19,    11,    10,     0,     0,     0,     0,     0,
      14,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      43,    16,    38,     0,    18,     0,    17,     0,     0,     0,
      12,     0,     7,     0,     0,    15,    23,    41,     0,     0,
       0,     0,    73,     0,     0,     0,     0,    39,    74,    75,
      76,    52,    55,    56,     0,    53,     0,     0,     0,     0,
      10,    35,    36,    37,    13,     9,     0,    21,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    70,    71,
      72,    77,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    28,    42,     0,
      45,    49,    44,     0,    20,     0,     0,     0,    51,     0,
       0,     0,    54,    78,    57,    58,    63,    61,    59,    64,
      62,    60,    65,    66,    67,    68,    69,     0,    47,     0,
      24,     0,     0,    29,     0,    34,    33,    31,    46,     0,
      22,     0,     0,     0,    48,    32,    30,    50
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -50,   -50,   -50,   -50,   105,   -50,   -50,   -50,   -50,   159,
     -50,   -50,    -8,   -31,   -50,   -26,   -25,   -50,   -24,   -34,
     -49
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     6,    11,    28,    12,    19,    23,    26,    31,
      87,    44,    32,    46,    77,    71,    72,   149,    73,    95,
      75
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      47,    48,    49,    58,    74,   158,    20,     7,    47,    48,
      49,    78,    52,    53,     1,    98,    99,   100,    45,   101,
       8,     5,    90,   150,    53,    89,   159,    93,    15,     9,
      15,    79,    78,    16,     3,    16,    17,   101,    17,   113,
     114,   115,   116,    18,   121,   122,   118,    10,     4,    13,
      78,    47,    48,    49,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,    33,    91,    15,
      79,    14,    34,    21,    16,    22,    35,    17,    36,    37,
      24,    38,    25,    27,    54,    39,    15,    56,    40,    41,
      55,    16,   152,    57,    17,   154,   153,    59,    60,    61,
      29,    47,    48,    49,    30,    76,    15,    42,    80,    81,
      84,    16,    82,    43,    17,    83,   157,   151,    86,    85,
      29,    47,    48,    49,    50,   164,    88,   102,   124,   125,
      33,   166,   126,   129,   127,    34,    47,    48,    49,    35,
     128,    36,    37,   130,    38,   131,   119,   155,    39,    62,
     162,    40,   117,   132,   105,   106,   133,    36,   109,   110,
     111,   112,   113,   114,   115,    63,   120,   147,   148,   156,
      42,   111,   112,   113,   114,   115,    43,   160,   161,    64,
      65,   163,   167,   165,    66,   123,    51,     0,    33,     0,
      68,    69,    70,    34,     0,     0,     0,    35,     0,    36,
      37,     0,    38,     0,    94,     0,    39,    62,     0,    40,
      62,     0,     0,     0,     0,    36,     0,     0,    36,     0,
       0,     0,     0,    63,     0,     0,    63,    62,    42,     0,
       0,     0,     0,     0,    43,    36,     0,    64,    65,     0,
      64,    65,    66,    63,     0,    66,     0,    67,    68,    69,
      70,    68,    69,    70,     0,    62,     0,    64,    65,     0,
       0,     0,    66,    36,    62,     0,     0,    92,    68,    69,
      70,    63,    36,     0,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,     0,    64,    65,     0,     0,     0,
      66,     0,     0,     0,    64,    65,    68,    69,    70,    66,
       0,     0,     0,     0,     0,    68,    69,    97,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   103,     0,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115
};

static const yytype_int16 yycheck[] =
{
      26,    26,    26,    34,    38,    21,    14,     1,    34,    34,
      34,    20,    41,    42,     5,    64,    65,    66,    26,     7,
      14,    22,     1,    41,    42,    59,    42,    61,     3,    23,
       3,    40,    20,     8,    47,     8,    11,     7,    11,    36,
      37,    38,    76,    18,    78,    79,    77,    41,     0,    41,
      20,    77,    77,    77,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,     1,    47,     3,
      40,    16,     6,    47,     8,    22,    10,    11,    12,    13,
      47,    15,    20,    20,    24,    19,     3,    47,    22,    23,
      21,     8,   126,    41,    11,   129,   127,    20,    20,    20,
      17,   127,   127,   127,    21,    20,     3,    41,    47,    41,
      21,     8,    41,    47,    11,    41,   147,   125,    25,    47,
      17,   147,   147,   147,    21,   159,    19,    41,    47,    42,
       1,   162,    20,    24,    21,     6,   162,   162,   162,    10,
      21,    12,    13,    21,    15,    21,     1,    41,    19,     4,
       9,    22,    23,    21,    28,    29,    21,    12,    32,    33,
      34,    35,    36,    37,    38,    20,    21,    21,    21,    41,
      41,    34,    35,    36,    37,    38,    47,    47,    21,    34,
      35,    25,    21,    41,    39,    80,    27,    -1,     1,    -1,
      45,    46,    47,     6,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    15,    -1,     1,    -1,    19,     4,    -1,    22,
       4,    -1,    -1,    -1,    -1,    12,    -1,    -1,    12,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    20,     4,    41,    -1,
      -1,    -1,    -1,    -1,    47,    12,    -1,    34,    35,    -1,
      34,    35,    39,    20,    -1,    39,    -1,    41,    45,    46,
      47,    45,    46,    47,    -1,     4,    -1,    34,    35,    -1,
      -1,    -1,    39,    12,     4,    -1,    -1,    44,    45,    46,
      47,    20,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    -1,    34,    35,    -1,    -1,    -1,
      39,    -1,    -1,    -1,    34,    35,    45,    46,    47,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    26,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     5,    51,    47,     0,    22,    52,     1,    14,    23,
      41,    53,    55,    41,    16,     3,     8,    11,    18,    56,
      62,    47,    22,    57,    47,    20,    58,    20,    54,    17,
      21,    59,    62,     1,     6,    10,    12,    13,    15,    19,
      22,    23,    41,    47,    61,    62,    63,    65,    66,    68,
      21,    59,    41,    42,    24,    21,    47,    41,    63,    20,
      20,    20,     4,    20,    34,    35,    39,    41,    45,    46,
      47,    65,    66,    68,    69,    70,    20,    64,    20,    40,
      47,    41,    41,    41,    21,    47,    25,    60,    19,    69,
       1,    47,    44,    69,     1,    69,    20,    47,    70,    70,
      70,     7,    41,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    69,    23,    63,     1,
      21,    69,    69,    54,    47,    42,    20,    21,    21,    24,
      21,    21,    21,    21,    70,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    70,    70,    70,    21,    21,    67,
      41,    62,    69,    63,    69,    41,    41,    63,    21,    42,
      47,    21,     9,    25,    69,    41,    63,    21
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    50,    51,    52,    52,    52,    52,    53,    53,    54,
      54,    55,    56,    56,    56,    56,    57,    58,    58,    58,
      59,    59,    60,    60,    61,    62,    62,    62,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    64,    64,    65,    66,    66,    66,    67,    67,
      68,    68,    69,    69,    69,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    70
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     5,     2,     2,     2,     0,     6,     2,     3,
       0,     4,     4,     5,     4,     5,     3,     2,     2,     0,
       4,     3,     4,     0,     4,     1,     1,     1,     3,     5,
       7,     5,     7,     5,     5,     2,     2,     2,     1,     2,
       3,     2,     2,     0,     3,     3,     5,     4,     3,     0,
       7,     4,     1,     1,     3,     1,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       2,     2,     2,     1,     1,     1,     1,     2,     3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 127 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-3].str))); if((yyvsp[-1].listaNos)!=NULL){filhos=juntarFilhos(filhos,(yyvsp[-1].listaNos));}no prog=criarNo("Program",filhos,NULL); printar(prog,0);(yyval.ptrNo)=prog;apagarArvore(prog);}}
#line 1477 "y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 129 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0) (yyval.listaNos)=(yyvsp[-1].listaNos);}
#line 1483 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 130 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=criar(); if((yyvsp[-1].listaNos)!=NULL){l=juntarFilhos(l,(yyvsp[-1].listaNos));}if((yyvsp[0].listaNos)!=NULL){l=juntarFilhos(l,(yyvsp[0].listaNos));}(yyval.listaNos)=l;}}
#line 1489 "y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 131 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=criar();if((yyvsp[-1].listaNos)!=NULL){l=juntarFilhos(l,(yyvsp[-1].listaNos));}adicionarNo(l,(yyvsp[0].ptrNo)); (yyval.listaNos)=l;}}
#line 1495 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 132 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0) (yyval.listaNos)=NULL;}
#line 1501 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 136 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=criar();adicionarNo(l,criarNo("Id",NULL,(yyvsp[-2].str))); if((yyvsp[-1].listaNos)!=NULL)l=juntarFilhos(l,(yyvsp[-1].listaNos));(yyval.listaNos)=declaracoesVariaveis("FieldDecl",(yyvsp[-3].ptrNo),l);}}
#line 1507 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 137 "jac.y" /* yacc.c:1646  */
    {}
#line 1513 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 140 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=criar();if((yyvsp[-2].listaNos)!=NULL){l=juntarFilhos(l,(yyvsp[-2].listaNos));}adicionarNo(l,criarNo("Id",NULL,(yyvsp[0].str)));(yyval.listaNos)=l;}}
#line 1519 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 141 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0) (yyval.listaNos)=NULL;}
#line 1525 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 144 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-1].ptrNo));adicionarNo(filhos,(yyvsp[0].ptrNo));(yyval.ptrNo)=criarNo("MethodDecl",filhos,NULL);}}
#line 1531 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 148 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,(yyvsp[-3].ptrNo)); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-2].str)));
													adicionarNo(filhos,criarNo("MethodParams",NULL,NULL)); (yyval.ptrNo)=criarNo("MethodHeader",filhos,NULL);}}
#line 1538 "y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 150 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,(yyvsp[-4].ptrNo)); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-3].str))); 
												 	adicionarNo(filhos,criarNo("MethodParams",(yyvsp[-1].listaNos),NULL)); (yyval.ptrNo)=criarNo("MethodHeader",filhos,NULL);}}
#line 1545 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 152 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Void",NULL,NULL)); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-2].str))); 
												 	adicionarNo(filhos,criarNo("MethodParams",NULL,NULL)); (yyval.ptrNo)=criarNo("MethodHeader",filhos,NULL);}}
#line 1552 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 154 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Void",NULL,NULL)); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-3].str))); 
												 	adicionarNo(filhos,criarNo("MethodParams",(yyvsp[-1].listaNos),NULL)); (yyval.ptrNo)=criarNo("MethodHeader",filhos,NULL);}}
#line 1559 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 158 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){(yyval.ptrNo)=criarNo("MethodBody",(yyvsp[-1].listaNos),NULL);}}
#line 1565 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 161 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=criar();if((yyvsp[-1].listaNos)!=NULL){l=juntarFilhos(l,(yyvsp[-1].listaNos));}adicionarNo(l,(yyvsp[0].ptrNo)); (yyval.listaNos)=l;}}
#line 1571 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 162 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=(yyvsp[-1].listaNos);if(l!=NULL){(yyval.listaNos)=juntarFilhos(l,(yyvsp[0].listaNos));}else{(yyval.listaNos)=(yyvsp[0].listaNos);}}}
#line 1577 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 163 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0) (yyval.listaNos)=NULL;}
#line 1583 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 166 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=criar(); adicionarNo(l,criarNo("StringArray",NULL,NULL)); adicionarNo(l,criarNo("Id",NULL,(yyvsp[0].str))); lista aux=criar();
												adicionarNo(aux,criarNo("ParamDecl",l,NULL));(yyval.listaNos)=aux;}}
#line 1590 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 168 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-1].str))); 
												no c=criarNo("ParamDecl",filhos,NULL);lista l=criar();adicionarNo(l,c); if((yyvsp[0].listaNos)!=NULL)l=juntarFilhos(l,(yyvsp[0].listaNos));(yyval.listaNos)=l;}}
#line 1597 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 172 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();lista l=criar();if((yyvsp[-3].listaNos)!=NULL){l=juntarFilhos(l,(yyvsp[-3].listaNos));}adicionarNo(filhos,(yyvsp[-1].ptrNo)); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[0].str))); no c=criarNo("ParamDecl",filhos,NULL);
												adicionarNo(l,c);(yyval.listaNos)=l;}}
#line 1604 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 174 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.listaNos)=NULL;}
#line 1610 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 177 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=criar();adicionarNo(l,criarNo("Id",NULL,(yyvsp[-2].str))); if((yyvsp[-1].listaNos)!=NULL)l=juntarFilhos(l,(yyvsp[-1].listaNos));(yyval.listaNos)=declaracoesVariaveis("VarDecl",(yyvsp[-3].ptrNo),l);}}
#line 1616 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 180 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=criarNo("Bool",NULL,NULL);}
#line 1622 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 181 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=criarNo("Int",NULL,NULL);}
#line 1628 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 182 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=criarNo("Double",NULL,NULL);}
#line 1634 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 189 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=(yyvsp[-1].listaNos); if(l==NULL){(yyval.ptrNo)=NULL;}else{ if(l->nrElementos>1){(yyval.ptrNo)=criarNo("Block",l,NULL);}else if(l->nrElementos==1){ (yyval.ptrNo)=l->raiz;}else{(yyval.ptrNo)=NULL;}}}}
#line 1640 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 190 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,(yyvsp[-2].ptrNo));if((yyvsp[0].ptrNo)!=NULL){adicionarNo(filhos,(yyvsp[0].ptrNo));} else{adicionarNo(filhos, criarNo("Block",NULL,NULL));}
		 												adicionarNo(filhos,criarNo("Block",NULL,NULL));(yyval.ptrNo)=criarNo("If",filhos,NULL);}}
#line 1647 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 192 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,(yyvsp[-4].ptrNo));if((yyvsp[-2].ptrNo)!=NULL){adicionarNo(filhos,(yyvsp[-2].ptrNo));} else{adicionarNo(filhos, criarNo("Block",NULL,NULL));}
		 												if((yyvsp[0].ptrNo)!=NULL){adicionarNo(filhos,(yyvsp[0].ptrNo));} else{adicionarNo(filhos, criarNo("Block",NULL,NULL));}(yyval.ptrNo)=criarNo("If",filhos,NULL);}}
#line 1654 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 194 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,(yyvsp[-2].ptrNo));if((yyvsp[0].ptrNo)!=NULL){adicionarNo(filhos,(yyvsp[0].ptrNo));} else{adicionarNo(filhos, criarNo("Block",NULL,NULL));} (yyval.ptrNo)=criarNo("While",filhos,NULL);}}
#line 1660 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 195 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();if((yyvsp[-5].ptrNo)!=NULL){adicionarNo(filhos,(yyvsp[-5].ptrNo));}else{adicionarNo(filhos, criarNo("Block",NULL,NULL));} adicionarNo(filhos,(yyvsp[-2].ptrNo));(yyval.ptrNo)=criarNo("DoWhile",filhos,NULL);}}
#line 1666 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 196 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,(yyvsp[-2].ptrNo));(yyval.ptrNo)=criarNo("Print",filhos,NULL);}}
#line 1672 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 197 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("StrLit",NULL,(yyvsp[-2].str)));(yyval.ptrNo)=criarNo("Print",filhos,NULL);}}
#line 1678 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 198 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=(yyvsp[-1].ptrNo);}
#line 1684 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 199 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=(yyvsp[-1].ptrNo);}
#line 1690 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 200 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=(yyvsp[-1].ptrNo);}
#line 1696 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 201 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=NULL;}
#line 1702 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 202 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){(yyval.ptrNo)=criarNo("Return",NULL,NULL);}}
#line 1708 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 203 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,(yyvsp[-1].ptrNo));(yyval.ptrNo)=criarNo("Return",filhos,NULL);}}
#line 1714 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 204 "jac.y" /* yacc.c:1646  */
    {}
#line 1720 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 207 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista l=criar();if((yyvsp[-1].listaNos)==NULL && (yyvsp[0].ptrNo)==NULL){(yyval.listaNos)=NULL;}else{if((yyvsp[-1].listaNos)!=NULL){l=juntarFilhos(l,(yyvsp[-1].listaNos));} adicionarNo(l,(yyvsp[0].ptrNo));(yyval.listaNos)=l;}}}
#line 1726 "y.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 208 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.listaNos)=NULL;}
#line 1732 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 211 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-2].str)));adicionarNo(filhos,(yyvsp[0].ptrNo)); 
                                                        (yyval.ptrNo)=criarNo("Assign",filhos,NULL);}}
#line 1739 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 215 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-2].str)));(yyval.ptrNo)=criarNo("Call",filhos,NULL);}}
#line 1745 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 216 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-4].str)));adicionarNo(filhos,(yyvsp[-2].ptrNo)); if((yyvsp[-1].listaNos)!=NULL){
														filhos=juntarFilhos(filhos,(yyvsp[-1].listaNos));}(yyval.ptrNo)=criarNo("Call",filhos,NULL);}}
#line 1752 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 218 "jac.y" /* yacc.c:1646  */
    {}
#line 1758 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 221 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar();if((yyvsp[-2].listaNos)!=NULL){filhos=juntarFilhos(filhos,(yyvsp[-2].listaNos));}adicionarNo(filhos,(yyvsp[0].ptrNo));(yyval.listaNos)=filhos;}}
#line 1764 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 222 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.listaNos)=NULL;}
#line 1770 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 225 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-4].str)));adicionarNo(filhos,(yyvsp[-2].ptrNo));(yyval.ptrNo)=criarNo("ParseArgs",filhos,NULL);}}
#line 1776 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 226 "jac.y" /* yacc.c:1646  */
    {}
#line 1782 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 228 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){(yyval.ptrNo)=(yyvsp[0].ptrNo);}}
#line 1788 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 229 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){(yyval.ptrNo)=(yyvsp[0].ptrNo);}}
#line 1794 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 230 "jac.y" /* yacc.c:1646  */
    {}
#line 1800 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 233 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){(yyval.ptrNo)=(yyvsp[0].ptrNo);}}
#line 1806 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 234 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){(yyval.ptrNo)=(yyvsp[0].ptrNo);}}
#line 1812 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 235 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("And",filhos,NULL);}}
#line 1818 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 236 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Or",filhos,NULL);}}
#line 1824 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 237 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Eq",filhos,NULL);}}
#line 1830 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 238 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Geq",filhos,NULL);}}
#line 1836 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 239 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Gt",filhos,NULL);}}
#line 1842 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 240 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Leq",filhos,NULL);}}
#line 1848 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 241 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Lt",filhos,NULL);}}
#line 1854 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 242 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Neq",filhos,NULL);}}
#line 1860 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 243 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Add",filhos,NULL);}}
#line 1866 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 244 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Sub",filhos,NULL);}}
#line 1872 "y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 245 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Mul",filhos,NULL);}}
#line 1878 "y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 246 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Div",filhos,NULL);}}
#line 1884 "y.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 247 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[-2].ptrNo)); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Mod",filhos,NULL);}}
#line 1890 "y.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 248 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Plus",filhos,NULL);}}
#line 1896 "y.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 249 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Minus",filhos,NULL);}}
#line 1902 "y.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 250 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,(yyvsp[0].ptrNo)); (yyval.ptrNo)=criarNo("Not",filhos,NULL);}}
#line 1908 "y.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 251 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=criarNo("BoolLit",NULL,(yyvsp[0].str));}
#line 1914 "y.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 252 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=criarNo("DecLit",NULL,(yyvsp[0].str));}
#line 1920 "y.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 253 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=criarNo("RealLit",NULL,(yyvsp[0].str));}
#line 1926 "y.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 254 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0)(yyval.ptrNo)=criarNo("Id",NULL,(yyvsp[0].str));}
#line 1932 "y.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 255 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,(yyvsp[-1].str)));(yyval.ptrNo)=criarNo("Length",filhos,NULL);}}
#line 1938 "y.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 256 "jac.y" /* yacc.c:1646  */
    {if(t==1 && erros==0){(yyval.ptrNo)=(yyvsp[-1].ptrNo);}}
#line 1944 "y.tab.c" /* yacc.c:1646  */
    break;


#line 1948 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 263 "jac.y" /* yacc.c:1906  */


void yyerror (const char *s) { 
	erros++;
	// so imprime erros de sintaxe se flag -l e -1 nao selecionada
	if(flag==0){
		printf ("Line %d, col %d: %s: %s\n", linhas, (int)(colunas-strlen(yytext)), s, yytext); 
	} 
}
