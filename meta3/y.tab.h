/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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

union YYSTYPE
{
#line 36 "jac.y" /* yacc.c:1909  */
 
	
	lexInfo*  info; 

	no ptrNo;
	lista listaNos;

#line 160 "y.tab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
