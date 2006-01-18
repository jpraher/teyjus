/****************************************************************************/
/*                                                                          */
/* File dataformat.c.                                                       */
/* The header file identifies the low-level representation of data objects  */
/* that are manipulated by the machine, through various structure types.    */ 
/****************************************************************************/
#ifndef DATAFORMAT_C
#define DFTAFORMAT_C

#include <math.h>
#include <string.h>
#include "dataformat.h"


/********************************************************************/
/*                                                                  */
/*          Type Representation                                     */
/*                                                                  */
/********************************************************************/
typedef struct                  //sorts
{
    DF_TY_TAG         tag;       //DF_TY_TAG_SORT
    DF_TY_TABIND      kindTableIndex;
} DF_TY_SORT;


typedef struct                  //reference
{
    DF_TY_TAG         tag;       //DF_TY_TAG_REF
    DF_TYPE_PTR       target;
} DF_TY_REF;


typedef struct                  //type arrows
{
    DF_TY_TAG         tag;       //DF_TY_TAG_ARROW
    DF_TYPE_PTR       args;
} DF_TY_ARROW;

//type functors
typedef struct
{
    DF_TY_ARITY       arity;
    DF_TY_TABIND      kindTableIndex;
} DF_TY_FUNC;

typedef struct                   //types structures
{
    DF_TY_TAG         tag;       //DF_TY_TAG_STR
    DF_TY_FUNC        *funcArgs;
} DF_TY_STR;


/*interface functions for type recognization */

BOOLEAN DF_TY_IsSort(DF_TYPE_PTR tp)  // is sort?
{
    return (tp -> tag == DF_TY_TAG_SORT);
}

BOOLEAN DF_TY_IsRef(DF_TYPE_PTR tp)   // is reference? (including free var)
{
    return (tp -> tag == DF_TY_TAG_REF);
}

BOOLEAN DF_TY_IsFreeVar(DF_TYPE_PTR tp)  // is free var?
{
    return ((tp -> tag == DF_TY_TAG_REF) && (((DF_TY_REF*)tp) -> target == tp));
}

BOOLEAN DF_TY_IsArrow(DF_TYPE_PTR tp)   // is type arrow?
{
    return (tp -> tag == DF_TY_TAG_ARROW);
}

BOOLEAN DF_TY_IsStr(DF_TYPE_PTR tp)     // is type structure?
{
    return (tp -> tag == DF_TY_TAG_STR);
}

/*interface functions for type decomposition */
//generic type
DF_TY_TAG DF_TY_Tag(DF_TYPE_PTR tp)   // extracting tag
{
    return tp -> tag;
}

//sorts
DF_TY_TABIND DF_TY_KTableIndex(DF_TYPE_PTR tp) // extracting kind table index
{
    return ((DF_TY_SORT*)tp) -> kindTableIndex;
}

//reference
DF_TYPE_PTR DF_TY_RefTarget(DF_TYPE_PTR tp) //extracting reference target
{
    return ((DF_TY_REF*)tp) -> target;
}


//arrows
DF_TYPE_PTR DF_TY_ArrowArgs(DF_TYPE_PTR tp) //extracting addr of args
{
    return ((DF_TY_ARROW*)tp) -> args;
}

//structures

DF_TY_TABIND DF_TY_StrFunc(DF_TYPE_PTR tp) //extracting kind table index of 
                                          //functor
{
    return (((DF_TY_STR*)tp) -> funcArgs) -> kindTableIndex;
}

DF_TY_ARITY DF_TY_StrArity(DF_TYPE_PTR tp) //extracting arity of functor
{
    return (((DF_TY_STR*)tp) -> funcArgs) -> arity;
}


DF_TYPE_PTR DF_TY_StrArgs(DF_TYPE_PTR tp) //extracting address of arg vector
{
    return (DF_TYPE_PTR)((((DF_TY_STR*)tp) -> funcArgs)) + 1;
}

//extracting the address of functor which is immediately followed by arg vector
DF_TYPE_PTR DF_TY_StrFuncAndArgs(DF_TYPE_PTR tp) 
{
    return (DF_TYPE_PTR)(((DF_TY_STR*)tp) -> funcArgs);
}

/********************************************/
/* interface functions for type composition */
/********************************************/
//sort
void DF_TY_MkSort_(DF_TYPE_PTR tp, DF_TY_TABIND kindTableIndex)
{
    ((DF_TY_SORT*)tp) -> tag = DF_TY_TAG_SORT;
    ((DF_TY_SORT*)tp) -> kindTableIndex = kindTableIndex;
}

DF_TYPE_PTR DF_TY_MkSort(DF_TYPE_PTR tp, DF_TY_TABIND kindTableIndex)
{
    ((DF_TY_SORT*)tp) -> tag = DF_TY_TAG_SORT;
    ((DF_TY_SORT*)tp) -> kindTableIndex = kindTableIndex;
    return (tp + 1);
}

//reference
void DF_TY_MkRef_(DF_TYPE_PTR tp, DF_TYPE_PTR target) 
{
    ((DF_TY_REF*)tp) -> tag = DF_TY_TAG_REF;
    ((DF_TY_REF*)tp) -> target = target;
}

DF_TYPE_PTR DF_TY_MkRef(DF_TYPE_PTR tp, DF_TYPE_PTR target)
{
    ((DF_TY_REF*)tp) -> tag = DF_TY_TAG_REF;
    ((DF_TY_REF*)tp) -> target = target;
    return (tp + 1);
}

//free variable
void DF_TY_MkFreeVar_(DF_TYPE_PTR tp)
{
    ((DF_TY_REF*)tp) -> tag = DF_TY_TAG_REF;
    ((DF_TY_REF*)tp) -> target = tp;
}

DF_TYPE_PTR DF_TY_MkFreeVar(DF_TYPE_PTR tp)
{
    ((DF_TY_REF*)tp) -> tag = DF_TY_TAG_REF;
    ((DF_TY_REF*)tp) -> target = tp;
    return (tp + 1);
}

//arrows
void DF_TY_MkArrow_(DF_TYPE_PTR tp, DF_TYPE_PTR args)
{
    ((DF_TY_ARROW*)tp) -> tag = DF_TY_TAG_ARROW;
    ((DF_TY_ARROW*)tp) -> args = args;
}

DF_TYPE_PTR DF_TY_MkArrow(DF_TYPE_PTR tp, DF_TYPE_PTR args)
{
    ((DF_TY_ARROW*)tp) -> tag = DF_TY_TAG_ARROW;
    ((DF_TY_ARROW*)tp) -> args = args;
    return (tp + 1);
}


//structures
void DF_TY_MkStr_(DF_TYPE_PTR tp, DF_TYPE_PTR func)
{
    ((DF_TY_STR*)tp) -> tag = DF_TY_TAG_STR;
    ((DF_TY_STR*)tp) -> funcArgs = (DF_TY_FUNC*)func;
}

DF_TYPE_PTR DF_TY_MkStr(DF_TYPE_PTR tp, DF_TYPE_PTR func)
{
    ((DF_TY_STR*)tp) -> tag = DF_TY_TAG_STR;
    ((DF_TY_STR*)tp) -> funcArgs = (DF_TY_FUNC*)func;
    return (tp + 1);
}


//functor can only be created on the heap
DF_TYPE_PTR DF_TY_MkFunc(DF_TYPE_PTR func, DF_TY_TABIND index, 
                         DF_TY_ARITY arity)
{
    ((DF_TY_FUNC*)func) -> arity = arity;
    ((DF_TY_FUNC*)func) -> kindTableIndex = index;
    return (DF_TYPE_PTR)(func + 1);
}

/********************/ 
/* type dereference */
/********************/
DF_TYPE_PTR DF_TY_Deref(DF_TYPE_PTR tp)
{
    while ((DF_TY_IsRef(tp)) && (tp != ((DF_TY_REF*)tp) -> target)) 
        tp = ((DF_TY_REF*)tp) -> target;
    return tp;
}

  
/****************************************************************************
 *                                                                          *
 *                         TERM REPRESENTATION                              *
 *                                                                          *
 ****************************************************************************/
typedef struct                  //existential variable
{
    DF_TAG           tag;      // DF_TM_TAG_VAR
    DF_UNIVIND       univIndex;
} DF_VAR;


typedef struct              //de Bruijn index 
{  
    DF_TAG        tag;      // DF_TM_TAG_BV
    DF_EMBEDLEV   index;
} DF_BV;

/*Note that the first three common fields of un-typed and typed constant have
  to be kept in the same places. This relies on the common struct alignment
  rules of C. */
typedef struct              //(un-typed) constant
{
    DF_TAG        tag;      // DF_TM_TAG_CONST
    DF_UNIVIND    univIndex;
    DF_TABIND     symbolTableIndex;
} DF_CONST;

typedef struct              // typed constant: TERM_DF_TAG_CONST
{
    DF_TAG        tag;      // DF_TM_TAG_CONST
    DF_UNIVIND    univIndex;
    DF_TABIND     symbolTableIndex;
    DF_TYPE       *typeEnv;
} DF_TCONST;


typedef struct             // integers
{
    DF_TAG        tag;     // DF_TM_TAG_INT
    long int      value;
} DF_INT;


typedef struct             // floats
{
    DF_TAG        tag;     // DF_TM_TAG_FLOAT
    float         value;
} DF_FLOAT;


typedef struct             // string
{
    DF_TAG             tag;     // DF_TM_TAG_STR
    unsigned short int length; 
    char               *value;
} DF_STR;


typedef struct             // stream
{
    DF_TAG        tag;     // DF_TM_TAG_STREAM
    DF_TABIND     index;
} DF_STREAM;

typedef struct             // empty list 
{
    DF_TAG        tag;     // DF_TM_TAG_NIL
} DF_NIL;


typedef struct             // reference  
{
    DF_TAG        tag;     // DF_TM_TAG_REF
    DF_TERM_PTR   target;
} DF_REF;

typedef struct             // list constructor
{
    DF_TAG        tag;     // DF_TM_TAG_CONS
    DF_TERM_PTR   args;
} DF_CONS;


typedef struct             // abstractions
{
    DF_TAG        tag;     // DF_TM_TAG_LAM
    DF_EMBEDLEV   embedLevel; 
    DF_TERM_PTR   body;
} DF_LAM;

typedef struct             // applications
{
    DF_TAG        tag;     // DF_TM_TAG_APP
    DF_ARITY      arity;
    void*         dummy;   // place holder enforcing an atomic term size before
                           // functor (protect functor in destructive change)
    DF_TERM       functor;
    DF_TERM_PTR   args;
} DF_APP;


typedef struct            //suspensions 
{
    DF_TAG         tag;   // DF_TM_TAG_SUSP
    DF_EMBEDLEV    ol;
    DF_EMBEDLEV    nl;
    DF_TERM_PTR    termSkel;
    DF_ENV_PTR     envList;
} DF_SUSP;


/* The sizes in words of various terms (may be redudant) */

/* atomic terms and cons: their sizes are enforced to be two words */
/*
#define  DF_ATOMSIZE   2

#define  DF_VARSIZE    2
#define  DF_BVSIZE     2
#define  DF_CONSTSIZE  2
#define  DF_INTSIZE    2
#define  DF_FLOATSIZE  2
#define  DF_STRSIZE    2
#define  DF_STREAMSIZE 2
#define  DF_NILSIZE    2
#define  DF_REFSIZE    2

#define  DF_CONSSIZE   2
*/
/* The sizes of TCONST and SUSP are different on 32- and 64-bits machines */
/*
#define  DF_TCONSTSIZE (int)ceil(sizeof(DF_TCONST)/WORDSIZE)
#define  DF_SUSPSIZE   (int)ceil(sizeof(DF_SUSP)/WORDSIZE)
#define  DF_APPSIZE    4
#define  DF_LAMSIZE    2
*/
/* The sizes of enviroment items */
/*
#define  DF_ENVSIZE        3
#define  DF_DUMMYENVSIZE   2
*/

/*Recognizers for terms */
BOOLEAN DF_IsAtomic(DF_TERM_PTR tp) /* whether a term is atomic: ref is not
                                       included in atomic terms nor non-atomic
                                       ones */
{
    return (tp -> tag < DF_TM_TAG_REF);
}

BOOLEAN DF_IsNAtomic(DF_TERM_PTR tp)
{
    return (tp -> tag > DF_TM_TAG_REF);
}


BOOLEAN DF_IsFV(DF_TERM_PTR tp)   //is var?
{
    return (tp -> tag == DF_TM_TAG_VAR);
}

BOOLEAN DF_IsConst(DF_TERM_PTR tp) //is constant (typed/untyped)?
{
    return (tp -> tag == DF_TM_TAG_CONST);
}

BOOLEAN DF_IsInt(DF_TERM_PTR tp)   //is integer?
{
    return (tp -> tag == DF_TM_TAG_INT);
}

BOOLEAN DF_IsFloat(DF_TERM_PTR tp) //is float?
{
    return (tp -> tag == DF_TM_TAG_FLOAT);
}

BOOLEAN DF_IsNil(DF_TERM_PTR tp)   //is empty list?
{
    return (tp -> tag == DF_TM_TAG_NIL);
}

BOOLEAN DF_IsStr(DF_TERM_PTR tp)   //is string?
{
    return (tp -> tag == DF_TM_TAG_STR);
}

BOOLEAN DF_IsBV(DF_TERM_PTR tp)  //is lambda bound variable?
{
    return (tp -> tag == DF_TM_TAG_BVAR);
}


BOOLEAN DF_IsStream(DF_TERM_PTR tp) //is stream?
{
    return (tp -> tag == DF_TM_TAG_STREAM);
}

BOOLEAN DF_IsRef(DF_TERM_PTR tp)   //is reference?
{
    return (tp -> tag == DF_TM_TAG_REF);
}

BOOLEAN DF_IsCons(DF_TERM_PTR tp)  //is list cons?
{
    return (tp -> tag == DF_TM_TAG_CONS);
}

BOOLEAN DF_IsLam(DF_TERM_PTR tp)   //is abstraction?
{
    return (tp -> tag == DF_TM_TAG_LAM);
}

BOOLEAN DF_IsApp(DF_TERM_PTR tp)   //is application?
{
    return (tp-> tag == DF_TM_TAG_APP);
}

BOOLEAN DF_isSusp(DF_TERM_PTR tp)  //is suspension?
{
    return (tp-> tag == DF_TM_TAG_SUSP);
}

//environment item (list)

BOOLEAN DF_EmptyEnv(DF_ENV_PTR ep) //is empty environment list?
{
    return (ep == DF_EMPTYENV);
}

BOOLEAN DF_IsDummy(DF_ENV_PTR ep) /*for both ENV and DUMMYENV: rely on struct 
                                    alignment rules */
{
    return ep -> isDummy;
}


/* Term decomposition */

//generic terms
DF_TAG DF_Tag(DF_TERM_PTR tp) // exacting tag 
{
    return tp -> tag;
}


//variable and constant (typed/untyped)
DF_UNIVIND DF_UnivIndex(DF_TERM_PTR tp)  /*extracting universe index
                                           for VAR, CONST and TCONST: 
                                           rely on struct alignment rules */
{
    return ((DF_VAR*)tp)->univIndex;
}


//constant (typed/untyped)
DF_UNIVIND DF_ConstUnivIndex(DF_TERM_PTR tp) /*extracting universe index
                                               for both CONST and TCONST: 
                                               rely on struct alignment rules */
{
    return ((DF_CONST*)tp)->univIndex;
}

DF_TABIND DF_ConstTabIndex(DF_TERM_PTR tp) /*extracting symbol table index 
                                             for both CONST and TCONST: rely on
                                             struct alignment rules   */
{
    return ((DF_CONST*)tp)->symbolTableIndex;
}


//typed constants
DF_TYPE* DF_TConstType(DF_TERM_PTR tp) //extracting the address of type env
{
    return ((DF_TCONST*)tp)->typeEnv;
}


//integer
long DF_IntValue(DF_TERM_PTR tp)       //extracting integer value
{
    return ((DF_INT*)tp)->value;
}
long DF_IntABS(DF_TERM_PTR tp)         //absolute value
{
    return labs(((DF_INT*)tp)->value);
}
long DF_IntNEG(DF_TERM_PTR tp)      //negation
{
    return -(((DF_INT*)tp)->value);
}

//float
float DF_FloatValue(DF_TERM_PTR tp)    //extracting float value
{
    return ((DF_FLOAT*)tp)->value;
}
float DF_FloatABS(DF_TERM_PTR tp)      //absolute value
{
    return fabs(((DF_FLOAT*)tp)->value);
}
float DF_FloatNEG(DF_TERM_PTR tp)      //negation
{
    return -(((DF_FLOAT*)tp)->value);
}

//string
char* DF_StrValue(DF_TERM_PTR tp)      //extracting string value
{
    return ((DF_STR*)tp)->value;
}
int   DF_StrLength(DF_TERM_PTR tp)     //extracting string length
{
    return (int)(((DF_STR*)tp)->length);
}

    
//stream
DF_TABIND DF_StreamTabIndex(DF_TERM_PTR tp)  //extracting stream index
{
    return ((DF_STREAM*)tp)->index;
}


//lambda bound variable 
DF_EMBEDLEV DF_BVIndex(DF_TERM_PTR tp)      //extracting de Bruijn index
{
    return ((DF_BV*)tp)->index;
}


//reference
DF_TERM_PTR DF_RefTarget(DF_TERM_PTR tp)    //extracting the address of target
{ 
    return ((DF_REF*)tp)->target;
}

//list cons
DF_TERM_PTR DF_ConsArgs(DF_TERM_PTR tp)    //extracting the address of cons args
{
    return ((DF_CONS*)tp)->args;
}


//abstraction
DF_EMBEDLEV DF_LamEmbedLev(DF_TERM_PTR tp)  //extracting abstraction level
{
    return ((DF_LAM*)tp)->embedLevel;
}

DF_TERM_PTR DF_LamBody(DF_TERM_PTR tp)   //extracting addr of abstraction body
{
    return ((DF_LAM*)tp)->body;
}

//application
DF_ARITY DF_AppArity(DF_TERM_PTR tp)    //extracting arity
{
    return ((DF_APP*)tp)->arity;
}

DF_TERM_PTR DF_AppFunc(DF_TERM_PTR tp) //extracting addr of functor
{
    return &(((DF_APP*)tp)->functor);
}

DF_TERM DF_AppFuncTerm(DF_TERM_PTR tp)       //extracting the functor
{
    return ((DF_APP*)tp)->functor;
}


DF_TERM_PTR DF_AppArgs(DF_TERM_PTR tp)   //extracting addr of arg vector
{
    return ((DF_APP*)tp)->args;
}


//suspension
DF_EMBEDLEV DF_SuspOL(DF_TERM_PTR tp)    //extracting ol
{
    return ((DF_SUSP*)tp)->ol;
}

DF_EMBEDLEV DF_SuspNL(DF_TERM_PTR tp)    //extracting nl
{
    return ((DF_SUSP*)tp)->nl;
}

DF_TERM_PTR DF_SuspTermSkel(DF_TERM_PTR tp)  //extracting addr of term skel
{
    return ((DF_SUSP*)tp)->termSkel;
}

DF_ENV_PTR DF_SuspEnv(DF_TERM_PTR tp)  //extracting addr of environment list
{
    return ((DF_SUSP*)tp)->envList;
}


//environment item (dummy/pair) 
DF_EMBEDLEV DF_EnvIndex(DF_ENV_PTR ep)  /*extracting l in @l or (t,l)
                                        for both ENV and DUMMYENV: rely on 
                                        struct alignment rules */
{
    return ep -> embedLevel;
}

//pair environment item
DF_TERM_PTR DF_EnvTerm(DF_ENV_PTR ep) //extracting t in (t,l)
{
    return ep -> term;
}

//environment list
DF_ENV_PTR DF_EnvListRest(DF_ENV_PTR ep) /* extracting the tail of env list
                                            for both ENV and DUMMYENV: 
                                            rely on struct alignment rules */
{
    return ep->rest;
}

DF_ENV_PTR DF_EnvNth(DF_ENV_PTR ep, int n) //extracting the nth item 
{
    int i; 
    for (i=n; (i!=1); i--) ep = ep -> rest;
    return ep;
}


/* Term Construction */

//copy atomic terms
void DF_CopyAtom_(DF_TERM_PTR from, DF_TERM_PTR to)
{
    *to = *from;
}

DF_TERM_PTR DF_CopyAtom(DF_TERM_PTR from, DF_TERM_PTR to)
{
    *to = *from;
    return (to+1);
}


//variable
void DF_MkVar_(DF_TERM_PTR tp, DF_UNIVIND univIndex)
{
    ((DF_VAR*)tp) -> tag = DF_TM_TAG_VAR;    
    ((DF_VAR*)tp) -> univIndex = univIndex;
}


DF_TERM_PTR DF_MkVar(DF_TERM_PTR tp, DF_UNIVIND univIndex)
{
    ((DF_VAR*)tp) -> tag = DF_TM_TAG_VAR;
    ((DF_VAR*)tp) -> univIndex = univIndex;
    return tp + 1;
}

//lambda bound variable
void DF_MkBV_(DF_TERM_PTR tp, DF_EMBEDLEV index)
{
    ((DF_BV*)tp) -> tag = DF_TM_TAG_BVAR;
    ((DF_BV*)tp) -> index = index;
}

DF_TERM_PTR DF_MkBV(DF_TERM_PTR tp, DF_EMBEDLEV index)
{
    ((DF_BV*)tp) -> tag = DF_TM_TAG_BVAR;
    ((DF_BV*)tp) -> index = index;
    return tp + 1;
}

//untyped constant
void DF_MkConst_(DF_TERM_PTR tp, DF_UNIVIND univIndex, 
                 DF_TABIND symbolTableIndex)
{
    ((DF_CONST*)tp) -> tag = DF_TM_TAG_CONST;
    ((DF_CONST*)tp) -> univIndex = univIndex;
    ((DF_CONST*)tp) -> symbolTableIndex = symbolTableIndex;
}

DF_TERM_PTR DF_MkConst(DF_TERM_PTR tp, DF_UNIVIND univIndex, 
                       DF_TABIND symbolTableIndex)
{
    ((DF_CONST*)tp) -> tag = DF_TM_TAG_CONST;
    ((DF_CONST*)tp) -> univIndex = univIndex;
    ((DF_CONST*)tp) -> symbolTableIndex = symbolTableIndex;
    return tp + 1;
}

//typed constant
void DF_MkTConst_(DF_TERM_PTR tp, DF_UNIVIND univIndex, 
                  DF_TABIND symbolTableIndex, DF_TYPE* typeEnv)
{
    ((DF_TCONST*)tp) -> tag = DF_TM_TAG_CONST;
    ((DF_TCONST*)tp) -> univIndex = univIndex;
    ((DF_TCONST*)tp) -> symbolTableIndex = symbolTableIndex;
    ((DF_TCONST*)tp) -> typeEnv = typeEnv;
}

DF_TERM_PTR DF_MkTConst(DF_TERM_PTR tp, DF_UNIVIND univIndex, 
                   DF_TABIND symbolTableIndex, DF_TYPE* typeEnv)
{
    ((DF_TCONST*)tp) -> tag = DF_TM_TAG_CONST;
    ((DF_TCONST*)tp) -> univIndex = univIndex;
    ((DF_TCONST*)tp) -> symbolTableIndex = symbolTableIndex;
    ((DF_TCONST*)tp) -> typeEnv = typeEnv;
    
    tp = (DF_TERM_PTR)(((DF_TCONST*)tp) + 1);
    return tp;
}

//integer
void DF_MkInt_(DF_TERM_PTR tp, long value)
{
    ((DF_INT*)tp) -> tag = DF_TM_TAG_INT;
    ((DF_INT*)tp) -> value = value;
}

DF_TERM_PTR DF_MkInt(DF_TERM_PTR tp, long value)
{
    ((DF_INT*)tp) -> tag = DF_TM_TAG_INT;
    ((DF_INT*)tp) -> value = value;
    return tp + 1;
}


//float
void DF_MkFloat_(DF_TERM_PTR tp, float value)
{
    ((DF_FLOAT*)tp) -> tag = DF_TM_TAG_FLOAT;
    ((DF_FLOAT*)tp) -> value = value;
}

DF_TERM_PTR DF_MkFloat(DF_TERM_PTR tp, float value)
{
    ((DF_FLOAT*)tp) -> tag = DF_TM_TAG_FLOAT;
    ((DF_FLOAT*)tp) -> value = value;
    return tp + 1;
}

//string
void DF_MkStr_(DF_TERM_PTR tp, char* value)
{
    ((DF_STR*)tp) -> tag = DF_TM_TAG_STR;
    ((DF_STR*)tp) -> length = strlen(value);
    ((DF_STR*)tp) -> value = value;
}

DF_TERM_PTR DF_MkStr(DF_TERM_PTR tp, char* value)
{
    ((DF_STR*)tp) -> tag = DF_TM_TAG_STR;
    ((DF_STR*)tp) -> length = strlen(value);
    ((DF_STR*)tp) -> value = value;
    return tp + 1;
}
                
//stream
void DF_MkStream_(DF_TERM_PTR tp, DF_TABIND index)
{
    ((DF_STREAM*)tp) -> tag = DF_TM_TAG_STREAM;
    ((DF_STREAM*)tp) -> index = index;
}

DF_TERM_PTR DF_MkStream(DF_TERM_PTR tp, DF_TABIND index)
{
    ((DF_STREAM*)tp) -> tag = DF_TM_TAG_STREAM;
    ((DF_STREAM*)tp) -> index = index;
    return tp + 1;
}


//empty list
void DF_MkNil_(DF_TERM_PTR tp)
{
    ((DF_NIL*)tp) -> tag = DF_TM_TAG_NIL;
}

DF_TERM_PTR DF_MkNil(DF_TERM_PTR tp)
{
    ((DF_NIL*)tp) -> tag = DF_TM_TAG_NIL;
    return tp + 1;
}


//reference
void DF_MkRef_(DF_TERM_PTR tp, DF_TERM_PTR target)
{
    ((DF_REF*)tp) -> tag = DF_TM_TAG_REF;
    ((DF_REF*)tp) -> target = target;
}

DF_TERM_PTR DF_MkRef(DF_TERM_PTR tp, DF_TERM_PTR target)
{
    ((DF_REF*)tp) -> tag = DF_TM_TAG_REF;
    ((DF_REF*)tp) -> target = target;
    return tp + 1;
}

//list cons
void DF_MkCons_(DF_TERM_PTR tp, DF_TERM_PTR args)
{
    ((DF_CONS*)tp) -> tag = DF_TM_TAG_CONS;
    ((DF_CONS*)tp) -> args = args;
}

DF_TERM_PTR DF_MkCons(DF_TERM_PTR tp, DF_TERM_PTR args)
{
    ((DF_CONS*)tp) -> tag = DF_TM_TAG_CONS;
    ((DF_CONS*)tp) -> args = args;
    return tp + 1;
}

//abstraction
void DF_MkLam_(DF_TERM_PTR tp, DF_EMBEDLEV embedLevel, DF_TERM_PTR body)
{
    ((DF_LAM*)tp) -> tag = DF_TM_TAG_LAM;
    ((DF_LAM*)tp) -> embedLevel = embedLevel;
    ((DF_LAM*)tp) -> body = body;
}

DF_TERM_PTR DF_MkLam(DF_TERM_PTR tp, DF_EMBEDLEV embedLevel, DF_TERM_PTR body)
{
    ((DF_LAM*)tp) -> tag = DF_TM_TAG_LAM;
    ((DF_LAM*)tp) -> embedLevel = embedLevel;
    ((DF_LAM*)tp) -> body = body;
    return tp + 1;
}

//application
void DF_MkApp_(DF_TERM_PTR tp, DF_ARITY arity, DF_TERM_PTR func, 
               DF_TERM_PTR args)
{
    ((DF_APP*)tp) -> tag = DF_TM_TAG_APP;
    ((DF_APP*)tp) -> arity = arity;
    ((DF_APP*)tp) -> functor = *func;
    
    ((DF_APP*)tp) -> args = args;
}

DF_TERM_PTR DF_MkApp(DF_TERM_PTR tp, DF_ARITY arity, DF_TERM_PTR func, 
                     DF_TERM_PTR args)
{
    ((DF_APP*)tp) -> tag = DF_TM_TAG_APP;
    ((DF_APP*)tp) -> arity = arity;
    ((DF_APP*)tp) -> functor = *func;    
    ((DF_APP*)tp) -> args = args;
    
    tp = (DF_TERM_PTR)(((DF_APP*)tp) + 1);
    return tp;
}


//suspension
void DF_MkSusp_(DF_TERM_PTR tp, DF_EMBEDLEV ol, DF_EMBEDLEV nl, 
                DF_TERM_PTR termSkel, DF_ENV_PTR envList)
{
    ((DF_SUSP*)tp) -> tag = DF_TM_TAG_SUSP;
    ((DF_SUSP*)tp) -> ol = ol;
    ((DF_SUSP*)tp) -> nl = nl;
    ((DF_SUSP*)tp) -> termSkel = termSkel;
    ((DF_SUSP*)tp) -> envList = envList;
}

DF_TERM_PTR DF_MkSusp(DF_TERM_PTR tp, DF_EMBEDLEV ol, DF_EMBEDLEV nl, 
                      DF_TERM_PTR termSkel, DF_ENV_PTR envList)
{
    ((DF_SUSP*)tp) -> tag = DF_TM_TAG_SUSP;
    ((DF_SUSP*)tp) -> ol = ol;
    ((DF_SUSP*)tp) -> nl = nl;
    ((DF_SUSP*)tp) -> termSkel = termSkel;
    ((DF_SUSP*)tp) -> envList = envList;
    
    tp = (DF_TERM_PTR)(((DF_SUSP*)tp) + 1);
    return tp;
}

//pair environment item
void DF_MkEnv_(DF_ENV_PTR ep, DF_ENV_PTR rest, DF_EMBEDLEV embedLevel,
               DF_TERM_PTR term)
{
    ep -> rest = rest;
    ep -> isDummy = 0;
    ep -> embedLevel = embedLevel;
    ep -> term = term;
}

DF_ENV_PTR DF_MkEnv(DF_ENV_PTR ep, DF_ENV_PTR rest, DF_EMBEDLEV embedLevel,
                    DF_TERM_PTR term)
{
    ep -> rest = rest;
    ep -> isDummy = 0;
    ep -> embedLevel = embedLevel;
    ep -> term = term;
    return ep + 1;
}

//dummy environment item
void DF_MkDummyEnv_(DF_ENV_PTR ep, DF_ENV_PTR rest, DF_EMBEDLEV embedLevel)
{
    ((DF_DUMMYENV*)ep) -> rest = rest;
    ((DF_DUMMYENV*)ep) -> isDummy = 1;
    ((DF_DUMMYENV*)ep) -> embedLevel = embedLevel;
}

DF_ENV_PTR DF_MkDummyEnv(DF_ENV_PTR ep, DF_ENV_PTR rest, 
                         DF_EMBEDLEV embedLevel)
{
    ((DF_DUMMYENV*)ep) -> rest = rest;
    ((DF_DUMMYENV*)ep) -> isDummy = 1;
    ((DF_DUMMYENV*)ep) -> embedLevel = embedLevel;
    
    return (DF_ENV_PTR)(((DF_DUMMYENV*)ep) + 1);
}

/***********************************************************/
/*interface functions for in-place updating certain fields */
/***********************************************************/

//updating unverse index
void DF_ModUnivIndex(DF_TERM_PTR tp, DF_UNIVIND univIndex)
{
    ((DF_VAR*)tp) -> univIndex = univIndex;
}


/*********************************************/
/* term dereference                          */
/*********************************************/
DF_TERM_PTR DF_Deref(DF_TERM_PTR tp)
{
    while (DF_IsRef(tp)) tp = ((DF_REF*)tp) -> target;
    return tp;
}

/***************************************************************************/
/*interface functions for addr calculation, may be needed for detecting    */
/*memory error                                                             */
/* (some are not implemented yet)                                          */
/***************************************************************************/

//return an address increased by the size of an app head from the given one
MEM_PTR DF_IncAtomic(MEM_PTR tp)
{
    return (MEM_PTR)(((DF_TERM_PTR)tp)+1);
}

//return an address increased by the size of n atomic term from the given
MEM_PTR DF_IncNAtomic(MEM_PTR tp, int n)
{
    return (MEM_PTR)(((DF_TERM_PTR)tp)+n);
}

//return an address increased by the size of a suspension from the given one
MEM_PTR DF_IncSusp(MEM_PTR tp)
{
    return (MEM_PTR)(((DF_SUSP*)tp)+1);
}

//return an address increased by the size of a dummy env item from the given 
MEM_PTR DF_IncEnvDummy(MEM_PTR ep)
{
    return (MEM_PTR)(((DF_DUMMYENV*)ep)+1);
}

//return an address increased by the size of n dummy env items from the given
MEM_PTR DF_IncNEnvDummy(MEM_PTR ep, int n)
{
    return (MEM_PTR)(((DF_DUMMYENV*)ep)+n);
}

//return an address increased by the size of a pair env item froms the given 
MEM_PTR DF_IncEnvPair(MEM_PTR ep)
{
    return (MEM_PTR)(((DF_ENV*)ep)+1);
}

//return an address increased by the size of n pair env items froms the given
MEM_PTR DF_IncNEnvPair(MEM_PTR ep, int n)
{
    return (MEM_PTR)(((DF_ENV*)ep)+n);
}

//return an address increased by the size of an app head from the given one
MEM_PTR DF_IncApp(MEM_PTR tp)
{
    return (MEM_PTR)(((DF_APP*)tp)+1);
}

//increasing sizeof(APP)+ n*atomic size, where n is arity 
MEM_PTR DF_IncAppNArgs(MEM_PTR tp, DF_ARITY n)
{
    return (MEM_PTR)(((DF_TERM_PTR)(((DF_APP*)tp)+1))+n);
}


//return an address increased by the size of an abstraction from the given
MEM_PTR DF_IncLam(MEM_PTR tp)
{
    return (MEM_PTR)(((DF_LAM*)tp)+1);
}

//return an address increased by the size of lam+susp+(n dummy env items)
MEM_PTR DF_IncSuspLamNDummyEnv(MEM_PTR tp, int n)
{
    return (MEM_PTR)(((DF_TERM_PTR)(((DF_LAM*)(((DF_SUSP*)tp)+1))+1))+n);
}

//return an address increased by the size of a cons from the given
MEM_PTR DF_IncCons(MEM_PTR tp)
{
    return (MEM_PTR)(((DF_CONS*)tp)+1);
}

#endif //DATAFORMAT_C
