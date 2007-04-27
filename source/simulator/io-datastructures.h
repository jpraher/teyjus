/****************************************************************************
 *                                                                          *
 *  File io-datastructures.h.                                               *
 *                                                                          *
 ****************************************************************************/

#ifndef IODATASTRUCTURES_H
#define IODATASTRUCTURES_H

#include "mcstring.h"
#include "dataformats.h"
#include "mctypes.h"

/*****************************************************************************
 * A data structure for maintaining information about query term variables   *
 * and other free variables encountered in the course of displaying answers. *
 *****************************************************************************/
/* number of entries in the table for such variables. */
#define IO_MAX_FREE_VARS   500

/* Structure of each entry in the table; display name, and the rigid
   designator in the form of the memory cell corresponding to the variable are
   maintained. */
typedef struct 
{
    DF_StrDataPtr   varName;
    DF_TermPtr      rigdes;
} IO_FreeVarInfo;

/* The table itself */
extern IO_FreeVarInfo IO_freeVarTab[IO_MAX_FREE_VARS];

/* index for the topmost cell that has been used */
extern int IO_freeVarTabTop;

/* initialize */
void IO_initIO();

/* check if the free term variable table is full */
Boolean IO_freeVarTabFull(int incSize);

/* make an entry in the free term variable table */
void IO_enterFreeVarTab(DF_StrDataPtr name, DF_TermPtr varLoc);


#endif  //IODATASTRUCTURES_H
