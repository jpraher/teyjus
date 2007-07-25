#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "code.h"
#include "ld_message.h"

TwoBytes LD_BVRTAB_numBvrTabs;
WordPtr* LD_BVRTAB_BvrTabs;

WordPtr LD_BVRTAB_LoadBvrTab(MEM_GmtEnt* ent);

void LD_BVRTAB_LoadBvrTabs(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_BVRTAB_numBvrTabs=LD_FILE_GET2();
  LD_detail("Loading %d bound variable tables\n",count);
  LD_BVRTAB_BvrTabs=(WordPtr*)EM_malloc(count*sizeof(WordPtr));
  
  for(i=0;i<count;i++)
  {
    LD_BVRTAB_BvrTabs[i]=LD_BVRTAB_LoadBvrTab(ent);
  }
  return;
}

WordPtr LD_BVRTAB_LoadBvrTab(MEM_GmtEnt* ent)
{
  int numEntries=LD_FILE_GET2();
  int i;
  
  ///\todo Make this actually a hash table.
  //Word* tab=LD_LOADER_ExtendModSpace(ent,2*sizeof(Word)*numEntries); --XQ
  Word* tab=LD_LOADER_ExtendModSpace(ent,2*numEntries);
  for(i=0;i<numEntries;i++)
  {
    tab[2*i]  =(Word)LD_FILE_GET1();
    tab[2*i+1]=(Word)LD_CODE_GetCodeInd();
  }
  
  return (WordPtr)tab;
}

WordPtr LD_BVRTAB_GetBvrTabAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_BVRTAB_numBvrTabs)
    EM_THROW(LD_LoadError);
  return LD_BVRTAB_BvrTabs[i];
}
