#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <endian.h>
#include <byteswap.h>
#include <bits/wordsize.h>
#include "file.h"
#include "../system/error.h"

#define DEBUG(x) printf("%s\n",x)
#define SWAPENDIAN

char* LK_FILE_LinkCodeExt=".lpo";
char* LK_FILE_ByteCodeExt=".lpa";

int LK_FILE_OpenInput(char* modname, char* extension)
{
  char* buf=EM_malloc(strlen(modname)+strlen(extension)+1);
  sprintf(buf,"%s%s",modname,extension);
  
  int fd=open(buf,O_RDONLY,0000);
  if(fd==-1)
  {
    EM_THROW(LK_LinkError);
  }
  return fd;
}

int LK_FILE_OpenOutput(char* modname, char* extension)
{
  char* buf=EM_malloc(strlen(modname)+strlen(extension)+1);
  sprintf(buf,"%s%s",modname,extension);
  
  int fd=open(buf,O_WRONLY|O_CREAT|O_TRUNC,0666);
  if(fd==-1)
  {
    EM_THROW(LK_LinkError);
  }
  return fd;
}

void LK_FILE_xPipe(int fd[2])
{
  if(pipe(fd))
    EM_THROW(LK_LinkError);
}

void LK_FILE_Close(int fd)
{
  close(fd);///\todo handle close error?
}

Word LK_FILE_GETWord(int fd)
{
  Word tmp;
  read(fd,&tmp,sizeof(tmp));
#if BYTE_ORDER == LITTLE_ENDIAN
# if __WORDSIZE == 32
  return (Word)bswap_32((unsigned int)tmp);
# elif __WORDSIZE == 64
  return bswap_64(tmp);
# endif
#elif BYTE_ORDER == BIG_ENDIAN
  return tmp;
#endif
}

INT4 LK_FILE_GET4(int fd)
{
  INT4 tmp;
  read(fd,&tmp,sizeof(tmp));
#if BYTE_ORDER == LITTLE_ENDIAN
  return bswap_32(tmp);
#elif BYTE_ORDER == BIG_ENDIAN
  return tmp;
#endif
}

TwoBytes LK_FILE_GET2(int fd)
{
  TwoBytes tmp;
  read(fd,&tmp,sizeof(tmp));
#if BYTE_ORDER == LITTLE_ENDIAN
  return bswap_16(tmp);
#elif BYTE_ORDER == BIG_ENDIAN
  return tmp;
#endif
}

Byte LK_FILE_GET1(int fd)
{
  Byte tmp;
  read(fd,&tmp,sizeof(tmp));
  return tmp;
}

char* LK_FILE_GetString(int fd)
{
  char* tmp;
  Byte size=LK_FILE_GET1(fd);
  //printf("Name:%d ",size);//DEBUG
  tmp=EM_malloc(size+1);
  read(fd,tmp,size);
  tmp[size]='\0';
  //printf("\"%s\"\n",tmp);//DEBUG
  return tmp;
}

void LK_FILE_PUT1(int fd, Byte x)
{
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUT2(int fd, TwoBytes x)
{
#if BYTE_ORDER == LITTLE_ENDIAN
    x=bswap_16(x);
#elif BYTE_ORDER == BIG_ENDIAN
#endif
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUT4(int fd, INT4 x)
{
#if BYTE_ORDER == LITTLE_ENDIAN
  x=bswap_32(x);
#elif BYTE_ORDER == BIG_ENDIAN
#endif
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUTN(int fd, void* data,int n)
{
  write(fd,data,n);
}

void LK_FILE_PUTWord(int fd, Word x)
{
#if BYTE_ORDER == LITTLE_ENDIAN
# if __WORDSIZE == 32
  x=(Word)bswap_32((unsigned int)x);
# elif __WORDSIZE == 64
  x=bswap_64(x);
# endif
#elif BYTE_ORDER == BIG_ENDIAN
#endif
  write(fd,&x,sizeof(x));
}

void LK_FILE_PutString(int fd, char* str)
{
  Byte size=strlen(str);
  LK_FILE_PUT1(fd,size);
  write(fd,str,size);
}
