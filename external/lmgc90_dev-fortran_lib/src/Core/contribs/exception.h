
#include <setjmp.h>

#ifndef EXCEPTION_H
#define EXCEPTION_H
 //excatly like this, no static !
 extern jmp_buf except_buf;
#endif

 void jump(int * err);

