
#include "exception.h"

 jmp_buf except_buf;

 void jump(int * err)
 {
   longjmp(except_buf,*err);
 }
