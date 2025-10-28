
#include "stdio.h"
#include "exception.h"

int main(void)
{
  int v;
  int err  = 1;
  char * m = "throwing 1";

  if( (v = setjmp(except_buf)) == 0 )
    printf("nothing wrong\n");
  else
  {
    printf("not right\n");
    return 1;
  }

  if( (v = setjmp(except_buf)) == 0 )
  {
    
    printf("trying something wrong\n");
    jump(&err);
    //longjmp(except_buf,1);
    printf("should not be here\n");
    return 1;
  }
  else
  {
    printf("%s\n",m);
    return 0;
  }

};
