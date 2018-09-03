#include <stdio.h>

void (*fun1)(char);
void (*fun2)(double);

/// putchard - putchar that takes a double and returns 0.
double putchard (double X) {
  //putchar((char)X);
  (*fun1)((char)X);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
double printd(double X) {
  //printf("%f\n", X);
  (*fun2)(X);
  return 0;
}
