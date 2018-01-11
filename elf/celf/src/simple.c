#include <stdio.h>

int sum(int,int);

int main(int argc, char* argv[])
{
    int x, y, z;
    x = 17;
    y = 25;
    z = sum(x,y);
    printf("The sum of %d and %d is %d\n", x, y, z);
    return 0;
}

int sum(int x, int y)
{
    return x+y;
}
