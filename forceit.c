#include <stdio.h>
#include <stdlib.h>

int main() {
    const unsigned megabytes = 512;
    printf("Creating %d\n", megabytes);
    void *derp = calloc(megabytes, 1024*1024);

    while (1);
    return 0;
}
