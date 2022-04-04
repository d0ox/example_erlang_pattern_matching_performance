#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>

#define print_bits(x)                                             \
  do {                                                            \
    typeof(x) a__ = (x);                                          \
    char *p__ = (char *)&a__ + sizeof(x) - 1;                     \
    size_t bytes__ = sizeof(x);                                   \
    printf(#x ": ");                                              \
    while (bytes__--) {                                           \
      char bits__ = 8;                                            \
      while (bits__--) putchar(*p__ & (1 << bits__) ? '1' : '0'); \
      p__--;                                                      \
    }                                                             \
    putchar('\n');                                                \
  } while (0)


const char* filename = "example.bin";

typedef union {
    struct {
        unsigned char fin : 1;
        unsigned char rsv1: 1;
        unsigned char rsv2: 1;
        unsigned char rsv3: 1;
        unsigned char optcode: 4;
        unsigned char mask: 1;
        unsigned char lenght: 1;
     
    } header;
    unsigned char rest;
} WebsocketDataPacket;

int main(void) {

    clock_t t;
    t = clock();  
  
    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        perror("open\n");
        exit(EXIT_FAILURE);
    }

    struct stat sb;
    if (stat(filename, &sb) == -1) {
        perror("stat");
        exit(EXIT_FAILURE);
    }

    char* file_contents = malloc(sb.st_size);
    read(fd, file_contents, sb.st_size);

    union WebsocketDataPacket* wdp = &file_contents;
    print_bits(wdp.header);

    // Recording end time.
    t = clock() - t;      
    double time_taken = ((double)t)/CLOCKS_PER_SEC; // in seconds
  
  
    printf("read data: %s\n %f microsec", file_contents, time_taken /1000000 );
    close(fd);

    free(file_contents);

    exit(EXIT_SUCCESS);
}