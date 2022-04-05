#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>

const char* filename = "example1.bin";

struct header{        
        unsigned char fin;
        unsigned char rsv1;
        unsigned char rsv2;
        unsigned char rsv3;
        unsigned char optcode;
        unsigned char mask;
        unsigned short lenght; 
        double extended_lenght;       
    };

const unsigned char mFin = 0x80;
const unsigned char mRsv1 = 0x40;
const unsigned char mRsv2 = 0x20;
const unsigned char mRsv3 = 0x10;
const unsigned char mOptcode = 0x0f;
const unsigned char mMask = 0x80;
const unsigned char mLenght = 0x7f;

void print_bin(unsigned char value)
{
    for (int i = sizeof(char) * 7; i >= 0; i--)
        printf("%d", (value & (1 << i)) >> i );
    putc('\n', stdout);
}

struct header read_head(unsigned char* stream){
  struct header h;
  // byte 0 
  h.fin = (stream[0]& mFin)>>7; 
  h.rsv1 = (stream[0]& mRsv1)>>6; 
  h.rsv2 = (stream[0]& mRsv2)>>5; 
  h.rsv3 = (stream[0]& mRsv3)>>4;
  h.optcode = stream[0]&mOptcode; 
  //byte 1
  h.mask = stream[1]&mMask>>7;
  h.lenght = (unsigned short)stream[1]&mLenght;
  
  if(h.lenght == 126){
    printf("test: %d", *((unsigned short*)(&stream[2]))); 
    h.extended_lenght = (double)*((unsigned short*)(&stream[2])); 
  }
  return h; 
}


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

    unsigned char* file_contents = malloc(sb.st_size);

    read(fd, file_contents, sb.st_size);
    

    struct header myHead;
    myHead = read_head(file_contents);
           
    // 1st byte 1000 0001
    //print_bin(myHead.fin);
    //print_bin(myHead.rsv1);
    //print_bin(myHead.rsv2);
    //print_bin(myHead.rsv3);
    //print_bin(myHead.optcode);
    //print_bin(myHead.mask);
    //print_bin(myHead.lenght);


    // Recording end time.
    t = clock() - t;      
    double time_taken = ((double)t)/CLOCKS_PER_SEC; // in seconds
    

  
    //printf("read data: %s\n %f microsec", file_contents, time_taken /1000000 );
    close(fd);
    printf("0                   1                   2                   3\n");
  printf(" 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1\n");
  printf("+-+-+-+-+-------+-+-------------+-------------------------------+\n");
  printf("|F|R|R|R| opcode|M| Payload len |    Extended payload length    |\n");
  printf("|I|S|S|S|   %d   |A|     %d     |             %d                 |\n",myHead.optcode ,myHead.lenght, myHead.extended_lenght);
  printf("|N|V|V|V|       |S|             |                               |\n");
  printf("| | | | |       |K|             |                               |\n");
  printf("|%d|%d|%d|%d|       |%d|             |                               |\n",myHead.fin,myHead.rsv1,myHead.rsv2,myHead.rsv3,myHead.mask);
  printf("+-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +\n");
  printf("|     Extended payload length continued, if payload len == 127  |\n");
  printf("+ - - - - - - - - - - - - - - - +-------------------------------+\n");
  printf("|                               |Masking-key, if MASK set to 1  |\n");
  printf("+-------------------------------+-------------------------------+\n");
  printf("| Masking-key (continued)       |          Payload Data         |\n");
  printf("+-------------------------------- - - - - - - - - - - - - - - - +\n");

    free(file_contents);

    exit(EXIT_SUCCESS);
}