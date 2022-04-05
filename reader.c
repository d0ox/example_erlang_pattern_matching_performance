#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <string.h>

const char* filename = "example1.bin";

struct header{        
        unsigned char fin;
        unsigned char rsv1;
        unsigned char rsv2;
        unsigned char rsv3;
        unsigned char optcode;
        unsigned char mask;
        unsigned short lenght; 
        unsigned long long extended_lenght;
        unsigned char masking_key[5]; 
        unsigned char* data; 
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

  stream++; 
  //byte 1
  h.mask = (stream[0]&mMask)>>7;
  h.lenght = (unsigned short)stream[0]&mLenght;

  stream++; 
  
  if(h.lenght == 126){
    h.extended_lenght = (((unsigned short)stream[0]) << 8) | stream[1];
    stream = stream +  2; 
  }

  if(h.lenght == 127){
    h.extended_lenght = (((unsigned long long)stream[0]) << 24) 
                      | (((unsigned long long)stream[1]) << 16) 
                      | (((unsigned long long)stream[2]) << 8) 
                      | stream[3];
    stream = stream + 4; 
  }

  if(h.mask){
    memcpy(h.masking_key, stream, 4);
    h.masking_key[5]="\0";
    stream = stream + 4;
  }

  const int len = h.lenght < 126 ? h.lenght: h.extended_lenght;
  h.data =  (unsigned char *)malloc(len+1);  
  memcpy(h.data, stream, len);  
  h.data[len]="\0";

  return h; 
}

int main(void) {

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
    // Recording start time
     struct timespec tstart={0,0}, tend={0,0};
    clock_gettime(CLOCK_MONOTONIC, &tstart);
    for(int i=0;i<1000;i++){
      read_head(file_contents);
    }
    // Recording end time.
    clock_gettime(CLOCK_MONOTONIC, &tend);
    double computation_time = ((double)tend.tv_sec + 1.0e-9*tend.tv_nsec) - ((double)tstart.tv_sec + 1.0e-9*tstart.tv_nsec);
    printf("read data: %s\n %f seconds\n", file_contents, computation_time );

    myHead = read_head(file_contents);
           
     

    

  
    
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
    printf("| Masking-key (continued)%s       |          Payload Data: %s        |\n",myHead.masking_key, myHead.data);
    printf("+-------------------------------- - - - - - - - - - - - - - - - +\n");

    free(file_contents);

    exit(EXIT_SUCCESS);
}