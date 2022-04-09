#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <chrono>
using namespace std;

#include "../paqFe/include/paqFe/StreamFile.hpp"

int iopen(const char* pathname, int flag) {
  int fd = open(pathname, flag);
  if(fd == -1) {

    printf("failed to open file %s, %d\n", pathname, errno);
    exit(1);
  }
  return fd;
}

int main(int argc, char** argv) {
  int h2c = iopen("/dev/xdma0_h2c_0", O_RDWR);
  int c2h = iopen("/dev/xdma0_c2h_0", O_RDWR);
  FILE* outputDump = fopen("./t", "wb");

  constexpr int PacketSize = 4096;
  constexpr size_t TestDataSize = 500 * 1024 * 1024;
  pid_t pid = fork();
  if(pid == 0) {
    auto start = chrono::high_resolution_clock::now();

    char* buf = (char*)malloc(TestDataSize);
    for(size_t i=0;i<TestDataSize;i++) {
      buf[i] = i & 0xFF;
    }

    size_t size = 0;
    size = write(h2c, buf, TestDataSize);
    free(buf);


    auto end = chrono::high_resolution_clock::now();
    double time = chrono::duration_cast<chrono::nanoseconds>(end - start).count() * 1e-9;
    double bandwidth = size / time / 1024 / 1024;
    printf("%d bytes commited\n", TestDataSize);
    printf("%d bytes written, write BW: %lf MBs\n", TestDataSize, bandwidth);
  } else {
    auto start = chrono::high_resolution_clock::now();

    char* buf = (char*)malloc(TestDataSize * 4);

    ssize_t size = read(c2h, buf, TestDataSize * 2);
    
    auto end = chrono::high_resolution_clock::now();
    double time = chrono::duration_cast<chrono::nanoseconds>(end - start).count() * 1e-9;
    double bandwidth = size / time / 1024;
    printf(" %lx %ld bytes read, read BW: %lf Kilo Bytes/s\n", size, size, bandwidth);

    fwrite(buf, 1, size, outputDump);
    int lastCnt = 0;
    int errorCnt = 0;
    int errorStart = 0;
    uint16_t *data = (uint16_t*)buf;
    for(size_t i=0;i<TestDataSize;i++) {
      uint16_t x = data[i];
      uint8_t d = x & 0xFF;
      if(i < 8)
        printf("%x\n", x);
      if(x & 0x200) {
        if(i != (TestDataSize - 1)) {
          printf("Detect misplaced last singal at %d.\n", i);
        }
        lastCnt += 1;
      }
      if(d != (i & 0xFF)) {
        if(!errorCnt) {
          printf("first error %x, expected %x\n", d, (i & 0xFF));
          errorStart = i;
        }
        errorCnt += 1;

      }
    }
    printf("%d Errors met after %d bytes\n", errorCnt, errorStart);
    printf("%d Last singal met.\n", lastCnt);
    free(buf);
    wait(NULL); 
  }

  return 0;
}
  