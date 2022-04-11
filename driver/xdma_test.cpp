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

int read_test(int dev_c2h, size_t size) {
  auto start = chrono::high_resolution_clock::now();

  char* buf = (char*)malloc(size * 4);

  ssize_t nread = read(dev_c2h, buf, size * 4);
  
  auto end = chrono::high_resolution_clock::now();
  double time = chrono::duration_cast<chrono::nanoseconds>(end - start).count() * 1e-9;
  double bandwidth = nread / time / 1024 / 1024;
  if(nread != size * 2) {
    printf("[ERROR] error numbers of bytes\n");  
  }
  printf("%ld bytes read, read BW: %lf MB/s\n", nread, bandwidth);

  int lastCnt = 0;
  int errorCnt = 0;
  int errorStart = 0;
  uint16_t *data = (uint16_t*)buf;
  for(size_t i=0;i<size;i++) {
    uint16_t x = data[i];
    uint8_t d = x & 0xFF;
    if(x & 0x200) {
      if(i != (size - 1)) {
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
  return errorCnt;
}

int write_test(int dev_h2c, size_t size) {
  auto start = chrono::high_resolution_clock::now();

  char* buf = (char*)malloc(size);
  for(size_t i=0;i<size;i++) {
    buf[i] = i & 0xFF;
  }

  size_t nwrite = write(dev_h2c, buf, size);
  free(buf);


  auto end = chrono::high_resolution_clock::now();
  double time = chrono::duration_cast<chrono::nanoseconds>(end - start).count() * 1e-9;
  double bandwidth = nwrite / time / 1024 / 1024;
  printf("%d bytes written, write BW: %lf MBs\n", nwrite, bandwidth);

  return nwrite;
}

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
  int c2h = iopen("/dev/xdma0_c2h_0", O_RDWR | O_TRUNC);
  FILE* outputDump = fopen("./t", "wb");

  constexpr size_t TestDataSize = 10 * 1024 * 1024;
  pid_t pid = fork();
  if(pid == 0) {
    write_test(h2c, TestDataSize);
  } else {
    read_test(c2h, TestDataSize);
    wait(NULL); 
  }

  return 0;
}
  