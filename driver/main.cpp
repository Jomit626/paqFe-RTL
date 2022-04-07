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
  int opt;
  const char *input_pathname = NULL, *output_pathname = NULL, *device_pathname = NULL;
  while((opt = getopt(argc, argv, "vi:o:")) != -1) {
    switch (opt) {
    case 'i':
      input_pathname = optarg;
      break;
    case 'o':
      output_pathname = optarg;
      break;
    default:
      break;
    }
  }

  if(!input_pathname || !output_pathname)
    return 1;
  int h2c = iopen("/dev/xdma0_h2c_0", O_WRONLY);
  int c2h = iopen("/dev/xdma0_c2h_0", O_RDONLY);
  FILE* fin = fopen(input_pathname, "rb");
  fseek(fin, 0L, SEEK_END);
  size_t fin_size = ftell(fin);
  rewind(fin);

  paqFe::internal::FilesOStream<8> fout;
  fout.open(output_pathname);

  constexpr int PacketSize = 1024;
  pid_t pid = fork();
  if(pid == 0) {
    auto start = chrono::high_resolution_clock::now();
    char* buf = (char*)malloc(fin_size);
    size_t size = 0;
    size = fread(buf, 1, fin_size, fin);
    size = write(h2c, buf, size);
    int e = errno;
    free(buf);
    auto end = chrono::high_resolution_clock::now();
    double time = chrono::duration_cast<chrono::nanoseconds>(end - start).count() * 1e-9;
    double bandwidth = size / time / 1024;

    printf("errno:%d, filesize %d, %d bytes written, write BW: %lf Kilo Bytes/s\n", e, fin_size, size, bandwidth);
  } else {

    auto start = chrono::high_resolution_clock::now();
    char buf[PacketSize + 64];
    int total = 0, packetCnt = 0;
    int n = read(c2h, buf, PacketSize);
    paqFe::Header h;
    h.origin_size.dw = fin_size;
    fout.write_header(&h);
    while(n == PacketSize) {
      packetCnt++;
      uint16_t *data = (uint16_t*)buf;
      int len = data[PacketSize / 2 - 1];
      if(len == 0) {
        printf("end Packet\n");
        break;
      }
      for(int i=0;i<len;i++) {
        uint16_t pack = data[i];
        uint8_t idx = (pack >> 8) & 0xFF;
        uint8_t byte = pack & 0xFF;
        fout.write_byte(byte, idx);
      }

      total += len;
      n = read(c2h, buf, PacketSize);
    }
    if(n != PacketSize) {
      int e = errno;
      printf("error packet read %d byte, errno:%d\n", n, e);
    }
    printf("read total:%d\n", total);
    auto end = chrono::high_resolution_clock::now();
    double time = chrono::duration_cast<chrono::nanoseconds>(end - start).count() * 1e-9;
    double bandwidth = fin_size / time / 1024;
    printf("Time elapsed precise: %lfs, read BW: %lf Kilo Bytes/s\n", time, bandwidth);
    wait(NULL); 
  }

  return 0;
}
  