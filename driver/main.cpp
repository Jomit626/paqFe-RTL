#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <chrono>
#include <thread> 
#include <functional>
using namespace std;

#include "paqFe/StreamFile.hpp"
#include "paqFe/models/CRC.hpp"

constexpr int PacketSize = 65536;
int verbose = 0;

using namespace paqFe::internal;

int iopen(const char* pathname, int flag) {
  int fd = open(pathname, flag);
  if(fd == -1) {

    printf("failed to open file %s, %d\n", pathname, errno);
    exit(1);
  }
  return fd;
}

void write_to_card(int h2c, FILE* file, paqFe::Header *h) {

  // get file size
  ssize_t file_size = 0;
  fseek(file, 0L, SEEK_END);
  file_size = ftell(file);
  rewind(file);
  CRC<32> crc;
  /*
  if(file_size > 12312)
  */

  char* buf = (char*)malloc(file_size);
  ssize_t n = 0;
  n = fread(buf, 1, file_size, file);

  uint32_t check_sum = 0;
  for(ssize_t i=0;i<n;i++) {
    uint8_t byte = buf[i];
    check_sum = crc.next(check_sum, byte);
  }

  auto start = chrono::high_resolution_clock::now();
  n = write(h2c, buf, n);
  int e = errno;
  free(buf);

  h->crc32_check_sum.dw = check_sum;
  h->origin_size.dw = file_size;

  auto end = chrono::high_resolution_clock::now();
  double time = chrono::duration_cast<chrono::nanoseconds>(end - start).count() * 1e-9;
  double bandwidth = n / time / 1024 / 1024;

  printf("filesize %d, %d bytes written, write BW: %lf MBs\n", file_size, n, bandwidth);

  return ;
}

void read_from_card(int c2h, paqFe::internal::FilesOStream<8> *fout) {
  auto start = chrono::high_resolution_clock::now();

  char buf[PacketSize + 64];
  int total = 0, packetCnt = 0;
  int n = read(c2h, buf, PacketSize);
  //printf("Packet len %d\n", n);

  while(n == PacketSize) {
    packetCnt++;

    uint16_t *data = (uint16_t*)buf;
    int len = data[PacketSize / 2 - 1];
    if(len == 0) {
      //printf("end Packet\n");
      break;
    } else {
      for(int i=0;i<len;i++) {
        uint16_t pack = data[i];
        uint8_t idx = (pack >> 8) & 0xFF;
        uint8_t byte = pack & 0xFF;
        if(idx >= 0 &&idx < 8){
          fout->write_byte(byte, idx);
        }
      }

      total += len;
    }

    n = read(c2h, buf, PacketSize);
    //printf("Packet len %d\n", n);
  }
  if(n != PacketSize) {
    printf("error packet read %d byte, errno:%d\n", n, errno);
  }
  printf("%d packets, read total:%d\n", packetCnt, total);
  auto end = chrono::high_resolution_clock::now();
  double time = chrono::duration_cast<chrono::nanoseconds>(end - start).count() * 1e-9;
  double bandwidth = total / time / 1024;
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
    case 'v':
      verbose = 1;
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
  paqFe::Header h;
  fout.open(output_pathname);

  thread writer(bind(write_to_card, h2c, fin, &h));
  thread reader(bind(read_from_card, c2h, &fout));
  
  writer.join();
  reader.join();

  fout.write_header(&h);
  fout.close();


  return 0;
}
  