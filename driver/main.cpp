#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>

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
  int fin = iopen(input_pathname, O_RDONLY);
  paqFe::internal::FilesOStream<8> fout;
  fout.open(output_pathname);

  constexpr size_t buffersize = 1024 * 1024;
  constexpr int PacketSize = 64;
  char buf[buffersize];
  pid_t pid = fork();
  if(pid == 0) {
    sleep(1);
    size_t size = 0;
    size = read(fin, buf, buffersize);
    size = write(h2c, buf, size);
    printf("%d bytes written\n", size);
  } else {
    int total = 0, packetCnt = 0;
    int n = read(c2h, buf, PacketSize);
    while(n == PacketSize) {
      packetCnt++;
      printf("read packet:%d %d\n", n, errno);
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
    wait(NULL); 
  }

  return 0;
}
  