#include <cstdio>
#include <cerrno>
#include <cstdlib>
#include <unistd.h>

#include "paqFe/models/CRC.hpp"

using namespace paqFe::internal;

void generate_db(FILE* fin, FILE* fout) {
  CRC<32> crc;
  uint32_t chk = 0;

  uint8_t data[128];
  uint8_t output;
  size_t n;
  while((n = fread(data, 1, 128, fin))) {
    for(size_t i=0;i<n;i++) {
      uint8_t byte = data[i];
      chk = crc.next(chk, byte);
    }
  }

  // write tail
  fprintf(fout, "%u\n", chk);
}