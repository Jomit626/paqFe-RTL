#include <cstdio>

#include "paqFe/paqFe.hpp"
#include "paqFe/models/Orders.hpp"

using namespace paqFe::internal;

FILE* gfout;

class OrdersWrapper : public OrdersDefault {
public:
  using Parent = OrdersDefault;

  uint64_t C = 0;
  uint64_t C1 = 0;
  uint64_t C2 = 0;
  uint64_t C3 = 0;
  uint64_t C4 = 0;
  uint64_t C5 = 0;
  uint64_t CWord = 0;

  uint32_t H1 = 0;
  uint32_t H2 = 0;
  uint32_t H3 = 0;
  uint32_t H4 = 0;
  uint32_t H5 = 0;
  uint32_t HWord = 0;

  int salt = 0;

  bool updateContextNibble0(uint8_t nibble, uint8_t byte) {
    salt += 1;
    C = ((C << 4) | nibble);
    C1 = (C & 0xFF) << 5;
    C2 = (C & 0xFFFF) << 5;
    C3 = (C & 0xFFFFFF) << 5;
    C4 = (C & 0xFFFFFFFF) << 5;
    C5 = (C & 0xFFFFFFFFFF) << 5;
    
    bool isWord = false;
    if (byte>=65 && byte<=90) {
      byte += 32;
      isWord = true;
    } else if ((byte>=97 && byte<=122)) {
      isWord = true;
    }
    
    if (isWord) {
      CWord = (CWord ^ byte) << 5;
    } else {
      CWord = 0;
    }
    updateHash();

    return true;
  }

  bool updateContextNibble1(uint8_t nibble) {
    C = ((C << 4) | nibble);
    C1 = C1 + nibble + 16;
    C2 = C2 + nibble + 16;
    C3 = C3 + nibble + 16;
    C4 = C4 + nibble + 16;
    C5 = C5 + nibble + 16;
    CWord = CWord + nibble + 16;
    updateHash();

    return true;
  }

  void updateHash() {
    H1 = C1 & Parent::O1Mask;
    H2 = tab_hashing<21, Parent::O2AddrWidth0>(O2HashTab, C2) & Parent::O2Mask;
    H3 = tab_hashing<29, Parent::O3AddrWidth0>(O3HashTab, C3) & Parent::O3Mask;
    H4 = tab_hashing<37, Parent::O4AddrWidth0>(O4HashTab, C4) & Parent::O4Mask;
    H5 = tab_hashing<45, Parent::O5AddrWidth0>(O5HashTab, C5) & Parent::O4Mask;
    HWord = (CWord ^ (CWord >> 32) ^ (CWord >> 16)) & Parent::OWordMask;
  }


  OrdersWrapper() : Parent() {

  }

  void predict(uint8_t bit, Prob *pp, Context *pctx) {
    Parent::predict(bit, pp, pctx);
  }

  void predict_byte(uint8_t byte, Prob *pp, Context *pctx, size_t pstride, size_t ctxstride) {
    Parent::predict_byte(byte, pp, pctx, pstride, ctxstride);

    uint8_t nibble0 = (byte >> 4) & 0xF;
    uint8_t nibble1 = byte & 0xF;

    fprintf(gfout, "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n", nibble0, H1, H2, H3, H4, H5, HWord, C1 & 0xFF, C2 & 0xFF, C3 & 0xFF, C4 & 0xFF, C5 & 0xFF, CWord & 0xFF);
    updateContextNibble1(nibble0);

    fprintf(gfout, "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n", nibble1, H1, H2, H3, H4, H5, HWord, C1 & 0xFF, C2 & 0xFF, C3 & 0xFF, C4 & 0xFF, C5 & 0xFF, CWord & 0xFF);
    updateContextNibble0(nibble1, byte);
  }
};

void generate_db(FILE* fin, FILE* fout) {
  gfout = fout;

  OrdersWrapper &model  = *(new OrdersWrapper());

  uint8_t data[128];
  size_t n = 0;
  Prob prob = 2048;
  Prob p[8][OrdersWrapper::nProb];
  Context ctx[8][OrdersWrapper::nCtx];
  while((n = fread(data, 1, 128, fin))) {
    for(int i=0;i<n;i++) {
      uint8_t byte = data[i];
      model.predict_byte(byte, &p[0][0], &ctx[0][0], OrdersWrapper::Parent::nProb, OrdersWrapper::Parent::nCtx);
    }
  }

  delete &model;
}
