package paqFe.state

import chisel3._
import chisel3.util.Cat

object StateShiftLut {
  def apply(state : UInt, bit : UInt) = {
    require(state.getWidth == 8 && bit.getWidth == 1);

    val shift_lut : Vec[UInt] = VecInit (
      0x01.U, 0x02.U, 0x03.U, 0x05.U, 0x04.U, 0x06.U, 0x07.U, 0x0A.U, 
      0x08.U, 0x0C.U, 0x09.U, 0x0D.U, 0x0B.U, 0x0E.U, 0x0F.U, 0x13.U, 
      0x10.U, 0x17.U, 0x11.U, 0x18.U, 0x12.U, 0x19.U, 0x14.U, 0x1B.U, 
      0x15.U, 0x1C.U, 0x16.U, 0x1D.U, 0x1A.U, 0x1E.U, 0x1F.U, 0x21.U, 
      0x20.U, 0x23.U, 0x20.U, 0x23.U, 0x20.U, 0x23.U, 0x20.U, 0x23.U, 
      0x22.U, 0x25.U, 0x22.U, 0x25.U, 0x22.U, 0x25.U, 0x22.U, 0x25.U, 
      0x22.U, 0x25.U, 0x22.U, 0x25.U, 0x24.U, 0x27.U, 0x24.U, 0x27.U, 
      0x24.U, 0x27.U, 0x24.U, 0x27.U, 0x26.U, 0x28.U, 0x29.U, 0x2B.U, 
      0x2A.U, 0x2D.U, 0x2A.U, 0x2D.U, 0x2C.U, 0x2F.U, 0x2C.U, 0x2F.U, 
      0x2E.U, 0x31.U, 0x2E.U, 0x31.U, 0x30.U, 0x33.U, 0x30.U, 0x33.U, 
      0x32.U, 0x34.U, 0x35.U, 0x2B.U, 0x36.U, 0x39.U, 0x36.U, 0x39.U, 
      0x38.U, 0x3B.U, 0x38.U, 0x3B.U, 0x3A.U, 0x3D.U, 0x3A.U, 0x3D.U, 
      0x3C.U, 0x3F.U, 0x3C.U, 0x3F.U, 0x3E.U, 0x41.U, 0x3E.U, 0x41.U, 
      0x32.U, 0x42.U, 0x43.U, 0x37.U, 0x44.U, 0x39.U, 0x44.U, 0x39.U, 
      0x46.U, 0x49.U, 0x46.U, 0x49.U, 0x48.U, 0x4B.U, 0x48.U, 0x4B.U, 
      0x4A.U, 0x4D.U, 0x4A.U, 0x4D.U, 0x4C.U, 0x4F.U, 0x4C.U, 0x4F.U, 
      0x3E.U, 0x51.U, 0x3E.U, 0x51.U, 0x40.U, 0x52.U, 0x53.U, 0x45.U, 
      0x54.U, 0x47.U, 0x54.U, 0x47.U, 0x56.U, 0x49.U, 0x56.U, 0x49.U, 
      0x2C.U, 0x3B.U, 0x2C.U, 0x3B.U, 0x3A.U, 0x3D.U, 0x3A.U, 0x3D.U, 
      0x3C.U, 0x31.U, 0x3C.U, 0x31.U, 0x4C.U, 0x59.U, 0x4C.U, 0x59.U, 
      0x4E.U, 0x5B.U, 0x4E.U, 0x5B.U, 0x50.U, 0x5C.U, 0x5D.U, 0x45.U, 
      0x5E.U, 0x57.U, 0x5E.U, 0x57.U, 0x60.U, 0x2D.U, 0x60.U, 0x2D.U, 
      0x30.U, 0x63.U, 0x30.U, 0x63.U, 0x58.U, 0x65.U, 0x58.U, 0x65.U, 
      0x50.U, 0x66.U, 0x67.U, 0x45.U, 0x68.U, 0x57.U, 0x68.U, 0x57.U, 
      0x6A.U, 0x39.U, 0x6A.U, 0x39.U, 0x3E.U, 0x6D.U, 0x3E.U, 0x6D.U, 
      0x58.U, 0x6F.U, 0x58.U, 0x6F.U, 0x50.U, 0x70.U, 0x71.U, 0x55.U, 
      0x72.U, 0x57.U, 0x72.U, 0x57.U, 0x74.U, 0x39.U, 0x74.U, 0x39.U, 
      0x3E.U, 0x77.U, 0x3E.U, 0x77.U, 0x58.U, 0x79.U, 0x58.U, 0x79.U, 
      0x5A.U, 0x7A.U, 0x7B.U, 0x55.U, 0x7C.U, 0x61.U, 0x7C.U, 0x61.U, 
      0x7E.U, 0x39.U, 0x7E.U, 0x39.U, 0x3E.U, 0x81.U, 0x3E.U, 0x81.U, 
      0x62.U, 0x83.U, 0x62.U, 0x83.U, 0x5A.U, 0x84.U, 0x85.U, 0x55.U, 
      0x86.U, 0x61.U, 0x86.U, 0x61.U, 0x88.U, 0x39.U, 0x88.U, 0x39.U, 
      0x3E.U, 0x8B.U, 0x3E.U, 0x8B.U, 0x62.U, 0x8D.U, 0x62.U, 0x8D.U, 
      0x5A.U, 0x8E.U, 0x8F.U, 0x5F.U, 0x90.U, 0x61.U, 0x90.U, 0x61.U, 
      0x44.U, 0x39.U, 0x44.U, 0x39.U, 0x3E.U, 0x51.U, 0x3E.U, 0x51.U, 
      0x62.U, 0x93.U, 0x62.U, 0x93.U, 0x64.U, 0x94.U, 0x95.U, 0x5F.U, 
      0x96.U, 0x6B.U, 0x96.U, 0x6B.U, 0x6C.U, 0x97.U, 0x6C.U, 0x97.U, 
      0x64.U, 0x98.U, 0x99.U, 0x5F.U, 0x9A.U, 0x6B.U, 0x6C.U, 0x9B.U, 
      0x64.U, 0x9C.U, 0x9D.U, 0x5F.U, 0x9E.U, 0x6B.U, 0x6C.U, 0x9F.U, 
      0x64.U, 0xA0.U, 0xA1.U, 0x69.U, 0xA2.U, 0x6B.U, 0x6C.U, 0xA3.U, 
      0x6E.U, 0xA4.U, 0xA5.U, 0x69.U, 0xA6.U, 0x75.U, 0x76.U, 0xA7.U, 
      0x6E.U, 0xA8.U, 0xA9.U, 0x69.U, 0xAA.U, 0x75.U, 0x76.U, 0xAB.U, 
      0x6E.U, 0xAC.U, 0xAD.U, 0x69.U, 0xAE.U, 0x75.U, 0x76.U, 0xAF.U, 
      0x6E.U, 0xB0.U, 0xB1.U, 0x69.U, 0xB2.U, 0x75.U, 0x76.U, 0xB3.U, 
      0x6E.U, 0xB4.U, 0xB5.U, 0x73.U, 0xB6.U, 0x75.U, 0x76.U, 0xB7.U, 
      0x78.U, 0xB8.U, 0xB9.U, 0x73.U, 0xBA.U, 0x7F.U, 0x80.U, 0xBB.U, 
      0x78.U, 0xBC.U, 0xBD.U, 0x73.U, 0xBE.U, 0x7F.U, 0x80.U, 0xBF.U, 
      0x78.U, 0xC0.U, 0xC1.U, 0x73.U, 0xC2.U, 0x7F.U, 0x80.U, 0xC3.U, 
      0x78.U, 0xC4.U, 0xC5.U, 0x73.U, 0xC6.U, 0x7F.U, 0x80.U, 0xC7.U, 
      0x78.U, 0xC8.U, 0xC9.U, 0x73.U, 0xCA.U, 0x7F.U, 0x80.U, 0xCB.U, 
      0x78.U, 0xCC.U, 0xCD.U, 0x73.U, 0xCE.U, 0x7F.U, 0x80.U, 0xCF.U, 
      0x78.U, 0xD0.U, 0xD1.U, 0x7D.U, 0xD2.U, 0x7F.U, 0x80.U, 0xD3.U, 
      0x82.U, 0xD4.U, 0xD5.U, 0x7D.U, 0xD6.U, 0x89.U, 0x8A.U, 0xD7.U, 
      0x82.U, 0xD8.U, 0xD9.U, 0x7D.U, 0xDA.U, 0x89.U, 0x8A.U, 0xDB.U, 
      0x82.U, 0xDC.U, 0xDD.U, 0x7D.U, 0xDE.U, 0x89.U, 0x8A.U, 0xDF.U, 
      0x82.U, 0xE0.U, 0xE1.U, 0x7D.U, 0xE2.U, 0x89.U, 0x8A.U, 0xE3.U, 
      0x82.U, 0xE4.U, 0xE5.U, 0x7D.U, 0xE6.U, 0x89.U, 0x8A.U, 0xE7.U, 
      0x82.U, 0xE8.U, 0xE9.U, 0x7D.U, 0xEA.U, 0x89.U, 0x8A.U, 0xEB.U, 
      0x82.U, 0xEC.U, 0xED.U, 0x7D.U, 0xEE.U, 0x89.U, 0x8A.U, 0xEF.U, 
      0x82.U, 0xF0.U, 0xF1.U, 0x7D.U, 0xF2.U, 0x89.U, 0x8A.U, 0xF3.U, 
      0x82.U, 0xF4.U, 0xF5.U, 0x87.U, 0xF6.U, 0x89.U, 0x8A.U, 0xF7.U, 
      0x8C.U, 0xF8.U, 0xF9.U, 0x87.U, 0xFA.U, 0x45.U, 0x50.U, 0xFB.U, 
      0x8C.U, 0xFC.U, 0xF9.U, 0x87.U, 0xFA.U, 0x45.U, 0x50.U, 0xFB.U, 
      0x8C.U, 0xFC.U, 0x00.U, 0x00.U, 0x00.U, 0x00.U, 0x00.U, 0x00.U, 
    )

    shift_lut(Cat(state, bit));
  }
}