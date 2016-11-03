using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.Tutorials.Simple_nat
{
  class tcp_t
  {
    public ushort srcPort; // 16;
    public ushort dstPort; // 16;
    public uint seqNo; // 32;
    public uint ackNo; // 32;
    public byte dataOffset; // 4;
    public byte res; // 4;
    public byte flags; // 8;
    public ushort window; // 16;
    public ushort checksum; // 16;
    public ushort urgentPtr; // 16;
  }
}
