using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  class ipv4_t
  {
    public byte version; // 4
    public byte ihl; // 4
    public byte diffserv; // 8
    public ushort totalLen; // 16
    public ushort identification; // 16
    public byte flags; // 3
    public ushort fragOffset; // 13
    public byte ttl; // 8
    public byte protocol; // 8
    public ushort hdrChecksum; // 16
    public uint srcAddr; // 32
    public uint dstAddr; // 32
    public byte[] options; // * (variable length options) // FIXME: how to deal with variable length fields?

    public uint length { get { return ((uint)ihl & 0xF) * 4; } }
    public const uint max_length = 60;
  }
}
