using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  sealed class ethernet_t
  {
    public readonly uint length = 12;
    public uint offset;

    public ulong dstAddr;     // width 48
    public ulong srcAddr;     // width 48
    public ushort etherType;  // width 16

    public ethernet_t(uint offset)
    {
      this.offset = offset;
    }

    public void Extract(byte[] data)
    {
      dstAddr = BitHelper.Extract48(data, offset);
      srcAddr = BitHelper.Extract48(data, offset + 5);
      etherType = BitHelper.Extract16(data, offset + 10);
    }

    public void Write(byte[] data)
    {
      BitHelper.Write48(data, offset, dstAddr);
      BitHelper.Write48(data, offset + 5, srcAddr);
      BitHelper.Write16(data, offset + 10, etherType);
    }
  }
}
