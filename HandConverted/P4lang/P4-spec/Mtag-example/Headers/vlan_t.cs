using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  sealed class vlan_t
  {
    public readonly uint length = 4;
    public uint offset;

    public byte pcp;          // width 3
    public bool cfi;          // width 1
    public ushort vid;        // width 12
    public ushort ethertype;  // width 16

    public vlan_t(uint offset)
    {
      this.offset = offset;
    }

    public void Extract(byte[] arr)
    {
      pcp = (byte)BitHelper.ExtractBits(arr, offset * 8, 3);
      cfi = BitHelper.ExtractBit(arr, offset * 8 + 3);
      vid = BitHelper.ExtractBits(arr, offset * 8 + 4, 12);
      ethertype = BitHelper.Extract16(arr, offset + 2);
    }

    public void Write(byte[] arr)
    {
      // TODO
    }
  }
}
