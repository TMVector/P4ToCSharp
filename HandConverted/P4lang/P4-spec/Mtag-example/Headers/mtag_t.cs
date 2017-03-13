using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  sealed class mtag_t
  {
    public readonly uint length = 6;
    public uint offset;

    public byte up1;          // width 8
    public byte up2;          // width 8
    public byte down1;        // width 8
    public byte down2;        // width 8
    public ushort ethertype;  // width 16

    public mtag_t(uint offset)
    {
      this.offset = offset;
    }

    public void Extract(byte[] arr)
    {
      up1 = BitHelper.Extract8(arr, offset);
      up2 = BitHelper.Extract8(arr, offset + 1);
      down1 = BitHelper.Extract8(arr, offset + 2);
      down2 = BitHelper.Extract8(arr, offset + 3);
      ethertype = BitHelper.Extract16(arr, offset + 4);
    }

    public void Write(byte[] arr)
    {
      BitHelper.Write8(arr, offset, up1);
      BitHelper.Write8(arr, offset + 1, up2);
      BitHelper.Write8(arr, offset + 2, down1);
      BitHelper.Write8(arr, offset + 3, down2);
      BitHelper.Write16(arr, offset + 4, ethertype);
    }
  }
}
