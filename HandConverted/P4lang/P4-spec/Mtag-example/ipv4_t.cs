using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  class ipv4_t
  {
    public uint offset;

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
    public byte[] options; // * (variable length options)

    public uint length { get { return ((uint)ihl & 0xF) * 4; } }
    public const uint max_length = 60;

    public ipv4_t(uint offset)
    {
      this.offset = offset;
    }

    public void Extract(byte[] arr)
    {
      version = (byte)BitHelper.ExtractBits(arr, offset * 8, 4);
      ihl = (byte)BitHelper.ExtractBits(arr, offset * 8 + 4, 4);
      diffserv = BitHelper.Extract8(arr, offset + 1);
      totalLen = BitHelper.Extract16(arr, offset + 2);
      identification = BitHelper.Extract16(arr, offset + 4);
      flags = (byte)BitHelper.ExtractBits(arr, offset + 48, 3);
      fragOffset = BitHelper.ExtractBits(arr, offset + 51, 13);
      ttl = BitHelper.Extract8(arr, offset + 8);
      protocol = BitHelper.Extract8(arr, offset + 9);
      hdrChecksum = BitHelper.Extract16(arr, offset + 10);
      srcAddr = BitHelper.Extract32(arr, offset + 12);
      dstAddr = BitHelper.Extract32(arr, offset + 16);
      options = BitHelper.ExtractBytes(arr, offset + 20, length - 20);
    }
    public void Write(byte[] arr)
    {
      // TODO
    }
  }
}
