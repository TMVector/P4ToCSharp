﻿using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  sealed class ipv4_t
  {
    public uint offset;

    public byte version;          // width 4
    public byte ihl;              // width 4
    public byte diffserv;         // width 8
    public ushort totalLen;       // width 16
    public ushort identification; // width 16
    public byte flags;            // width 3
    public ushort fragOffset;     // width 13
    public byte ttl;              // width 8
    public byte protocol;         // width 8
    public ushort hdrChecksum;    // width 16
    public uint srcAddr;          // width 32
    public uint dstAddr;          // width 32
    public byte[] options;        // variable width

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
      BitHelper.WriteBits(arr, offset * 8, 4, version);
      BitHelper.WriteBits(arr, offset * 8 + 4, 4, ihl);
      BitHelper.Write8(arr, offset + 1, diffserv);
      BitHelper.Write16(arr, offset + 2, totalLen);
      BitHelper.Write16(arr, offset + 4, identification);
      BitHelper.WriteBits(arr, offset + 48, 3, flags);
      BitHelper.WriteBits(arr, offset + 51, 13, fragOffset);
      BitHelper.Write8(arr, offset + 8, ttl);
      BitHelper.Write8(arr, offset + 9, protocol);
      BitHelper.Write16(arr, offset + 10, hdrChecksum);
      BitHelper.Write32(arr, offset + 12, srcAddr);
      BitHelper.Write32(arr, offset + 16, dstAddr);
      BitHelper.WriteBytes(arr, offset + 20, length - 20, options);
    }
  }
}
