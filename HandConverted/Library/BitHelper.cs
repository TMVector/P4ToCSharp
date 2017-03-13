using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.Library
{
  public static class BitHelper
  {
    public static bool ExtractBit(byte[] arr, uint bitOffset)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      byte data = Extract8(arr, startByte);
      data >>= (int)localBitOffset;
      return (data & 1) != 0;
    }

    public static ushort ExtractBits(byte[] arr, uint bitOffset, uint bitLength)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      uint data = Extract32(arr, startByte);
      data >>= (int)localBitOffset;
      data &= (~0u) >> (32 - (int)bitLength);
      return (ushort)data;
    }

    public static byte Extract8(byte[] arr, uint offset)
    {
      return arr[offset];
    }

    public static unsafe ushort Extract16(byte[] arr, uint offset)
    {
      fixed (byte* p = &arr[offset])
      {
        return *((ushort*)p);
      }
    }

    public static unsafe uint Extract32(byte[] arr, uint offset)
    {
      fixed (byte* p = &arr[offset])
      {
        return *((uint*)p);
      }
    }

    public static ulong Extract48(byte[] arr, uint offset)
    {
      return Extract64(arr, offset) & 0x0000FFFFFFFFFFFFuL;
    }

    public static unsafe ulong Extract64(byte[] arr, uint offset)
    {
      fixed (byte* p = &arr[offset])
      {
        return *((ulong*)p);
      }
    }

    public static byte[] ExtractBytes(byte[] arr, uint offset, uint length)
    {
      byte[] r = new byte[length];
      Buffer.BlockCopy(arr, (int)offset, r, 0, (int)length);
      return r;
    }

    // FIXME: bounds on Write methods

    public static void WriteBit(byte[] arr, uint bitOffset, bool value)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      byte data = Extract8(arr, startByte);
      data &= (byte)(~(1 << (int)bitOffset));
      if (value)
        data |= (byte)(1 << (int)bitOffset);
      Write8(arr, startByte, data);
    }

    public static void WriteBits(byte[] arr, uint bitOffset, uint bitLength, ushort value)
    {
      uint startByte = bitOffset / 8;
      int localBitOffset = (int)bitOffset % 8;
      uint data = Extract32(arr, startByte);
      uint mask = ((~0u) >> (32 - (int)bitLength)) << localBitOffset;
      data &= ~mask;
      data |= (uint)(value << localBitOffset);
      Write32(arr, startByte, data);
    }

    public static void Write8(byte[] arr, uint offset, byte value)
    {
      arr[offset] = value;
    }

    public static unsafe void Write16(byte[] arr, uint offset, ushort value)
    {
      fixed (byte* p = &arr[offset])
      {
        *((ushort*)p) = value;
      }
    }

    public static unsafe void Write32(byte[] arr, uint offset, uint value)
    {
      fixed (byte* p = &arr[offset])
      {
        *((uint*)p) = value;
      }
    }

    public static unsafe void Write48(byte[] arr, uint offset, ulong value)
    {
      fixed (byte* p = &arr[offset])
      {
        *((uint*)p) = (uint)value;
        *((ushort*)(p + 4)) = (ushort)(value >> 32);
      }
    }

    public static unsafe void Write64(byte[] arr, uint offset, ulong value)
    {
      fixed (byte* p = &arr[offset])
      {
        *((ulong*)p) = value;
      }
    }

    public static void WriteBytes(byte[] arr, uint offset, uint length, byte[] value)
    {
      Buffer.BlockCopy(value, 0, arr, (int)offset, (int)length);
    }

    public static ulong Concat(ulong a, ulong b, int b_length)
    {
      return (a << b_length) | b; // FIXME should either mask b or make explicit the expectation that it matches length
    }
  }
}
