using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library
{
  public static class BitHelper
  {
    // FIXME perhaps better to have internal methods that extract C# datatypes, and public ones that wrap it as bitN?
    public static bit1 ExtractBit(byte[] arr, uint bitOffset)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      byte data = Extract8(arr, startByte).Value;
      data >>= (int)localBitOffset;
      return new bit1((byte)(data & 1));
    }

    public static ushort ExtractBits(byte[] arr, uint bitOffset, uint bitLength)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      uint data = Extract32(arr, startByte).Value; // FIXME is it okay to extract 32 bits when we don't know it's safe?
      data >>= (int)localBitOffset;
      data &= (~0u) >> (32 - (int)bitLength);
      return (ushort)data;
    }

    public static bit8 Extract8(byte[] arr, uint offset)
    {
      return new bit8(arr[offset]);
    }

    public static unsafe bit16 Extract16(byte[] arr, uint offset)
    {
      fixed (byte* p = &arr[offset])
      {
        return new bit16(*((ushort*)p));
      }
    }

    public static unsafe bit32 Extract32(byte[] arr, uint offset)
    {
      fixed (byte* p = &arr[offset])
      {
        return new bit32(*((uint*)p));
      }
    }

    public static bit48 Extract48(byte[] arr, uint offset)
    {
      // FIXME is it okay to extract 64 bits when we don't know it's safe? :S
      return new bit48(Extract64(arr, offset).Value & 0x0000FFFFFFFFFFFFuL);
    }

    public static unsafe bit64 Extract64(byte[] arr, uint offset)
    {
      fixed (byte* p = &arr[offset])
      {
        return new bit64(*((ulong*)p));
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
      byte data = Extract8(arr, startByte).Value;
      data &= (byte)(~(1 << (int)bitOffset));
      if (value)
        data |= (byte)(1 << (int)bitOffset);
      Write8(arr, startByte, new bit8(data));
    }

    public static void WriteBits(byte[] arr, uint bitOffset, uint bitLength, ushort value)
    {
      uint startByte = bitOffset / 8;
      int localBitOffset = (int)bitOffset % 8;
      uint data = Extract32(arr, startByte).Value;
      uint mask = ((~0u) >> (32 - (int)bitLength)) << localBitOffset;
      data &= ~mask;
      data |= (uint)(value << localBitOffset);
      Write32(arr, startByte, new bit32(data));
    }

    public static void Write8(byte[] arr, uint offset, bit8 value)
    {
      arr[offset] = value.Value;
    }

    public static unsafe void Write16(byte[] arr, uint offset, bit16 value)
    {
      fixed (byte* p = &arr[offset])
      {
        *((ushort*)p) = value.Value;
      }
    }

    public static unsafe void Write32(byte[] arr, uint offset, bit32 value)
    {
      fixed (byte* p = &arr[offset])
      {
        *((uint*)p) = value.Value;
      }
    }

    public static unsafe void Write48(byte[] arr, uint offset, bit48 value)
    {
      fixed (byte* p = &arr[offset])
      {
        *((uint*)p) = (uint)value.Value;
        *((ushort*)(p + 4)) = (ushort)(value.Value >> 32);
      }
    }

    public static unsafe void Write64(byte[] arr, uint offset, bit64 value)
    {
      fixed (byte* p = &arr[offset])
      {
        *((ulong*)p) = value.Value;
      }
    }

    public static void WriteBytes(byte[] arr, uint offset, uint length, byte[] value)
    {
      Buffer.BlockCopy(value, 0, arr, (int)offset, (int)length);
    }

    // FIXME I can see this is going to be a pain with the bitN structs...
    public static ulong Concat(ulong a, ulong b, int b_length)
    {
      return (a << b_length) | b; // FIXME should either mask b or make explicit the expectation that it matches length
    }
  }
}
