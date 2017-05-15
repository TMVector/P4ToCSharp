using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  public static class BitHelper
  {
    // FIXME throw errors like pp.65/66 in spec

    public static unsafe ulong flip(ulong v, int w)
    {
      ulong rv;
      byte* p = (byte*)&rv, op = (byte*)&v + (w-1);
      for (int i = 0; i < w; i++)
        *p++ = *op--;
      return rv;
    }

    // FIXME perhaps better to have internal methods that extract C# datatypes, and public ones that wrap it as bitN?
    public static bit1 Extract1(byte[] arr, uint bitOffset)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      byte data = Extract8(arr, startByte).Value;
      data >>= (int)localBitOffset;
      return new bit1((byte)(data & 1));
    }

    // FIXME redo this method
    public static bitN ExtractN(byte[] arr, uint bitOffset, int bitLength)
    {
      // TODO write some smarter logic for var width extraction
      Debug.Assert(bitLength > 0);
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      uint data = Extract32(arr, startByte).Value; // FIXME is it okay to extract 32 bits when we don't know it's safe?
      data >>= (int)localBitOffset;
      data &= (~0u) >> (32 - (int)bitLength);
      return new bitN(bitLength, data); // FIXME this is the wrong endianness? (at least for STF)
    }

    public static bit4 Extract4(byte[] arr, uint bitOffset)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      byte data = Extract8(arr, startByte).Value;
      data >>= (int)localBitOffset;
      return new bit4((byte)(data & 0xF));
    }

    public static bit8 Extract8(byte[] arr, uint bitOffset)
    {
      Debug.Assert(bitOffset % 8 == 0, "Offset for Extract8 must be a multiple of 8");
      return new bit8(arr[bitOffset / 8]);
    }

    public static unsafe bit16 Extract16(byte[] arr, uint bitOffset)
    {
      Debug.Assert(bitOffset % 8 == 0, "Offset for Extract16 must be a multiple of 8");
      fixed (byte* p = &arr[bitOffset / 8])
      {
        return new bit16((ushort)flip(*((ushort*)p), 2));
      }
    }

    public static unsafe bit32 Extract32(byte[] arr, uint bitOffset)
    {
      Debug.Assert(bitOffset % 8 == 0, "Offset for Extract32 must be a multiple of 8");
      fixed (byte* p = &arr[bitOffset / 8])
      {
        return new bit32((uint)flip(*((uint*)p), 4));
      }
    }

    public static unsafe bit48 Extract48(byte[] arr, uint bitOffset)
    {
      Debug.Assert(bitOffset % 8 == 0, "Offset for Extract48 must be a multiple of 8");
      ulong rv;
      fixed (byte* p = &arr[bitOffset / 8])
      {
        rv = *((uint*)p);
        rv |= ((ulong)*((ushort*)(p + 4))) << 32;
      }
      return new bit48(flip(rv, 6));
    }

    public static unsafe bit64 Extract64(byte[] arr, uint bitOffset)
    {
      Debug.Assert(bitOffset % 8 == 0, "Offset for Extract64 must be a multiple of 8");
      fixed (byte* p = &arr[bitOffset / 8])
      {
        return new bit64(flip(*((ulong*)p), 8));
      }
    }

    public static byte[] ExtractBytes(byte[] arr, uint bitOffset, uint length)
    {
      Debug.Assert(bitOffset % 8 == 0, "Offset for ExtractBytes must be a multiple of 8");
      byte[] r = new byte[length];
      Buffer.BlockCopy(arr, (int)(bitOffset / 8), r, 0, (int)length);
      return r;
    }

    // FIXME: bounds on Write methods

    public static void Write1(byte[] arr, uint bitOffset, bit1 value)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      byte data = Extract8(arr, startByte).Value;
      data &= (byte)(~(1 << (int)localBitOffset));
      data |= (byte)(value.Value << (int)localBitOffset);
      Write8(arr, startByte, new bit8(data));
    }

    public static void WriteN(byte[] arr, uint bitOffset, bitN value)
    {
      uint startByte = bitOffset / 8;
      int localBitOffset = (int)bitOffset % 8;
      uint data = Extract32(arr, startByte).Value;
      uint mask = ((~0u) >> (32 - (int)value.BitWidth)) << localBitOffset;
      data &= ~mask;
      data |= (uint)(value.Value << localBitOffset);
      Write32(arr, startByte, new bit32(data));
    }

    public static void Write4(byte[] arr, uint bitOffset, bit4 value)
    {
      uint startByte = bitOffset / 8;
      uint localBitOffset = bitOffset % 8;
      byte data = Extract8(arr, startByte).Value;
      data &= (byte)(~(0xF << (int)localBitOffset));
      data |= (byte)(value.Value << (int)localBitOffset);
      Write8(arr, startByte, new bit8(data));
    }

    public static void Write8(byte[] arr, uint bitOffset, bit8 value)
    {
      Debug.Assert(bitOffset % 8 == 0, "Offset for Write8 must be a multiple of 8");
      arr[bitOffset / 8] = value.Value;
    }

    public static unsafe void Write16(byte[] arr, uint bitOffset, bit16 value)
    {
      var v = (ushort)flip(value.Value, 2);
      Debug.Assert(bitOffset % 8 == 0, "Offset for Write16 must be a multiple of 8");
      fixed (byte* p = &arr[bitOffset / 8])
      {
        *((ushort*)p) = v;
      }
    }

    public static unsafe void Write32(byte[] arr, uint bitOffset, bit32 value)
    {
      var v = (uint)flip(value.Value, 4);
      Debug.Assert(bitOffset % 8 == 0, "Offset for Write32 must be a multiple of 8");
      fixed (byte* p = &arr[bitOffset / 8])
      {
        *((uint*)p) = v;
      }
    }

    public static unsafe void Write48(byte[] arr, uint bitOffset, bit48 value)
    {
      var v = flip(value.Value, 6);
      Debug.Assert(bitOffset % 8 == 0, "Offset for Write48 must be a multiple of 8");
      fixed (byte* p = &arr[bitOffset / 8])
      {
        *((uint*)p) = (uint)value.Value;
        *((ushort*)(p + 4)) = (ushort)(value.Value/* >> 32*/);
      }
    }

    public static unsafe void Write64(byte[] arr, uint bitOffset, bit64 value)
    {
      var v = flip(value.Value, 8);
      Debug.Assert(bitOffset % 8 == 0, "Offset for Write64 must be a multiple of 8");
      fixed (byte* p = &arr[bitOffset / 8])
      {
        *((ulong*)p) = value.Value;
      }
    }

    public static void WriteBytes(byte[] arr, uint byteOffset, uint length, byte[] value)
    {
      Buffer.BlockCopy(value, 0, arr, (int)byteOffset, (int)length);
    }

    // FIXME I can see this is going to be a pain with the bitN structs...
    public static ulong Concat(ulong a, ulong b, int b_length)
    {
      return (a << b_length) | b; // FIXME should either mask b or make explicit the expectation that it matches length
    }
  }
}
