using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  // TODO add arithmetic operators for C#-widths
  // TODO add signed equivalents (only for C#-widths?) int32, etc
  // TODO handle legal explicit casts from spec 8.9.1

  public interface IBitString
  {
    int BitWidth { get; }
    UInt64 Value { get; }
    //byte[] LargeValue { get; }
  }

  public static class BitStringExtensions
  {
    /// NOTE v[m:l] -> m is MSB, L is LSB, LSB starts at 0 e.g. 0xF0[7:4]=0x0F; 0xF0[3:0]=0x00
    public static ulong Slice(this IBitString bits, int from, int to)
    {
      return (bits.Value >> (bits.BitWidth - to)) & ~(~0uL << (to - from + 1));
    }
    public static ulong SetSlice(this IBitString bits, int from, int to, ulong val)
    {
      ulong mask = (~0uL << to) & (~0uL >> (63 - from));
      return
        // Clear bits
        (bits.Value & ~mask)
        // Set bits
        | ((val << to) & mask);
    }
    public static bitN SliceN(this IBitString bits, int from, int to)
    {
      ulong val = bits.Slice(from, to);
      return bitN.OfValue(val, to - from + 1);
    }
  }

  // Dynamic width bitstring (up to 64 wide)
  public struct bitN : IBitString
  {
    public int BitWidth { get; }
    int IBitString.BitWidth { get { return BitWidth; } }

    public UInt64 Value { get; }

    UInt64 IBitString.Value { get { return (UInt64)Value; } }

    public bitN(int width, UInt64 val)
    {
      Debug.Assert(width > 0 && width <= 64);
      BitWidth = width;
      Debug.Assert(val < (1u << BitWidth));
      Value = val; // FIXME mask? (Debug.Assert is not checked in Release)
    }

    public static bool operator ==(bitN a, bitN b)
    {
      return a.Value == b.Value;
    }

    public static bool operator ==(bitN a, UInt64 b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bitN a, bitN b)
    {
      return !(a == b);
    }
    public static bool operator !=(bitN a, UInt64 b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bitN)
        return this == (bitN)obj;
      else if (obj is UInt64)
        return this == (UInt64)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return Value.GetHashCode();
    }

    // NOTE we don't support any arithmetic for this datatype
    public static bool operator <(bitN a, IBitString b) { return a.Value < b.Value; }
    public static bool operator >(bitN a, IBitString b) { return a.Value > b.Value; }

    /// <summary>Truncates the value</summary>
    public static bitN OfValue(UInt64 v, int width)
    {
      return new bitN(width, (UInt64)(v & ~(~0uL << width)));
    }

    public bitN SetSliceN(int from, int to, ulong val)
    {
      ulong v = ((IBitString)this).SetSlice(from, to, val);
      return new bitN(this.BitWidth, v);
    }
  }

  public struct bit1 : IBitString
  {
    public const int BitWidth = 1;
    int IBitString.BitWidth { get { return BitWidth; } }

#if KIWI
    [Kiwi.HwWidth(BitWidth)]
#endif
    public byte Value { get; }

    UInt64 IBitString.Value { get { return (UInt64)Value; } }

    public bit1(byte val)
    {
      Debug.Assert(val < (1u << BitWidth));
      Value = val; // FIXME mask? (Debug.Assert is not checked in Release)
    }

    public static bool operator ==(bit1 a, bit1 b)
    {
      return a.Value == b.Value;
    }

    public static bool operator ==(bit1 a, byte b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bit1 a, bit1 b)
    {
      return !(a == b);
    }
    public static bool operator !=(bit1 a, byte b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bit1)
        return this == (bit1)obj;
      else if (obj is byte)
        return this == (byte)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return Value;
    }

    public static bool operator <(bit1 a, IBitString b) { return a.Value < b.Value; }
    public static bool operator >(bit1 a, IBitString b) { return a.Value > b.Value; }

    public static implicit operator bit1(bool v)
    {
      if (v)
        return new bit1(1);
      else
        return new bit1(0);
    }
    public static implicit operator bool(bit1 v)
    {
      return v.Value == 1;
    }
    public static implicit operator bit1(uint v)
    {
      return OfValue(v);
    }
    public static implicit operator uint(bit1 v)
    {
      return v.Value;
    }

    /// <summary>Truncates the value</summary>
    private static bit1 OfValue(UInt64 v)
    {
      return new bit1((byte)(v & ~(~0uL << BitWidth)));
    }
    public static explicit operator bit1(bitN v) { return OfValue(v.Value); }
    public static explicit operator bit1(bit4 v) { return OfValue(v.Value); }
    public static explicit operator bit1(bit8 v) { return OfValue(v.Value); }
    public static explicit operator bit1(bit16 v) { return OfValue(v.Value); }
    public static explicit operator bit1(bit32 v) { return OfValue(v.Value); }
    public static explicit operator bit1(bit48 v) { return OfValue(v.Value); }
    public static explicit operator bit1(bit64 v) { return OfValue(v.Value); }
  }

  public struct bit4 : IBitString
  {
    public const int BitWidth = 4;
    int IBitString.BitWidth { get { return BitWidth; } }

#if KIWI
    [Kiwi.HwWidth(BitWidth)]
#endif
    public byte Value { get; }

    UInt64 IBitString.Value { get { return (UInt64)Value; } }

    public bit4(byte val)
    {
      Debug.Assert(val < (1u << BitWidth));
      Value = val; // FIXME mask?
    }

    public static bool operator ==(bit4 a, bit4 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator ==(bit4 a, byte b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bit4 a, bit4 b)
    {
      return !(a == b);
    }
    public static bool operator !=(bit4 a, byte b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bit4)
        return this == (bit4)obj;
      else if (obj is byte)
        return this == (byte)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return Value;
    }

    public static bool operator <(bit4 a, IBitString b) { return a.Value < b.Value; }
    public static bool operator >(bit4 a, IBitString b) { return a.Value > b.Value; }

    public static implicit operator bit4(uint v)
    {
      return OfValue(v);
    }
    public static implicit operator uint(bit4 v)
    {
      return v.Value;
    }

    /// <summary>Truncates the value</summary>
    private static bit4 OfValue(UInt64 v)
    {
      return new bit4((byte)(v & ~(~0uL << BitWidth)));
    }
    public static explicit operator bit4(bitN v) { return OfValue(v.Value); }
    public static explicit operator bit4(bit1 v) { return OfValue(v.Value); }
    public static explicit operator bit4(bit8 v) { return OfValue(v.Value); }
    public static explicit operator bit4(bit16 v) { return OfValue(v.Value); }
    public static explicit operator bit4(bit32 v) { return OfValue(v.Value); }
    public static explicit operator bit4(bit48 v) { return OfValue(v.Value); }
    public static explicit operator bit4(bit64 v) { return OfValue(v.Value); }
  }

  public struct bit8 : IBitString
  {
    public const int BitWidth = 8;
    int IBitString.BitWidth { get { return BitWidth; } }

    public Byte Value { get; }

    UInt64 IBitString.Value { get { return (UInt64)Value; } }

    public bit8(Byte val)
    {
      Value = val;
    }

    public static bool operator ==(bit8 a, bit8 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator ==(bit8 a, byte b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bit8 a, bit8 b)
    {
      return !(a == b);
    }
    public static bool operator !=(bit8 a, byte b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bit8)
        return this == (bit8)obj;
      else if (obj is byte)
        return this == (byte)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return Value;
    }

    public static bool operator <(bit8 a, IBitString b) { return a.Value < b.Value; }
    public static bool operator >(bit8 a, IBitString b) { return a.Value > b.Value; }

    public static bit8 operator +(bit8 a, uint i)
    {
      return new bit8((byte)(a.Value + i)); // FIXME what about overflow, etc.? What is the P4 behaviour?
    }
    public static bit8 operator -(bit8 a, uint i)
    {
      return new bit8((byte)(a.Value - i)); // FIXME overflow
    }

    public static implicit operator bit8(uint v)
    {
      return OfValue(v);
    }
    public static implicit operator uint(bit8 v)
    {
      return v.Value;
    }

    /// <summary>Truncates the value</summary>
    private static bit8 OfValue(UInt64 v)
    {
      return new bit8((byte)(v & ~(~0uL << BitWidth)));
    }
    public static explicit operator bit8(bitN v) { return OfValue(v.Value); }
    public static explicit operator bit8(bit1 v) { return OfValue(v.Value); }
    public static explicit operator bit8(bit4 v) { return OfValue(v.Value); }
    public static explicit operator bit8(bit16 v) { return OfValue(v.Value); }
    public static explicit operator bit8(bit32 v) { return OfValue(v.Value); }
    public static explicit operator bit8(bit48 v) { return OfValue(v.Value); }
    public static explicit operator bit8(bit64 v) { return OfValue(v.Value); }
  }

  public struct bit16 : IBitString
  {
    public const int BitWidth = 16;
    int IBitString.BitWidth { get { return BitWidth; } }

    public UInt16 Value { get; }

    UInt64 IBitString.Value { get { return (UInt64)Value; } }

    public bit16(UInt16 val)
    {
      Value = val;
    }

    public static bool operator ==(bit16 a, bit16 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator ==(bit16 a, UInt16 b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bit16 a, bit16 b)
    {
      return !(a == b);
    }
    public static bool operator !=(bit16 a, UInt16 b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bit16)
        return this == (bit16)obj;
      else if (obj is UInt16)
        return this == (UInt16)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return Value;
    }

    public static bool operator <(bit16 a, IBitString b) { return a.Value < b.Value; }
    public static bool operator >(bit16 a, IBitString b) { return a.Value > b.Value; }

    public static bit16 operator +(bit16 a, uint i)
    {
      return new bit16((UInt16)(a.Value + i)); // FIXME what about overflow, etc.? What is the P4 behaviour?
    }
    public static bit16 operator -(bit16 a, uint i)
    {
      return new bit16((UInt16)(a.Value - i)); // FIXME overflow
    }

    public static implicit operator bit16(uint v)
    {
      return OfValue(v);
    }
    public static implicit operator uint(bit16 v)
    {
      return v.Value;
    }

    /// <summary>Truncates the value</summary>
    private static bit16 OfValue(UInt64 v)
    {
      return new bit16((UInt16)(v & ~(~0uL << BitWidth)));
    }
    public static explicit operator bit16(bitN v) { return OfValue(v.Value); }
    public static explicit operator bit16(bit1 v) { return OfValue(v.Value); }
    public static explicit operator bit16(bit4 v) { return OfValue(v.Value); }
    public static explicit operator bit16(bit8 v) { return OfValue(v.Value); }
    public static explicit operator bit16(bit32 v) { return OfValue(v.Value); }
    public static explicit operator bit16(bit48 v) { return OfValue(v.Value); }
    public static explicit operator bit16(bit64 v) { return OfValue(v.Value); }
  }

  public struct bit32 : IBitString
  {
    public const int BitWidth = 32;
    int IBitString.BitWidth { get { return BitWidth; } }

    public UInt32 Value { get; }

    UInt64 IBitString.Value { get { return (UInt64)Value; } }

    public bit32(UInt32 val)
    {
      Value = val;
    }

    public static bool operator ==(bit32 a, bit32 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator ==(bit32 a, UInt32 b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bit32 a, bit32 b)
    {
      return !(a == b);
    }
    public static bool operator !=(bit32 a, UInt32 b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bit32)
        return this == (bit32)obj;
      else if (obj is UInt32)
        return this == (UInt32)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return (int)Value;
    }

    public static bool operator <(bit32 a, IBitString b) { return a.Value < b.Value; }
    public static bool operator >(bit32 a, IBitString b) { return a.Value > b.Value; }

    public static bit32 operator +(bit32 a, uint i)
    {
      return new bit32(a.Value + i);
    }
    public static bit32 operator -(bit32 a, uint i)
    {
      return new bit32(a.Value - i);
    }
    public static bit32 operator >>(bit32 a, int i)
    {
      return new bit32(a.Value >> i);
    }
    public static bit32 operator <<(bit32 a, int i)
    {
      return new bit32(a.Value << i);
    }

    public static implicit operator bit32(uint v)
    {
      return OfValue(v);
    }
    public static implicit operator uint(bit32 v)
    {
      return v.Value;
    }

    /// <summary>Truncates the value</summary>
    private static bit32 OfValue(UInt64 v)
    {
      return new bit32((UInt32)(v & ~(~0uL << BitWidth)));
    }
    public static explicit operator bit32(bitN v) { return OfValue(v.Value); }
    public static explicit operator bit32(bit1 v) { return OfValue(v.Value); }
    public static explicit operator bit32(bit4 v) { return OfValue(v.Value); }
    public static explicit operator bit32(bit8 v) { return OfValue(v.Value); }
    public static explicit operator bit32(bit16 v) { return OfValue(v.Value); }
    public static explicit operator bit32(bit48 v) { return OfValue(v.Value); }
    public static explicit operator bit32(bit64 v) { return OfValue(v.Value); }
  }

  public struct bit48 : IBitString
  {
    public const int BitWidth = 48;
    int IBitString.BitWidth { get { return BitWidth; } }

#if KIWI
    [Kiwi.HwWidth(BitWidth)]
#endif
    public UInt64 Value { get; }

    UInt64 IBitString.Value { get { return (UInt64)Value; } }

    public bit48(UInt64 val)
    {
      Debug.Assert(val < (1uL << BitWidth));
      Value = val;
    }

    public static bool operator ==(bit48 a, bit48 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator ==(bit48 a, UInt64 b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bit48 a, bit48 b)
    {
      return !(a == b);
    }
    public static bool operator !=(bit48 a, UInt64 b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bit48)
        return this == (bit48)obj;
      else if (obj is UInt64)
        return this == (UInt64)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return (int)Value;
    }

    public static bool operator <(bit48 a, IBitString b) { return a.Value < b.Value; }
    public static bool operator >(bit48 a, IBitString b) { return a.Value > b.Value; }

    public static implicit operator bit48(ulong v)
    {
      return OfValue(v);
    }
    public static implicit operator ulong(bit48 v)
    {
      return v.Value;
    }

    /// <summary>Truncates the value</summary>
    private static bit48 OfValue(UInt64 v)
    {
      return new bit48((UInt64)(v & ~(~0uL << BitWidth)));
    }
    public static explicit operator bit48(bitN v) { return OfValue(v.Value); }
    public static explicit operator bit48(bit1 v) { return OfValue(v.Value); }
    public static explicit operator bit48(bit4 v) { return OfValue(v.Value); }
    public static explicit operator bit48(bit8 v) { return OfValue(v.Value); }
    public static explicit operator bit48(bit16 v) { return OfValue(v.Value); }
    public static explicit operator bit48(bit32 v) { return OfValue(v.Value); }
    public static explicit operator bit48(bit64 v) { return OfValue(v.Value); }
  }

  public struct bit64 : IBitString
  {
    public const int BitWidth = 64;
    int IBitString.BitWidth { get { return BitWidth; } }

    public UInt64 Value { get; }

    UInt64 IBitString.Value { get { return (UInt64)Value; } }

    public bit64(UInt64 val)
    {
      Value = val;
    }

    public static bool operator ==(bit64 a, bit64 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator ==(bit64 a, UInt64 b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bit64 a, bit64 b)
    {
      return !(a == b);
    }
    public static bool operator !=(bit64 a, UInt64 b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bit64)
        return this == (bit64)obj;
      else if (obj is UInt64)
        return this == (UInt64)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return (int)Value;
    }

    public static bool operator <(bit64 a, IBitString b) { return a.Value < b.Value; }
    public static bool operator >(bit64 a, IBitString b) { return a.Value > b.Value; }

    public static bit64 operator +(bit64 a, ulong i)
    {
      return new bit64(a.Value + i);
    }
    public static bit64 operator -(bit64 a, ulong i)
    {
      return new bit64(a.Value - i);
    }
    public static bit64 operator >>(bit64 a, int i)
    {
      return new bit64(a.Value >> i);
    }
    public static bit64 operator <<(bit64 a, int i)
    {
      return new bit64(a.Value << i);
    }

    public static implicit operator bit64(ulong v)
    {
      return OfValue(v);
    }
    public static implicit operator ulong(bit64 v)
    {
      return v.Value;
    }

    /// <summary>Truncates the value</summary>
    private static bit64 OfValue(UInt64 v)
    {
      return new bit64((UInt64)(v & ~(~0uL << BitWidth)));
    }
    public static explicit operator bit64(bitN v) { return OfValue(v.Value); }
    public static explicit operator bit64(bit1 v) { return OfValue(v.Value); }
    public static explicit operator bit64(bit4 v) { return OfValue(v.Value); }
    public static explicit operator bit64(bit8 v) { return OfValue(v.Value); }
    public static explicit operator bit64(bit16 v) { return OfValue(v.Value); }
    public static explicit operator bit64(bit32 v) { return OfValue(v.Value); }
    public static explicit operator bit64(bit48 v) { return OfValue(v.Value); }
  }
}
