﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  // FIXME make all these consistent and handle weird sizes either dynamically or by generating another struct type
  // FIXME is arithmetic defined for bit<N>? vss-example does ttl-1. What about bitN op bitM?
  //       In my proposal I said I would only handle arithmetic on types that are the same width as C# primitives.

  public interface IBitString
  {
    int BitWidth { get; }
  }

  public struct bit1 : IBitString
  {
    public const int BitWidth = 1;
    int IBitString.BitWidth { get { return BitWidth; } }

#if KIWI
    [Kiwi.HwWidth(BitWidth)]
#endif
    public byte Value { get; }


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

    public static explicit operator bit1(uint v)
    {
      Debug.Assert(v < (1u << BitWidth));
      return new bit1((byte)(v & ~(~0u << BitWidth)));
    }
    public static explicit operator uint(bit1 v)
    {
      return v.Value;
    }
  }

  public struct bit4 : IBitString
  {
    public const int BitWidth = 4;
    int IBitString.BitWidth { get { return BitWidth; } }

#if KIWI
    [Kiwi.HwWidth(BitWidth)]
#endif
    public byte Value { get; }

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

    public static explicit operator bit4(uint v)
    {
      Debug.Assert(v < (1u << BitWidth));
      return new bit4((byte)(v & ~(~0u << BitWidth)));
    }
    public static explicit operator uint(bit4 v)
    {
      return v.Value;
    }
  }

  public struct bit8 : IBitString
  {
    public const int BitWidth = 8;
    int IBitString.BitWidth { get { return BitWidth; } }

    public Byte Value { get; }

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

    public static bit8 operator +(bit8 a, uint i)
    {
      return new bit8((byte)(a.Value - i)); // FIXME what about overflow, etc.? What is the P4 behaviour?
    }
    public static bit8 operator -(bit8 a, uint i)
    {
      return new bit8((byte)(a.Value - i)); // FIXME overflow
    }

    public static explicit operator bit8(uint v)
    {
      Debug.Assert(v < (1u << BitWidth));
      return new bit8((byte)(v & ~(~0u << BitWidth)));
    }
    public static explicit operator uint(bit8 v)
    {
      return v.Value;
    }
  }

  public struct bit16 : IBitString
  {
    public const int BitWidth = 16;
    int IBitString.BitWidth { get { return BitWidth; } }

    public UInt16 Value { get; }

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

    public static bit16 operator +(bit16 a, uint i)
    {
      return new bit16((UInt16)(a.Value - i)); // FIXME what about overflow, etc.? What is the P4 behaviour?
    }
    public static bit16 operator -(bit16 a, uint i)
    {
      return new bit16((UInt16)(a.Value - i)); // FIXME overflow
    }

    public static explicit operator bit16(uint v)
    {
      Debug.Assert(v < (1u << BitWidth));
      return new bit16((UInt16)(v & ~(~0u << BitWidth)));
    }
    public static explicit operator uint(bit16 v)
    {
      return v.Value;
    }
  }

  public struct bit32 : IBitString
  {
    public const int BitWidth = 32;
    int IBitString.BitWidth { get { return BitWidth; } }

    public UInt32 Value { get; }

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

    public static bit32 operator +(bit32 a, uint i)
    {
      return new bit32(a.Value - i);
    }
    public static bit32 operator -(bit32 a, uint i)
    {
      return new bit32(a.Value - i);
    }

    public static explicit operator bit32(uint v)
    {
      Debug.Assert(v < (1u << BitWidth));
      return new bit32(v & ~(~0u << BitWidth));
    }
    public static explicit operator uint(bit32 v)
    {
      return v.Value;
    }
  }

  public struct bit48 : IBitString
  {
    public const int BitWidth = 48;
    int IBitString.BitWidth { get { return BitWidth; } }

#if KIWI
    [Kiwi.HwWidth(BitWidth)]
#endif
    public UInt64 Value { get; }

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

    public static explicit operator bit48(ulong v)
    {
      Debug.Assert(v < (1uL << BitWidth));
      return new bit48(v & ~(~0uL << BitWidth));
    }
    public static explicit operator ulong(bit48 v)
    {
      return v.Value;
    }
  }

  public struct bit64 : IBitString
  {
    public const int BitWidth = 64;
    int IBitString.BitWidth { get { return BitWidth; } }

    public UInt64 Value { get; }

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

    public static bit64 operator +(bit64 a, ulong i)
    {
      return new bit64(a.Value - i);
    }
    public static bit64 operator -(bit64 a, ulong i)
    {
      return new bit64(a.Value - i);
    }

    public static explicit operator bit64(ulong v)
    {
      Debug.Assert(v < (1uL << BitWidth));
      return new bit64(v & ~(~0uL << BitWidth));
    }
    public static explicit operator ulong(bit64 v)
    {
      return v.Value;
    }
  }

  // We use a type parameter to statically enforce similar-width assignments, etc. without hardcoding
  public struct bitw<TWidth> : IBitString where TWidth : N
  {
    public int BitWidth { get; }
    public UInt16 Value { get; }

    public bitw(UInt16 value) : this(N.GetValue<TWidth>(), value) { }
    public bitw(int width, UInt16 value)
    {
      Debug.Assert(width == N.GetValue<TWidth>());
      Debug.Assert(value < (1u << width));
      BitWidth = width;
      Value = value;
    }

    public static bool operator ==(bitw<TWidth> a, bitw<TWidth> b)
    {
      return a.BitWidth == b.BitWidth && a.Value == b.Value;
    }
    public static bool operator ==(bitw<TWidth> a, UInt16 b)
    {
      return a.Value == b;
    }
    public static bool operator !=(bitw<TWidth> a, bitw<TWidth> b)
    {
      return !(a == b);
    }
    public static bool operator !=(bitw<TWidth> a, UInt16 b)
    {
      return !(a == b);
    }
    public override bool Equals(object obj)
    {
      if (obj is bitw<TWidth>)
        return this == (bitw<TWidth>)obj;
      else if (obj is UInt16)
        return this == (UInt16)obj;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return (int)Value;
    }

    public static bitw<TWidth> operator +(bitw<TWidth> a, UInt16 i)
    {
      return new bitw<TWidth>(a.BitWidth, (UInt16)(a.Value - i));
    }
    public static bitw<TWidth> operator -(bitw<TWidth> a, UInt16 i)
    {
      return new bitw<TWidth>(a.BitWidth, (UInt16)(a.Value - i));
    }

    public static explicit operator bitw<TWidth>(ulong v)
    {
      int width = N.GetValue<TWidth>();
      return new bitw<TWidth>(width, (UInt16)(v & ~(~0uL << width)));
    }
    public static explicit operator UInt16(bitw<TWidth> v)
    {
      return v.Value;
    }
  }

  public class N
  {
    // Private ctor so effectively sealed to public
    private N() { }

    // Types to parameterise bitw by width
    public sealed class N2 : N { }
    public sealed class N3 : N { }
    public sealed class N5 : N { }
    public sealed class N6 : N { }
    public sealed class N7 : N { }
    public sealed class N9 : N { }
    public sealed class N10 : N { }
    public sealed class N11 : N { }
    public sealed class N12 : N { }
    public sealed class N13 : N { }
    public sealed class N14 : N { }
    public sealed class N15 : N { }

    public static int GetValue<TN>() where TN : N
    {
      if (typeof(TN) == typeof(N2)) return 2;
      if (typeof(TN) == typeof(N3)) return 3;
      if (typeof(TN) == typeof(N5)) return 5;
      if (typeof(TN) == typeof(N6)) return 6;
      if (typeof(TN) == typeof(N7)) return 7;
      if (typeof(TN) == typeof(N9)) return 9;
      if (typeof(TN) == typeof(N10)) return 10;
      if (typeof(TN) == typeof(N11)) return 11;
      if (typeof(TN) == typeof(N12)) return 12;
      if (typeof(TN) == typeof(N13)) return 13;
      if (typeof(TN) == typeof(N14)) return 14;
      if (typeof(TN) == typeof(N15)) return 15;
      throw new NotImplementedException("Subtype of N not handled in N.GetValue");
    }
  }
}
