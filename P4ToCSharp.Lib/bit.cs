using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  // FIXME make all these consistent and handle weird sizes either dynamically or by generating another struct type
  // FIXME is arithmetic defined for bit<N>? vss-example does ttl-1

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
    public Byte Value { get; }


    public bit1(Byte val)
    {
      Value = val;
    }

    public static bool operator ==(bit1 a, bit1 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator !=(bit1 a, bit1 b)
    {
      return !(a == b);
    }
  }

  public struct bit4 : IBitString
  {
    public const int BitWidth = 4;
    int IBitString.BitWidth { get { return BitWidth; } }

#if KIWI
    [Kiwi.HwWidth(BitWidth)]
#endif
    public Byte Value { get; }

    public bit4(Byte val)
    {
      Value = val;
    }

    public static bool operator ==(bit4 a, bit4 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator !=(bit4 a, bit4 b)
    {
      return !(a == b);
    }

    public static explicit operator bit4(uint v) // FIXME is this the best way to handle InfInt conversion?
    {
      Debug.Assert(v < (1u << BitWidth));

      return new bit4((byte)(v & ~(~1u << BitWidth)));
    }
    public static explicit operator bit4(int v) { return (bit4)(uint)v; }
    public static explicit operator bit4(ushort v) { return (bit4)(uint)v; }

    public static explicit operator int(bit4 v)
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
    public static bool operator !=(bit8 a, bit8 b)
    {
      return !(a == b);
    }
    public static bit8 operator -(bit8 a, int i)
    {
      return new bit8((byte)(a.Value - i));
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
    public static bool operator !=(bit16 a, bit16 b)
    {
      return !(a == b);
    }

    public static explicit operator bit16(int v)
    {
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
    public static bool operator !=(bit32 a, bit32 b)
    {
      return !(a == b);
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
      Value = val;
    }

    public static bool operator ==(bit48 a, bit48 b)
    {
      return a.Value == b.Value;
    }
    public static bool operator !=(bit48 a, bit48 b)
    {
      return !(a == b);
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
    public static bool operator !=(bit64 a, bit64 b)
    {
      return !(a == b);
    }
  }
}
