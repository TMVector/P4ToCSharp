using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library
{
  // FIXME make all these consistent and handle weird sizes either dynamically or by generating another struct type
  // FIXME is arithmetic defined for bit<N>? vss-example does ttl-1

  public struct bit1
  {
    public const int BitWidth = 1;

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

  public struct bit4
  {
    public const int BitWidth = 4;
    
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

    public static implicit operator bit4(int v) // FIXME is this the best way to handle InfInt conversion?
    {
      throw new NotImplementedException(); // FIXME implement
    }
  }

  public struct bit8
  {
    public const int BitWidth = 8;
    
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
  }

  public struct bit16
  {
    public const int BitWidth = 16;
    
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
  }

  public struct bit32
  {
    public const int BitWidth = 32;
    
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

  public struct bit48
  {
    public const int BitWidth = 48;
    
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

  public struct bit64
  {
    public const int BitWidth = 64;
    
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
