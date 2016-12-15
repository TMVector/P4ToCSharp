using HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library;
using System;
namespace HandConverted
{
  public static class Core
  {
    public class Error : Exception
    {
      public static Error NoError { get; } = null;
    }
    public sealed class PacketTooShort : Error { } // not enough bits in packet for extract
    public sealed class NoMatch : Error { } // match statement has no matches
    public sealed class EmptyStack : Error { } // reference to .last in an empty header stack
    public sealed class FullStack : Error { } // reference to .next in a full header stack
    public sealed class OverwritingHeader : Error { } // extracting on top of a valid header
    public sealed class HeaderTooShort : Error { } // extracting too many bits into a varbit field
    public sealed class ParserTimeout : Error { } // parser execution time limit exceeded

    public sealed class Packet_in
    {
      public void Extract<T>(out T hdr) where T : HeaderBase
      { }
      public void Extract<T>(out T variableSizeHeader, uint variableFieldSizeInBits) where T : HeaderBase
      { }
      public T Lookahead<T>() where T : HeaderBase
      { }
      public void Advance(uint sizeInBits)
      { }
      public uint Length()
      { }
    }
    public sealed class Packet_out
    {
      public void Emit<T>(T hdr) where T : HeaderBase
      { }
      public void Emit<T>(bool condition, T data) where T : HeaderBase
      { }
    }
    public static void NoAction() { }
    public enum Match_kind
    {
      Exact,
      Ternary,
      Lpm
    }
  }
}
