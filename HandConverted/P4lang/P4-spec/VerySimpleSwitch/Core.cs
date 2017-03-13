using HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library;
using System;
namespace HandConverted
{
  // Since p4c generates a single output file for p4 and includes, this is for core.p4 on its own
  public static class core
  {
    // FIXME are we going with an exception model or a railroads model? Could just use enum in the latter case since p4c collates them
    public class error : Exception
    {
      public static error NoError { get; } = null;
    }
    public sealed class PacketTooShort : error { } 
    public sealed class NoMatch : error { }
    public sealed class StackOutOfBounds : error { }
    public sealed class OverwritingHeader : error { }
    public sealed class HeaderTooShort : error { }
    public sealed class ParserTimeout : error { }

    // NOTE this isn't generated, this should be checked against a DLL (exists, matches) and that type should be used throughout generated code
    // extern
    public class packet_in : IExternObject
    {
      public readonly byte[] RawData;
      public readonly int Length;

      public packet_in(byte[] data)
      {
        RawData = data;
      }
      
      // FIXME can this also be used with structs?
      // NOTE should set validy=true if succeeds
      public void extract<T>(out T hdr) where T : HeaderBase; // NOTE this constraint on T is not in the JSON - will this need to be a special case in the matching? (this is in an extern object, so user can specifiy?)
      public void extract<T>(out T variableSizeHeader, bit32 variableFieldSizeInBits) where T : HeaderBase;
      public T lookahead<T>() where T : HeaderBase;
      public void advance(bit32 sizeInBits);
      public bit32 length() { return new bit32((uint)Length); }
    }

    // extern
    public class packet_out : IExternObject // FIXME because this is an extern object, are its copy semantics C# like?
    {
      public byte[] RawData;
      public int Length;

      public void emit<T>(T hdr) where T : HeaderBase;
      public void emit<T>(bool condition, T data) where T : HeaderBase;
    }

    // extern
    public interface verify_t : IExternFunction // NOTE need global, static instance of this. Added _t to handle name conflict
    {
      void apply(bool check, error toSignal); // NOTE only directionless parameters
    }
    public static verify_t verify = null; // NOTE need global, static instance of this
    
    public static void NoAction() { }

    public enum match_kind
    {
      exact,
      ternary,
      lpm
    }
  }
}
