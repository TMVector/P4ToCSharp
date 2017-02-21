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
    public interface packet_in : IExternObject
    {
      // FIXME can this also be used with structs?
      // NOTE should set validy=true if succeeds
      void extract<T>(out T hdr) where T : HeaderBase; // NOTE this constraint on T is not in the JSON - will this need to be a special case in the matching?
      void extract<T>(out T variableSizeHeader, bit32 variableFieldSizeInBits) where T : HeaderBase;
      T lookahead<T>() where T : HeaderBase;
      void advance(bit32 sizeInBits);
      bit32 length();
    }

    // extern
    public interface packet_out : IExternObject
    {
      void emit<T>(T hdr) where T : HeaderBase;
      void emit<T>(bool condition, T data) where T : HeaderBase;
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
