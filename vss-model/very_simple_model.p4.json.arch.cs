using System;
using P4ToCSharp.Library;
using PortId = P4ToCSharp.Library.bit4;

namespace vss_model
{
  [P4Architecture]
  public class Architecture
  {
    [P4(P4Type.Error, "error")]
    public class error : P4ToCSharp.Library.error
    {
      public static readonly error NoError = new error();
      public static readonly error PacketTooShort = new error();
      public static readonly error NoMatch = new error();
      public static readonly error StackOutOfBounds = new error();
      public static readonly error OverwritingHeader = new error();
      public static readonly error HeaderTooShort = new error();
      public static readonly error ParserTimeout = new error();

      protected error()
      {
      }
    }

    [P4(P4Type.ExternObject, "packet_in")]
    public interface packet_in
    {
      void extract<T>(out T hdr);
      void extract<T>(out T variableSizeHeader, bit32 variableFieldSizeInBits);
      T lookahead<T>();
      void advance(bit32 sizeInBits);
      bit32 length();
    }

    [P4(P4Type.ExternObject, "packet_out")]
    public interface packet_out
    {
      void emit<T>(T hdr);
      void emit<T>(bool condition, T data);
    }

    [P4(P4Type.ExternFunction, "verify")]
    static void verify(bool condition, P4ToCSharp.Library.error err)
    {
      throw new NotImplementedException();
    }

    [P4(P4Type.MatchKind, "")]
    public enum MatchKind
    {
      exact,
      ternary,
      lpm
    }

    [P4(P4Type.Struct, "InControl")]
    public sealed class InControl : IStruct
    {
      public PortId inputPort;
    }

    [P4(P4Type.Struct, "OutControl")]
    public sealed class OutControl : IStruct
    {
      public PortId outputPort;
    }

    [P4(P4Type.Parser, "Parser")]
    public interface Parser<H> : IParser
    {
      void apply(packet_in b, out H parsedHeaders);
    }

    [P4(P4Type.Control, "Pipe")]
    public interface Pipe<H> : IControl
    {
      void apply(H headers_capture, ref H headers, P4ToCSharp.Library.error parseError, InControl inCtrl, out OutControl outCtrl);
    }

    [P4(P4Type.Control, "Deparser")]
    public interface Deparser<H> : IControl
    {
      void apply(H outputHeaders_capture, ref H outputHeaders, packet_out b);
    }

    [P4(P4Type.Package, "VSS")]
    public interface VSS<H> : IPackage
    {
      void Use(Parser<H> p, Pipe<H> map, Deparser<H> d);
    }

    [P4(P4Type.ExternObject, "Ck16")]
    public interface Ck16
    {
      void clear();
      void update<T>(T data);
      bit16 get();
    }
  }
}