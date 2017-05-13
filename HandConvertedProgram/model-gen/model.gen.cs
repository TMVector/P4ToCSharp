using P4ToCSharp.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using PortId = P4ToCSharp.Library.bit4; // NOTE FQ name

namespace HandConvertedProgram
{
  public static class ModelInterface
  {
    // core
    //======

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

      protected error() { }
    }

    [P4(P4Type.ExternObject, "packet_in")]
    public interface packet_in : IExternObject
    {
      // NOTE the target manufacturer could add a T:new() constraint
      void extract<T>(out T hdr);
      void extract<T>(out T variableSizeHeader,
                      bit32 variableFieldSizeInBits);
      T lookahead<T>();
      void advance(bit32 sizeInBits);
      bit32 length();
    }

    [P4(P4Type.ExternObject, "packet_out")]
    public interface packet_out : IExternObject
    {
      void emit<T>(T hdr);
      void emit<T>(bool condition, T data);
    }

    // NOTE since these are static methods, we cannot generate interfaces, so we generate an example
    [P4(P4Type.ExternFunction, "verify")]
    static void verify(bool cond, error err)
    {
      throw new NotImplementedException();
    }

    // NOTE - not model, just stdlib
    [P4(P4Type.Action, "NoAction")]
    static void NoAction()
    {
      throw new NotImplementedException();
    }

    // FIXME - What about how tables are grabbed, etc?
    [P4(P4Type.MatchKind, "match_kind")]
    public enum match_kind
    {
      exact,
      ternary,
      lpm
    }

    // VSS
    //======

    // typedef is a using at the top

    [P4(P4Type.Const, "REAL_PORT_COUNT")]
    public static readonly PortId REAL_PORT_COUNT = 8; // Cannot be const because struct

    [P4(P4Type.Struct, "InControl")]
    public sealed class InControl : IStruct
    {
      public PortId inputPort;
    }

    [P4(P4Type.Const, "RECIRCULATE_IN_PORT")]
    public static readonly PortId RECIRCULATE_IN_PORT = 0xD;

    [P4(P4Type.Const, "CPU_IN_PORT")]
    public static readonly PortId CPU_IN_PORT = 0xE;

    [P4(P4Type.Struct, "OutControl")]
    public sealed class OutControl : IStruct
    {
      public PortId outputPort;
    }

    [P4(P4Type.Const, "DROP_PORT")]
    public static readonly PortId DROP_PORT = 0xF;

    [P4(P4Type.Const, "CPU_OUT_PORT")]
    public static readonly PortId CPU_OUT_PORT = 0xE;

    [P4(P4Type.Const, "RECIRCULATE_OUT_PORT")]
    public static readonly PortId RECIRCULATE_OUT_PORT = 0xD;


    [P4(P4Type.Parser, "Parser")]
    public interface Parser<H> : IParser
    {
      void apply(packet_in b, out H parsedHeaders); // NOTE params moved from Parser<H>(...) -> explicit apply method
    }

    [P4(P4Type.Control, "Pipe")]
    public interface Pipe<H> : IControl
    {
      void apply(ref H headers,
                 error parseError,
                 InControl inCtrl,
                 out OutControl outCtrl);
    }

    [P4(P4Type.Control, "Deparser")]
    public interface Deparser<H> : IControl
    {
      void apply(ref H outputHeaders,
                 packet_out b);
    }

    [P4(P4Type.Package, "VSS")]
    public interface VSS<H> : IPackage
    {
      void use(Parser<H> p,
               Pipe<H> map,
               Deparser<H> d);
    }

    [P4(P4Type.ExternObject, "Ck16")]
    public interface Ck16 : IExternObject
    {
      // FIXME cannot have ctor in interface.   Will have to check the implementing type when found with reflection
      //Ck16();

      void clear();

      void update<T>(T data);

      bit16 get();
    }
  }
}

/*
core.p4
=======

error {
    NoError,           // no error
    PacketTooShort,    // not enough bits in packet for extract
    NoMatch,           // match expression has no matches
    StackOutOfBounds,  // reference to invalid element of a header stack
    OverwritingHeader, // one header is extracted twice
    HeaderTooShort,    // extracting too many bits in a varbit field
    ParserTimeout      // parser execution time limit exceeded
}

extern packet_in {
    // T must be a fixed-size header type
    void extract<T>(inout T hdr); // NOTE I changed from out -> inout
    // T must be a header containing exactly 1 varbit field
    void extract<T>(inout T variableSizeHeader, // NOTE I changed from out -> inout
                    in bit<32> variableFieldSizeInBits);
    // does not advance the cursor
    T lookahead<T>();
    // skip this many bits from packet
    void advance(in bit<32> sizeInBits);
    // packet length in bytes
    bit<32> length();
}

extern packet_out {
    void emit<T>(in T hdr);
    void emit<T>(in bool condition, in T data);
}

action NoAction() {}

match_kind {
    exact,
    ternary,
    lpm
}


very_simple_model.p4
====================
#include <core.p4>

// Various constants and structure definitions
// ports are represented using 4-bit values
typedef bit<4> PortId;
// only 8 ports are "real"
const PortId REAL_PORT_COUNT = 4w8;  // 4w8 is the number 8 in 4 bits
// metadata accompanying an input packet
struct InControl
{
  PortId inputPort;
}

/* special input port values
const PortId RECIRCULATE_IN_PORT = 0xD;
const PortId CPU_IN_PORT = 0xE;
/* metadata that must be computed for outgoing packets
struct OutControl
{
  PortId outputPort;
}

/* special output port values for outgoing packet
const PortId DROP_PORT = 0xF;
const PortId CPU_OUT_PORT = 0xE;
const PortId RECIRCULATE_OUT_PORT = 0xD;
/* Prototypes for all programmable blocks
/**
 * Programmable parser.
 * @param <H> type of headers; defined by user
 * @param b input packet
 * @param parsedHeaders headers constructed by parser
 *
parser Parser<H>(packet_in b,
                 out H parsedHeaders);
/**
 * Match-action pipeline
 * @param <H> type of input and output headers
 * @param headers headers received from the parser and sent to the deparser
 * @param parseError error that may have surfaced during parsing
 * @param inCtrl information from target, accompanying input packet
 * @param outCtrl information for target, accompanying output packet
 *
control Pipe<H>(inout H headers,
                in error parseError, // parser error
                in InControl inCtrl, // input port
                out OutControl outCtrl); // output port
/**
 * Switch deparser.
 * @param <H> type of headers; defined by user
 * @param b output packet
 * @param outputHeaders headers for output packet
 *
control Deparser<H>(inout H outputHeaders,
                    packet_out b);

/**
 * Top-level package declaration – must be instantiated by user.
 * The arguments to the package indicate blocks that
 * must be instantiated by the user.
 * @param <H> user-defined type of the headers processed.
 *
package VSS<H>(Parser<H> p,
               Pipe<H> map,
               Deparser<H> d);

// Target-specific objects that can be instantiated

// Checksum unit
extern Ck16 {
    Ck16();
void clear();           // prepare unit for computation
void update<T>(in T data); // add data to checksum
bit<16> get(); // get the checksum for the data added since last clear
}
 */
