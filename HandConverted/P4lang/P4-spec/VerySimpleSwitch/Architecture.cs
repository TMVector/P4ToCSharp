using System;

namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch
{
  public static class Architecture
  {
    /* ports are represented using 4-bit values */
    public struct PortId_t
    {
      public const int BitWidth = 4;

      public byte Value { get; }

      public PortId_t(byte val)
      {
        Value = val;
      }
    }

    /* only 8 ports are “real” */
    public static readonly PortId_t REAL_PORT_COUNT = new PortId_t(8);

    /* metadata accompanying an input packet */
    public class InControl
    {
      PortId_t inputPort;
    }

    /* special input port values */
    public static readonly PortId_t RECIRCULATE_INPUT_PORT = new PortId_t(0xD);
    public static readonly PortId_t CPU_INPUT_PORT = new PortId_t(0xE);

    /* metadata that must be computed for outgoing packets */
    public class OutControl
    {
      PortId_t outputPort;
    }

    /* special output port values for outgoing packet */
    public static readonly PortId_t DROP_PORT = new PortId_t(0xF);
    public static readonly PortId_t CPU_OUT_PORT = new PortId_t(0xE);
    public static readonly PortId_t RECIRCULATE_OUT_PORT = new PortId_t(0xD);

    /**
     * Programmable parser.
     * @param <H> type of headers; defined by user
     * @param b input packet
     * @param parsedHeaders headers constructed by parser
     */
    public interface Parser<H> : Parser
    {
      void Apply(Core.paket_in b,
                 out H parsedHeaders);
    }

    /**
     * Match-action pipeline
     * @param <H> type of input and output headers
     * @param headers headers received from the parser and sent to the deparser
     * @param parseError error that may have surfaced during parsing
     * @param inCtrl information from architecture, accompanying input packet
     * @param outCtrl information for architecture, accompanying output packet
     */
    public interface Pipe<H> : Control
    {
      void Apply(ref H headers,
                 Error parseError, // parser error
                 InControl inCtrl, // input port
                 out OutControl outCtrl);  // output port
    }

    /**
     * Switch deparser.
     * @param <H> type of headers; defined by user
     * @param b output packet
     * @param outputHeaders headers for output packet
     */
    public interface Deparser<H> : Control
    {
      void Apply(ref H outputHeaders,
                 Core.packet_out b);
    }

    /**
     * Top-level package declaration - must be instantiated by user.
     * The arguments to the package indicate blocks that
     * must be instantiated by the user.
     * @param <H> user-defined type of the headers processed.
     */
    interface VSS<H> : Package
    {
      Parser<H> p { get; }
      Pipe<H> map { get; }
      Deparser<H> d { get; }
    }

    // Checksum unit
    public sealed class Checksum16
    {
      // prepare unit for computation
      public void clear()
      { }

      // add data to checksum
      public void update(HeaderBase dt)
      { }
      public void update(Bitstring dt)
      { }

      // remove data from existing checksum
      public void remove(bool condition, HeaderBase dt)
      { }
      public void remove(bool condition, Bitstring dt)
      { }

      // get the checksum for the data added since last clear
      public ushort get()
      { }
    }
  }
}
