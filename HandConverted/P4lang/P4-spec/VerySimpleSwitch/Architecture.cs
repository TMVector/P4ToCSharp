using System;

namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch
{
  // Architecture definition file for the "Simple" switch
  public static class Architecture
  {
    public sealed class Checksum16
    {
      // prepare unit
      public void clear()
      { }

      // add data to be checksummed
      public void update(HeaderBase dt)
      { }
      public void update(Bitstring dt)
      { }

      // conditionally add data to be checksummed
      public void update(bool condition, HeaderBase dt)
      { }
      public void update(bool condition, Bitstring dt)
      { }

      // get the checksum of all data added since the last clear
      public ushort get()
      { }
    }

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

    /* List of blocks that must be implemented */
    public interface Parser<H> : Parser
    {
      void Apply(Core.paket_in b,
                 out H parsedHeaders);
    }
    public interface MAP<H> : Control
    {
      void Apply(ref H headers,
                 Error parseError, // parser error
                 InControl inCtrl, // input port
                 out OutControl outCtrl);  // output port
    }
    public interface Deparser<H> : Control
    {
      void Apply(ref H outputHeaders,
                 Core.packet_out b);
    }

    /** 
     * Simple switch declaration.
     * H is the user-defined type of the headers processed
     */
    interface Simple<H> : Package
    {
      Parser<H> p { get; }
      MAP<H> map { get; }
      Deparser<H> d { get; }
    }
  }
}
