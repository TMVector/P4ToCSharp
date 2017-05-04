using HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library;
using System;
using static HandConverted.core; // In reality this would be inlined unless we do some manual splitting on the IR

// NOTE will need to collate typedefs and place in usings
using PortId = HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library.bit4;

namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch
{
  // NOTE this is the translated p4 code, not the architecture implementation
  public static class very_simple_model
  {
    public static readonly PortId REAL_PORT_COUNT = new bit4(8); // NOTE bit4 because literal is 4w8 (FIXME is this literal a bit<4> or an int?)

    // FIXME should this be a struct or a class?
    public sealed class InControl : IStruct
    {
      public PortId inputPort;
    }
    
    public static readonly PortId RECIRCULATE_INPUT_PORT = (PortId)0xD; // NOTE literal is InfInt, v=13, base=16

    public static readonly PortId CPU_INPUT_PORT = (PortId)0xE;
    
    public sealed class OutControl : IStruct
    {
      public PortId outputPort;
    }
    
    public static readonly PortId DROP_PORT = (PortId)0xF;

    public static readonly PortId CPU_OUT_PORT = (PortId)0xE;

    public static readonly PortId RECIRCULATE_OUT_PORT = (PortId)0xD;
    
    // NOTE as with externs, these interfaces won't be generated, they will be found in a referenced DLL and used.
    // However, for parser, controls, packages, P4 doesn't use explicit implements, so we need to check every definition against the relevant declarations and explicitly implement in C#
    public interface Parser<H> : IParser
    {
      void apply(packet_in b,
                 out H parsedHeaders);
    }
    
    public interface Pipe<H> : IControl
    {
      void apply(ref H headers,
                 error parseError,
                 InControl inCtrl,
                 out OutControl outCtrl);
    }
    
    public interface Deparser<H> : IControl
    {
      void apply(ref H outputHeaders,
                 packet_out b);
    }
    
    public abstract class VSS<H> : Library.P4Program, IPackage where H : class // User must provide implementation of IPackages (should the skeletons be generated as separate source?)
    {
      Parser<H> p { get; }
      Pipe<H> map { get; }
      Deparser<H> d { get; }
      protected VSS(Parser<H> p,
                    Pipe<H> map,
                    Deparser<H> d)
      {
        this.p = p;
        this.map = map;
        this.d = d;
      }

      public override void process_packet(int in_port, byte[] packet_data)
      {
        // Provided by the user - NOT generated
        var inCtrl = new InControl() { inputPort = (PortId)in_port };
        var outCtrl = new OutControl();
        packet_in packetIn = new packet_in(packet_data);
        H headers = null; // FIXME is this okay?
        error err = error.NoError;
        try
        {
          p.apply(packetIn, out headers); // FIXME where does the error come from? Catch?
        } catch (error ex)
        {
          err = ex;
        }
        map.apply(ref headers, err, inCtrl, out outCtrl);
        packet_out packetOut = new packet_out();
        d.apply(ref headers, packetOut);

        this.send_packet((int)outCtrl.outputPort, packetOut.RawData, packetOut.Length);
      }
    }
    
    public interface Ck16 : IExternObject {
      void clear();
      void update<T>(T dt);
      bit16 get();
    }
  }
}
