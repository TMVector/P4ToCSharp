using HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static HandConverted.core;
using static HandConverted.P4lang.P4_spec.VerySimpleSwitch.very_simple_model;

// NOTE that the type name here has to be globally(?) qualified
using EthernetAddress = HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library.bit48;
using IPv4Address = HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library.bit32;

// NOTE copied forward typedefs from separate file - won't be neccesary if all in the same file
using PortId = HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library.bit4;


namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch
{
  public static class Program
  {
    // standard Ethernet header
    public sealed class Ethernet_h : HeaderBase
    {
      public readonly uint length = 12; // FIXME should this be a abstract property in HeaderBase? (it doesn't come from the P4 source)

      public EthernetAddress dstAddr;
      public EthernetAddress srcAddr;
      public bit16 etherType;

      // NOTE these methods aren't in the P4 - they are generated to help packet_in, etc.
      public override void Extract(byte[] data, uint offset)
      {
        dstAddr = BitHelper.Extract48(data, offset);
        srcAddr = BitHelper.Extract48(data, offset + 5);
        etherType = BitHelper.Extract16(data, offset + 10);
      }

      public override void Write(byte[] data, uint offset)
      {
        BitHelper.Write48(data, offset, dstAddr);
        BitHelper.Write48(data, offset + 5, srcAddr);
        BitHelper.Write16(data, offset + 10, etherType);
      }
    }
    
    public sealed class IPv4_h : HeaderBase
    {
      public bit4 version;
      public bit4 ihl;
      public bit8 diffserv;
      public bit16 totalLen;
      public bit16 identification;
      public bit3 flags; // FIXME which bitN's should be generated, and which should be already in Library? Or should there be a dynamic type for some N's? But then how can we constrain the length?
      public bit13 fragOffset;
      public bit8 ttl;
      public bit8 protocol;
      public bit16 hdrChecksum;
      public IPv4Address srcAddr;
      public IPv4Address dstAddr;

      // FIXME what other properties does header have? Check these are in the spec
      public uint length { get { return ((uint)ihl.Value & 0xF) * 4; } } // NOTE this is NOT in the current P4 source!
      public const uint max_length = 60;

      public override void Extract(byte[] arr, uint offset)
      {
        version = (byte)BitHelper.ExtractBits(arr, offset * 8, 4);
        ihl = (byte)BitHelper.ExtractBits(arr, offset * 8 + 4, 4);
        diffserv = BitHelper.Extract8(arr, offset + 1);
        totalLen = BitHelper.Extract16(arr, offset + 2);
        identification = BitHelper.Extract16(arr, offset + 4);
        flags = (byte)BitHelper.ExtractBits(arr, offset + 48, 3);
        fragOffset = BitHelper.ExtractBits(arr, offset + 51, 13);
        ttl = BitHelper.Extract8(arr, offset + 8);
        protocol = BitHelper.Extract8(arr, offset + 9);
        hdrChecksum = BitHelper.Extract16(arr, offset + 10);
        srcAddr = BitHelper.Extract32(arr, offset + 12);
        dstAddr = BitHelper.Extract32(arr, offset + 16);
      }

      public override void Write(byte[] arr, uint offset)
      {
        BitHelper.WriteBits(arr, offset * 8, 4, version.Value); // NOTE will need to handle these as special cases?
        BitHelper.WriteBits(arr, offset * 8 + 4, 4, ihl.Value);
        BitHelper.Write8(arr, offset + 1, diffserv);
        BitHelper.Write16(arr, offset + 2, totalLen);
        BitHelper.Write16(arr, offset + 4, identification);
        BitHelper.WriteBits(arr, offset + 48, 3, flags);
        BitHelper.WriteBits(arr, offset + 51, 13, fragOffset);
        BitHelper.Write8(arr, offset + 8, ttl);
        BitHelper.Write8(arr, offset + 9, protocol);
        BitHelper.Write16(arr, offset + 10, hdrChecksum);
        BitHelper.Write32(arr, offset + 12, srcAddr);
        BitHelper.Write32(arr, offset + 16, dstAddr);
      }
    }
    
    // NOTE in reality these would have been collated with core error by p4c
    public sealed class IPv4OptionsNotSupported : error { }
    public sealed class IPv4IncorrectVersion : error { }
    public sealed class IPv4ChecksumError : error { }
    
    public sealed class Parsed_packet : IStruct
    {
      public Ethernet_h ethernet;
      public IPv4_h ip;
    }

    public sealed class TopParser : IParser, very_simple_model.Parser<Parsed_packet> // NOTE this interface would actually be an external one, and found by searching for matches
    {
      // parserLocals
      private Ck16 ck;

      public void apply(packet_in b, out Parsed_packet p)
      {
        ck = new Ck16();  // FIXME how are we handling instantiation of externs?

        p = new Parsed_packet();
        start(b, p);
      }

      private void start(packet_in b, Parsed_packet p)
      {
        // NOTE type unknown and no type arg specified
        b.extract(out p.ethernet); // NOTE P4 doesn't specify direction of argument, only of parameter at declaration (i.e. not at call site)
        switch (p.ethernet.etherType.Value) // NOTE P4 doesn't specify value
        {
          case 0x0800: // InfInt
            parse_ipv4(b, p); // NOTE P4 just gives us parse_ipv4 - could it also give us more args?
            break;
          // no default rule: all other packets rejected FIXME do we need to generate code for this?
        }
      }

      private void parse_ipv4(packet_in b, Parsed_packet p)
      {
        b.extract(out p.ip);
        verify.apply(p.ip.version == 4, new IPv4IncorrectVersion()); // FIXME how are we handling errors? D: NOTE P4 gives it as error.IPV4IncorrectVersion
        verify.apply(p.ip.ihl == 5, new IPv4OptionsNotSupported());
        ck.clear();
        ck.update(p.ip);
        verify.apply(ck.get() == new bit16(0), new IPv4ChecksumError());
        // transition accept
      }
    }

    public sealed class TopPipe : IControl, very_simple_model.Pipe<Parsed_packet>
    {
      public sealed class Drop_action : IAction
      {
        public void apply(ref Parsed_packet headers, error parseError, InControl inCtrl, ref OutControl outCtrl) // NOTE these params are ALL from TopPipe, not the action itself...
        {
          outCtrl.outputPort = DROP_PORT;
        }
      }

      public sealed class Set_nhop : IAction
      {
        void apply(Parsed_packet headers, core.error parseError, InControl inCtrl, ref OutControl outCtrl, // NOTE out param outCtrl -> ref for the action
                   out IPv4Address nextHop,
                   IPv4Address ipv4_dest,
                   PortId port)
        {
          nextHop = ipv4_dest;
          headers.ip.ttl = new bit8((byte)(headers.ip.ttl.Value - 1));
          outCtrl.outputPort = port;
        }
      }

      ///////////////////////////////////////////////////////
      // GOT THIS FAR
      //////////////////////////////////////////////////////

      public sealed class ipv4_match : ITable
      {
        private Library.Table<IPv4Address> table = new Library.LpmTable<IPv4Address>();

        public enum ActionList
        {
          Drop_action,
          Set_nhop
        }
        public class A { } public class B { } public class C { }
        public class apply_result : Library.apply_result<ActionList>
        {
          public apply_result(bool hit, ActionList action) : base(hit, action) { }
        }
        private abstract class ActionBase
        {
          public ActionList Action { get; }
          protected readonly A inField1;
          public ActionBase(ActionList action, A inField1)
          {
            this.Action = action;
            this.inField1 = inField1;
          }
          public abstract apply_result apply(out C outField1);
          public sealed class Drop_action : ActionBase
          {
            private B directionlessField1;
            public Drop_action(A inField1, B directionlessField1) : base(ActionList.Drop_action, inField1)
            {
              this.directionlessField1 = directionlessField1;
            }
            public override apply_result apply(out C outField1)
            {
            }
          }
        }

        public int Size { get; } = 1024;
        public void DefaultAction { get; } = Drop_action;

        public void buildKey(Parsed_packet p)
        {
          return p.ip.dstAddr;
        }

        public void apply(ref Parsed_packet headers, error parseError, InControl inCtrl, out OutControl outCtrl)
        {
          // FIXME sort out copy-semantics for args

          apply_result result;

          var lookupKey = buildKey(args);
          ActionBase RA = table.Lookup(lookupKey);
          if (RA == null)
          {
            result.hit = false;
            RA = DefaultAction;
          }
          else
          {
            result.hit = true;
          }

        }
      }
     /**
      * Computes address of next IPv4 hop and output port
      * based on the IPv4 destination of the current packet.
      * Decrements packet IPv4 TTL.
      * @param nextHop IPv4 address of next hop
      */
     table ipv4_match(out IPv4Address nextHop) {
        key = { headers.ip.dstAddr: lpm; }  // longest-prefix match
        actions = {
          Drop_action;
          Set_nhop(nextHop);
        }

        size = 1024;
        default_action = Drop_action;
      }

      /**
       * Send the packet to the CPU port
       */
      action Send_to_cpu()
          { outCtrl.outputPort = CPU_OUT_PORT; }

      /**
       * Check packet TTL and send to CPU if expired.
       */
      table check_ttl() {
        key = { headers.ip.ttl: exact; }
        actions = { Send_to_cpu; NoAction; }
        const default_action = NoAction; // defined in core.p4
      }

      /**
       * Set the destination MAC address of the packet
       * @param dmac destination MAC address.
       */
      action Set_dmac(EthernetAddress dmac)
      { headers.ethernet.dstAddr = dmac; }
      /**
       * Set the destination Ethernet address of the packet
       * based on the next hop IP address.
       * @param nextHop IPv4 address of next hop.
       */
      table dmac(in IPv4Address nextHop) {
        key = { nextHop: exact; }
        actions = {
          Drop_action;
          Set_dmac;
        }
        size = 1024;
        default_action = Drop_action;
      }

      /**
       * Set the source MAC address.
       * @param smac: source MAC address to use
       */
      action Set_smac(EthernetAddress smac)
       { headers.ethernet.srcAddr = smac; }

      /**
       * Set the source mac address based on the output port.
       */
      table smac() {
        key = { outCtrl.outputPort: exact; }
        actions = {
          Drop_action
          Set_smac;
        }
        size = 16;
        default_action = Drop_action;
      }


      public void apply(ref Parsed_packet headers,
                        core.error parseError, // parser error
                        InControl inCtrl, // input port
                        out OutControl outCtrl)
      {
        outCtrl = new OutControl();

        IPv4Address nextHop; // temporary variable

        if (parseError != error.NoError)
        {
          Drop_action(headers, parseError, inCtrl, outCtrl);  // invoke drop directly
          return;
        }

        ipv4_match.Apply(headers, parseError, inCtrl, outCtrl, out nextHop); // Match result will go into nextHop
        if (outCtrl.outputPort == DROP_PORT) return;

        check_ttl.Apply(headers, parseError, inCtrl, outCtrl);
        if (outCtrl.outputPort == CPU_OUT_PORT) return;

        dmac.Apply(headers, parseError, inCtrl, outCtrl, nextHop);
        if (outCtrl.outputPort == DROP_PORT) return;

        smac.Apply(headers, parseError, inCtrl, outCtrl);
      }
    }
  }
}
