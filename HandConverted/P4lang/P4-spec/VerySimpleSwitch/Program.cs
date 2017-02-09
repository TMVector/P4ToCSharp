using HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static HandConverted.Core;
using static HandConverted.P4lang.P4_spec.VerySimpleSwitch.Architecture;

using EthernetAddress = System.UInt64;//HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library.BitString; // FIXME no size connotations...
using IPv4Address = System.UInt32;


namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch
{
  public static class Program
  {
    // standard Ethernet header
    sealed class Ethernet_h : HeaderBase
    {
      public readonly uint length = 12;

      public EthernetAddress dstAddr;     // width 48
      public EthernetAddress srcAddr;     // width 48
      public ushort etherType;  // width 16

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

    // IPv4 header without options
    sealed class IPv4_h : HeaderBase
    {
      public uint offset;

      public byte version;          // width 4
      public byte ihl;              // width 4
      public byte diffserv;         // width 8
      public ushort totalLen;       // width 16
      public ushort identification; // width 16
      public byte flags;            // width 3
      public ushort fragOffset;     // width 13
      public byte ttl;              // width 8
      public byte protocol;         // width 8
      public ushort hdrChecksum;    // width 16
      public IPv4Address srcAddr;          // width 32
      public IPv4Address dstAddr;          // width 32

      public uint length { get { return ((uint)ihl & 0xF) * 4; } }
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
        BitHelper.WriteBits(arr, offset * 8, 4, version);
        BitHelper.WriteBits(arr, offset * 8 + 4, 4, ihl);
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

    // Declare user-defined errors that may be signaled during parsing
    public sealed class IPv4OptionsNotSupported : Error { }
    public sealed class IPv4IncorrectVersion : Error { }
    public sealed class IPv4ChecksumError : Error { }

    // List of all recognized headers
    sealed class Parsed_packet
    {
      public Ethernet_h ethernet;
      public IPv4_h ip;
    }

    sealed class TopParser : Architecture.Parser<Parsed_packet>
    {
      private Error error = 0;
      private Checksum16 ck;

      public void Apply(Packet_in b, out Parsed_packet p)
      {
        ck = new Checksum16();  // instantiate checksum unit

        p = new Parsed_packet();
        Start(b, p);
      }

      private void Start(Packet_in b, Parsed_packet p)
      {
        b.Extract(out p.ethernet);
        switch (p.ethernet.etherType)
        {
          case 0x0800:
            Parse_ipv4(b, p);
            break;
          // no default rule: all other packets rejected
        }
      }

      private void Parse_ipv4(Packet_in b, Parsed_packet p)
      {
        b.Extract(out p.ip);
        if (p.ip.version == 4)
        {
          error = Error.IPv4IncorrectVersion;
          return;
        }
        if (p.ip.ihl == 5)
        {
          error = Error.IPv4OptionsNotSupported;
          return;
        }
        ck.Clear();
        ck.Update(p.ip);
        // Verify that packet checksum is zero
        if (ck.Get() == 0)
        {
          error = Error.IPv4ChecksumError;
          return;
        }
        // Accept
      }
    }

    // match-action pipeline section
    sealed class TopPipe : Architecture.Pipe<Parsed_packet>
    {
      /**
       * Indicates that a packet is dropped by setting the
       * output port to the DROP_PORT
       */
      void Drop_action(Parsed_packet headers, Core.Error parseError, InControl inCtrl, OutControl outCtrl)
      {
        outCtrl.outputPort = DROP_PORT;
      }

      /**
       * Set the next hop and the output port.
       * Decrements ipv4 ttl field.
       * @param ivp4_dest ipv4 address of next hop
       * @param port output port
       */
      void Set_nhop(Parsed_packet headers, Core.Error parseError, InControl inCtrl, OutControl outCtrl,
                    out IPv4Address nextHop,
                    IPv4Address ipv4_dest,
                    PortId_t port)
      {
        nextHop = ipv4_dest;
        headers.ip.ttl = (byte)(headers.ip.ttl - 1);
        outCtrl.outputPort = port;
      }

      sealed class Ipv4_match : ITable
      {
        private Library.LpmTable<IPv4Address> table = new Library.LpmTable<IPv4Address>();

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

        public void Apply(Parsed_packet headers, Core.Error parseError, InControl inCtrl, OutControl outCtrl, out IPv4Address nextHop)
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


      public void Apply(ref Parsed_packet headers,
                        Core.Error parseError, // parser error
                        InControl inCtrl, // input port
                        out OutControl outCtrl)
      {
        outCtrl = new OutControl();

        IPv4Address nextHop; // temporary variable

        if (parseError != Error.NoError)
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
