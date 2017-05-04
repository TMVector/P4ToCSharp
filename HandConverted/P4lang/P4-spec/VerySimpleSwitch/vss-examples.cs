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
  public sealed class Program
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
        version = (bit4)BitHelper.ExtractBits(arr, offset * 8, 4);
        ihl = (bit4)BitHelper.ExtractBits(arr, offset * 8 + 4, 4);
        diffserv = BitHelper.Extract8(arr, offset + 1);
        totalLen = BitHelper.Extract16(arr, offset + 2);
        identification = BitHelper.Extract16(arr, offset + 4);
        flags = (bit3)BitHelper.ExtractBits(arr, offset + 48, 3);
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

      public void apply(packet_in b, out Parsed_packet p, out error parseError)
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
            return parse_ipv4(b, p); // NOTE P4 just gives us parse_ipv4 - could it also give us more args?
            break;
          default: parseError = error.NoMatch; // no default rule: all other packets rejected FIXME do we need to generate code for this?
        }
      }

      private void parse_ipv4(packet_in b, Parsed_packet p)
      {
        b.extract(out p.ip);
        verify.apply(p.ip.version == (bit4)4, new IPv4IncorrectVersion()); // FIXME how are we handling errors? D: NOTE P4 gives it as error.IPV4IncorrectVersion
        verify.apply(p.ip.ihl == (bit4)5, new IPv4OptionsNotSupported());
        ck.clear();
        ck.update(p.ip);
        verify.apply(ck.get() == (bit16)0, new IPv4ChecksumError());
        // transition accept
      }
    }

    public sealed class TopPipe : IControl, very_simple_model.Pipe<Parsed_packet>
    {
      // You can kind of think of this like a closure for the apply method
      private sealed class TopPipe_Args
      {
        public Parsed_packet headers { get; set; } // inout
        public error parseError { get; }
        public InControl inCtrl { get; }
        public OutControl outCtrl { get; set; } // out

        public TopPipe_Args(Parsed_packet headers, core.error parseError, InControl inCtrl, OutControl outCtrl)
        {
          this.headers = headers;
          this.parseError = parseError;
          this.inCtrl = inCtrl;
          this.outCtrl = outCtrl;
        }
      }

      //action Drop_action()
      //{ outCtrl.outputPort = DROP_PORT; }
      private static void Drop_action(TopPipe_Args TopPipe_Args) // NOTE these params are ALL from TopPipe, not the action itself...
      {
        TopPipe_Args.outCtrl.outputPort = DROP_PORT; // NOTE will need to add `arg.` to relevant identifiers
      }

      //action Set_nhop(out IPv4Address nextHop, // NOTE directioned parameters must be bound in the actions list specification (so not by api?)
      //                IPv4Address ipv4_dest, // NOTE directionless parameters bound by control plane
      //                PortId port)
      //{
      //  nextHop = ipv4_dest;
      //  headers.ip.ttl = headers.ip.ttl - 1;
      //  outCtrl.outputPort = port;
      //}
      private static void Set_nhop(TopPipe_Args args,
                        ref IPv4Address nextHop,
                        IPv4Address ipv4_dest,
                        PortId port)
      {
        nextHop = ipv4_dest;
        args.headers.ip.ttl = new bit8((byte)(args.headers.ip.ttl.Value - 1));
        args.outCtrl.outputPort = port;
      }

      ///////////////////////////////////////////////////////
      // GOT THIS FAR
      //////////////////////////////////////////////////////

      //table ipv4_match(out IPv4Address nextHop)
      //{
      //  key = { headers.ip.dstAddr : lpm; }
      //  actions = {
      //    Drop_action;
      //    Set_nhop(nextHop); // NOTE arguments specified in action list are not evaluated until the action's execution
      //  }
      //  size = 1024;
      //  default_action = Drop_action; // NOTE initial value. Can also be declared const.
      //  // NOTE must it must be *exactly* identical inc. params, but directionless params must be specified
      //}
      private sealed class ipv4_match_t : ITable
      {
        // If there were multiple fields, then lookup would return table for lookup of next field (could combine same lookup types)
        private Library.LpmTable<IPv4Address, ActionBase> table = new Library.LpmTable<IPv4Address, ActionBase>();

        public enum action_list
        {
          Drop_action,
          Set_nhop
        }
        public sealed class apply_result : Library.apply_result<action_list>
        {
          public apply_result(bool hit, action_list action_run) : base(hit, action_run) { }
        }
        // This class is what is returned from the final table lookup. So directionless (control plane) args should be bound as instance fields, and directioned args should be passed in OnApply
        private abstract class ActionBase
        {
          public action_list Action { get; }
          public ActionBase(action_list action)
          {
            this.Action = action;
          }
          public abstract void OnApply(TopPipe_Args args, ref IPv4Address nextHop);
          public sealed class Drop_action_Action : ActionBase // NOTE this class is different from Drop_action because it holds any parameters set by API/action-list
          {
            public Drop_action_Action() : base(action_list.Drop_action)
            {
            }
            public override void OnApply(TopPipe_Args args, ref IPv4Address nextHop)
            {
              // FIXME this action doesn't actually set the out param - how do we handle that?
              Drop_action(args);
            }
          }
          public sealed class Set_nhop_Action : ActionBase
          {
            readonly IPv4Address ipv4_dest;
            readonly PortId port;
            public Set_nhop_Action(IPv4Address ipv4_dest, PortId port) : base(action_list.Set_nhop)
            {
              this.ipv4_dest = ipv4_dest;
              this.port = port;
            }
            public override void OnApply(TopPipe_Args args, ref IPv4Address nextHop) // FIXME need to bind parameters from control plane...
            {
              Set_nhop(args, ref nextHop, ipv4_dest, port);
            }
          }
        }

        private int size { get; } = 1024;
        private ActionBase default_action { get; } = new ActionBase.Drop_action_Action();

        public apply_result apply(TopPipe_Args args, ref IPv4Address nextHop)
        {
          // FIXME sort out copy-semantics for args

          apply_result result;

          // Would chain lookups here if multiple fields
          ActionBase RA = table.Lookup(args.headers.ip.dstAddr);
          if (RA == null)
          {
            result = new apply_result(false, default_action.Action);
          }
          else
          {
            result = new apply_result(true, RA.Action);
          }
          //evaluate_and_copy_in_RA_args(RA);
          //execute(RA);
          //copy_out_RA_args(RA);
          //copy_out_table_args(args);
          RA.OnApply(args, ref nextHop);
          return result;
        }
      }
      private ipv4_match_t ipv4_match { get; } = new ipv4_match_t();

      //action Send_to_cpu()
      //    { outCtrl.outputPort = CPU_OUT_PORT; }
      private static void Send_to_cpu(TopPipe_Args args)
      {
        args.outCtrl.outputPort = CPU_OUT_PORT;
      }

      //table check_ttl() {
      //  key = { headers.ip.ttl: exact; }
      //  actions = { Send_to_cpu; NoAction; }
      //  const default_action = NoAction; // defined in core.p4
      //}
      private sealed class check_ttl_t
      {
        private Library.ExactTable<bit8, ActionBase> table = new Library.ExactTable<bit8, ActionBase>();

        public enum action_list
        {
          Send_to_cpu,
          NoAction
        }
        public sealed class apply_result : Library.apply_result<action_list>
        {
          public apply_result(bool hit, action_list action_run) : base(hit, action_run) { }
        }
        public abstract class ActionBase
        {
          public action_list Action { get; }
          public ActionBase(action_list action)
          {
            this.Action = action;
          }
          public abstract void OnApply(TopPipe_Args args);
          public sealed class Send_to_cpu_Action : ActionBase
          {
            public Send_to_cpu_Action() : base(action_list.Send_to_cpu) { }
            public override void OnApply(TopPipe_Args args)
            {
              Send_to_cpu(args);
            }
          }
          public sealed class NoAction_Action : ActionBase
          {
            public NoAction_Action() : base(action_list.NoAction) { }
            public override void OnApply(TopPipe_Args args)
            {
              NoAction(); // NOTE external action, so doesn't take TopPipe `args`
            }
          }
        }

        private ActionBase default_action { get; } = new ActionBase.NoAction_Action();

        public apply_result apply(TopPipe_Args args)
        {
          apply_result result;

          ActionBase RA = table.Lookup(args.headers.ip.ttl);
          if (RA == null)
          {
            result = new apply_result(false, default_action.Action);
          }
          else
          {
            result = new apply_result(true, RA.Action);
          }
          //evaluate_and_copy_in_RA_args(RA);
          //execute(RA);
          //copy_out_RA_args(RA);
          //copy_out_table_args(args);
          RA.OnApply(args);
          return result;
        }
      }
      private check_ttl_t check_ttl = new check_ttl_t();

      //action Set_dmac(EthernetAddress dmac)
      //{ headers.ethernet.dstAddr = dmac; }
      private static void Set_dmac(TopPipe_Args args, EthernetAddress dmac)
      {
        args.headers.ethernet.dstAddr = dmac;
      }

      //table dmac(in IPv4Address nextHop) {
      //  key = { nextHop: exact; }
      //  actions = {
      //    Drop_action;
      //    Set_dmac;
      //  }
      //  size = 1024;
      //  default_action = Drop_action;
      //}
      private sealed class dmac_t
      {
        private Library.ExactTable<IPv4Address, ActionBase> table = new Library.ExactTable<IPv4Address, ActionBase>();

        public enum action_list
        {
          Drop_action,
          Set_dmac
        }
        public sealed class apply_result : Library.apply_result<action_list>
        {
          public apply_result(bool hit, action_list action_run) : base(hit, action_run) { }
        }
        public abstract class ActionBase
        {
          public action_list Action { get; }
          public ActionBase(action_list action)
          {
            this.Action = action;
          }
          public abstract void OnApply(TopPipe_Args args);
          public sealed class Drop_action_Action : ActionBase
          {
            public Drop_action_Action() : base(action_list.Drop_action) { }
            public override void OnApply(TopPipe_Args args)
            {
              Drop_action(args);
            }
          }
          public sealed class Set_dmac_Action : ActionBase
          {
            readonly EthernetAddress dmac;
            public Set_dmac_Action(EthernetAddress dmac) : base(action_list.Set_dmac)
            {
              this.dmac = dmac;
            }
            public override void OnApply(TopPipe_Args args)
            {
              Set_dmac(args, dmac);
            }
          }
        }

        private int size { get; } = 1024;
        private ActionBase default_action { get; } = new ActionBase.Drop_action_Action();

        public apply_result apply(TopPipe_Args args, IPv4Address nextHop)
        {
          apply_result result;

          ActionBase RA = table.Lookup(nextHop);
          if (RA == null)
          {
            result = new apply_result(false, default_action.Action);
          }
          else
          {
            result = new apply_result(true, RA.Action);
          }
          //evaluate_and_copy_in_RA_args(RA);
          //execute(RA);
          //copy_out_RA_args(RA);
          //copy_out_table_args(args);
          RA.OnApply(args);
          return result;
        }
      }
      private dmac_t dmac = new dmac_t();

      //action Set_smac(EthernetAddress smac)
      // { headers.ethernet.srcAddr = smac; }
      private static void Set_smac(TopPipe_Args args, EthernetAddress smac)
      {
        args.headers.ethernet.srcAddr = smac;
      }

      //table smac() {
      //  key = { outCtrl.outputPort: exact; }
      //  actions = {
      //    Drop_action
      //    Set_smac;
      //  }
      //  size = 16;
      //  default_action = Drop_action;
      //}
      private sealed class smac_t
      {
        private Library.ExactTable<PortId, ActionBase> table = new Library.ExactTable<PortId, ActionBase>();

        public enum action_list
        {
          Drop_action,
          Set_smac
        }
        public sealed class apply_result : Library.apply_result<action_list>
        {
          public apply_result(bool hit, action_list action_run) : base(hit, action_run) { }
        }
        public abstract class ActionBase
        {
          public action_list Action { get; }
          public ActionBase(action_list action)
          {
            this.Action = action;
          }
          public abstract void OnApply(TopPipe_Args args);
          public sealed class Drop_action_Action : ActionBase
          {
            public Drop_action_Action() : base(action_list.Drop_action) { }
            public override void OnApply(TopPipe_Args args)
            {
              Drop_action(args);
            }
          }
          public sealed class Set_smac_Action : ActionBase
          {
            readonly EthernetAddress smac;
            public Set_smac_Action(EthernetAddress smac) : base(action_list.Set_smac)
            {
              this.smac = smac;
            }
            public override void OnApply(TopPipe_Args args)
            {
              Set_smac(args, smac);
            }
          }
        }

        private int size { get; } = 16;
        private ActionBase default_action { get; } = new ActionBase.Drop_action_Action();

        public apply_result apply(TopPipe_Args args)
        {
          apply_result result;

          ActionBase RA = table.Lookup(args.outCtrl.outputPort);
          if (RA == null)
          {
            result = new apply_result(false, default_action.Action);
          }
          else
          {
            result = new apply_result(true, RA.Action);
          }
          //evaluate_and_copy_in_RA_args(RA);
          //execute(RA);
          //copy_out_RA_args(RA);
          //copy_out_table_args(args);
          RA.OnApply(args);
          return result;
        }
      }
      private smac_t smac = new smac_t();


      public void apply(ref Parsed_packet headers,
                        core.error parseError, // parser error
                        InControl inCtrl, // input port
                        out OutControl outCtrl)
      {
        outCtrl = new OutControl(); // NOTE instantiate all out params to satisfy csc
        TopPipe_Args args = new TopPipe_Args(headers, parseError, inCtrl, outCtrl);

        IPv4Address nextHop = new IPv4Address(); // Have to instantiate ref params, more compact than using out params and instantiating at bottom

        if (parseError != error.NoError)
        {
          Drop_action(args);
          return;
        }

        ipv4_match.apply(args, ref nextHop);
        if (args.outCtrl.outputPort == DROP_PORT) return;

        check_ttl.apply(args);
        if (args.outCtrl.outputPort == CPU_OUT_PORT) return;

        dmac.apply(args, nextHop);
        if (args.outCtrl.outputPort == DROP_PORT) return;

        smac.apply(args);

        // Copy params back out
        headers = args.headers;
        outCtrl = args.outCtrl;
      }
    }

    public sealed class TopDeparser : IControl, very_simple_model.Deparser<Parsed_packet>
    {
      Ck16 ck = new Ck16();
      public void apply(ref Parsed_packet p, packet_out b)
      {
        b.emit(p.ethernet);
        if (p.ip.isValid())
        {
          ck.clear();
          p.ip.hdrChecksum = (bit16)0;
          ck.update(p.ip);
          p.ip.hdrChecksum = ck.get();
        }
        b.emit(p.ip);
      }
    }

    public sealed class Vss_Example : very_simple_model.VSS<Parsed_packet> // This inherits the PAX interface, and will be used as a packet processor
    {
      public Vss_Example() : base(new TopParser(), new TopPipe(), new TopDeparser())
      {
      }
    }
  }
}
