using System;
using P4ToCSharp.Library;
using static Architecture;
using PortId = P4ToCSharp.Library.bit4;
using EthernetAddress = P4ToCSharp.Library.bit48;
using IPv4Address = P4ToCSharp.Library.bit32;

public class Program
{
    public class error : Architecture.error
    {
        public static readonly error NoError = new error();
        public static readonly error PacketTooShort = new error();
        public static readonly error NoMatch = new error();
        public static readonly error StackOutOfBounds = new error();
        public static readonly error OverwritingHeader = new error();
        public static readonly error HeaderTooShort = new error();
        public static readonly error ParserTimeout = new error();
        public static readonly error IPv4OptionsNotSupported = new error();
        public static readonly error IPv4IncorrectVersion = new error();
        public static readonly error IPv4ChecksumError = new error();

        protected error()
        {
        }
    }

    static void verify(bool condition, P4ToCSharp.Library.error err)
    {
        Bootstrapper.VSSModel.verify(condition, err);
    }

    static void NoAction()
    {
    }

    public static readonly PortId REAL_PORT_COUNT = (PortId)8;
    public static readonly PortId RECIRCULATE_IN_PORT = (PortId)0xD;
    public static readonly PortId CPU_IN_PORT = (PortId)0xE;
    public static readonly PortId DROP_PORT = (PortId)0xF;
    public static readonly PortId CPU_OUT_PORT = (PortId)0xE;
    public static readonly PortId RECIRCULATE_OUT_PORT = (PortId)0xD;

    public sealed class Ethernet_h : HeaderBase
    {
        public EthernetAddress dstAddr;
        public EthernetAddress srcAddr;
        public bit16 etherType;

        public override void Parse(byte[] data, uint offset)
        {
            offset *= 8;
            dstAddr = BitHelper.Extract48(data, offset + 0);
            srcAddr = BitHelper.Extract48(data, offset + 48);
            etherType = BitHelper.Extract16(data, offset + 96);
        }

        public override void Deparse(byte[] data, uint offset)
        {
            offset *= 8;
            BitHelper.Write48(data, offset + 0, dstAddr);
            BitHelper.Write48(data, offset + 48, srcAddr);
            BitHelper.Write16(data, offset + 96, etherType);
        }
    }

    public sealed class Ipv4_h : HeaderBase
    {
        public bit4 version;
        public bit4 ihl;
        public bit8 diffserv;
        public bit16 totalLen;
        public bit16 identification;
        public bitN flags;
        public bitN fragOffset;
        public bit8 ttl;
        public bit8 protocol;
        public bit16 hdrChecksum;
        public IPv4Address srcAddr;
        public IPv4Address dstAddr;

        public override void Parse(byte[] data, uint offset)
        {
            offset *= 8;
            version = BitHelper.Extract4(data, offset + 0);
            ihl = BitHelper.Extract4(data, offset + 4);
            diffserv = BitHelper.Extract8(data, offset + 8);
            totalLen = BitHelper.Extract16(data, offset + 16);
            identification = BitHelper.Extract16(data, offset + 32);
            flags = BitHelper.ExtractN(data, offset + 48, 3);
            fragOffset = BitHelper.ExtractN(data, offset + 51, 13);
            ttl = BitHelper.Extract8(data, offset + 64);
            protocol = BitHelper.Extract8(data, offset + 72);
            hdrChecksum = BitHelper.Extract16(data, offset + 80);
            srcAddr = BitHelper.Extract32(data, offset + 96);
            dstAddr = BitHelper.Extract32(data, offset + 128);
        }

        public override void Deparse(byte[] data, uint offset)
        {
            offset *= 8;
            BitHelper.Write4(data, offset + 0, version);
            BitHelper.Write4(data, offset + 4, ihl);
            BitHelper.Write8(data, offset + 8, diffserv);
            BitHelper.Write16(data, offset + 16, totalLen);
            BitHelper.Write16(data, offset + 32, identification);
            BitHelper.WriteN(data, offset + 48, flags);
            BitHelper.WriteN(data, offset + 51, fragOffset);
            BitHelper.Write8(data, offset + 64, ttl);
            BitHelper.Write8(data, offset + 72, protocol);
            BitHelper.Write16(data, offset + 80, hdrChecksum);
            BitHelper.Write32(data, offset + 96, srcAddr);
            BitHelper.Write32(data, offset + 128, dstAddr);
        }
    }

    public sealed class Parsed_packet : IStruct
    {
        public Ethernet_h ethernet;
        public Ipv4_h ip;
    }

    sealed class TopParser : Architecture.Parser<Parsed_packet>
    {
        Ck16 ck;

        public TopParser()
        {
            ck = new Bootstrapper.VSSModel.Ck16_impl();
        }

        public void apply(packet_in b, out Parsed_packet p)
        {
            p = new Parsed_packet();
            start(b, p);
        }

        void start(packet_in b, Parsed_packet p)
        {
            b.extract<Ethernet_h>(out p.ethernet);
            switch ((System.UInt16)p.ethernet.etherType)
            {
                case 0x800:
                    parse_ipv4(b, p);
                    break;
            }
        }

        void parse_ipv4(packet_in b, Parsed_packet p)
        {
            b.extract<Ipv4_h>(out p.ip);
            Bootstrapper.VSSModel.verify(p.ip.version == 4, error.IPv4IncorrectVersion);
            Bootstrapper.VSSModel.verify(p.ip.ihl == 5, error.IPv4OptionsNotSupported);
            ck.clear();
            ck.update<Ipv4_h>(p.ip);
            Bootstrapper.VSSModel.verify(ck.get() == 0, error.IPv4ChecksumError);
            accept(b, p);
        }

        void accept(packet_in b, Parsed_packet p)
        {
        }

        void reject(packet_in b, Parsed_packet p)
        {
        }
    }

    sealed class TopPipe : Architecture.Pipe<Parsed_packet>
    {
        class TopPipe_Args
        {
            public Parsed_packet headers { get; set; }
            public P4ToCSharp.Library.error parseError { get; }
            public InControl inCtrl { get; }
            public OutControl outCtrl { get; set; }

            public TopPipe_Args(Parsed_packet headers, P4ToCSharp.Library.error parseError, InControl inCtrl, OutControl outCtrl)
            {
                this.headers = headers;
                this.parseError = parseError;
                this.inCtrl = inCtrl;
                this.outCtrl = outCtrl;
            }
        }

        public TopPipe()
        {
        }

        public void apply(Parsed_packet headers_capture, ref Parsed_packet headers, P4ToCSharp.Library.error parseError, InControl inCtrl, out OutControl outCtrl)
        {
            outCtrl = new OutControl();
            headers = headers_capture;
            TopPipe_Args TopPipe_Args = new TopPipe_Args(headers, parseError, inCtrl, outCtrl);
            IPv4Address nextHop;
            if (TopPipe_Args.parseError != error.NoError)
            {
                Drop_action(TopPipe_Args);
                return;
            }

            ipv4_match.apply(TopPipe_Args, out nextHop);
            if (TopPipe_Args.outCtrl.outputPort == DROP_PORT)
                return;
            check_ttl.apply(TopPipe_Args);
            if (TopPipe_Args.outCtrl.outputPort == CPU_OUT_PORT)
                return;
            dmac.apply(TopPipe_Args, nextHop);
            if (TopPipe_Args.outCtrl.outputPort == DROP_PORT)
                return;
            smac.apply(TopPipe_Args);
        }

        static void Drop_action(TopPipe_Args TopPipe_Args)
        {
            TopPipe_Args.outCtrl.outputPort = DROP_PORT;
        }

        static void Set_nhop(TopPipe_Args TopPipe_Args, out IPv4Address nextHop, IPv4Address ipv4_dest, PortId port)
        {
            nextHop = new IPv4Address();
            nextHop = ipv4_dest;
            TopPipe_Args.headers.ip.ttl = TopPipe_Args.headers.ip.ttl - 1;
            TopPipe_Args.outCtrl.outputPort = port;
        }

        ipv4_match_t ipv4_match = new ipv4_match_t();

        private sealed class ipv4_match_t
        {
            LpmTable<bit32, ActionBase> lookup = new LpmTable<bit32, ActionBase>();

            public apply_result apply(TopPipe_Args TopPipe_Args, out IPv4Address nextHop)
            {
                nextHop = new IPv4Address();
                apply_result result;
                ActionBase RA = lookup?[TopPipe_Args.headers.ip.dstAddr];
                if (RA == null)
                    result = new apply_result(false, default_action.Action);
                else
                    result = new apply_result(true, RA.Action);
                RA.OnApply(TopPipe_Args, out nextHop);
                return result;
            }

            public enum action_list
            {
                Drop_actionTopPipe_Args,
                Set_nhopTopPipe_ArgsoutnextHop
            }

            public sealed class apply_result : apply_result<action_list>
            {
                public apply_result(bool hit, action_list action_run) : base(hit, action_run)
                {
                }
            }

            private abstract class ActionBase
            {
                public action_list Action { get; }

                public ActionBase(action_list action)
                {
                    this.Action = action;
                }

                public abstract void OnApply(TopPipe_Args TopPipe_Args, out IPv4Address nextHop);

                public sealed class Drop_actionTopPipe_Args_Action : ActionBase
                {
                    public Drop_actionTopPipe_Args_Action() : base(action_list.Drop_actionTopPipe_Args)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args, out IPv4Address nextHop)
                    {
                        nextHop = new IPv4Address();
                        Drop_action(TopPipe_Args);
                    }
                }

                public sealed class Set_nhopTopPipe_ArgsoutnextHop_Action : ActionBase
                {
                    readonly IPv4Address ipv4_dest;
                    readonly PortId port;

                    public Set_nhopTopPipe_ArgsoutnextHop_Action(IPv4Address ipv4_dest, PortId port) : base(action_list.Set_nhopTopPipe_ArgsoutnextHop)
                    {
                        this.ipv4_dest = ipv4_dest;
                        this.port = port;
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args, out IPv4Address nextHop)
                    {
                        nextHop = new IPv4Address();
                        Set_nhop(TopPipe_Args, out nextHop, ipv4_dest, port);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Drop_actionTopPipe_Args_Action();
        }

        static void Send_to_cpu(TopPipe_Args TopPipe_Args)
        {
            TopPipe_Args.outCtrl.outputPort = CPU_OUT_PORT;
        }

        check_ttl_t check_ttl = new check_ttl_t();

        private sealed class check_ttl_t
        {
            ExactTable<bit8, ActionBase> lookup = new ExactTable<bit8, ActionBase>();

            public apply_result apply(TopPipe_Args TopPipe_Args)
            {
                apply_result result;
                ActionBase RA = lookup?[TopPipe_Args.headers.ip.ttl];
                if (RA == null)
                    result = new apply_result(false, default_action.Action);
                else
                    result = new apply_result(true, RA.Action);
                RA.OnApply(TopPipe_Args);
                return result;
            }

            public enum action_list
            {
                Send_to_cpuTopPipe_Args,
                NoAction
            }

            public sealed class apply_result : apply_result<action_list>
            {
                public apply_result(bool hit, action_list action_run) : base(hit, action_run)
                {
                }
            }

            private abstract class ActionBase
            {
                public action_list Action { get; }

                public ActionBase(action_list action)
                {
                    this.Action = action;
                }

                public abstract void OnApply(TopPipe_Args TopPipe_Args);

                public sealed class Send_to_cpuTopPipe_Args_Action : ActionBase
                {
                    public Send_to_cpuTopPipe_Args_Action() : base(action_list.Send_to_cpuTopPipe_Args)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        Send_to_cpu(TopPipe_Args);
                    }
                }

                public sealed class NoAction_Action : ActionBase
                {
                    public NoAction_Action() : base(action_list.NoAction)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        NoAction();
                    }
                }
            }

            private ActionBase default_action = new ActionBase.NoAction_Action();
        }

        static void Set_dmac(TopPipe_Args TopPipe_Args, EthernetAddress dmac)
        {
            TopPipe_Args.headers.ethernet.dstAddr = dmac;
        }

        dmac_t dmac = new dmac_t();

        private sealed class dmac_t
        {
            ExactTable<bit32, ActionBase> lookup = new ExactTable<bit32, ActionBase>();

            public apply_result apply(TopPipe_Args TopPipe_Args, IPv4Address nextHop)
            {
                apply_result result;
                ActionBase RA = lookup?[nextHop];
                if (RA == null)
                    result = new apply_result(false, default_action.Action);
                else
                    result = new apply_result(true, RA.Action);
                RA.OnApply(TopPipe_Args, nextHop);
                return result;
            }

            public enum action_list
            {
                Drop_actionTopPipe_Args,
                Set_dmacTopPipe_Args
            }

            public sealed class apply_result : apply_result<action_list>
            {
                public apply_result(bool hit, action_list action_run) : base(hit, action_run)
                {
                }
            }

            private abstract class ActionBase
            {
                public action_list Action { get; }

                public ActionBase(action_list action)
                {
                    this.Action = action;
                }

                public abstract void OnApply(TopPipe_Args TopPipe_Args, IPv4Address nextHop);

                public sealed class Drop_actionTopPipe_Args_Action : ActionBase
                {
                    public Drop_actionTopPipe_Args_Action() : base(action_list.Drop_actionTopPipe_Args)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args, IPv4Address nextHop)
                    {
                        Drop_action(TopPipe_Args);
                    }
                }

                public sealed class Set_dmacTopPipe_Args_Action : ActionBase
                {
                    readonly EthernetAddress dmac;

                    public Set_dmacTopPipe_Args_Action(EthernetAddress dmac) : base(action_list.Set_dmacTopPipe_Args)
                    {
                        this.dmac = dmac;
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args, IPv4Address nextHop)
                    {
                        Set_dmac(TopPipe_Args, dmac);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Drop_actionTopPipe_Args_Action();
        }

        static void Set_smac(TopPipe_Args TopPipe_Args, EthernetAddress smac)
        {
            TopPipe_Args.headers.ethernet.srcAddr = smac;
        }

        smac_t smac = new smac_t();

        private sealed class smac_t
        {
            ExactTable<bit4, ActionBase> lookup = new ExactTable<bit4, ActionBase>();

            public apply_result apply(TopPipe_Args TopPipe_Args)
            {
                apply_result result;
                ActionBase RA = lookup?[TopPipe_Args.outCtrl.outputPort];
                if (RA == null)
                    result = new apply_result(false, default_action.Action);
                else
                    result = new apply_result(true, RA.Action);
                RA.OnApply(TopPipe_Args);
                return result;
            }

            public enum action_list
            {
                Drop_actionTopPipe_Args,
                Set_smacTopPipe_Args
            }

            public sealed class apply_result : apply_result<action_list>
            {
                public apply_result(bool hit, action_list action_run) : base(hit, action_run)
                {
                }
            }

            private abstract class ActionBase
            {
                public action_list Action { get; }

                public ActionBase(action_list action)
                {
                    this.Action = action;
                }

                public abstract void OnApply(TopPipe_Args TopPipe_Args);

                public sealed class Drop_actionTopPipe_Args_Action : ActionBase
                {
                    public Drop_actionTopPipe_Args_Action() : base(action_list.Drop_actionTopPipe_Args)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        Drop_action(TopPipe_Args);
                    }
                }

                public sealed class Set_smacTopPipe_Args_Action : ActionBase
                {
                    readonly EthernetAddress smac;

                    public Set_smacTopPipe_Args_Action(EthernetAddress smac) : base(action_list.Set_smacTopPipe_Args)
                    {
                        this.smac = smac;
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        Set_smac(TopPipe_Args, smac);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Drop_actionTopPipe_Args_Action();
        }
    }

    sealed class TopDeparser : Architecture.Deparser<Parsed_packet>
    {
        class TopDeparser_Args
        {
            public Parsed_packet p { get; set; }
            public packet_out b { get; }

            public TopDeparser_Args(Parsed_packet p, packet_out b)
            {
                this.p = p;
                this.b = b;
            }
        }

        public TopDeparser()
        {
        }

        public void apply(Parsed_packet p_capture, ref Parsed_packet p, packet_out b)
        {
            p = p_capture;
            TopDeparser_Args TopDeparser_Args = new TopDeparser_Args(p, b);
            TopDeparser_Args.b.emit<Ethernet_h>(TopDeparser_Args.p.ethernet);
            if (TopDeparser_Args.p.ip.isValid())
            {
                ck.clear();
                TopDeparser_Args.p.ip.hdrChecksum = 0;
                ck.update<Ipv4_h>(TopDeparser_Args.p.ip);
                TopDeparser_Args.p.ip.hdrChecksum = ck.get();
            }

            TopDeparser_Args.b.emit<Ipv4_h>(TopDeparser_Args.p.ip);
        }

        Ck16 ck = new Bootstrapper.VSSModel.Ck16_impl();
    }

    public static void Main()
    {
        new Bootstrapper.VSSModel.VSS_impl<Parsed_packet>().Use(new TopParser(), new TopPipe(), new TopDeparser());
    }
}