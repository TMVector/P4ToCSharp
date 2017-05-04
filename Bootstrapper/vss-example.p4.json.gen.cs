using P4ToCSharp.Library;
using packet_in = Bootstrapper.VSSModel.packet_in;
using packet_out = Bootstrapper.VSSModel.packet_out;
using PortId = P4ToCSharp.Library.bit4;
using Ck16 = Bootstrapper.VSSModel.Ck16;
using EthernetAddress = P4ToCSharp.Library.bit48;
using IPv4Address = P4ToCSharp.Library.bit32;

public class Program
{
    public enum error
    {
        NoError,
        PacketTooShort,
        NoMatch,
        StackOutOfBounds,
        OverwritingHeader,
        HeaderTooShort,
        ParserTimeout,
        IPv4OptionsNotSupported,
        IPv4IncorrectVersion,
        IPv4ChecksumError
    }

    public interface packet_in
    {
        void extract<T>(T hdr);
        void extract<T>(T variableSizeHeader, bit32 variableFieldSizeInBits);
        T lookahead<T>();
        void advance(bit32 sizeInBits);
        bit32 length();
    }

    public interface packet_out
    {
        void emit<T>(T hdr);
        void emit<T>(bool condition, T data);
    }

    static void verify(bool check, error toSignal)
    {
        Bootstrapper.VSSModel.verify(check, toSignal);
    }

    static void NoAction()
    {
    }

    enum MatchKind
    {
        exact,
        ternary,
        lpm
    }

    public static readonly PortId REAL_PORT_COUNT = (PortId)8;

    public sealed class InControl : IStruct
    {
        public PortId inputPort { get; set; }
    }

    public static readonly PortId RECIRCULATE_IN_PORT = (PortId)0xD;
    public static readonly PortId CPU_IN_PORT = (PortId)0xE;

    public sealed class OutControl : IStruct
    {
        public PortId outputPort { get; set; }
    }

    public static readonly PortId DROP_PORT = (PortId)0xF;
    public static readonly PortId CPU_OUT_PORT = (PortId)0xE;
    public static readonly PortId RECIRCULATE_OUT_PORT = (PortId)0xD;

    interface Parser<H> : IParser
    {
        void Apply(packet_in b, ref H parsedHeaders);
    }

    interface Pipe<H> : IControl
    {
        void Apply(ref H headers, error parseError, InControl inCtrl, ref OutControl outCtrl);
    }

    interface Deparser<H> : IControl
    {
        void Apply(ref H outputHeaders, packet_out b);
    }

    class VSS<H> : IPackage
    {
        Parser<H> p { get; }
        Pipe<H> map { get; }
        Deparser<H> d { get; }

        VSS(Parser<H> p, Pipe<H> map, Deparser<H> d)
        {
            this.p = p;
            this.map = map;
            this.d = d;
        }
    }

    public interface Ck16
    {
        void Ck16();
        void clear();
        void update<T>(T data);
        bit16 get();
    }

    public sealed class Ethernet_h : HeaderBase
    {
        public EthernetAddress dstAddr { get; set; }
        public EthernetAddress srcAddr { get; set; }
        public bit16 etherType { get; set; }

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
        public bit4 version { get; set; }
        public bit4 ihl { get; set; }
        public bit8 diffserv { get; set; }
        public bit16 totalLen { get; set; }
        public bit16 identification { get; set; }
        public bit3 flags { get; set; }
        public bit13 fragOffset { get; set; }
        public bit8 ttl { get; set; }
        public bit8 protocol { get; set; }
        public bit16 hdrChecksum { get; set; }
        public IPv4Address srcAddr { get; set; }
        public IPv4Address dstAddr { get; set; }

        public override void Parse(byte[] data, uint offset)
        {
            offset *= 8;
            version = BitHelper.Extract4(data, offset + 0);
            ihl = BitHelper.Extract4(data, offset + 4);
            diffserv = BitHelper.Extract8(data, offset + 8);
            totalLen = BitHelper.Extract16(data, offset + 16);
            identification = BitHelper.Extract16(data, offset + 32);
            flags = BitHelper.Extract3(data, offset + 48);
            fragOffset = BitHelper.Extract13(data, offset + 51);
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
            BitHelper.Write3(data, offset + 48, flags);
            BitHelper.Write13(data, offset + 51, fragOffset);
            BitHelper.Write8(data, offset + 64, ttl);
            BitHelper.Write8(data, offset + 72, protocol);
            BitHelper.Write16(data, offset + 80, hdrChecksum);
            BitHelper.Write32(data, offset + 96, srcAddr);
            BitHelper.Write32(data, offset + 128, dstAddr);
        }
    }

    public sealed class Parsed_packet : IStruct
    {
        public Ethernet_h ethernet { get; set; }
        public Ipv4_h ip { get; set; }
    }

    sealed class TopParser : IParser
    {
        public TopParser()
        {
        }

        void Apply(packet_in b, ref Parsed_packet p)
        {
            start(b, p);
        }

        void start(packet_in b, Parsed_packet p)
        {
            b.extract<Ethernet_h>(p.ethernet);
            switch ((System.UInt16)p.ethernet.etherType)
            {
                case 0x800:
                    parse_ipv4(b, p);
                    break;
            }
        }

        void parse_ipv4(packet_in b, Parsed_packet p)
        {
            b.extract<Ipv4_h>(p.ip);
            verify(p.ip.version == 4, error.IPv4IncorrectVersion);
            verify(p.ip.ihl == 5, error.IPv4OptionsNotSupported);
            ck.clear();
            ck.update<Ipv4_h>(p.ip);
            verify(ck.get() == 0, error.IPv4ChecksumError);
            accept(b, p);
        }

        void accept(packet_in b, Parsed_packet p)
        {
        }

        void reject(packet_in b, Parsed_packet p)
        {
        }
    }

    sealed class TopPipe : IControl
    {
        class TopPipe_Args
        {
            public Parsed_packet headers { get; set; }
            public error parseError { get; }
            public InControl inCtrl { get; }
            public OutControl outCtrl { get; set; }

            public TopPipe_Args(Parsed_packet headers, error parseError, InControl inCtrl, OutControl outCtrl)
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

        void apply(ref Parsed_packet headers, error parseError, InControl inCtrl, ref OutControl outCtrl)
        {
            TopPipe_Args TopPipe_Args = new TopPipe_Args(headers, parseError, inCtrl, outCtrl);
            IPv4Address nextHop;
            if (TopPipe_Args.parseError != error.NoError)
            {
                Drop_action();
                return;
            }

            ipv4_match.apply(nextHop);
            if (TopPipe_Args.outCtrl.outputPort == DROP_PORT)
                return;
            check_ttl.apply();
            if (TopPipe_Args.outCtrl.outputPort == CPU_OUT_PORT)
                return;
            dmac.apply(nextHop);
            if (TopPipe_Args.outCtrl.outputPort == DROP_PORT)
                return;
            smac.apply();
        }

        static void Drop_action(TopPipe_Args TopPipe_Args)
        {
            TopPipe_Args.outCtrl.outputPort = DROP_PORT;
        }

        static void Set_nhop(TopPipe_Args TopPipe_Args, ref IPv4Address nextHop, IPv4Address ipv4_dest, PortId port)
        {
            nextHop = ipv4_dest;
            TopPipe_Args.headers.ip.ttl = TopPipe_Args.headers.ip.ttl - 1;
            TopPipe_Args.outCtrl.outputPort = port;
        }

        private sealed class ipv4_match_t
        {
            LpmTable<bit32, ActionBase> lookup = new LpmTable<bit32, ActionBase>();

            public apply_result apply(TopPipe_Args TopPipe_Args, ref IPv4Address nextHop)
            {
                apply_result result;
                ActionBase RA = lookup?[TopPipe_Args.headers.ip.dstAddr];
                if (RA == null)
                    result = new apply_result(false, default_action.Action);
                else
                    result = new apply_result(true, RA.Action);
                RA.OnApply(TopPipe_Args, ref nextHop);
                return result;
            }

            public enum action_list
            {
                Drop_action,
                Set_nhopnextHop
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

                public abstract void OnApply(TopPipe_Args TopPipe_Args, ref IPv4Address nextHop);

                public sealed class Drop_action_Action : ActionBase
                {
                    public Drop_action_Action() : base(action_list.Drop_action)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args, ref IPv4Address nextHop)
                    {
                        Drop_action(TopPipe_Args);
                    }
                }

                public sealed class Set_nhopnextHop_Action : ActionBase
                {
                    readonly IPv4Address ipv4_dest;
                    readonly PortId port;

                    public Set_nhopnextHop_Action(IPv4Address ipv4_dest, PortId port) : base(action_list.Set_nhopnextHop)
                    {
                        this.ipv4_dest = ipv4_dest;
                        this.port = port;
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args, ref IPv4Address nextHop)
                    {
                        Set_nhop(TopPipe_Args, ref nextHop, ipv4_dest, port);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Drop_action_Action();
        }

        ipv4_match_t ipv4_match = new ipv4_match_t();

        static void Send_to_cpu(TopPipe_Args TopPipe_Args)
        {
            TopPipe_Args.outCtrl.outputPort = CPU_OUT_PORT;
        }

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
                Send_to_cpu,
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

                public sealed class Send_to_cpu_Action : ActionBase
                {
                    public Send_to_cpu_Action() : base(action_list.Send_to_cpu)
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
                        NoAction(TopPipe_Args);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.NoAction_Action();
        }

        check_ttl_t check_ttl = new check_ttl_t();

        static void Set_dmac(TopPipe_Args TopPipe_Args, EthernetAddress dmac)
        {
            TopPipe_Args.headers.ethernet.dstAddr = dmac;
        }

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
                Drop_action,
                Set_dmac
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

                public sealed class Drop_action_Action : ActionBase
                {
                    public Drop_action_Action() : base(action_list.Drop_action)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args, IPv4Address nextHop)
                    {
                        Drop_action(TopPipe_Args);
                    }
                }

                public sealed class Set_dmac_Action : ActionBase
                {
                    readonly EthernetAddress dmac;

                    public Set_dmac_Action(EthernetAddress dmac) : base(action_list.Set_dmac)
                    {
                        this.dmac = dmac;
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args, IPv4Address nextHop)
                    {
                        Set_dmac(TopPipe_Args, dmac);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Drop_action_Action();
        }

        dmac_t dmac = new dmac_t();

        static void Set_smac(TopPipe_Args TopPipe_Args, EthernetAddress smac)
        {
            TopPipe_Args.headers.ethernet.srcAddr = smac;
        }

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
                Drop_action,
                Set_smac
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

                public sealed class Drop_action_Action : ActionBase
                {
                    public Drop_action_Action() : base(action_list.Drop_action)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        Drop_action(TopPipe_Args);
                    }
                }

                public sealed class Set_smac_Action : ActionBase
                {
                    readonly EthernetAddress smac;

                    public Set_smac_Action(EthernetAddress smac) : base(action_list.Set_smac)
                    {
                        this.smac = smac;
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        Set_smac(TopPipe_Args, smac);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Drop_action_Action();
        }

        smac_t smac = new smac_t();
    }

    sealed class TopDeparser : IControl
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

        void apply(ref Parsed_packet p, packet_out b)
        {
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

        Ck16 ck = new Ck16();
    }

    VSS<Parsed_packet> main = new VSS<Parsed_packet>(new TopParser(), new TopPipe(), new TopDeparser());
}