using System;
using P4ToCSharp.Library;
using static vss_model.Architecture;
using PortId = P4ToCSharp.Library.bit4;
using EthernetAddress = P4ToCSharp.Library.bit48;
using IPv4Address = P4ToCSharp.Library.bit32;

public class Program
{
    public class error : vss_model.Architecture.error
    {
        public static readonly error NoError = new error();
        public static readonly error PacketTooShort = new error();
        public static readonly error NoMatch = new error();
        public static readonly error StackOutOfBounds = new error();
        public static readonly error HeaderTooShort = new error();
        public static readonly error ParserTimeout = new error();
        public static readonly error IPv4OptionsNotSupported = new error();
        public static readonly error IPv4IncorrectVersion = new error();
        public static readonly error IPv4ChecksumError = new error();

        protected error()
        {
        }
    }

    static void verify(bool check, P4ToCSharp.Library.error toSignal)
    {
        vss_model.VSSModel.verify(check, toSignal);
    }

    static void NoAction()
    {
    }

    public static readonly PortId REAL_PORT_COUNT = ((PortId)8);
    public static readonly PortId RECIRCULATE_IN_PORT = ((PortId)0xD);
    public static readonly PortId CPU_IN_PORT = ((PortId)0xE);
    public static readonly PortId DROP_PORT = ((PortId)0xF);
    public static readonly PortId CPU_OUT_PORT = ((PortId)0xE);
    public static readonly PortId RECIRCULATE_OUT_PORT = ((PortId)0xD);

    public sealed class Ethernet_h : HeaderBase
    {
        public EthernetAddress destAddr;
        public EthernetAddress srcAddr;
        public bit16 etherType;

        public override void Parse(byte[] data, uint offset)
        {
            offset *= 8;
            destAddr = ((bit48)BitHelper.Extract48(data, offset + 0));
            srcAddr = ((bit48)BitHelper.Extract48(data, offset + 48));
            etherType = ((bit16)BitHelper.Extract16(data, offset + 96));
            length = 112;
            setValid();
        }

        public override void Deparse(byte[] data, uint offset)
        {
            offset *= 8;
            BitHelper.Write48(data, offset + 0, ((bit48)destAddr));
            BitHelper.Write48(data, offset + 48, ((bit48)srcAddr));
            BitHelper.Write16(data, offset + 96, ((bit16)etherType));
        }
    }

    public sealed class IPV4_h : HeaderBase
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
            version = ((bit4)BitHelper.Extract4(data, offset + 0));
            ihl = ((bit4)BitHelper.Extract4(data, offset + 4));
            diffserv = ((bit8)BitHelper.Extract8(data, offset + 8));
            totalLen = ((bit16)BitHelper.Extract16(data, offset + 16));
            identification = ((bit16)BitHelper.Extract16(data, offset + 32));
            flags = bitN.OfValue(BitHelper.ExtractN(data, offset + 48, 3), width: 3);
            fragOffset = bitN.OfValue(BitHelper.ExtractN(data, offset + 51, 13), width: 13);
            ttl = ((bit8)BitHelper.Extract8(data, offset + 64));
            protocol = ((bit8)BitHelper.Extract8(data, offset + 72));
            hdrChecksum = ((bit16)BitHelper.Extract16(data, offset + 80));
            srcAddr = ((bit32)BitHelper.Extract32(data, offset + 96));
            dstAddr = ((bit32)BitHelper.Extract32(data, offset + 128));
            length = 160;
            setValid();
        }

        public override void Deparse(byte[] data, uint offset)
        {
            offset *= 8;
            BitHelper.Write4(data, offset + 0, ((bit4)version));
            BitHelper.Write4(data, offset + 4, ((bit4)ihl));
            BitHelper.Write8(data, offset + 8, ((bit8)diffserv));
            BitHelper.Write16(data, offset + 16, ((bit16)totalLen));
            BitHelper.Write16(data, offset + 32, ((bit16)identification));
            BitHelper.WriteN(data, offset + 48, bitN.OfValue(flags, width: 3));
            BitHelper.WriteN(data, offset + 51, bitN.OfValue(fragOffset, width: 13));
            BitHelper.Write8(data, offset + 64, ((bit8)ttl));
            BitHelper.Write8(data, offset + 72, ((bit8)protocol));
            BitHelper.Write16(data, offset + 80, ((bit16)hdrChecksum));
            BitHelper.Write32(data, offset + 96, ((bit32)srcAddr));
            BitHelper.Write32(data, offset + 128, ((bit32)dstAddr));
        }
    }

    public sealed class Parsed_Packet_struct : IStruct
    {
        public Ethernet_h ethernet;
        public IPV4_h ip;
    }

    sealed class TopParser : vss_model.Architecture.Parser<Parsed_Packet_struct>
    {
        Ck16 ck;

        public TopParser()
        {
            ck = new vss_model.VSSModel.Ck16_impl();
        }

        public void apply(packet_in b_param, out Parsed_Packet_struct p)
        {
            p = new Parsed_Packet_struct();
            start(b_param, p);
        }

        void start(packet_in b_param, Parsed_Packet_struct p)
        {
            b_param.extract<Ethernet_h>(out p.ethernet);
            switch ((System.UInt16)p.ethernet.etherType)
            {
                case 0x800:
                    parse_ipv4(b_param, p);
                    break;
            }
        }

        void parse_ipv4(packet_in b_param, Parsed_Packet_struct p)
        {
            b_param.extract<IPV4_h>(out p.ip);
            vss_model.VSSModel.verify((p.ip.version == 4), error.IPv4IncorrectVersion);
            vss_model.VSSModel.verify((p.ip.ihl == 5), error.IPv4OptionsNotSupported);
            ck.clear();
            ck.update<IPV4_h>(p.ip);
            vss_model.VSSModel.verify((ck.get() == 0), error.IPv4ChecksumError);
            accept(b_param, p);
        }

        void accept(packet_in b_param, Parsed_Packet_struct p)
        {
        }

        void reject(packet_in b_param, Parsed_Packet_struct p)
        {
        }
    }

    sealed class TopPipe : vss_model.Architecture.Pipe<Parsed_Packet_struct>
    {
        class TopPipe_Args
        {
            public TopPipe Instance;
            public Parsed_Packet_struct headers;
            public P4ToCSharp.Library.error parseError;
            public InControl inCtrl;
            public OutControl outCtrl;

            public TopPipe_Args(TopPipe Instance, Parsed_Packet_struct headers, P4ToCSharp.Library.error parseError, InControl inCtrl, OutControl outCtrl)
            {
                this.Instance = Instance;
                this.headers = headers;
                this.parseError = parseError;
                this.inCtrl = inCtrl;
                this.outCtrl = outCtrl;
            }
        }

        public TopPipe()
        {
        }

        public void apply(Parsed_Packet_struct headers_capture, ref Parsed_Packet_struct headers, P4ToCSharp.Library.error parseError, InControl inCtrl, out OutControl outCtrl)
        {
            outCtrl = new OutControl();
            headers = headers_capture;
            TopPipe_Args TopPipe_Args = new TopPipe_Args(this, headers, parseError, inCtrl, outCtrl);
            if ((TopPipe_Args.parseError != error.NoError))
            {
                TopPipe_Args.Instance.Drop_action(TopPipe_Args);
                return;
            }

            TopPipe_Args.Instance.ipv4_match_table.apply(TopPipe_Args);
            if ((TopPipe_Args.outCtrl.outputPort == DROP_PORT))
                return;
            TopPipe_Args.Instance.check_ttl.apply(TopPipe_Args);
            if ((TopPipe_Args.outCtrl.outputPort == CPU_OUT_PORT))
                return;
            TopPipe_Args.Instance.dmac.apply(TopPipe_Args);
            if ((TopPipe_Args.outCtrl.outputPort == DROP_PORT))
                return;
            TopPipe_Args.Instance.smac.apply(TopPipe_Args);
        }

        void Drop_action(TopPipe_Args TopPipe_Args)
        {
            TopPipe_Args.outCtrl.outputPort = DROP_PORT;
        }

        IPv4Address nextHop;

        void Set_nhop_action(TopPipe_Args TopPipe_Args, IPv4Address ipv4_dest, PortId port)
        {
            TopPipe_Args.Instance.nextHop = ipv4_dest;
            TopPipe_Args.headers.ip.ttl = ((bit8)(TopPipe_Args.headers.ip.ttl - 1));
            TopPipe_Args.outCtrl.outputPort = port;
        }

        ipv4_match_table_t ipv4_match_table = new ipv4_match_table_t();

        private sealed class ipv4_match_table_t : ITable
        {
            P4ToCSharp.Library.LpmTable<bit32, ActionBase> lookup = new P4ToCSharp.Library.LpmTable<bit32, ActionBase>();

            public ipv4_match_table_t()
            {
            }

            public apply_result apply(TopPipe_Args TopPipe_Args)
            {
                apply_result result;
                ActionBase RA = lookup?[TopPipe_Args.headers.ip.dstAddr];
                if (RA == null)
                {
                    result = new apply_result(false, default_action.Action);
                    RA = default_action;
                }
                else
                    result = new apply_result(true, RA.Action);
                RA.OnApply(TopPipe_Args);
                return result;
            }

            public enum action_list
            {
                Drop_action,
                Set_nhop_action
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
                        TopPipe_Args.Instance.Drop_action(TopPipe_Args);
                    }
                }

                public sealed class Set_nhop_action_Action : ActionBase
                {
                    readonly IPv4Address ipv4_dest;
                    readonly PortId port;

                    public Set_nhop_action_Action(IPv4Address ipv4_dest, PortId port) : base(action_list.Set_nhop_action)
                    {
                        this.ipv4_dest = ipv4_dest;
                        this.port = port;
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        TopPipe_Args.Instance.Set_nhop_action(TopPipe_Args, ipv4_dest, port);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Set_nhop_action_Action();
        }

        void Send_to_cpu(TopPipe_Args TopPipe_Args)
        {
            TopPipe_Args.outCtrl.outputPort = CPU_OUT_PORT;
        }

        check_ttl_t check_ttl = new check_ttl_t();

        private sealed class check_ttl_t : ITable
        {
            P4ToCSharp.Library.ExactTable<bit8, ActionBase> lookup = new P4ToCSharp.Library.ExactTable<bit8, ActionBase>();

            public check_ttl_t()
            {
            }

            public apply_result apply(TopPipe_Args TopPipe_Args)
            {
                apply_result result;
                ActionBase RA = lookup?[TopPipe_Args.headers.ip.ttl];
                if (RA == null)
                {
                    result = new apply_result(false, default_action.Action);
                    RA = default_action;
                }
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
                        TopPipe_Args.Instance.Send_to_cpu(TopPipe_Args);
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

        void Set_dmac(TopPipe_Args TopPipe_Args, EthernetAddress dmac)
        {
            TopPipe_Args.headers.ethernet.destAddr = dmac;
        }

        dmac_t dmac = new dmac_t();

        private sealed class dmac_t : ITable
        {
            P4ToCSharp.Library.ExactTable<bit32, ActionBase> lookup = new P4ToCSharp.Library.ExactTable<bit32, ActionBase>();

            public dmac_t()
            {
            }

            public apply_result apply(TopPipe_Args TopPipe_Args)
            {
                apply_result result;
                ActionBase RA = lookup?[TopPipe_Args.Instance.nextHop];
                if (RA == null)
                {
                    result = new apply_result(false, default_action.Action);
                    RA = default_action;
                }
                else
                    result = new apply_result(true, RA.Action);
                RA.OnApply(TopPipe_Args);
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

                public abstract void OnApply(TopPipe_Args TopPipe_Args);

                public sealed class Drop_action_Action : ActionBase
                {
                    public Drop_action_Action() : base(action_list.Drop_action)
                    {
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        TopPipe_Args.Instance.Drop_action(TopPipe_Args);
                    }
                }

                public sealed class Set_dmac_Action : ActionBase
                {
                    readonly EthernetAddress dmac;

                    public Set_dmac_Action(EthernetAddress dmac) : base(action_list.Set_dmac)
                    {
                        this.dmac = dmac;
                    }

                    public override void OnApply(TopPipe_Args TopPipe_Args)
                    {
                        TopPipe_Args.Instance.Set_dmac(TopPipe_Args, dmac);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Drop_action_Action();
        }

        void Set_smac(TopPipe_Args TopPipe_Args, EthernetAddress smac)
        {
            TopPipe_Args.headers.ethernet.srcAddr = smac;
        }

        smac_t smac = new smac_t();

        private sealed class smac_t : ITable
        {
            P4ToCSharp.Library.ExactTable<bit4, ActionBase> lookup = new P4ToCSharp.Library.ExactTable<bit4, ActionBase>();

            public smac_t()
            {
            }

            public apply_result apply(TopPipe_Args TopPipe_Args)
            {
                apply_result result;
                ActionBase RA = lookup?[TopPipe_Args.outCtrl.outputPort];
                if (RA == null)
                {
                    result = new apply_result(false, default_action.Action);
                    RA = default_action;
                }
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
                        TopPipe_Args.Instance.Drop_action(TopPipe_Args);
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
                        TopPipe_Args.Instance.Set_smac(TopPipe_Args, smac);
                    }
                }
            }

            private ActionBase default_action = new ActionBase.Drop_action_Action();
        }
    }

    sealed class TopDeparser : vss_model.Architecture.Deparser<Parsed_Packet_struct>
    {
        class TopDeparser_Args
        {
            public TopDeparser Instance;
            public Parsed_Packet_struct p;
            public packet_out b;

            public TopDeparser_Args(TopDeparser Instance, Parsed_Packet_struct p, packet_out b)
            {
                this.Instance = Instance;
                this.p = p;
                this.b = b;
            }
        }

        public TopDeparser()
        {
        }

        public void apply(Parsed_Packet_struct p_capture, ref Parsed_Packet_struct p, packet_out b)
        {
            p = p_capture;
            TopDeparser_Args TopDeparser_Args = new TopDeparser_Args(this, p, b);
            TopDeparser_Args.b.emit<Ethernet_h>(TopDeparser_Args.p.ethernet);
            if (TopDeparser_Args.p.ip.isValid())
            {
                TopDeparser_Args.Instance.ck.clear();
                TopDeparser_Args.p.ip.hdrChecksum = ((bit16)0);
                TopDeparser_Args.Instance.ck.update<IPV4_h>(TopDeparser_Args.p.ip);
                TopDeparser_Args.p.ip.hdrChecksum = TopDeparser_Args.Instance.ck.get();
            }

            TopDeparser_Args.b.emit<IPV4_h>(TopDeparser_Args.p.ip);
        }

        Ck16 ck = new vss_model.VSSModel.Ck16_impl();
    }

    public sealed class Processor : vss_model.VSSModel.VSS_impl<Parsed_Packet_struct>
    {
        public Processor()
        {
            this.Use(new TopParser(), new TopPipe(), new TopDeparser());
        }
    }

    public static void Main()
    {
        new Processor().Run();
    }
}