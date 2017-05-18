//using System;
//using P4ToCSharp.Library;
//using static vss_model.Architecture;
//using PortId = P4ToCSharp.Library.bit4;
//using EthernetAddress = P4ToCSharp.Library.bit48;
//using IPv4Address = P4ToCSharp.Library.bit32;

//public class Program
//{
//    public class error : vss_model.Architecture.error
//    {
//        public static readonly error IPv4OptionsNotSupported = new error();
//        public static readonly error IPv4IncorrectVersion = new error();
//        public static readonly error IPv4ChecksumError = new error();

//        protected error()
//        {
//        }
//    }

//    static void verify(bool check, P4ToCSharp.Library.error toSignal)
//    {
//        vss_model.VSSModel.verify(check, toSignal);
//    }

//    static void NoAction()
//    {
//    }

//    public static readonly PortId REAL_PORT_COUNT = ((PortId)8);
//    public static readonly PortId RECIRCULATE_IN_PORT = ((PortId)0xD);
//    public static readonly PortId CPU_IN_PORT = ((PortId)0xE);
//    public static readonly PortId DROP_PORT = ((PortId)0xF);
//    public static readonly PortId CPU_OUT_PORT = ((PortId)0xE);
//    public static readonly PortId RECIRCULATE_OUT_PORT = ((PortId)0xD);

//    public sealed class Ethernet_h : HeaderBase
//    {
//        public EthernetAddress dstAddr;
//        public EthernetAddress srcAddr;
//        public bit16 etherType;

//        public override void Parse(byte[] data, uint offset)
//        {
//            offset *= 8;
//            dstAddr = BitHelper.Extract48(data, offset + 0);
//            srcAddr = BitHelper.Extract48(data, offset + 48);
//            etherType = BitHelper.Extract16(data, offset + 96);
//            length = 112;
//            setValid();
//        }

//        public override void Deparse(byte[] data, uint offset)
//        {
//            offset *= 8;
//            BitHelper.Write48(data, offset + 0, dstAddr);
//            BitHelper.Write48(data, offset + 48, srcAddr);
//            BitHelper.Write16(data, offset + 96, etherType);
//        }
//    }

//    public sealed class Ipv4_h : HeaderBase
//    {
//        public bit4 version;
//        public bit4 ihl;
//        public bit8 diffserv;
//        public bit16 totalLen;
//        public bit16 identification;
//        public bitN flags;
//        public bitN fragOffset;
//        public bit8 ttl;
//        public bit8 protocol;
//        public bit16 hdrChecksum;
//        public IPv4Address srcAddr;
//        public IPv4Address dstAddr;

//        public override void Parse(byte[] data, uint offset)
//        {
//            offset *= 8;
//            version = BitHelper.Extract4(data, offset + 0);
//            ihl = BitHelper.Extract4(data, offset + 4);
//            diffserv = BitHelper.Extract8(data, offset + 8);
//            totalLen = BitHelper.Extract16(data, offset + 16);
//            identification = BitHelper.Extract16(data, offset + 32);
//            flags = BitHelper.ExtractN(data, offset + 48, 3);
//            fragOffset = BitHelper.ExtractN(data, offset + 51, 13);
//            ttl = BitHelper.Extract8(data, offset + 64);
//            protocol = BitHelper.Extract8(data, offset + 72);
//            hdrChecksum = BitHelper.Extract16(data, offset + 80);
//            srcAddr = BitHelper.Extract32(data, offset + 96);
//            dstAddr = BitHelper.Extract32(data, offset + 128);
//            length = 160;
//            setValid();
//        }

//        public override void Deparse(byte[] data, uint offset)
//        {
//            offset *= 8;
//            BitHelper.Write4(data, offset + 0, version);
//            BitHelper.Write4(data, offset + 4, ihl);
//            BitHelper.Write8(data, offset + 8, diffserv);
//            BitHelper.Write16(data, offset + 16, totalLen);
//            BitHelper.Write16(data, offset + 32, identification);
//            BitHelper.WriteN(data, offset + 48, flags);
//            BitHelper.WriteN(data, offset + 51, fragOffset);
//            BitHelper.Write8(data, offset + 64, ttl);
//            BitHelper.Write8(data, offset + 72, protocol);
//            BitHelper.Write16(data, offset + 80, hdrChecksum);
//            BitHelper.Write32(data, offset + 96, srcAddr);
//            BitHelper.Write32(data, offset + 128, dstAddr);
//        }
//    }

//    public sealed class Parsed_packet : IStruct
//    {
//        public Ethernet_h ethernet;
//        public Ipv4_h ip;
//    }

//    sealed class TopParser : vss_model.Architecture.Parser<Parsed_packet>
//    {
//        public TopParser()
//        {
//        }

//        public void apply(packet_in b, out Parsed_packet p)
//        {
//            p = new Parsed_packet();
//            start(b, p);
//        }

//        void start(packet_in b, Parsed_packet p)
//        {
//            b.extract<Ethernet_h>(out p.ethernet);
//            Console.WriteLine("Extracted eth {0} {1} {2} {3}", 
//                              p.ethernet.isValid(), p.ethernet.srcAddr, p.ethernet.dstAddr, p.ethernet.etherType);
//            switch ((System.UInt16)p.ethernet.etherType)
//            {
//                case 0x800:
//                    parse_ipv4(b, p);
//                    break;
//            }
//        }

//        void parse_ipv4(packet_in b, Parsed_packet p)
//        {
//            b.extract<Ipv4_h>(out p.ip);
//            vss_model.VSSModel.verify((p.ip.version == 4), error.IPv4IncorrectVersion);
//            vss_model.VSSModel.verify((p.ip.ihl == 5), error.IPv4OptionsNotSupported);
//            accept(b, p);
//        }

//        void accept(packet_in b, Parsed_packet p)
//        {
//        }

//        void reject(packet_in b, Parsed_packet p)
//        {
//        }
//    }

//    sealed class TopPipe : vss_model.Architecture.Pipe<Parsed_packet>
//    {
//        class TopPipe_Args
//        {
//            public TopPipe Instance;
//            public Parsed_packet headers;
//            public P4ToCSharp.Library.error parseError;
//            public InControl inCtrl;
//            public OutControl outCtrl;

//            public TopPipe_Args(TopPipe Instance, Parsed_packet headers, P4ToCSharp.Library.error parseError, InControl inCtrl, OutControl outCtrl)
//            {
//                this.Instance = Instance;
//                this.headers = headers;
//                this.parseError = parseError;
//                this.inCtrl = inCtrl;
//                this.outCtrl = outCtrl;
//            }
//        }

//        public TopPipe()
//        {
//        }

//        public void apply(Parsed_packet headers_capture, ref Parsed_packet headers, P4ToCSharp.Library.error parseError, InControl inCtrl, out OutControl outCtrl)
//        {
//            outCtrl = new OutControl();
//            headers = headers_capture;
//            TopPipe_Args TopPipe_Args = new TopPipe_Args(this, headers, parseError, inCtrl, outCtrl);
//            if ((TopPipe_Args.parseError != error.NoError))
//            {
//                TopPipe_Args.Instance.Drop_action(TopPipe_Args);
//                return;
//            }

//            TopPipe_Args.Instance.rts_default.apply(TopPipe_Args);
//        }

//        void Drop_action(TopPipe_Args TopPipe_Args)
//        {
//            TopPipe_Args.outCtrl.outputPort = DROP_PORT;
//        }

//        void ReturnToSender(TopPipe_Args TopPipe_Args)
//        {
//            EthernetAddress t_eth = TopPipe_Args.headers.ethernet.srcAddr;
//            TopPipe_Args.headers.ethernet.srcAddr = TopPipe_Args.headers.ethernet.dstAddr;
//            TopPipe_Args.headers.ethernet.dstAddr = t_eth;
//            IPv4Address t_ip = TopPipe_Args.headers.ip.srcAddr;
//            TopPipe_Args.headers.ip.srcAddr = TopPipe_Args.headers.ip.dstAddr;
//            TopPipe_Args.headers.ip.dstAddr = TopPipe_Args.headers.ip.srcAddr;
//        }

//        rts_default_t rts_default = new rts_default_t();

//        private sealed class rts_default_t : ITable
//        {
//            public apply_result apply(TopPipe_Args TopPipe_Args)
//            {
//                apply_result result = new apply_result(false, default_action.Action);
//                default_action.OnApply(TopPipe_Args);
//                return result;
//            }

//            public enum action_list
//            {
//                TopPipe_Args_Instance_ReturnToSenderTopPipe_Args
//            }

//            public sealed class apply_result : apply_result<action_list>
//            {
//                public apply_result(bool hit, action_list action_run) : base(hit, action_run)
//                {
//                }
//            }

//            private abstract class ActionBase
//            {
//                public action_list Action { get; }

//                public ActionBase(action_list action)
//                {
//                    this.Action = action;
//                }

//                public abstract void OnApply(TopPipe_Args TopPipe_Args);

//                public sealed class TopPipe_Args_Instance_ReturnToSenderTopPipe_Args_Action : ActionBase
//                {
//                    public TopPipe_Args_Instance_ReturnToSenderTopPipe_Args_Action() : base(action_list.TopPipe_Args_Instance_ReturnToSenderTopPipe_Args)
//                    {
//                    }

//                    public override void OnApply(TopPipe_Args TopPipe_Args)
//                    {
//                        TopPipe_Args.Instance.ReturnToSender(TopPipe_Args);
//                    }
//                }
//            }

//            private ActionBase default_action = new ActionBase.TopPipe_Args_Instance_ReturnToSenderTopPipe_Args_Action();
//        }
//    }

//    sealed class TopDeparser : vss_model.Architecture.Deparser<Parsed_packet>
//    {
//        class TopDeparser_Args
//        {
//            public TopDeparser Instance;
//            public Parsed_packet p;
//            public packet_out b;

//            public TopDeparser_Args(TopDeparser Instance, Parsed_packet p, packet_out b)
//            {
//                this.Instance = Instance;
//                this.p = p;
//                this.b = b;
//            }
//        }

//        public TopDeparser()
//        {
//        }

//        public void apply(Parsed_packet p_capture, ref Parsed_packet p, packet_out b)
//        {
//            p = p_capture;
//            TopDeparser_Args TopDeparser_Args = new TopDeparser_Args(this, p, b);
//            TopDeparser_Args.b.emit<Ethernet_h>(TopDeparser_Args.p.ethernet);
//            if (TopDeparser_Args.p.ip.isValid())
//            {
//            }

//            TopDeparser_Args.b.emit<Ipv4_h>(TopDeparser_Args.p.ip);
//        }
//    }

//    public sealed class Processor : vss_model.VSSModel.VSS_impl<Parsed_packet>
//    {
//        public Processor()
//        {
//            this.Use(new TopParser(), new TopPipe(), new TopDeparser());
//        }
//    }

//    public static void Main()
//    {
//        new Processor().Run();
//    }
//}