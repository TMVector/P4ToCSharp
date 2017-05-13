using System;
using P4ToCSharp.Library;
using static v1model.Architecture;
using std_meta_t = v1model.Architecture.standard_metadata_t;

public class Program
{
    public class error : v1model.Architecture.error
    {
        public static readonly error NoError = new error();
        public static readonly error PacketTooShort = new error();
        public static readonly error NoMatch = new error();
        public static readonly error StackOutOfBounds = new error();
        public static readonly error OverwritingHeader = new error();
        public static readonly error HeaderTooShort = new error();
        public static readonly error ParserTimeout = new error();

        protected error()
        {
        }
    }

    static void verify(bool check, P4ToCSharp.Library.error toSignal)
    {
        v1model.Model.verify(check, toSignal);
    }

    static void NoAction()
    {
    }

    static void random(bit32 result, bit32 lo, bit32 hi)
    {
        v1model.Model.random(result, lo, hi);
    }

    static void digest<T>(bit32 receiver, T data)
    {
        v1model.Model.digest<T>(receiver, data);
    }

    static void mark_to_drop()
    {
        v1model.Model.mark_to_drop();
    }

    static void hash<O, T, D, M>(O result, HashAlgorithm algo, T @base, D data, M max)
    {
        v1model.Model.hash<O, T, D, M>(result, algo, base, data, max);
    }

    static void resubmit<T>(T data)
    {
        v1model.Model.resubmit<T>(data);
    }

    static void recirculate<T>(T data)
    {
        v1model.Model.recirculate<T>(data);
    }

    static void clone(CloneType type, bit32 session)
    {
        v1model.Model.clone(type, session);
    }

    static void clone3<T>(CloneType type, bit32 session, T data)
    {
        v1model.Model.clone3<T>(type, session, data);
    }

    static void truncate(bit32 length)
    {
        v1model.Model.truncate(length);
    }

    public sealed class S : IStruct
    {
        public bit32 x;
    }

    public sealed class T : HeaderBase
    {
        public bit32 y;

        public override void Parse(byte[] data, uint offset)
        {
            offset *= 8;
            y = BitHelper.Extract32(data, offset + 0);
        }

        public override void Deparse(byte[] data, uint offset)
        {
            offset *= 8;
            BitHelper.Write32(data, offset + 0, y);
        }
    }

    public sealed class H : IStruct
    {
        public T s;
    }

    public sealed class M : IStruct
    {
        public S s;
    }

    sealed class VerifyChecksumI : v1model.Architecture.VerifyChecksum<H, M>
    {
        class VerifyChecksumI_Args
        {
            public H hdr;
            public M meta;

            public VerifyChecksumI_Args(H hdr, M meta)
            {
                this.hdr = hdr;
                this.meta = meta;
            }
        }

        public VerifyChecksumI()
        {
        }

        public void apply(H hdr, M meta_capture, ref M meta)
        {
            meta = meta_capture;
            VerifyChecksumI_Args VerifyChecksumI_Args = new VerifyChecksumI_Args(hdr, meta);
        }
    }

    sealed class ParserI : v1model.Architecture.Parser<H, M>
    {
        public ParserI()
        {
        }

        public void apply(packet_in b, out H parsedHdr, M meta_capture, ref M meta, std_meta_t std_meta_capture, ref std_meta_t std_meta)
        {
            parsedHdr = new H();
            meta = meta_capture;
            std_meta = std_meta_capture;
            start(b, parsedHdr, meta, std_meta);
        }

        void start(packet_in b, H parsedHdr, M meta, std_meta_t std_meta)
        {
            accept(b, parsedHdr, meta, std_meta);
        }

        void accept(packet_in b, H parsedHdr, M meta, std_meta_t std_meta)
        {
        }

        void reject(packet_in b, H parsedHdr, M meta, std_meta_t std_meta)
        {
        }
    }

    sealed class ctrl
    {
        class ctrl_Args
        {
            public M meta;

            public ctrl_Args(M meta)
            {
                this.meta = meta;
            }
        }

        public ctrl()
        {
        }

        public void apply(M meta_capture, ref M meta)
        {
            meta = meta_capture;
            ctrl_Args ctrl_Args = new ctrl_Args(meta);
        }
    }

    sealed class IngressI : v1model.Architecture.Ingress<H, M>
    {
        class IngressI_Args
        {
            public H hdr;
            public M meta;
            public std_meta_t std_meta;

            public IngressI_Args(H hdr, M meta, std_meta_t std_meta)
            {
                this.hdr = hdr;
                this.meta = meta;
                this.std_meta = std_meta;
            }
        }

        public IngressI()
        {
        }

        public void apply(H hdr_capture, ref H hdr, M meta_capture, ref M meta, std_meta_t std_meta_capture, ref std_meta_t std_meta)
        {
            hdr = hdr_capture;
            meta = meta_capture;
            std_meta = std_meta_capture;
            IngressI_Args IngressI_Args = new IngressI_Args(hdr, meta, std_meta);
            do_ctrl.apply(IngressI_Args.meta, ref IngressI_Args.meta);
        }

        ctrl do_ctrl = new ctrl();
    }

    sealed class EgressI : v1model.Architecture.Egress<H, M>
    {
        class EgressI_Args
        {
            public H hdr;
            public M meta;
            public std_meta_t std_meta;

            public EgressI_Args(H hdr, M meta, std_meta_t std_meta)
            {
                this.hdr = hdr;
                this.meta = meta;
                this.std_meta = std_meta;
            }
        }

        public EgressI()
        {
        }

        public void apply(H hdr_capture, ref H hdr, M meta_capture, ref M meta, std_meta_t std_meta_capture, ref std_meta_t std_meta)
        {
            hdr = hdr_capture;
            meta = meta_capture;
            std_meta = std_meta_capture;
            EgressI_Args EgressI_Args = new EgressI_Args(hdr, meta, std_meta);
        }
    }

    sealed class ComputeChecksumI : v1model.Architecture.ComputeChecksum<H, M>
    {
        class ComputeChecksumI_Args
        {
            public H hdr;
            public M meta;

            public ComputeChecksumI_Args(H hdr, M meta)
            {
                this.hdr = hdr;
                this.meta = meta;
            }
        }

        public ComputeChecksumI()
        {
        }

        public void apply(H hdr_capture, ref H hdr, M meta_capture, ref M meta)
        {
            hdr = hdr_capture;
            meta = meta_capture;
            ComputeChecksumI_Args ComputeChecksumI_Args = new ComputeChecksumI_Args(hdr, meta);
        }
    }

    sealed class DeparserI : v1model.Architecture.Deparser<H>
    {
        class DeparserI_Args
        {
            public packet_out b;
            public H hdr;

            public DeparserI_Args(packet_out b, H hdr)
            {
                this.b = b;
                this.hdr = hdr;
            }
        }

        public DeparserI()
        {
        }

        public void apply(packet_out b, H hdr)
        {
            DeparserI_Args DeparserI_Args = new DeparserI_Args(b, hdr);
        }
    }

    public static void Main()
    {
        new v1model.Model.V1Switch<H, M>().Use(new ParserI(), new VerifyChecksumI(), new IngressI(), new EgressI(), new ComputeChecksumI(), new DeparserI());
    }
}