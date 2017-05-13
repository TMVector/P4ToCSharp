using System;
using P4ToCSharp.Library;

[P4Architecture]
public class Architecture
{
    [P4(P4Type.Error, "error")]
    public class error : P4ToCSharp.Library.error
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

    [P4(P4Type.ExternObject, "packet_in")]
    public interface packet_in
    {
        void extract<T>(out T hdr);
        void extract<T>(out T variableSizeHeader, bit32 variableFieldSizeInBits);
        T lookahead<T>();
        void advance(bit32 sizeInBits);
        bit32 length();
    }

    [P4(P4Type.ExternObject, "packet_out")]
    public interface packet_out
    {
        void emit<T>(T hdr);
        void emit<T>(bool condition, T data);
    }

    [P4(P4Type.ExternFunction, "verify")]
    static void verify(bool check, P4ToCSharp.Library.error toSignal)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.MatchKind, "")]
    public enum MatchKind
    {
        exact,
        ternary,
        lpm,
        range,
        selector
    }

    [P4(P4Type.Struct, "standard_metadata_t")]
    public sealed class standard_metadata_t : IStruct
    {
        public bitN ingress_port;
        public bitN egress_spec;
        public bitN egress_port;
        public bit32 clone_spec;
        public bit32 instance_type;
        public bit1 drop;
        public bit16 recirculate_port;
        public bit32 packet_length;
    }

    [P4(P4Type.ExternObject, "Checksum16")]
    public interface Checksum16
    {
        bit16 get<D>(D data);
    }

    [P4(P4Type.Enum, "CounterType")]
    public enum CounterType
    {
        packets,
        bytes,
        packets_and_bytes
    }

    [P4(P4Type.ExternObject, "counter")]
    public interface counter
    {
        void count(bit32 index);
    }

    [P4(P4Type.ExternObject, "direct_counter")]
    public interface direct_counter
    {
    }

    [P4(P4Type.ExternObject, "meter")]
    public interface meter
    {
        void execute_meter<T>(bit32 index, out T result);
    }

    [P4(P4Type.ExternObject, "direct_meter")]
    public interface direct_meter<T>
    {
        void read(out T result);
    }

    [P4(P4Type.ExternObject, "register")]
    public interface register<T>
    {
        void read(out T result, bit32 index);
        void write(bit32 index, T value);
    }

    [P4(P4Type.ExternObject, "action_profile")]
    public interface action_profile
    {
    }

    [P4(P4Type.ExternFunction, "random")]
    static void random(bit32 result, bit32 lo, bit32 hi)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "digest")]
    static void digest<T>(bit32 receiver, T data)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.Enum, "HashAlgorithm")]
    public enum HashAlgorithm
    {
        crc32,
        crc32_custom,
        crc16,
        crc16_custom,
        random,
        identity
    }

    [P4(P4Type.ExternFunction, "mark_to_drop")]
    static void mark_to_drop()
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "hash")]
    static void hash<O, T, D, M>(O result, HashAlgorithm algo, T @base, D data, M max)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.ExternObject, "action_selector")]
    public interface action_selector
    {
    }

    [P4(P4Type.Enum, "CloneType")]
    public enum CloneType
    {
        I2E,
        E2E
    }

    [P4(P4Type.ExternFunction, "resubmit")]
    static void resubmit<T>(T data)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "recirculate")]
    static void recirculate<T>(T data)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "clone")]
    static void clone(CloneType type, bit32 session)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "clone3")]
    static void clone3<T>(CloneType type, bit32 session, T data)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "truncate")]
    static void truncate(bit32 length)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.Parser, "Parser")]
    public interface Parser<H, M> : IParser
    {
        void apply(packet_in b, out H parsedHdr, M meta_capture, ref M meta, standard_metadata_t standard_metadata_capture, ref standard_metadata_t standard_metadata);
    }

    [P4(P4Type.Control, "VerifyChecksum")]
    public interface VerifyChecksum<H, M> : IControl
    {
        void apply(H hdr, M meta_capture, ref M meta);
    }

    [P4(P4Type.Control, "Ingress")]
    public interface Ingress<H, M> : IControl
    {
        void apply(H hdr_capture, ref H hdr, M meta_capture, ref M meta, standard_metadata_t standard_metadata_capture, ref standard_metadata_t standard_metadata);
    }

    [P4(P4Type.Control, "Egress")]
    public interface Egress<H, M> : IControl
    {
        void apply(H hdr_capture, ref H hdr, M meta_capture, ref M meta, standard_metadata_t standard_metadata_capture, ref standard_metadata_t standard_metadata);
    }

    [P4(P4Type.Control, "ComputeChecksum")]
    public interface ComputeChecksum<H, M> : IControl
    {
        void apply(H hdr_capture, ref H hdr, M meta_capture, ref M meta);
    }

    [P4(P4Type.Control, "Deparser")]
    public interface Deparser<H> : IControl
    {
        void apply(packet_out b, H hdr);
    }

    [P4(P4Type.Package, "V1Switch")]
    public interface V1Switch<H, M> : IPackage
    {
        void Use(Parser<H, M> p, VerifyChecksum<H, M> vr, Ingress<H, M> ig, Egress<H, M> eg, ComputeChecksum<H, M> ck, Deparser<H> dep);
    }
}