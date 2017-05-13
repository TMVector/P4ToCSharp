using System;
using P4ToCSharp.Library;
using PortId = P4ToCSharp.Library.bit4;
using EthernetAddress = P4ToCSharp.Library.bit48;
using IPv4Address = P4ToCSharp.Library.bit32;

[P4Architecture()]
public class Architecture
{
    [P4(P4Type.Error, "error")]
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
    static void verify(bool condition, error err)
    {
        throw new NotImplementedException();
    }

    [P4(P4Type.MatchKind, "")]
    public enum MatchKind
    {
        exact,
        ternary,
        lpm
    }

    [P4(P4Type.Struct, "InControl")]
    public sealed class InControl : IStruct
    {
        public PortId inputPort;
    }

    [P4(P4Type.Struct, "OutControl")]
    public sealed class OutControl : IStruct
    {
        public PortId outputPort;
    }

    [P4(P4Type.Parser, "Parser")]
    public interface Parser<H> : IParser
    {
        void Apply(packet_in b, out H parsedHeaders);
    }

    [P4(P4Type.Control, "Pipe")]
    public interface Pipe<H> : IControl
    {
        void Apply(H headers_capture, ref H headers, error parseError, InControl inCtrl, out OutControl outCtrl);
    }

    [P4(P4Type.Control, "Deparser")]
    public interface Deparser<H> : IControl
    {
        void Apply(H outputHeaders_capture, ref H outputHeaders, packet_out b);
    }

    [P4(P4Type.Package, "VSS")]
    public interface VSS<H> : IPackage
    {
        void Use(Parser<H> p, Pipe<H> map, Deparser<H> d);
    }

    [P4(P4Type.ExternObject, "Ck16")]
    public interface Ck16
    {
        void clear();
        void update<T>(T data);
        bit16 get();
    }

    [P4(P4Type.Header, "Ethernet_h")]
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

    [P4(P4Type.Header, "Ipv4_h")]
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

    [P4(P4Type.Struct, "Parsed_packet")]
    public sealed class Parsed_packet : IStruct
    {
        public Ethernet_h ethernet;
        public Ipv4_h ip;
    }
}