using System;
using P4ToCSharp.Library;
using PortId = P4ToCSharp.Library.bit4;

public class Architecture
{
    public enum error
    {
        NoError,
        PacketTooShort,
        NoMatch,
        StackOutOfBounds,
        OverwritingHeader,
        HeaderTooShort,
        ParserTimeout
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
        throw new NotImplementedException();
    }

    public sealed class InControl : IStruct
    {
        public PortId inputPort { get; set; }
    }

    public sealed class OutControl : IStruct
    {
        public PortId outputPort { get; set; }
    }

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
        void clear();
        void update<T>(T data);
        bit16 get();
    }
}