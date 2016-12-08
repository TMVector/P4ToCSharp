error {
    NoError,
    PacketTooShort,    // not enough bits in packet for extract
    NoMatch,           // match statement has no matches
    EmptyStack,        // reference to .last in an empty header stack
    FullStack,         // reference to .next in a full header stack
    OverwritingHeader, // extracting on top of a valid header
    HeaderTooShort,    // extracting too many bits into a varbit field
    ParserTimeout      // parser execution time limit exceeded
}
extern packet_in {
    // packet abstraction
    void extract<T>(out T hdr);
    void extract<T>(out T variableSizeHeader,
                    in bit<32> variableFieldSizeInBits);
    T lookahead<T>();
    void advance(in bit<32> sizeInBits);
    bit<32> length(); //packet length in bytes
}
extern packet_out {
    void emit<T>(in T hdr);
    void emit<T>(in bool condition, in T data);
}
action NoAction() {}
match_kind {
    exact,
    ternary,
    lpm
}
