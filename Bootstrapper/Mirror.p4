/*
Modified by Jonny Shipton, May 2017. Original header below:

Copyright 2013-present Barefoot Networks, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <core.p4>
#include "very_simple_model.p4"

typedef bit<48>  EthernetAddress;
typedef bit<32>  IPv4Address;

// standard Ethernet header
header Ethernet_h {
    EthernetAddress dstAddr;
    EthernetAddress srcAddr;
    bit<16>         etherType;
}

// IPv4 header without options
header Ipv4_h {
    bit<4>       version;
    bit<4>       ihl;
    bit<8>       diffserv;
    bit<16>      totalLen;
    bit<16>      identification;
    bit<3>       flags;
    bit<13>      fragOffset;
    bit<8>       ttl;
    bit<8>       protocol;
    bit<16>      hdrChecksum;
    IPv4Address  srcAddr;
    IPv4Address  dstAddr;
}

// Parser section

// Declare user-defined errors that may be signaled during parsing
error {
    IPv4OptionsNotSupported,
    IPv4IncorrectVersion,
    IPv4ChecksumError
}

// List of all recognized headers
struct Parsed_packet {
    Ethernet_h ethernet;
    Ipv4_h     ip;
}

parser TopParser(packet_in b, out Parsed_packet p) {
    //Ck16() ck;  // instantiate checksum unit

    state start {
        b.extract(p.ethernet);
        transition select(p.ethernet.etherType) {
            0x0800 : parse_ipv4;
            // no default rule: all other packets rejected
        }
    }

    state parse_ipv4 {
        b.extract(p.ip);
        verify(p.ip.version == 4w4, error.IPv4IncorrectVersion);
        verify(p.ip.ihl == 4w5, error.IPv4OptionsNotSupported);
        //ck.clear();
        //ck.update(p.ip);
        // Verify that packet checksum is zero
        //verify(ck.get() == 16w0, error.IPv4ChecksumError);
        transition accept;
    }
}

// match-action pipeline section

control TopPipe(inout Parsed_packet headers,
                in error parseError, // parser error
                in InControl inCtrl, // input port
                out OutControl outCtrl) {
	action Drop_action()
	{ outCtrl.outputPort = DROP_PORT; }
    
	action ReturnToSender()
	{
		EthernetAddress t_eth = headers.ethernet.srcAddr;
		headers.ethernet.srcAddr = headers.ethernet.dstAddr;
		headers.ethernet.dstAddr = t_eth;

		IPv4Address t_ip = headers.ip.srcAddr;
		headers.ip.srcAddr = headers.ip.dstAddr;
		headers.ip.dstAddr = headers.ip.srcAddr;

		//headers.ip.ttl = headers.ip.ttl-1;
	}

    table rts_default {
         actions = {
              ReturnToSender;
         }
         default_action = ReturnToSender;
    }

	apply {
		if (parseError != error.NoError) {
			Drop_action();  // invoke drop directly
			return;
		}

        rts_default.apply();
	}
}

// deparser section
control TopDeparser(inout Parsed_packet p, packet_out b) {
    //Ck16() ck;
    apply {
        b.emit(p.ethernet);
        if (p.ip.isValid()) {
            //ck.clear();                // prepare checksum unit
            //p.ip.hdrChecksum = 16w0;   // clear checksum
            //ck.update(p.ip);           // compute new checksum.
            //p.ip.hdrChecksum = ck.get();
        }
        b.emit(p.ip);
    }
}

// Instantiate the top-level VSS package.
// use TopParser for the p Parser, etc.
VSS(TopParser(),
    TopPipe(),
    TopDeparser()) main;
