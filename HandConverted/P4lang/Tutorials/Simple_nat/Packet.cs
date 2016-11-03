using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.Tutorials.Simple_nat
{
  class Packet
  {
    const uint ETHERTYPE_IPV4 = 0x0800; // FIXME #defines don't specify a datatype - is width defined in spec?
    const uint IP_PROT_TCP = 0x06; // Is width from number width? Usage?
    const uint CPU_MIRROR_SESSION_ID = 250;

    /* Headers */
    cpu_header_t cpu_header = null;
    ethernet_t ethernet = null;
    ipv4_t ipv4 = null;
    tcp_t tcp = null;

    /* Metadata */
    standard_metadata_t standard_metadata;
    intrinsic_metadata_t intrinsinc_metadata = null;
    meta_t meta = null;
  }
}
