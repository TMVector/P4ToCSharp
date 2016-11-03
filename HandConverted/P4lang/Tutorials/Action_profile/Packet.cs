using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.Tutorials.Action_profile
{
  class Packet
  {
    const uint ETHERTYPE_IPV4 = 0x0800; // FIXME: are #defines always uints? What type are they?

    /* Headers */
    ethernet_t ethernet;
    ipv4_t ipv4;

    /* Metadata */
    standard_metadata_t standard_metadata;
    intrinsic_metadata_t intrinsic_metadata;

    /* Parser */
    void Parse() // FIXME: return type? args?
    {
      parse_ethernet();
    }
    void parse_ethernet()
    {
      ethernet_t.Extract();

      switch (ethernet.etherType)
      {
        case ETHERTYPE_IPV4:
          parse_ipv4();
          break;
        default:
          ingress(); // FIXME: What does ingress mean in this context? (L65)
      }
    }

    /* Actions */
    void _drop()
    {
      drop(); // FIXME: What does this mean in our context?
    }
    void _nop()
    {
    }
    void set_ecmp_nexthop(byte port) // FIXME: width of port?
    {
      standard_metadata.egress_spec = port;
    }

    /* Field lists (Not tangible outside of use?) */
    /* Field list calculations */
    ushort ecmp_hash()
    {
      // TODO: perform bmv2_hash on [ipv4.srcAddr; ipv4.protocol]
    }

    /* Action selectors */
    //?


  }
}
