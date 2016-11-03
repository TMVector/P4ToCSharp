using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.Switch.Mirror
{
  class ipv4_metadata_t
  {
    // FIXME: How do datatypes map? How is width enforced?

    uint lkp_ipv4_sa; // 32
    uint lkp_ipv4_da; // 32
    bool ipv4_unicast_enabled; // 1
    byte ipv4_urpf_mode; // 2
  }
}
