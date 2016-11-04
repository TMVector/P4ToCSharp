using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  sealed class local_metadata_t
  {
    // FIXME: will you be able to copy comments over from P4 using their front-end?
    public ushort cpu_code;     // width 16 // Code for packet going to CPU
    public byte port_type;      // width 4  // Type of port: up, down, local...
    public bool ingress_error;  // width 1  // An error in ingress port check
    public bool was_mtagged;    // width 1  // Track if pkt was mtagged on ingr
    public bool copy_to_cpu;    // width 1  // Special code resulting in copy to CPU
    public bool bad_packet;     // width 1  // Other error indication
    public byte color;          // width 8  // For metering
  }
}
