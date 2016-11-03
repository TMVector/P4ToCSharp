using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  class ethernet_t
  {
    public ulong dstAddr; // 48
    public ulong srcAddr; // 48
    public ushort etherType; // 16
  }
}
