using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  class vlan_t
  {
    public byte pcp; // 3;
    public bool cfi; // 1;
    public ushort vid; // 12;
    public ushort ethertype; // 16;
  }
}
