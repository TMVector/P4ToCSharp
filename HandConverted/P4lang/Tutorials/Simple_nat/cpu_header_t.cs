using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.Tutorials.Simple_nat
{
  class cpu_header_t
  {
    public ulong preamble; // 64;
    public byte device; // 8;
    public byte reason; // 8;
    public byte if_index; // 8;
  }
}
