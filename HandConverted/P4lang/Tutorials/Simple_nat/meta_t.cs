using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.Tutorials.Simple_nat
{
  class meta_t
  {
    public bool do_forward; // 1;
    public uint ipv4_sa; // 32;
    public uint ipv4_da; // 32;
    public ushort tcp_sp; // 16;
    public ushort tcp_dp; // 16;
    public uint nhop_ipv4; // 32;
    public uint if_ipv4_addr; // 32;
    public ulong if_mac_addr; // 48;
    public bool is_ext_if; // 1;
    public ushort tcpLength; // 16;
    public byte if_index; // 8;
  }
}
