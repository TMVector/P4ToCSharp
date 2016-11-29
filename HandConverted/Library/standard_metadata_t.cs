using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.Library
{
  public sealed class standard_metadata_t
  {
    // Specified on p45 of the v1.1 spec

    // FIXME: the get/set specified here is only from the POV of a user - some fields cannot be specified at instantiation
    public byte ingress_port { get; }
    public ushort packet_length { get; }
    public byte egress_spec { get; set; }
    public byte egress_port { get; }
    public byte egress_instance { get; }
    public PacketInstanceType instance_type { get; }
    public ushort parser_status { get; }
    public string parser_error_location { get; }
  }
}
