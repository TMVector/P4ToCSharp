using HandConverted.P4lang.P4_spec.Mtag_example.Parser;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example.Actions
{
  static class Action
  {
    public static void common_copy_pkt_to_cpu(ParsedRepresentation pr, ushort cpu_code, bool bad_packet)
    {
      // FIXME: action primitives should be executed in parallel
      pr.local_metadata.copy_to_cpu = true;
      pr.local_metadata.cpu_code = cpu_code;
      pr.local_metadata.bad_packet = bad_packet;
    }
    public static void common_drop_pkt(ParsedRepresentation pr, bool do_copy, ushort cpu_code, bool bad_packet)
    {
      pr.local_metadata.copy_to_cpu = do_copy;
      pr.local_metadata.cpu_code = cpu_code;
      pr.local_metadata.bad_packet = bad_packet;
      //pr.drop(); // FIXME - how do we deal with it behaving differently in ingress and egress?
    }
    public static void common_set_port_type(ParsedRepresentation pr, byte port_type, bool ingress_error)
    {
      pr.local_metadata.port_type = port_type;
      pr.local_metadata.ingress_error = ingress_error;
    }
  }
}
