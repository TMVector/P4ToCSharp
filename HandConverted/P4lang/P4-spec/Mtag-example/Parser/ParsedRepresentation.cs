using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example.Parser
{
  sealed class ParsedRepresentation : ParsedRepresentationBase
  {
    public byte[] data;

    /* Headers */
    public ethernet_t ethernet;
    public vlan_t vlan;
    public mtag_t mtag;
    public ipv4_t ipv4;

    /* Metadata */
    public local_metadata_t local_metadata;

    public ParsedRepresentation(byte[] data)
    {
      this.data = data;
    }
  }
}
