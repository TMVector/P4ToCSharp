using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example.Parser
{
  sealed class Parser : IParser<ParsedRepresentation>
  {
    public ParsedRepresentation Start(byte[] data)
    {
      ParsedRepresentation pr = new ParsedRepresentation(data);
      Ethernet(pr, 0);
      return pr;
    }

    void Ethernet(ParsedRepresentation pr, uint offset)
    {
      pr.ethernet = new ethernet_t(offset);
      pr.ethernet.Extract(pr.data);
      var latest = pr.ethernet;

      switch (latest.etherType)
      {
        case 0x8100:
          Vlan(pr, offset + latest.length);
          break;
        case 0x0800:
          Ipv4(pr, offset + latest.length);
          break;
        default:
          return; // ingress
      }
    }

    void Vlan(ParsedRepresentation pr, uint offset)
    {
      pr.vlan = new vlan_t(offset);
      pr.vlan.Extract(pr.data);
      var latest = pr.vlan;

      switch (latest.ethertype)
      {
        case 0x800:
          Ipv4(pr, offset + latest.length);
          break;
        default:
          return; // ingress
      }
    }

    void Mtag(ParsedRepresentation pr, uint offset)
    {
      pr.mtag = new mtag_t(offset);
      pr.mtag.Extract(pr.data);
      var latest = pr.mtag;

      switch (latest.ethertype)
      {
        case 0x800:
          Ipv4(pr, offset + latest.length);
          break;
        default:
          return; // ingress
      }
    }

    void Ipv4(ParsedRepresentation pr, uint offset)
    {
      pr.ipv4 = new ipv4_t(offset);
      pr.ipv4.Extract(pr.data);
      var latest = pr.ipv4;

      return; // ingress
    }

  }
}
