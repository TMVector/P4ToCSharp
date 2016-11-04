using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  class ParsedRepresentation
  {
    byte[] data;

    /* Headers */
    public ethernet_t ethernet;
    public vlan_t vlan;
    public mtag_t mtag;
    public ipv4_t ipv4;

    /* Metadata */
    standard_metadata_t standard_metadata;
    local_metadata_t local_metadata;

    /* Parser */
    public void Start()
    {
      ParseEthernet(0);
    }
    void ParseEthernet(uint offset)
    {
      ethernet = new ethernet_t(offset);
      ethernet.Extract(data);
      var latest = ethernet;

      switch (latest.etherType)
      {
        case 0x8100:
          ParseVlan(offset + latest.length);
          break;
        case 0x0800:
          ParseIpv4(offset + latest.length);
          break;
        default:
          return; // ingress
      }
    }
    void ParseVlan(uint offset)
    {
      vlan = new vlan_t(offset);
      vlan.Extract(data);
      var latest = vlan;

      switch (latest.ethertype)
      {
        case 0x800:
          ParseIpv4(offset + latest.length);
          break;
        default:
          return; // ingress
      }
    }
    void ParseMtag(uint offset)
    {
      mtag = new mtag_t(offset);
      mtag.Extract(data);
      var latest = mtag;

      switch (mtag.ethertype)
      {
        case 0x800:
          ParseIpv4(offset + latest.length);
          break;
        default:
          return; // ingress
      }
    }
    void ParseIpv4(uint offset)
    {
      ipv4 = new ipv4_t(offset);
      ipv4.Extract(data);
      var latest = ipv4;

      return; // ingress
    }

    /* Actions */
    void common_copy_pkt_to_cpu(ushort cpu_code, bool bad_packet)
    {
      local_metadata.copy_to_cpu = true;
      local_metadata.cpu_code = cpu_code;
      local_metadata.bad_packet = bad_packet;
    }
    void common_drop_pkt(bool do_copy, ushort cpu_code, bool bad_packet)
    {
      local_metadata.copy_to_cpu = do_copy;
      local_metadata.cpu_code = cpu_code;
      local_metadata.bad_packet = bad_packet;
      drop(); // FIXME - what is this?
    }
    void common_set_port_type(byte port_type, bool ingress_error)
    {
      local_metadata.port_type = port_type;
      local_metadata.ingress_error = ingress_error;
    }
  }
}
