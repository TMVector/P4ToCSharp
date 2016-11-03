using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.Switch
{
  class Control : DataEntryPoint
  {
    public void OnIngress(byte[] data, byte arrivalPort)
    {
      throw new NotImplementedException();

      // Create internal representation for packet

      // Add to queue?
      // OR invoke mirror lookup
    }

    void set_valid_outer_ipv4_packet()
  }
}
