using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example.Mtag_aggregation
{
  static class Action
  {
    public static void use_mtag_up1(ParsedRepresentation pr)
    {
      pr.standard_metadata.egress_spec = pr.mtag.up1;
    }
    public static void use_mtag_up2(ParsedRepresentation pr)
    {
      pr.standard_metadata.egress_spec = pr.mtag.up2;
    }
    public static void use_mtag_down1(ParsedRepresentation pr)
    {
      pr.standard_metadata.egress_spec = pr.mtag.down1;
    }
    public static void use_mtag_down2(ParsedRepresentation pr)
    {
      pr.standard_metadata.egress_spec = pr.mtag.down2;
    }
  }
}
