using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Reflection;

namespace HandConverted.P4lang.P4_spec.Mtag_example.Parser
{
  sealed class identify_port : MatchActionUnitBase<byte, ParsedRepresentation>
  {
    protected override int Size { get { return 64; } }

    protected override byte GetKey(ParsedRepresentation pr)
    {
      return pr.standard_metadata.ingress_port;
    }
  }
}
