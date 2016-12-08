using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Reflection;

namespace HandConverted.P4lang.P4_spec.Mtag_example.Parser
{
  sealed class select_output_port : MatchActionUnitBase<byte, ParsedRepresentation>
  {
    protected override int Size { get { return 4; } }

    protected override byte GetKey(ParsedRepresentation pr)
    {
      return pr.local_metadata.port_type;
    }
  }
}
