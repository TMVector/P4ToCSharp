using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Reflection;

namespace HandConverted.P4lang.P4_spec.Mtag_example.Parser
{
  sealed class check_mtag : MatchActionUnitBase<bool, ParsedRepresentation>
  {
    protected override int Size { get { return 1; } }

    protected override bool GetKey(ParsedRepresentation pr)
    {
      return pr.mtag != null;
    }

    MethodInfo[] actions =
    {
      ((Action<ParsedRepresentation, bool, ushort, bool>)Actions.Action.common_drop_pkt).Method,
      ((Action<ParsedRepresentation, ushort, bool>)Actions.Action.common_copy_pkt_to_cpu).Method,
      ((System.Action)Action.no_op).Method
    };
  }
}
