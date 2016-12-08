using HandConverted.Library;
using System;
using System.Collections.Generic;
using System.Reflection;

namespace HandConverted.P4lang.P4_spec.Mtag_example.Parser
{
/*
table check_mtag {
    reads {
        mtag : valid; // Was mtag parsed?
    }
    actions { // Each table entry specifies *one* action
        common_drop_pkt;           // Deny if policy is to drop
        common_copy_pkt_to_cpu;    // Deny if policy is to go to CPU
        no_op;                     // Accept action
    }
    size : 1;
}
*/

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
