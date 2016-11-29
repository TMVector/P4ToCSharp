using HandConverted.Library;
using HandConverted.P4lang.P4_spec.Mtag_example.Parser;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.Mtag_example
{
  sealed class Control : ControlBase<ParsedRepresentation>
  {
    protected override IParser<ParsedRepresentation> Parser { get; } = new Parser.Parser();

    protected override void Ingress(ParsedRepresentation pr)
    {
      // FIXME implement tables
      // apply(check_mtag)
      // apply(identify_port)
      // apply(select_output_port)
    }
    protected override void Egress(ParsedRepresentation pr)
    {
    }
  }
}
