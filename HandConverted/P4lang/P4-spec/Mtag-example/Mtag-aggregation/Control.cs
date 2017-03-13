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

    private check_mtag check_mtag = new check_mtag();
    private identify_port identify_port = new identify_port();
    private select_output_port select_output_port = new select_output_port();

    protected override void Ingress(ParsedRepresentation pr)
    {
      check_mtag.Lookup(pr);
      identify_port.Lookup(pr);
      select_output_port.Lookup(pr);
    }
    protected override void Egress(ParsedRepresentation pr)
    {
    }
  }
}
