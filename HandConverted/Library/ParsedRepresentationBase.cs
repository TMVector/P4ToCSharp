using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.Library
{
  public abstract class ParsedRepresentationBase
  {
    public standard_metadata_t standard_metadata;

    public ParsedRepresentationBase()
    {
      standard_metadata = new standard_metadata_t();
    }
  }
}
