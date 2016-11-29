using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.Library
{
  public interface IParser<PR> where PR : ParsedRepresentationBase
  {
    PR Start(byte[] data); // FIXME need more info here, e.g. for standard_metadata (or the control could set that)
  }
}
