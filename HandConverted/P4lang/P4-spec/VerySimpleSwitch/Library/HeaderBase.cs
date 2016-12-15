using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library
{
  public abstract class HeaderBase
  {
    public abstract void Extract(byte[] data, uint offset);
    public abstract void Write(byte[] data, uint offset);
  }
}
