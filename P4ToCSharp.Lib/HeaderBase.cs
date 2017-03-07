using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  public abstract class HeaderBase
  {
    private bool validity;
    public bool isValid() => validity;
    public void setValid() => validity = true;
    public void setInvalid() => validity = false;

    public abstract void Parse(byte[] data, uint offset);
    public abstract void Deparse(byte[] data, uint offset);
  }
}
