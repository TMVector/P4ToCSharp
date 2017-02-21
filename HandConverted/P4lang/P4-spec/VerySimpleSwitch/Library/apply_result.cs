using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library
{
  public abstract class apply_result<T>
  {
    public bool hit { get; }
    public T action_run { get; }

    protected apply_result(bool hit, T action_run)
    {
      this.hit = hit;
      this.action_run = action_run;
    }
  }
}
