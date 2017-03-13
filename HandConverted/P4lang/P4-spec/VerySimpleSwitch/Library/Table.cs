using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.P4lang.P4_spec.VerySimpleSwitch.Library
{
  public sealed class LpmTable<TKey,TResult>
  {
    private TKey[] Keys;
    private TResult[] Values;

    public TResult Lookup(TKey key)
    {
    }
  }
  public sealed class ExactTable<TKey, TResult>
  {
    public TResult Lookup(TKey key)
    {

    }
  }
}
