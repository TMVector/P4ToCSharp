using System;
using System.Collections.Generic;

namespace HandConverted.Library
{
  public abstract class MatchActionUnitBase<TKey, TPR> where TPR : ParsedRepresentationBase
  {
    private Dictionary<TKey, Action<TPR>> table = new Dictionary<TKey, Action<TPR>>();
    private Action<TPR> default_action = null;

    protected abstract int Size { get; }

    // TODO: add runtime table management interface

    public void Lookup(TPR pr)
    {
      Action<TPR> result;
      if (table.TryGetValue(GetKey(pr), out result))
        result?.Invoke(pr);
      else
        default_action?.Invoke(pr);
    }

    protected abstract TKey GetKey(TPR pr);
  }
}
