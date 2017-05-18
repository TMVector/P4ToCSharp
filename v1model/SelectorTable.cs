using System;
using P4ToCSharp.Library;

namespace v1model
{
  [P4Lookup("selector")]
  public sealed class SelectorTable<TKey, TResult> : ILookup<TKey, TResult>
  {
    private TKey[] Keys;
    private TResult[] Values;

    public TResult this[TKey key]
    {
      get
      {
        throw new NotImplementedException();
      }
    }

    public void Add(TKey key, TResult result)
    {
      throw new NotImplementedException();
    }
  }
}
