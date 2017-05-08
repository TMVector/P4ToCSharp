using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  public interface ILookup<TKey,TResult>
  {
    TResult this[TKey key] { get; }
  }

  [P4Lookup("lpm")]
  public sealed class LpmTable<TKey,TResult> : ILookup<TKey,TResult>
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
  }

  [P4Lookup("exact")]
  public sealed class ExactTable<TKey, TResult> : ILookup<TKey, TResult>
  {
    public TResult this[TKey key]
    {
      get
      {
        throw new NotImplementedException();
      }
    }
  }

  [P4Lookup("ternary")]
  public sealed class TernaryTable<TKey, TResult> : ILookup<TKey, TResult>
  {
    public TResult this[TKey key]
    {
      get
      {
        throw new NotImplementedException();
      }
    }
  }
}
