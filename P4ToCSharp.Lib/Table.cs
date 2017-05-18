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
    void Add(TKey key, TResult result);
  }

  [P4Lookup("exact")]
  public sealed class ExactTable<TKey, TResult> : ILookup<TKey, TResult> where TResult : class
  {
    Dictionary<TKey, TResult> lu = new Dictionary<TKey, TResult>();

    public TResult this[TKey key]
    {
      get
      {
        TResult result;
        return lu.TryGetValue(key, out result) ? result : null;
      }
    }

    public void Add(TKey key, TResult result)
    {
      lu.Add(key, result);
    }
  }

  [P4Lookup("lpm")]
  public sealed class LpmTable<TKey,TResult> : ILookup<TKey,TResult> where TResult : class
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

  [P4Lookup("ternary")]
  public sealed class TernaryTable<TKey, TResult> : ILookup<TKey, TResult> where TResult : class
  {
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
