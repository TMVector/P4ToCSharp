using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  public class error
  {
    private static int counter = 0;

    public int Value { get; }

    protected error()
    {
      Value = counter++;
    }

    public override bool Equals(object obj)
    {
      if (obj is error)
        return ((error)obj).Value == Value;
      else
        return false;
    }
    public override int GetHashCode()
    {
      return Value;
    }
  }

  public class P4Exception : Exception
  {
    public error Error { get; }

    public P4Exception(error error)
    {
      Error = error;
    }
  }
}
