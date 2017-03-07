using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  public struct BitString
  {
    public byte BitLength { get; }

    public ulong Value { get; }

    public BitString(int bitLength, ulong value)
    {
      BitLength = (byte)bitLength;
      Value = value;
    }
  }
}
