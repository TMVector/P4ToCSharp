using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.Semantics
{
  class ParameterDirections
  {
    struct A { public int v; public A(int vv) { v = vv; } }

    public static void Main(string[] args)
    {
      A a = new A(0);
      Console.WriteLine(f(ref a, g(ref a), h(ref a)).v);
      Console.WriteLine(a.v);
    }

    static A f(ref A p, A q, A r)
    {
      Console.WriteLine("f({0}, {1}, {2})", p.v, q.v, r.v);
      p.v = 2;
      return new A(p.v + q.v + r.v);
    }
    static A g(ref A y)
    {
      Console.WriteLine("g({0})", y.v);
      y = new A(y.v + 1);
      return new A(y.v + 5);
    }
    static A h(ref A z)
    {
      Console.WriteLine("h({0})", z.v);
      z = new A(z.v + 100);
      return new A(z.v + 50);
    }
  }
}
