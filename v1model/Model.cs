using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using P4ToCSharp.Library;
using static v1model.Architecture;

namespace v1model
{
  public static class Model
  {
    public sealed class packet_in : Architecture.packet_in
    {
      public void extract<T>(out T hdr)
      {
        throw new NotImplementedException();
      }
      public void extract<T>(out T variableSizeHeader, bit32 variableFieldSizeInBits)
      {
        throw new NotImplementedException();
      }
      public T lookahead<T>()
      {
        throw new NotImplementedException();
      }
      public void advance(bit32 sizeInBits)
      {
        throw new NotImplementedException();
      }
      public bit32 length()
      {
        throw new NotImplementedException();
      }
    }

    public sealed class packet_out : Architecture.packet_out
    {
      public void emit<T>(T hdr)
      {
        throw new NotImplementedException();
      }
      public void emit<T>(bool condition, T data)
      {
        throw new NotImplementedException();
      }
    }

    [P4(P4Type.ExternFunction, "verify")]
    public static void verify(bool check, P4ToCSharp.Library.error toSignal)
    {
      throw new NotImplementedException();
    }

    public sealed class Checksum16 : Architecture.Checksum16
    {
      public bit16 get<D>(D data)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class counter : Architecture.counter
    {
      public void count(bit32 index)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class direct_counter : Architecture.direct_counter
    {
    }

    public sealed class meter : Architecture.meter
    {
      public void execute_meter<T>(bit32 index, out T result)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class direct_meter<T> : Architecture.direct_meter<T>
    {
      public void read(out T result)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class register<T> : Architecture.register<T>
    {
      public void read(out T result, bit32 index)
      {
        throw new NotImplementedException();
      }
      public void write(bit32 index, T value)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class action_profile : Architecture.action_profile
    {
    }

    [P4(P4Type.ExternFunction, "random")]
    public static void random(bit32 result, bit32 lo, bit32 hi)
    {
      throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "digest")]
    public static void digest<T>(bit32 receiver, T data)
    {
      throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "mark_to_drop")]
    public static void mark_to_drop()
    {
      throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "hash")]
    public static void hash<O, T, D, M>(O result, HashAlgorithm algo, T @base, D data, M max)
    {
      throw new NotImplementedException();
    }

    public sealed class action_selector : Architecture.action_selector
    {
    }

    [P4(P4Type.ExternFunction, "resubmit")]
    public static void resubmit<T>(T data)
    {
      throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "recirculate")]
    public static void recirculate<T>(T data)
    {
      throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "clone")]
    public static void clone(CloneType type, bit32 session)
    {
      throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "clone3")]
    public static void clone3<T>(CloneType type, bit32 session, T data)
    {
      throw new NotImplementedException();
    }

    [P4(P4Type.ExternFunction, "truncate")]
    public static void truncate(bit32 length)
    {
      throw new NotImplementedException();
    }

    public sealed class V1Switch<H, M> : Architecture.V1Switch<H, M>
    {
      public void Use(Parser<H, M> p, VerifyChecksum<H, M> vr, Ingress<H, M> ig, Egress<H, M> eg, ComputeChecksum<H, M> ck, Deparser<H> dep)
      {
        throw new NotImplementedException();
      }
    }
  }
}
