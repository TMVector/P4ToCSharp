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
    public sealed class packet_in : Core.packet_in, Architecture.packet_in
    {
      public packet_in(byte[] data) : base(data)
      {
      }
    }

    public sealed class packet_out : Core.packet_out, Architecture.packet_out
    {
      public standard_metadata_t smeta;
      public packet_out(standard_metadata_t smeta) : base()
      {
        this.smeta = smeta;
      }
    }

    [P4(P4Type.ExternFunction, "verify")]
    public static void verify(bool check, P4ToCSharp.Library.error toSignal)
    {
      Core.verify(check, toSignal);
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
      public counter(bit32 size, CounterType type)
      {
        throw new NotImplementedException();
      }
      public void count(bit32 index)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class direct_counter : Architecture.direct_counter
    {
      public direct_counter(CounterType type)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class meter : Architecture.meter
    {
      public meter(bit32 size, CounterType type)
      {
        throw new NotImplementedException();
      }
      public void execute_meter<T>(bit32 index, out T result)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class direct_meter<T> : Architecture.direct_meter<T>
    {
      public direct_meter(CounterType type)
      {
        throw new NotImplementedException();
      }
      public void read(out T result)
      {
        throw new NotImplementedException();
      }
    }

    public sealed class register<T> : Architecture.register<T>
    {
      public register(bit32 size)
      {
        throw new NotImplementedException();
      }
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
      public action_profile(bit32 size)
      {

      }
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
      public action_selector(HashAlgorithm algorithm, bit32 size, bit32 outputWidth)
      {
        throw new NotImplementedException();
      }
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
      public static byte[] GetBytes(string bitString)
      {
        byte[] arr = new byte[bitString.Length / 2];
        for (int i = 0; i < bitString.Length; i += 2)
          arr[i / 2] = Convert.ToByte(bitString.Substring(i, 2), 16);
        return arr;
      }
      public static string GetHex(byte[] arr)
      {
        StringBuilder sb = new StringBuilder(arr.Length * 2);
        for (int i = 0; i < arr.Length; i++)
          sb.Append(arr[i].ToString("X"));
        return sb.ToString();
      }
      public static bool ArrEquals(byte[] a, byte[] b)
      {
        if (a.Length != b.Length) return false;
        for (int i = 0; i < a.Length; i++)
        {
          if (a[i] != b[i]) return false;
        }
        return true;
      }
      public packet_out ProcessPacket(byte[] packet, Parser<H, M> p, VerifyChecksum<H, M> vr, Ingress<H, M> ig, Egress<H, M> eg, ComputeChecksum<H, M> ck, Deparser<H> dep)
      {
        H hdr = default(H);
        M meta = default(M);
        standard_metadata_t smeta = new standard_metadata_t();
        try
        {
          p.apply(new packet_in(packet), out hdr, meta, ref meta, smeta, ref smeta);
        }
        catch (P4Exception ex)
        {
          Console.Error.WriteLine("P4 exception. V1Model not sure how to give to remainder of pipeline.");
          Console.Error.WriteLine(ex.Error);
        }
        vr.apply(hdr, meta, ref meta);
        ig.apply(hdr, ref hdr, meta, ref meta, smeta, ref smeta);
        eg.apply(hdr, ref hdr, meta, ref meta, smeta, ref smeta);
        ck.apply(hdr, ref hdr, meta, ref meta);
        packet_out po = new packet_out(smeta);
        dep.apply(po, hdr);
        // TODO do something with the result
        return po;
      }
      public void Use(Parser<H, M> p, VerifyChecksum<H, M> vr, Ingress<H, M> ig, Egress<H, M> eg, ComputeChecksum<H, M> ck, Deparser<H> dep)
      {
        if (System.Diagnostics.Debugger.IsAttached)
          System.AppDomain.CurrentDomain.ProcessExit += (o,e) => Console.ReadKey();

        var args = Environment.GetCommandLineArgs().Skip(1).ToArray();
        if (args.Length == 0)
        {
          // TODO Process packets from the network
          throw new NotImplementedException("Processing packets from the network is not implemented yet.");
        }
        else
        {
          // Process packets from the STF file, checking outputs are correct
          string stfPath = args[0];
          if (!stfPath.EndsWith(".stf") || !System.IO.File.Exists(stfPath))
          {
            Console.Error.WriteLine("Invalid STF file specified or could not be found.");
            Environment.Exit(1);
          }
          System.IO.FileInfo stfFile = new System.IO.FileInfo(stfPath);
          using (var reader = stfFile.OpenText())
          {
            packet_out lastPacketOut = null;
            while (!reader.EndOfStream)
            {
              string line = reader.ReadLine().Split('#')[0];
              var lineParts = line.Split(' ');
              switch (lineParts[0])
              {
                case "": break;
                case "packet":
                  {
                    var intf = Convert.ToInt32(lineParts[1]);
                    var arr = GetBytes(String.Join("", lineParts.Skip(2)));
                    var po = ProcessPacket(arr, p, vr, ig, eg, ck, dep);
                    lastPacketOut = po;
                    break;
                  }
                case "expect":
                  {
                    var intf = Convert.ToInt32(lineParts[1]);
                    var arr = GetBytes(String.Join("", lineParts.Skip(2)));
                    if (lastPacketOut != null)
                    {
                      if (lastPacketOut.LengthInBits % 8 != 0)
                      {
                        Console.Error.WriteLine("Packet length was not an integral number of bytes ({0})", lastPacketOut.LengthInBits);
                        Environment.Exit(1);
                      }
                      var result = new byte[lastPacketOut.LengthInBits / 8];
                      Array.Copy(lastPacketOut.Data, result, result.Length);
                      if (ArrEquals(arr, result))
                      { }
                      else
                      {
                        Console.Error.WriteLine("Output not what was expected:");
                        Console.Error.WriteLine(" EXP: {0}", GetHex(arr));
                        Console.Error.WriteLine(" RCV: {0}", GetHex(result));
                        Environment.Exit(1);
                      }
                    }
                    else
                    {
                      Console.Error.WriteLine("Expected packet but there was none.");
                      Environment.Exit(1);
                    }
                    break;
                  }
                case "add":
                case "set_default":
                default:
                  Console.Error.WriteLine("Cannot handle command {0}", lineParts[0]);
                  Environment.Exit(1);
                  break;
              }
            }
          }
        }
        Console.WriteLine("Done.");
      }
    }
  }
}
