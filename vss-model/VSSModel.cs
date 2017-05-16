using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using P4ToCSharp.Library;
using static vss_model.Architecture;

namespace vss_model
{
  public static class VSSModel
  {
    [P4(P4Type.ExternFunction, "verify")]
    public static void verify(bool condition, P4ToCSharp.Library.error err)
    {
      Core.verify(condition, err);
    }

    public sealed class packet_in : Core.packet_in, Architecture.packet_in
    {
      public packet_in(byte[] data) : base(data)
      {
      }
    }

    public sealed class packet_out : Core.packet_out, Architecture.packet_out
    {
    }

    public class VSS_impl<H> : Pax.ByteBased_PacketProcessor, VSS<H>
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
      public packet_out ProcessPacket(byte[] packet, Parser<H> p, Pipe<H> map, Deparser<H> d)
      {
        H hdr = default(H);
        P4ToCSharp.Library.error parserError = Architecture.error.NoError; // FIXME move lib.error to Core.error?
        try
        {
          p.apply(new packet_in(packet), out hdr);
        }
        catch (P4Exception ex)
        {
          parserError = ex.Error;
          Console.Error.WriteLine("P4 exception. " + ex.Error);
        }
        var inCtrl = new InControl();
        OutControl outCtrl;
        map.apply(hdr, ref hdr, parserError, inCtrl, out outCtrl);
        packet_out po = new packet_out();
        d.apply(hdr, ref hdr, po);
        // TODO do something with the result
        return po;
      }

      public override void process_packet(int in_port, byte[] packet)
      {
        throw new NotImplementedException();


      }


      Parser<H> p; Pipe<H> map; Deparser<H> d;
      public void Use(Parser<H> p, Pipe<H> map, Deparser<H> d)
      {
        this.p = p;
        this.map = map;
        this.d = d;
      }

      public void Run()
      {
        if (System.Diagnostics.Debugger.IsAttached)
          System.AppDomain.CurrentDomain.ProcessExit += (o, e) => Console.ReadKey();

        var args = Environment.GetCommandLineArgs().Skip(1).ToArray();
        if (args.Length == 0)
        {
          Console.Error.WriteLine("Please specify a .STF file. To process packets from the network, please run via Pax.");
          Environment.Exit(1);
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
                    var po = ProcessPacket(arr, p, map, d);
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

    public sealed class Ck16_impl : Ck16
    {
      private Int32 sum32;

      public Ck16_impl()
      {
        clear();
      }

      public void clear()
      {
        // FIXME is the correct starting value?
        sum32 = 0;
      }

      public bit16 get()
      {
        sum32 = (sum32 & 0xFFFF) + (sum32 >> 16);
        sum32 = (sum32 & 0xFFFF) + (sum32 >> 16);
        return (ushort)(~sum32 & 0xFFFF);
      }

      private void update(UInt64 data)
      {
        unchecked
        {
          int a = (int)data;
          sum32 += (a & 0xFFFF) + (a >> 16);
          int b = (int)(data >> 32);
          sum32 += (b & 0xFFFF) + (b >> 16);
        }
      }
      public void update<T>(T data)
      {
        if (typeof(T) == typeof(IBitString))
        {
          var bitstring = (IBitString)data;
          update(bitstring.Value);
        }
        else if (typeof(T) == typeof(IStruct))
        {
          var @struct = (IStruct)data;
          throw new NotImplementedException("Ck16.update operation on structs is not implemented.");
          //ERROR
        }
        else if (typeof(T) == typeof(HeaderBase))
        {
          var header = (HeaderBase)(object)data;
          throw new NotImplementedException("Ck16.update operation on headers is not implemented.");
          //ERROR
        }
        else if (typeof(T) == typeof(Tuple<>)) // FIXME would array be better? Items aren't typed then...
        {
          // TODO handle tuples
          throw new NotImplementedException("Ck16.update operation on lists is not implemented.");
        }
        else
        {
          throw new NotImplementedException(String.Format("Ck16.update operation is not implemented for type {0}.", data.GetType().FullName));
        }
      }
    }
  }
}
