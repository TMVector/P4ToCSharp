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
      public const byte REAL_PORT_COUNT       = 8;
      public const byte RECIRCULATE_IN_PORT   = 0xD;
      public const byte CPU_IN_PORT           = 0xE;
      public const byte DROP_PORT             = 0xF;
      public const byte CPU_OUT_PORT          = 0xE;
      public const byte RECIRCULATE_OUT_PORT  = 0xD;
      public const byte MAX_PORT = 0xC;

      public static readonly byte[] PortMapping =
      {
        0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC
      };


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

      private struct ProcessResult { public packet_out po; public OutControl outCtrl; }
      private ProcessResult ProcessPacket(int in_port, byte[] packet)
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
        var inCtrl = new InControl() { inputPort = new bit4((byte)in_port) };
        OutControl outCtrl;
        map.apply(hdr, ref hdr, parserError, inCtrl, out outCtrl);
        packet_out po = new packet_out();
        d.apply(hdr, ref hdr, po);
        // TODO do something with the result
        return new ProcessResult() { po = po, outCtrl = outCtrl };
      }

      public override void process_packet(int in_port, byte[] packet)
      {
        Console.WriteLine("Packet received.");
        Console.WriteLine("{0} {1}", in_port, GetHex(packet));

        var result = ProcessPacket(in_port, packet);
        var po = result.po;
        var outCtrl = result.outCtrl;
        switch (outCtrl.outputPort)
        {
          case DROP_PORT:
            // Drop => don't send => do nothing
            Console.WriteLine("Dropping.");
            break;
          case CPU_OUT_PORT:
            {
              Console.WriteLine("Sending to CPU.");
              // TODO handle CPU packets
              throw new NotImplementedException("Sending packets to CPU is not supported yet.");
            }
          case RECIRCULATE_OUT_PORT:
            {
              Console.WriteLine("Recirculating.");
              // TODO handle recirculation
              throw new NotImplementedException("Recirculating packets is not supported yet.");
            }
          default:
            {
              int outIntf = PortMapping[outCtrl.outputPort.Value];
              if (outIntf < 0 || outIntf > MAX_PORT)
              {
                Console.WriteLine("Invalid port {0}. Dropping.", outIntf);
                break;
              }
              Console.WriteLine("Sending packet.");
              send_packet(outIntf, po.Data, (int)(po.LengthInBits / 8));
              break;
            }
        }

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
            ProcessResult? lastResult = null;
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
                    lastResult = ProcessPacket(intf, arr);
                    break;
                  }
                case "expect":
                  {
                    var intf = Convert.ToInt32(lineParts[1]);
                    var arr = GetBytes(String.Join("", lineParts.Skip(2)));
                    if (lastResult != null)
                    {
                      if (lastResult.Value.outCtrl.outputPort.Value != intf)
                      {
                        Console.Error.WriteLine("Output interface did not match expected ({0} <> {1})", lastResult.Value.outCtrl.outputPort.Value, intf);
                        Environment.Exit(1);
                      }
                      if (lastResult.Value.po.LengthInBits % 8 != 0)
                      {
                        Console.Error.WriteLine("Packet length was not an integral number of bytes ({0})", lastResult.Value.po.LengthInBits);
                        Environment.Exit(1);
                      }
                      var result = new byte[lastResult.Value.po.LengthInBits / 8];
                      Array.Copy(lastResult.Value.po.Data, result, result.Length);
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
