using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SharpPcap;

namespace HandConverted.Library
{
  public abstract class ControlBase<PR> : Pax.IHostbased_PacketProcessor where PR : ParsedRepresentationBase
  {
    protected abstract IParser<PR> Parser { get; } // Would be nice to express the non-null requirement to compiler somehow

    public void HandlePacket(byte[] data, int in_port)
    {
      PR pr = Parser.Start(data);
      pr.standard_metadata.ingress_port = in_port;

      Ingress(pr);

      Egress(pr);

      // TODO: sending packets
    }

    protected abstract void Ingress(PR pr);

    protected abstract void Egress(PR pr);

    public void packetHandler(object sender, CaptureEventArgs e)
    {
      int in_port = Pax.PaxConfig.rdeviceMap[e.Device.Name];
      HandlePacket(e.Packet.Data, in_port);
      e.Device.SendPacket(new PacketDotNet.Raw); // How to send a packet?
    }
  }
}
