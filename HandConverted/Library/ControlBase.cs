using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted.Library
{
  public abstract class ControlBase<PR> where PR : ParsedRepresentationBase
  {
    protected abstract IParser<PR> Parser { get; } // Would be nice to express the non-null requirement to compiler somehow

    public void HandlePacket(byte[] data /* + more args here (probably all in EventArgs) */)
    {
      PR pr = Parser.Start(data);

      Ingress(pr);

      // FIXME: Shouldn't there be queueing and buffering in between stages?

      Egress(pr);

      // TODO: sending packets
    }

    protected abstract void Ingress(PR pr);

    protected abstract void Egress(PR pr);
  }
}
