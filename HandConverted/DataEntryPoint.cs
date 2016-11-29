using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HandConverted
{
  interface DataEntryPoint
  {
    void OnIngress(byte[] data, byte arrivalPort);
  }
}
