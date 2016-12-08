/* ex1.cs
 * 
 * Hand converted C# code intended to be representative of ex1.p4
 * 
 * Copyright 2016 Jonny Shipton
 * 
control p()
{
    apply {
        int<32> a = 32s1;
        int<32> b = 32s1;
        int<32> c;
        bit<32> f;
        bit<16> e;
        bool    d;
    
        c = +b;
        c = -b;
        f = ~(bit<32>)b;
        f = (bit<32>)a & (bit<32>)b;
        f = (bit<32>)a | (bit<32>)b;
        f = (bit<32>)a ^ (bit<32>)b;
        f = (bit<32>)a << (bit<32>)b;
        f = (bit<32>)a >> (bit<32>)b;
        f = (bit<32>)a >> 4;
        f = (bit<32>)a << 6;
        c = a * b;
        e = ((bit<32>)a)[15:0];
        f = e ++ e;
        c = d ? a : b;
    
        d = a == b;
        d = a != b;
        d = a < b;
        d = a > b;
        d = a <= b;
        d = a >= b;
    
        d = !d;
        d = d && d;
        d = d || d;
        d = d == d;
        d = d != d;
    }
}

 */

using System;
using HandConverted.Library;

namespace HandConverted.P4lang.P4c.P4_16_samples
{
	public class Expression
	{
		public void p()
		{
			int a = 1;
			int b = 1;
			int c;
			uint f;
			ushort e;
			bool d;

			c = +b;
			c = -b;
			f = ~(uint)b;
			f = (uint)a & (uint)b;
			f = (uint)a | (uint)b;
			f = (uint)a ^ (uint)b;
			f = (uint)a << (int)(uint)b;
			f = (uint)a >> (int)(uint)b;
			f = (uint)a >> 4;
			f = (uint)a << 6;
			c = a * b;
			e = (ushort)(((uint)a) & 0xFFFF);
			f = (uint)BitHelper.Concat(e, e, 16);
      c = d ? a : b; // FIXME d uninitialised, but P4 doesn't specifiy behaviour in this case?
  
      d = a == b;
      d = a != b;
      d = a < b;
      d = a > b;
      d = a <= b;
      d = a >= b;
  
      d = !d;
      d = d && d;
      d = d || d;
      d = d == d;
      d = d != d;
		}
	}
}
