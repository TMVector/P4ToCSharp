/* ex1.cs
 * 
 * Hand converted C# code intended to be representative of ex1.p4
 * 
 * Copyright 2016 Jonny Shipton
 */

using System;

namespace HandConverted.P4lang.P4c.P4_16_samples
{
	public class ex1
	{
		public const byte x = 10;

		public sealed class S
		{
			public byte s;
		}

		public static void a(S w, out byte z)
		{
			z = (byte)(x + w.s);
		}
	}
}
