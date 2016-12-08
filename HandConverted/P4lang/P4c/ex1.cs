/* ex1.cs
 * 
 * Hand converted C# code intended to be representative of ex1.p4
 * 
 * Copyright 2016 Jonny Shipton
 * 
const bit<8> x = 10;
struct S { bit<8> s; }
action a(in S w, out bit<8> z) 
{
    z = x + w.s;
}
 */

using System;

namespace HandConverted.P4lang.P4c
{
	public class ex1
	{
		const byte x = 10;

		sealed class S
		{
			public byte s;
		}

		static void a(S w, out byte z)
		{
			z = (byte)(x + w.s);
		}
	}
}
