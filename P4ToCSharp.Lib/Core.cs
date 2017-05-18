using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  public static class Core
  {
    public abstract class packet_in
    {
      private byte[] data;
      private uint bitOffset = 0;

      public packet_in(byte[] data)
      {
        this.data = data;
      }

      public void extract<T>(out T hdr) where T : new()
      {
        if (typeof(HeaderBase).IsAssignableFrom(typeof(T)))
        {
          // T is a header type
          hdr = new T();
          var h = (HeaderBase)(object)hdr;
          h.Parse(data, bitOffset);
          bitOffset += h.Length;
        }
        else
        {
          throw new NotImplementedException(String.Format("Type {0} not handled in packet_in.extract", typeof(T).FullName));
        }
      }
      //void packet_in.extract<T>(out T headerLValue) // NOTE: I am working on the assumption this should actually be `inout`
      //{
      //  //ParserModel.verify(!headerLValue.valid$, error.OverwritingHeader); // Removed
      //  bitsToExtract = sizeofInBits(headerLValue);
      //  lastBitNeeded = this.nextBitIndex + bitsToExtract;
      //  ParserModel.verify(this.lengthInBits >= lastBitNeeded, error.PacketTooShort);
      //  headerLValue = this.data.extractBits(this.nextBitIndex, bitsToExtract);
      //  headerLValue.valid$ = true;
      //  if headerLValue.isNext$ {
      //    verify(headerLValue.nextIndex$ < headerLValue.size, error.StackOutOfBounds);
      //    headerLValue.nextIndex$ = headerLValue.nextIndex$ +1;
      //  }
      //  this.nextBitIndex += bitsToExtract;
      //}

      public void extract<T>(out T variableSizeHeader, bit32 variableFieldSizeInBits)
      {
        throw new NotImplementedException();
      }
      //void packet_in.extract<T>(out T headerLvalue,
      //                          in bit<32> variableFieldSize)
      //{
      //  //ParserModel.verify(!headerLvalue.valid$, error.OverwritingHeader); // Removed
      //  bitsToExtract = sizeOfFixedPart(headerLvalue) + variableFieldSize;
      //  lastBitNeeded = this.nextBitIndex + bitsToExtract;
      //  ParserModel.verify(this.lengthInBits >= lastBitNeeded, error.PacketTooShort);
      //  ParserModel.verify(bitsToExtract <= headerLvalue.maxSize, error.HeaderTooShort);
      //  headerLvalue = this.data.extractBits(this.nextBitIndex, bitsToExtract);
      //  headerLvalue.varbitField.size = variableFieldSize;
      //  headerLvalue.valid$ = true;
      //  if headerLValue.isNext$ {
      //    verify(headerLValue.nextIndex$ < headerLValue.size, error.StackOutOfBounds);
      //    headerLValue.nextIndex$ = headerLValue.nextIndex$ +1;
      //  }
      //  this.nextBitIndex += bitsToExtract;
      //}


      public T lookahead<T>()
      {
        throw new NotImplementedException();
      }
      //T packet_in.lookahead<T>()
      //{
      //  bitsToExtract = sizeof(T);
      //  lastBitNeeded = this.nextBitIndex + bitsToExtract;
      //  ParserModel.verify(this.lengthInBits >= lastBitNeeded, error.PacketTooShort);
      //  T tmp = this.data.extractBits(this.nextBitIndex, bitsToExtract);
      //  return tmp;
      //}


      public void advance(bit32 sizeInBits)
      {
        var lastBitNeeded = bitOffset + sizeInBits.Value;
        verify(data.Length * 8 >= lastBitNeeded, error.PacketTooShort);
        bitOffset += sizeInBits.Value;
      }
      //void packet_in.advance(bit<32> bits)
      //{
      //  lastBitNeeded = this.nextBitIndex + bits;
      //  ParserModel.verify(this.lengthInBits >= lastBitNeeded, error.PacketTooShort);
      //  this.nextBitIndex += bits;
      //}


      public bit32 length()
      {
        return (uint)data.Length * 8;
      }
    }

    public abstract class packet_out
    {
      byte[] data;
      UInt32 lengthInBits = 0;
      public byte[] Data { get { return data; } }
      public UInt32 LengthInBits { get { return lengthInBits; } }

      public packet_out()
      {
        data = new byte[1522]; // FIXME do we need to make this so big?
      }

      public void emit<T>(T hdr)
      {
        emit(true, hdr);
      }

      public void emit<T>(bool condition, T obj)
      {
        if (obj == null || !condition) return;

        if (typeof(IBitString).IsAssignableFrom(typeof(T)))
        {
          throw new NotImplementedException();
          // T is a base type
          var bitstring = (IBitString)(object)obj;
          // Written starting with the most-significant bit
        }
        else if (typeof(HeaderBase).IsAssignableFrom(typeof(T)))
        {
          // T is a header type
          var header = (HeaderBase)(object)obj;
          // Check valid
          if (header != null && header.isValid())
          {
            // Emit header
            header.Deparse(data, lengthInBits);
            this.lengthInBits += header.Length;
          }
        }
        else if (typeof(T) == typeof(HeaderBase[]))
        {
          throw new NotImplementedException();
          // T is a header stack
          var headerStack = (HeaderBase[])(object)obj;
          foreach (var header in headerStack)
          {
            if (header.isValid()) // Is valid
              emit(header);
          }
        }
        else if (typeof(IStruct).IsAssignableFrom(typeof(T)))
        {
          throw new NotImplementedException();
          // T is a struct type
          var @struct = (IStruct)(object)obj;
          // Need to emit each field in T in order... Reflection? D: Add a method to the struct? Like we do with Extract/Write on headers
        }
        else
        {
          throw new NotImplementedException(String.Format("Type {0} not handled in packet_in.extract", typeof(T).FullName));
        }
      }
    }
    //packet_out {
    //  byte[] data;
    //  unsigned lengthInBits;
    //  void initializeForWriting()
    //  {
    //    this.data.clear();
    //    this.lengthInBits = 0;
    //  }
    //  // append entire header if it is valid
    //  // T must be a header type
    //  void emit<T>(T header)
    //  {
    //    this.emit(header.valid$, header);
    //  }
    //  // append the data to the packet if the condition is true
    //  void emit<T>(bool cond, T data)
    //  {
    //    if (!cond) return;
    //    this.data.append(data);
    //    this.lengthInBits += data.lengthInBits;
    //  }
    //}
    // The first version only accepts headers, and the second one accepts arbitrary data.
    //We describe the two-argument emit method.For a base type T, emit:
    // • does nothing if the condition is false,
    // • otherwise it appends the data value to the tail of the packet_out.
    //For derived type, emit recursively proceeds on fields:
    // • If the argument is a header, its validity bit is AND-ed with the condition bit to determine; if
    //   the result is false no action is taken
    // • If the argument is a header stack, the emit statement is applied to each component of the
    //   stack starting from the element with index 0.
    // • If the argument is a struct containing multiple fields, the emit is recursively applied to each
    //   component of the struct in the order of their declaration in the struct.
    //Appending a bit-string or integer value to a packet_out writes the value starting with the mostsignificant
    //bit.This process is the inverse of data extraction.


    public static void verify(bool check, P4ToCSharp.Library.error toSignal)
    {
      if (!check)
        throw new P4Exception(toSignal);
    }
  }
}
