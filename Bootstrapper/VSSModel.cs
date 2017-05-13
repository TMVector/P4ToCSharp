using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using P4ToCSharp.Library;
using static Architecture;

namespace Bootstrapper
{
  public static class VSSModel
  {
    [P4(P4Type.ExternFunction, "verify")]
    public static void verify(bool condition, P4ToCSharp.Library.error err)
    {
      if (!condition)
        throw new P4Exception(err);
    }

    public sealed class packet_in_impl : packet_in
    {
      public void extract<T>(out T hdr)
      {
        throw new NotImplementedException();
      }
      //void packet_in.extract<T>(out T headerLValue)
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
        throw new NotImplementedException();
      }
      //void packet_in.advance(bit<32> bits)
      //{
      //  lastBitNeeded = this.nextBitIndex + bits;
      //  ParserModel.verify(this.lengthInBits >= lastBitNeeded, error.PacketTooShort);
      //  this.nextBitIndex += bits;
      //}

      public bit32 length()
      {
        throw new NotImplementedException();
      }
    }

    public sealed class packet_out_impl : packet_out
    {
      byte[] data;
      UInt32 lengthInBits = 0;

      public packet_out_impl()
      {
        // TODO init data array
      }

      public void emit<T>(T hdr)// where T : HeaderBase
      {
        //this.emit(hdr.valid$, hdr); // If hdr.valid$ ≝ (hdr != null), then emit'2 will need a null check also
      }

      public void emit<T>(bool condition, T data)
      {
        //if (!condition) return;

        //if (typeof(T).IsAssignableFrom(typeof(IBitString)))
        //{
        //  // T is a base type
        //  var bitstring = (IBitString)(object)data;
        //  // Written starting with the most-significant bit
        //}
        //else if (typeof(T).IsAssignableFrom(typeof(HeaderBase)))
        //{
        //  // T is a header type
        //  var header = (HeaderBase)(object)data;
        //  // Check valid
        //  if (header.isValid())
        //  {
        //    // TODO Emit header
        //  }
        //}
        //else if (typeof(T) == typeof(HeaderBase[]))
        //{
        //  // T is a header stack
        //  var headerStack = (HeaderBase[])(object)data;
        //  foreach (var header in headerStack)
        //  {
        //    if (header.isValid()) // Is valid
        //      emit(header);
        //  }
        //}
        //else if (typeof(T).IsAssignableFrom(typeof(IStruct)))
        //{
        //  // T is a struct type
        //  var @struct = (IStruct)(object)data;
        //  // Need to emit each field in T in order... Reflection? D: Add a method to the struct? Like we do with Extract/Write on headers
        //}
        //this.data.append(data);
        //this.lengthInBits += data.lengthInBits;
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

    public sealed class VSS_impl<H> : VSS<H>
    {
      public void Use(Parser<H> p, Pipe<H> map, Deparser<H> d)
      {
        // TODO the arch should be set up here to process packets
        // Expected to be called from the Main method?
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
        else if (typeof(T) == typeof(Tuple<>))
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
