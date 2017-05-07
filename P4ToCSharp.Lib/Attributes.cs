using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4ToCSharp.Library
{
  // See the attribute guidelines at http://go.microsoft.com/fwlink/?LinkId=85236

  [AttributeUsage(AttributeTargets.Class | AttributeTargets.Interface | AttributeTargets.Enum | AttributeTargets.Method | AttributeTargets.Struct | AttributeTargets.Field,
                  Inherited = true, AllowMultiple = false)]
  public sealed class P4Attribute : Attribute
  {
    public string P4Path { get; }

    public P4Type Type { get; }

    public P4Attribute(string p4Path, P4Type @type)
    {
      P4Path = p4Path;
      Type = @type;
    }
  }

  public enum P4Type
  {
    Action,
    Const,
    Control,
    Enum,
    Error,
    ExternFunction,
    ExternObject,
    Header,
    MatchKind,
    Package,
    Parser,
    Struct
  }

  [AttributeUsage(AttributeTargets.Class,
                  Inherited = true, AllowMultiple = false)]
  public sealed class P4LookupAttribute : Attribute
  {
    public string MatchKind { get; }

    public P4LookupAttribute(string matchKind)
    {
      if (String.IsNullOrWhiteSpace(matchKind))
        throw new ArgumentException("cannot be null or whitespace; must be a match_kind value.", "matchKind");
      MatchKind = matchKind;
    }
    public P4LookupAttribute(Enum matchKind) : this(matchKind.ToString("G"))
    {
    }
  }
}
