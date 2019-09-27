using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public interface Node
    {
        string TokenLiteral();
    }
}
