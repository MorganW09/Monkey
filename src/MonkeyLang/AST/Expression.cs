using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public abstract class Expression : Node
    {
        public abstract string TokenLiteral();

        public abstract void ExpressionNode();
    }
}
