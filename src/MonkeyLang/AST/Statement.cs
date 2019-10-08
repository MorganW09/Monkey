using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public abstract class Statement : Node
    {
        public abstract void StatementNode();

        public abstract string TokenLiteral();
    }
}
