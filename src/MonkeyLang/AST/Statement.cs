using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public abstract class Statement : Node
    {
        //public string TokenLiteral()
        //{
        //    throw new NotImplementedException();
        //}

        public abstract Node StatementNode();

        public abstract string TokenLiteral();
    }
}
