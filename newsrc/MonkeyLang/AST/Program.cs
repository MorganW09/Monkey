using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public class Program : Node
    {
        public Statement[] Statements { get; set; }

        public string TokenLiteral()
        {
            if (Statements.Length > 0)
                return Statements[0].TokenLiteral();
            else
                return "";
        }
    }
}
