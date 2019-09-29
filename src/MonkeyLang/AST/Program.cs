using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public class Program : Node
    {
        public Program()
        {
            Statements = new List<Statement>();
        }
        public List<Statement> Statements { get; set; }

        public string TokenLiteral()
        {
            if (Statements.Count > 0)
                return Statements[0].TokenLiteral();
            else
                return "";
        }
    }
}
