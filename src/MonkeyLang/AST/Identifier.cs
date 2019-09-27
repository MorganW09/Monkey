using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public class Identifier : Expression
    {
        public Token Token { get; set; }
        public string Value { get; set; }

        public override Node ExpressionNode()
        {
            return null;
        }

        public override string TokenLiteral()
        {
            return Token.Literal;
        }
    }
}
