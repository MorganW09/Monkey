using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public class Identifier : Expression
    {
        public Identifier(Token token)
        {
            Token = token;
            Value = token.Literal;
        }
        public Token Token { get; }
        public string Value { get;}

        public override void ExpressionNode()
        {
        }

        public override string TokenLiteral()
        {
            return Token.Literal;
        }
    }
}
