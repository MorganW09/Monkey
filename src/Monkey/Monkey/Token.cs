using System;
using System.Collections.Generic;
using System.Text;

namespace Monkey
{
    public class Token
    {
        public Token(TokenType tokenType, string literal)
        {
            TokenType = tokenType;
            Literal = literal;
        }

        public TokenType TokenType { get; private set; }
        public string Literal { get; private set; }

        public override bool Equals(object obj)
        {
            if ((obj == null) || !this.GetType().Equals(obj.GetType()))
                return false;
            else
            {
                var token = (Token)obj;
                return TokenType == token.TokenType && Literal == token.Literal;
            }
        }
    }
}
