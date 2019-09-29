using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public class LetStatement : Statement
    {

        public LetStatement(Token Token, Identifier Name)
        {
            this.Token = Token;
            this.Name = Name;
        }

        public Token Token { get; }
        public Identifier Name { get; }
        public Expression? Value { get; set; }
        public override Node StatementNode()
        {
            throw new NotImplementedException();
        }

        public override string TokenLiteral()
        {
            return Token.Literal;
        }
    }
}
