using System;
using System.Collections.Generic;
using System.Text;

namespace MonkeyLang.AST
{
    public class LetStatement : Statement
    {
        public Token Token { get; set; }
        public Identifier Name { get; set; }
        public Expression Value { get; set; }
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
