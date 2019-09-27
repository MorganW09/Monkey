using System;
using System.Collections.Generic;
using System.Text;
using MonkeyLang.Lexical;

namespace MonkeyLang.AST
{
    public class Parser
    {
        public Lexer Lexer { get; private set; }
        public Token curToken { get; private set; }
        public Token peekToken { get; private set; }

        public Parser(Lexer lexer)
        {
            Lexer = lexer;

            NextToken();
            NextToken();
        }

        public void NextToken()
        {
            curToken = peekToken;
            peekToken = Lexer.NextToken();
        }

        public Program ParseProgram()
        {
            return null;
        }
    }
}
