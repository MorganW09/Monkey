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


            curToken = peekToken;
            peekToken = Lexer.NextToken();

            curToken = peekToken;
            peekToken = Lexer.NextToken();
            //NextToken();
            //NextToken();
        }

        public void NextToken()
        {
            curToken = peekToken;
            peekToken = Lexer.NextToken();
        }

        internal bool curTokenIs(TokenType tokenType)
        {
            return curToken.Type == tokenType;
        }

        internal bool peekTokenIs(TokenType tokenType)
        {
            return peekToken.Type == tokenType;
        }

        internal bool expectPeek(TokenType tokenType)
        {
            if (peekTokenIs(tokenType))
            {
                NextToken();
                return true;
            }
            return false;
        }

        public LetStatement? ParseLetStatement()
        {
            if (!expectPeek(TokenType.IDENT))
                return null;

            var identifier = new Identifier(curToken);
            var statement = new LetStatement(curToken, identifier);

            if (!expectPeek(TokenType.ASSIGN))
                return null;

            while (!curTokenIs(TokenType.SEMICOLON))
            {
                NextToken();
            }

            return statement;
        }

        public Statement? ParseStatement()
        {
            var statement = curToken.Type switch
            {
                TokenType.LET => ParseLetStatement(),
                _ => null
            };
            return statement;
        }

        public Program ParseProgram()
        {
            var program = new Program();

            while (curToken.Type != TokenType.EOF)
            {
                var statement = ParseStatement();

                if (statement != null)
                    program.Statements.Add(statement);
                NextToken();
            }
            return program;
        }
    }
}
