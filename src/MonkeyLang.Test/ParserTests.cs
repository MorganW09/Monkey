using System;
using System.Collections.Generic;
using System.Text;
using MonkeyLang.AST;
using MonkeyLang.Lexical;
using Xunit;

namespace MonkeyLang.Test
{
    public class ParserTests
    {
        [Fact]
        public void Test_LetStatements()
        {
            var input = @"
let x = 5;
let y = 10;
let foobar = 838383;";

            var lexer = new Lexer(input);
            var parser = new Parser(lexer);

            var program = parser.ParseProgram();

            Assert.NotNull(program);

            Assert.Equal(3, program.Statements.Length);

            var expectedIdentifiers = new List<string>() { "x", "y", "foobar" };

            for (int i = 0; i < expectedIdentifiers.Count; i++)
            {
                var expectedIdentifier = expectedIdentifiers[i];
                var stm = program.Statements[i];
                testLetStatement(stm, expectedIdentifier);
            }
        }

        public void testLetStatement(Statement statement, string expectedIdentifier)
        {
            Assert.Equal("let", statement.TokenLiteral());

            Assert.Equal(typeof(LetStatement), statement.GetType());

            var letStatement = (LetStatement)statement;

            Assert.Equal(expectedIdentifier, letStatement.Name.Value);

            Assert.Equal(expectedIdentifier, letStatement.Name.TokenLiteral());
        }
    }
}
