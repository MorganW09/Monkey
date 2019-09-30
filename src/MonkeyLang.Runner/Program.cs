using System;
using System.Diagnostics.CodeAnalysis;
using MonkeyLang.Lexical;

namespace MonkeyLang.Runner
{
    class Program
    {
        [SuppressMessage("Microsoft.Usage", "CA1801")]
        static void Main(string[] args)
        {          
            Console.WriteLine(Properties.Resources.REPLMessage);
            string input = Console.ReadLine();

            var lexer = new Lexer(input);
            Console.WriteLine(input);

            while (true)
            {
                var token = lexer.NextToken();
                if (token.Type == TokenType.EOF)
                    break;
                Console.WriteLine(token.ToString());
            }

            Console.ReadKey();
        }
    }
}
