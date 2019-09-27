using System;
using MonkeyLang.Lexical;

namespace MonkeyLang.Runner
{
    class Program
    {
        static void Main(string[] args)
        {
            string input = String.Empty;
            Console.WriteLine("Enter code block:");
            input = Console.ReadLine();

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
