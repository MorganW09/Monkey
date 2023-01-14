module Lexer
    type LexerState =
        {
            input : string
            mutable position : int
            mutable readPosition : int
            mutable ch : char
        }

    let readChar (l: LexerState) =
        let newChar =
            match l.readPosition >= l.input.Length with
            | true -> '\000'
            | false -> l.input.Chars l.readPosition
        l.position <- l.readPosition
        l.readPosition <- l.readPosition + 1
        l.ch <- newChar