let parse str = Parser.main Lexer.token (Lexing.from_string str)
