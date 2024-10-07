#import "Basic";
#import "Jai_Lexer";

lex_string :: (s: string) -> *Lexer {
    lexer := New(Lexer);

    set_input_from_string(lexer, s);

    while 1 {
        token := peek_next_token(lexer);
        if token.type == .END_OF_INPUT break;

        eat_token(lexer);

        if token.type < 128 {
            data: [1] u8;
            data[0] = cast(u8) token.type;
            s: string;
            s.count = 1;
            s.data = data.data;
            print("%,%: '%'\n", token.l0, token.c0, s);
        } else {
            if token.type == {
                case .IDENT;     print("%,%: IDENT '%'\n", token.l0, token.c0, token.ident_value.name);
                case;            print("%,%: %\n", token.l0, token.c0, token.type);
            }
        }
    }

    return lexer;
}

