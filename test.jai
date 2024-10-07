#import "Basic";
#import "Jaijutsu";

main :: () {
    lexer := lex_string(TEST_INPUT);
    print("%\n", lexer.*);
}

TEST_INPUT :: #string JAI
Token :: struct {
    type: Token_Type = .ERROR;

    l0, c0: s32;
    l1, c1: s32 = -1;  @ThisIsANote

    union {
        ident\ _value: struct { name: Table_String; hash: u32; };
        integer_value: u64;
        float64_value: float64;
        string\_value: string;
    };

    value_flags: Value_Flags;  // If a number or a note. Mostly the old numeric_flags, but also has HERESTRING now.
    ident_is_backticked := false;  @v5-15

    token_scratch_buffer: [MAX_TOKEN_LENGTH] u8;  // @Robustness: Kind of lame, do something better!
}

HERE_STRING :: #string INSIDE
This is a here string. Yay.
INSIDE
JAI
