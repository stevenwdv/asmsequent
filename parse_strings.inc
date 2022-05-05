s_tok_e_unexpected              db "Expected '%c' instead of '%c' at index %llu",0
s_tok_e_unknown                 db "Illegal token '%c' at index %llu",0

s_parse_e_pop                   db "Tried to reduce %hhu term(s) for operation '%s', but found %hhu",0
s_parse_e_lparen                db "Too few closing parentheses",0
s_parse_e_rparen                db "Too many closing parentheses",0
s_parse_e_stack_multiple        db "%hhu parts have no connecting operator(s)",0