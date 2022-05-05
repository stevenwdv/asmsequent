;If you change these, also change the constants after doprint uses

s_indent                db "  |",0

s_op_not                db "!",0
s_op_and                db "&",0
s_op_or                 db "|",0
s_op_imp                db "->",0
s_op_eqv                db "<->",0
s_seqsep                db "@",0

s_lparen                db "(",0
s_rparen                db ")",0

align 8
s_tokens                dq s_op_not
s_ops                   dq s_op_and, s_op_or, s_op_imp, s_op_eqv,\
                           s_lparen, s_rparen, s_seqsep

s_act_lnot              db "!L    ",NEWL,0
s_act_land              db "&L    ",NEWL,0
s_act_lor               db "|L S  ",NEWL,0
s_act_limp              db "->L S ",NEWL,0
s_act_leqv              db "<->L S",NEWL,0
s_act_rnot              db "!R    ",NEWL,0
s_act_rand              db "&R S  ",NEWL,0
s_act_ror               db "|R    ",NEWL,0
s_act_rimp              db "->R   ",NEWL,0
s_act_reqv              db "<->R S",NEWL,0

align 8
s_actions               dq s_act_lnot, s_act_land, s_act_lor, s_act_limp, s_act_leqv,\
                           ACTION_RNOT - ACTION_LEQV - 1 dup(?),\
                           s_act_rnot, s_act_rand, s_act_ror, s_act_rimp, s_act_reqv

if PRINTMODE=0
s_seq_sep               db "--O",0
s_seq_closed            db " CLOSED",NEWL,0
s_seq_open              db " OPEN <<<",NEWL,0

else

s_seq                   db "sequent %p",10,0
s_seq_childs            db "CHILDS:",0
s_seq_state             db "STATE: %hhu",10,0
s_side                  db "side %p: ",0
s_side_terms            db "%llu term(s)",10,0
s_side_t                db "TRUE:",10,0
s_side_f                db "FALSE:",10,0
s_term                  db "term %p: ",0
s_term_type             db "type %hhu",10,0
end if