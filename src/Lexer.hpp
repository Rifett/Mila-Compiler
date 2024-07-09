#ifndef PJPPROJECT_LEXER_HPP
#define PJPPROJECT_LEXER_HPP

#include <iostream>

class Lexer
{
public:
    Lexer() = default;

    ~Lexer() = default;

    int gettok();

    const std::string& identifierStr() const { return this->m_IdentifierStr; }

    int numVal() { return this->m_NumVal; }

private:
    std::string m_IdentifierStr;

    int m_NumVal;
};

enum Token {
    tok_eof =                       -1,//   'EOF'
    tok_id =                        -2,//   '???'
    tok_num =                       -3,//   '???'
    tok_program =                   -4,//   'program'
    tok_var =                       -5,//   'var'
    tok_colon =                     -6,//   ':'
    tok_semi_colon =                -7,//   ';'
    tok_comma =                     -8,//   ','
    tok_int =                       -9,//   'integer'
    tok_arr =                       -10,//  'array'
    tok_op_br =                     -11,//  '['
    tok_cl_br =                     -12,//  ']'
    tok_double_dot =                -13,//  '..'
    tok_of =                        -14,//  'of'
    tok_begin =                     -15,//  'begin'
    tok_assign =                    -16,//  ':='
    tok_for =                       -17,//  'for'
    tok_to =                        -18,//  'to'
    tok_do =                        -19,//  'do'
    tok_writeln =                   -20,//  'writeln'
    tok_op_par =                    -21,//  '('
    tok_cl_par =                    -22,//  ')'
    tok_end =                       -23,//  'end'
    tok_if =                        -24,//  'if'
    tok_then =                      -25,//  'then'
    tok_dot =                       -26,//  '.'
    tok_readln =                    -27,//  'readln'
    tok_minus =                     -28,//  '-'
    tok_plus =                      -29,//  '+'
    tok_downto =                    -30,//  'downto'
    tok_mult =                      -31,//  '*'
    tok_div =                       -32,//  'div'
    tok_const =                     -33,//  'const'
    tok_eq =                        -34,//  '='
    tok_hex =                       -35,//  '$'
    tok_oct =                       -36,//  '&'
    tok_mod =                       -37,//  'mod'
    tok_func =                      -38,//  'function'
    tok_while =                     -39,//  'while'
    tok_gr =                        -40,//  '>'
    tok_ls =                        -41,//  '<'
    tok_dec =                       -42,//  'dec'
    tok_else =                      -43,//  'else'
    tok_gr_eq =                     -44,//  '>='
    tok_ls_eq =                     -45,//  '<='
    tok_proc =                      -46,//  'procedure'
    tok_write =                     -47,//  'write'
    tok_qot =                       -48,//  ' ' '
    tok_exit =                      -49,//  'exit'
    tok_not_eq =                    -50,//  '<>'
    tok_and =                       -51,//  'and'
    tok_or =                        -52,//  'or'
    tok_xor =                       -53,//  'xor'
    tok_not =                       -54,//  'not'
    tok_forward =                   -55//  'forward'
};

#endif //PJPPROJECT_LEXER_HPP
