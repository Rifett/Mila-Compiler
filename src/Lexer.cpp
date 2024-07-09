#include "Lexer.hpp"


int Lexer::gettok()
{
    int input = getc(stdin);

    //Get rid of spaces
    while (isspace(input))
        input = getc(stdin);

    //Input symbol is character -> identifier or a keyword
    if (isalpha(input))
    {
        m_IdentifierStr = input;

        while (isalnum(input = getc(stdin)) || input == '_')
            m_IdentifierStr += input;

        ungetc(input, stdin);

        if (m_IdentifierStr == "program")
            return tok_program;
        else if (m_IdentifierStr == "var")
            return tok_var;
        else if (m_IdentifierStr == "integer")
            return tok_int;
        else if (m_IdentifierStr == "array")
            return tok_arr;
        else if (m_IdentifierStr == "of")
            return tok_of;
        else if (m_IdentifierStr == "begin")
            return tok_begin;
        else if (m_IdentifierStr == "for")
            return tok_for;
        else if (m_IdentifierStr == "to")
            return tok_to;
        else if (m_IdentifierStr == "do")
            return tok_do;
        else if (m_IdentifierStr == "writeln")
            return tok_writeln;
        else if (m_IdentifierStr == "end")
            return tok_end;
        else if (m_IdentifierStr == "if")
            return tok_if;
        else if (m_IdentifierStr == "then")
            return tok_then;
        else if (m_IdentifierStr == "readln")
            return tok_readln;
        else if (m_IdentifierStr == "downto")
            return tok_downto;
        else if (m_IdentifierStr == "div")
            return tok_div;
        else if (m_IdentifierStr == "const")
            return tok_const;
        else if (m_IdentifierStr == "mod")
            return tok_mod;
        else if (m_IdentifierStr == "function")
            return tok_func;
        else if (m_IdentifierStr == "while")
            return tok_while;
        else if (m_IdentifierStr == "dec")
            return tok_dec;
        else if (m_IdentifierStr == "else")
            return tok_else;
        else if (m_IdentifierStr == "procedure")
            return tok_proc;
        else if (m_IdentifierStr == "write")
            return tok_write;
        else if (m_IdentifierStr == "exit")
            return tok_exit;
        else if (m_IdentifierStr == "and")
            return tok_and;
        else if (m_IdentifierStr == "or")
            return tok_or;
        else if (m_IdentifierStr == "xor")
            return tok_xor;
        else if (m_IdentifierStr == "not")
            return tok_not;
        else if (m_IdentifierStr == "forward")
            return tok_forward;
        else
            return tok_id;
    }

    //Input symbol is a number -> we have an integer value
    if (isdigit(input))
    {
        m_NumVal = input - '0';

        while (isdigit(input = getc(stdin)))
            m_NumVal = m_NumVal * 10 + (input - '0');

        ungetc(input, stdin);

        return tok_num;
    }

    //EOF case
    if ( input == EOF )
        return tok_eof;

    //Comma case
    if (input == ',')
        return tok_comma;

    //Brackets cases
    if (input == '[')
        return tok_op_br;
    if (input == ']')
        return tok_cl_br;

    //Semicolon case
    if (input == ';')
        return tok_semi_colon;

    //Parentheses cases
    if (input == '(')
        return tok_op_par;
    if (input == ')')
        return tok_cl_par;

    //Plus, minus and multiply cases
    if (input == '+')
        return tok_plus;
    if (input == '-')
        return tok_minus;
    if (input == '*')
        return tok_mult;

    //Equality case
    if (input == '=')
        return tok_eq;

    //Hex and octal cases
    if (input == '$')
    {
        m_IdentifierStr.clear();

        while (isxdigit(input = getc(stdin)))
            m_IdentifierStr += input;

        ungetc(input, stdin);

        return tok_hex;
    }
    if (input == '&')
    {
        m_IdentifierStr.clear();

        while (isdigit(input = getc(stdin)) && input < '8' && input >= '0')
            m_IdentifierStr += input;

        ungetc(input, stdin);

        return tok_oct;
    }

    //Quotation case
    if (input == '\'')
        return tok_qot;

    //Colon cases -> colon or assignment
    if (input == ':')
    {
        int next = getc(stdin);

        if (next == '=')
            return tok_assign;

        ungetc(next, stdin);
        return tok_colon;
    }

    //Dot cases -> dot or double dot (interval)
    if (input == '.')
    {
        int next = getc(stdin);

        if (next == '.')
            return tok_double_dot;

        ungetc(next, stdin);
        return tok_dot;
    }

    //Comparison cases
    if (input == '>')
    {
        int next = getc(stdin);

        if (next == '=')
            return tok_gr_eq;

        ungetc(next, stdin);
        return tok_gr;
    }

    if (input == '<')
    {
        int next = getc(stdin);

        if (next == '=')
            return tok_ls_eq;

        if (next == '>')
            return tok_not_eq;

        ungetc(next, stdin);
        return tok_ls;
    }

    throw std::runtime_error("Unspecified thing as an input!");
}
