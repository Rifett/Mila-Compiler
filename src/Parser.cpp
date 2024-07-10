#include "Parser.hpp"

using namespace llvm;

std::unique_ptr<LLVMContext> TheContext;
std::unique_ptr<Module> TheModule;
std::unique_ptr<IRBuilder<>> Builder;
std::map<std::string, std::map<std::string, Value *>> NamedValues;
BasicBlock *mainBlock;

Parser::Parser() {
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("mila", *TheContext);

    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

}

bool Parser::Parse()
{
    getNextToken();
    tokenMatch(tok_program);

    std::string program_name = m_Lexer.identifierStr();
    tokenMatch(tok_id);
    tokenMatch(tok_semi_colon);

    //Parse body of a program (definitions and declarations)
    std::vector<std::unique_ptr<Node>> program_body (std::move(parse_body()));

    //Parse statement of a program
    std::vector<std::unique_ptr<Node>> program_statement (std::move(parse_statement()));

    AST_root = std::make_unique<Program_node>(std::move(program_name), std::move(program_body), std::move(program_statement));
    return true;
}

std::unique_ptr<Module> Parser::Generate()
{
    AST_root->codegen();
    /*// create writeln function
    {
      std::vector<llvm::Type*> Ints(1, llvm::Type::getInt32Ty(*TheContext));
      llvm::FunctionType * FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(*TheContext), Ints, false);
      llvm::Function * F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "writeln", *TheModule);
      for (auto & Arg : F->args())
          Arg.setName("x");
    }

    // create main function
    {
        llvm::FunctionType * FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(*TheContext), false);
        llvm::Function * MainFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", *TheModule);

        // block
        llvm::BasicBlock * BB = llvm::BasicBlock::Create(*TheContext, "entry", MainFunction);
        Builder->SetInsertPoint(BB);

      // call writeln with value from lexel
      Builder->CreateCall(TheModule->getFunction("writeln"), {
              llvm::ConstantInt::get(*TheContext, llvm::APInt(32, m_Lexer.numVal()))
      });

      // return 0
      Builder->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
    }
*/
    return std::move(TheModule);
}

int Parser::getNextToken()
{
    cur_tok = m_Lexer.gettok();

    return cur_tok;
}

//--------------------------------------------HELPER FUNCTIONS-------------------------------------

void Parser::Binary_expr::count_args(int &cnt) {
    if (op == ",") {
        left->count_args(cnt);
        right->count_args(cnt);
    } else cnt++;
}

void Parser::Binary_expr::codegen_into(std::vector<Value *> &ArgsV) {
    if (op == ",") {
        left->codegen_into(ArgsV);
        right->codegen_into(ArgsV);
    } else {
        ArgsV.push_back(codegen());
        if (!ArgsV.back()) throw std::runtime_error("Something is wrong in codegen_into");
    }
}

void Parser::tokenMatch(Token token) {
    if (cur_tok == token)
        getNextToken();
    else throw std::runtime_error("Token mismatch");
}

//--------------------------------------------PARSING COMPONENTS FUNCTIONS-------------------------

//Expressions part
std::string Parser::parse_string() {
    char cur = getc(stdin);
    std::string str;

    while (cur != '\'') {
        str += cur;
        cur = getc(stdin);
    }

    return str;
}

std::unique_ptr<Parser::Node> Parser::E11() {
    std::unique_ptr<Node> parsed (E10());

    while (true) {
        if (cur_tok == tok_comma) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>(",", std::move(parsed), E10());
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E10() {
    std::unique_ptr<Node> parsed (E9());

    while (true) {
        if (cur_tok == tok_assign) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>(":=", std::move(parsed), E10());
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E9() {
    std::unique_ptr<Node> parsed (E8());

    while (true) {
        if (cur_tok == tok_or) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("or", std::move(parsed), E8());
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E8() {
    std::unique_ptr<Node> parsed (E7());

    while (true) {
        if (cur_tok == tok_xor) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("xor", std::move(parsed), E7());
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E7() {
    std::unique_ptr<Node> parsed (E6());

    while (true) {
        if (cur_tok == tok_and) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("and", std::move(parsed), E6());
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E6() {
    std::unique_ptr<Node> parsed (E5());

    while (true) {
        if (cur_tok == tok_eq) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("=", std::move(parsed), E5());
        } else if (cur_tok == tok_not_eq) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("<>", std::move(parsed), E5());
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E5() {
    std::unique_ptr<Node> parsed (E4());

    while (true) {
        if (cur_tok == tok_ls) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("<", std::move(parsed), E4());
        } else if (cur_tok == tok_gr) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>(">", std::move(parsed), E4());
        } if (cur_tok == tok_ls_eq) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("<=", std::move(parsed), E4());
        } else if (cur_tok == tok_gr_eq) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>(">=", std::move(parsed), E4());
        }  else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E4() {
    std::unique_ptr<Node> parsed (E3());

    while (true) {
        if (cur_tok == tok_plus) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("+", std::move(parsed), E3());
        } else if (cur_tok == tok_minus) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("-", std::move(parsed), E3());
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E3() {
    std::unique_ptr<Node> parsed (E2());

    while (true) {
        if (cur_tok == tok_mult) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("*", std::move(parsed), E2());
        } else if (cur_tok == tok_div) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("div", std::move(parsed), E2());
        } else if (cur_tok == tok_mod) {
            getNextToken();
            parsed = std::make_unique<Binary_expr>("mod", std::move(parsed), E2());
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E2() {
    if (cur_tok == tok_minus) {
        getNextToken();
        return std::make_unique<Neg_node>(E2());
    } else if (cur_tok == tok_not) {
        getNextToken();
        return std::make_unique<Not_node>(E2());
    } else return E1();
}

std::unique_ptr<Parser::Node> Parser::E1() {
    std::unique_ptr<Node> parsed (E0());

    while (true) {
        if (cur_tok == tok_op_br) {
            getNextToken();
            parsed = std::make_unique<Arr_access>(std::move(parsed), E11());
            getNextToken();
        } else if (cur_tok == tok_op_par) {
            getNextToken();
            parsed = std::make_unique<Func_call>(std::move(parsed), E11());
            getNextToken();
        } else return parsed;
    }
}

std::unique_ptr<Parser::Node> Parser::E0() {
    if (cur_tok == tok_num) {
        std::unique_ptr<Int_node> expr = std::make_unique<Int_node>(m_Lexer.numVal());
        getNextToken();
        return expr;
    } else if (cur_tok == tok_id) {
        std::unique_ptr<Id_node> expr = std::make_unique<Id_node>(m_Lexer.identifierStr());
        getNextToken();
        return expr;
    } else if (cur_tok == tok_op_par) {
        getNextToken();
        std::unique_ptr<Node> expr (E11());
        getNextToken();
        return expr;
    } else if (cur_tok == tok_qot) {
        std::unique_ptr<String_node> string = std::make_unique<String_node>(parse_string());
        getNextToken();
        return string;
    } else throw std::runtime_error("Blub in expressions");
}

//Body part
std::unique_ptr<Parser::Node> Parser::parse_process() {
    std::vector<std::unique_ptr<Node>> list_of_args;
    tokenMatch(tok_proc);

    std::string name = m_Lexer.identifierStr();
    tokenMatch(tok_id);
    tokenMatch(tok_op_par);

    while (cur_tok == tok_id) {
        list_of_args.push_back(std::make_unique<Int_var_decl>(m_Lexer.identifierStr()));
        tokenMatch(tok_id);
        tokenMatch(tok_colon);
        tokenMatch(tok_int);

        if (cur_tok == tok_cl_par) break;
        tokenMatch(tok_semi_colon);
    }

    tokenMatch(tok_cl_par);
    tokenMatch(tok_semi_colon);
    std::vector<std::unique_ptr<Node>> proc_body (parse_body());
    std::vector<std::unique_ptr<Node>> proc_statement (parse_statement());

    return std::make_unique<Proc_def>(name, std::move(list_of_args), std::move(proc_body), std::move(proc_statement));
}

std::unique_ptr<Parser::Node> Parser::parse_function() {
    std::vector<std::unique_ptr<Node>> list_of_args;
    tokenMatch(tok_func);

    std::string name = m_Lexer.identifierStr();
    tokenMatch(tok_id);
    tokenMatch(tok_op_par);

    while (cur_tok == tok_id) {
        list_of_args.push_back(std::make_unique<Int_var_decl>(m_Lexer.identifierStr()));
        tokenMatch(tok_id);
        tokenMatch(tok_colon);
        tokenMatch(tok_int);

        if (cur_tok == tok_cl_par) break;
        tokenMatch(tok_semi_colon);
    }

    tokenMatch(tok_cl_par);
    tokenMatch(tok_colon);
    tokenMatch(tok_int);
    tokenMatch(tok_semi_colon);

    if (cur_tok == tok_forward) {
        tokenMatch(tok_forward);
        tokenMatch(tok_semi_colon);
        return std::make_unique<Func_decl>(name, std::move(list_of_args));
    } else {
        std::vector<std::unique_ptr<Node>> func_body (parse_body());
        std::vector<std::unique_ptr<Node>> func_statement (parse_statement());

        return std::make_unique<Func_def>(name, std::move(list_of_args), std::move(func_body), std::move(func_statement));
    }
}

std::vector<std::unique_ptr<Parser::Node>> Parser::parse_vars() {
    std::vector<std::unique_ptr<Node>> vars;
    std::vector<std::string> names;

    tokenMatch(tok_var);

    while (cur_tok == tok_id) {
        names.push_back(std::move(m_Lexer.identifierStr()));
        tokenMatch(tok_id);

        if (cur_tok == tok_comma) {
            getNextToken();
        } else {
            tokenMatch(tok_colon);

            if (cur_tok == tok_int) {
                for (auto & i: names) {
                    vars.push_back(std::make_unique<Int_var_decl>(i));
                }
                tokenMatch(tok_int);
                tokenMatch(tok_semi_colon);
                names.clear();
            } else {
                tokenMatch(tok_arr);
                tokenMatch(tok_op_br);
                int from, to;
                if (cur_tok == tok_minus) {
                    getNextToken();
                    from = m_Lexer.numVal() * (-1);
                } else
                    from = m_Lexer.numVal();
                tokenMatch(tok_num);
                tokenMatch(tok_double_dot);
                if (cur_tok == tok_minus) {
                    getNextToken();
                    to = m_Lexer.numVal() * (-1);
                } else
                    to = m_Lexer.numVal();
                tokenMatch(tok_num);
                tokenMatch(tok_cl_br);
                tokenMatch(tok_of);
                tokenMatch(tok_int);
                tokenMatch(tok_semi_colon);

                for (auto & i: names) {
                    vars.push_back(std::make_unique<Arr_var_decl>(i, from, to));
                }
                names.clear();
            }
        }
    }

    return vars;
}

std::vector<std::unique_ptr<Parser::Node>> Parser::parse_consts() {
    std::vector<std::unique_ptr<Node>> list_of_consts;
    std::string name;

    tokenMatch(tok_const);

    while (cur_tok == tok_id) {
        name = m_Lexer.identifierStr();
        tokenMatch(tok_id);
        tokenMatch(tok_eq);
        if (cur_tok == tok_num) {
            list_of_consts.push_back(std::make_unique<Const_def>(name, m_Lexer.numVal()));
            getNextToken();
        }
        else if (cur_tok == tok_hex) {
            list_of_consts.push_back(std::make_unique<Const_def>(name, std::stoi(m_Lexer.identifierStr(),nullptr, 16)));
            getNextToken();
        }
        else {
            list_of_consts.push_back(std::make_unique<Const_def>(name, std::stoi(m_Lexer.identifierStr(),nullptr, 8)));
            tokenMatch(tok_oct);
        }
        tokenMatch(tok_semi_colon);
    }

    return list_of_consts;
}

//Statement part
std::vector<std::unique_ptr<Parser::Node>> Parser::parse_begin_end() {
    std::vector<std::unique_ptr<Node>> list;

    tokenMatch(tok_begin);

    while (cur_tok != tok_end)
        for (auto & i: parse_statement()) list.push_back(std::move(i));

    tokenMatch(tok_end);
    if (cur_tok != tok_semi_colon && cur_tok != tok_dot) throw std::runtime_error("Begin/end error");
    getNextToken();
    return list;
}

std::unique_ptr<Parser::Node> Parser::parse_for_loop() {
    tokenMatch(tok_for);
    std::unique_ptr<Node> from, to;
    from = parse_expression();

    bool to_inc;
    if (cur_tok == tok_to)
        to_inc = true;
    else if (cur_tok == tok_downto)
        to_inc = false;
    else throw std::runtime_error("Token mismatch");

    getNextToken();
    to = parse_expression();

    tokenMatch(tok_do);
    std::vector<std::unique_ptr<Node>> statement (parse_statement());

    return std::make_unique<For_loop>(std::move(from), std::move(to), std::move(statement), to_inc);
}

std::unique_ptr<Parser::Node> Parser::parse_single_expr() {
    std::unique_ptr<Node> expr (parse_expression());
    tokenMatch(tok_semi_colon);

    return expr;
}

std::unique_ptr<Parser::Node> Parser::parse_writeln() {
    tokenMatch(tok_writeln);
    tokenMatch(tok_op_par);
    std::unique_ptr<Node> expr (parse_expression());
    tokenMatch(tok_cl_par);
    tokenMatch(tok_semi_colon);
    return std::make_unique<Writeln_node>(std::move(expr));
}

std::unique_ptr<Parser::Node> Parser::parse_if_statement() {
    std::vector<std::unique_ptr<Node>> true_case, false_case;
    tokenMatch(tok_if);

    std::unique_ptr<Node> cond (parse_expression());
    tokenMatch(tok_then);

    true_case = parse_statement();
    if (cur_tok == tok_else) {
        getNextToken();
        false_case = parse_statement();
    }

    return std::make_unique<If_node>(std::move(cond), std::move(true_case), std::move(false_case));
}

std::unique_ptr<Parser::Node> Parser::parse_readln() {
    tokenMatch(tok_readln);
    tokenMatch(tok_op_par);
    std::unique_ptr<Node> expr (parse_expression());
    tokenMatch(tok_cl_par);
    tokenMatch(tok_semi_colon);
    return std::make_unique<Readln_node>(std::move(expr));
}

std::unique_ptr<Parser::Node> Parser::parse_while_loop() {
    tokenMatch(tok_while);

    std::unique_ptr<Node> cond (parse_expression());
    tokenMatch(tok_do);
    std::vector<std::unique_ptr<Node>> statement (parse_statement());

    return std::make_unique<While_loop>(std::move(cond), std::move(statement));
}

std::unique_ptr<Parser::Node> Parser::parse_dec() {
    tokenMatch(tok_dec);
    tokenMatch(tok_op_par);
    std::unique_ptr<Node> expr (parse_expression());
    tokenMatch(tok_cl_par);
    tokenMatch(tok_semi_colon);
    return std::make_unique<Dec_node>(std::move(expr));
}

std::unique_ptr<Parser::Node> Parser::parse_exit() {
    tokenMatch(tok_exit);
    tokenMatch(tok_semi_colon);
    return std::make_unique<Exit_node>();
}

std::unique_ptr<Parser::Node> Parser::parse_write() {
    tokenMatch(tok_write);
    tokenMatch(tok_op_par);
    std::unique_ptr<Node> expr (parse_expression());
    tokenMatch(tok_cl_par);
    tokenMatch(tok_semi_colon);
    return std::make_unique<Write_node>(std::move(expr));
}

std::unique_ptr<Parser::Node> Parser::parse_expression() {
    return E11();
}

//--------------------------------------------REDIRECTING FUNCTIONS--------------------------------

std::vector<std::unique_ptr<Parser::Node>> Parser::parse_body() {
    std::vector<std::unique_ptr<Node>> body;

    while (true)
        switch(cur_tok) {
            case tok_const:
                for (auto & i : parse_consts()) body.push_back(std::move(i));
                break;

            case tok_var:
                for (auto & i : parse_vars()) body.push_back(std::move(i));
                break;

            case tok_func:
                body.push_back(std::move(parse_function()));
                break;

            case tok_proc:
                body.push_back(std::move(parse_process()));
                break;

            default:
                return body;
        }
}

std::vector<std::unique_ptr<Parser::Node>> Parser::parse_statement() {
    std::vector<std::unique_ptr<Node>> statement;

    switch (cur_tok) {

        case tok_begin :
            for (auto &i: parse_begin_end())
                statement.push_back(std::move(i));
            break;
        case tok_for :
            statement.push_back(std::move(parse_for_loop()));
            break;
        case tok_writeln :
            statement.push_back(std::move(parse_writeln()));
            break;
        case tok_if :
            statement.push_back(std::move(parse_if_statement()));
            break;
        case tok_readln :
            statement.push_back(std::move(parse_readln()));
            break;
        case tok_while :
            statement.push_back(std::move(parse_while_loop()));
            break;
        case tok_dec :
            statement.push_back(std::move(parse_dec()));
            break;
        case tok_exit:
            statement.push_back(std::move(parse_exit()));
            break;
        case tok_write :
            statement.push_back(std::move(parse_write()));
            break;
        default:
            statement.push_back(std::move(parse_single_expr()));
            break;
    }

    return statement;
}

//--------------------------------------------ALLOCA FUNCTIONS-------------------------------------

Value *Parser::Arr_access::Alloca() {
    Value * _arr = arr->Alloca();
    Value *ptr = Builder->CreateGEP(Type::getInt32Ty(*TheContext), _arr, index->codegen());
    return ptr;
}

Value *Parser::Id_node::Alloca() {
    Value *V = NamedValues[Builder->GetInsertBlock()->getParent()->getName()][name];
    if (!V)
        throw std::runtime_error("Unknown identifier");
    return V;
}

//--------------------------------------------CODE GENERATION FUNCTIONS----------------------------

Value *Parser::Program_node::codegen()  {
    FunctionType* mainFuncType = FunctionType::get(Type::getInt32Ty(*TheContext), false);
    Function* mainFunc = Function::Create(mainFuncType, Function::ExternalLinkage, "main", *TheModule);
    mainBlock = BasicBlock::Create(*TheContext, "entry", mainFunc);
    Builder->SetInsertPoint(mainBlock);

    for (auto & i: body)
        i->codegen();
    for (auto & i: statement)
        i->codegen();
    return Builder->CreateRet(ConstantInt::get(Type::getInt32Ty(*TheContext), APInt(32, 0)));
}
//OK

Value *Parser::Const_def::codegen()  {
    if (NamedValues[Builder->GetInsertBlock()->getParent()->getName()][name])
        throw std::runtime_error("Redeclaration->const");
    Value * _const = Builder->CreateAlloca(Type::getInt32Ty(*TheContext));
    Builder->CreateStore(ConstantInt::get(*TheContext, APInt(32, val, true)), _const);
    NamedValues[Builder->GetInsertBlock()->getParent()->getName()][name] = _const;
    return _const;
}
//OK

Value *Parser::Int_var_decl::codegen() {
    if (NamedValues[Builder->GetInsertBlock()->getParent()->getName()][name])
        throw std::runtime_error("Redeclaration->int variable");
    Value *alloca = Builder->CreateAlloca(Type::getInt32Ty(*TheContext));
    NamedValues[Builder->GetInsertBlock()->getParent()->getName()][name] = alloca;
    return alloca;
}
//OK

Value *Parser::Arr_var_decl::codegen() {
    if (NamedValues[Builder->GetInsertBlock()->getParent()->getName()][name])
        throw std::runtime_error("Redeclaration->array variable");
    Value *alloca = Builder->CreateAlloca(Type::getInt32Ty(*TheContext),
                                          ConstantInt::get(*TheContext, APInt(32, end_index - st_index + 1, true)));
    alloca = Builder->CreateGEP(Type::getInt32Ty(*TheContext), alloca, ConstantInt::get(*TheContext, APInt(32, -st_index, true)));
    NamedValues[Builder->GetInsertBlock()->getParent()->getName()][name] = alloca;
    return alloca;
}
//OK

Value *Parser::Func_def::codegen()  {
    // First, check for an existing function from a previous declaration.
    Function *TheFunction = TheModule->getFunction(name);

    if (!TheFunction) {
        std::vector<Type *> Ints(args.size(), Type::getInt32Ty(*TheContext));
        FunctionType *FT =
                FunctionType::get(Type::getInt32Ty(*TheContext), Ints, false);

        TheFunction =
                Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());

        // Set names for all arguments.
        unsigned Idx = 0;
        for (auto &Arg : TheFunction->args())
            Arg.setName(reinterpret_cast<Id_node*>(args[Idx++].get())->name);
    }

    if (!TheFunction)
        throw std::runtime_error("Some troubles with functions");

    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    BasicBlock *Ret = BasicBlock::Create(*TheContext, "return", TheFunction);
    Builder->SetInsertPoint(BB);

    // Record the function arguments in the NamedValues map.
    for (auto &Arg : TheFunction->args())
    {
        // Create an alloca for this variable.
        Value *Alloca = Builder->CreateAlloca(Type::getInt32Ty(*TheContext), ConstantInt::get(*TheContext, APInt(32, 0, true)),
                                              Arg.getName());

        // Store the initial value int o the alloca.
        Builder->CreateStore(&Arg, Alloca);

        // Add arguments to variable symbol table.
        NamedValues[name][Arg.getName()] = Alloca;
    }

    //Put return value into NamedValues
    Value *Alloca = Builder->CreateAlloca(Type::getInt32Ty(*TheContext), ConstantInt::get(*TheContext, APInt(32, 0, true)),name);
    Builder->CreateStore(ConstantInt::get(*TheContext, APInt(32, 0, true)), Alloca);
    NamedValues[name][name] = Alloca;

    for (auto & i: body)
        i->codegen();

    for (auto & i: statement)
    {
        if (i != statement.back())
            i->codegen();
        else if (i->codegen())
        {
            Builder->CreateBr(Ret);
            // Finish off the function.
            Builder->SetInsertPoint(Ret);
            Builder->CreateRet(Builder->CreateLoad(NamedValues[name][name]));

            // Validate the generated code, checking for consistency.
            verifyFunction(*TheFunction);

            Builder->SetInsertPoint(mainBlock);
            return TheFunction;
        }
        else break;
    }

    // Error reading body, remove function.
    TheFunction->eraseFromParent();
    throw std::runtime_error("Function parsing failed");
}
//OK

Value *Parser::Func_decl::codegen()  {
    std::vector<Type *> Ints(args.size(), Type::getInt32Ty(*TheContext));
    FunctionType *FT =
            FunctionType::get(Type::getInt32Ty(*TheContext), Ints, false);

    Function *F =
            Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());

    // Set names for all arguments.
    unsigned Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(reinterpret_cast<Id_node*>(args[Idx++].get())->name);

    return F;
}
//OK

Value *Parser::Proc_def::codegen()  {
    // First, check for an existing function from a previous 'extern' declaration.
    Function *TheFunction = TheModule->getFunction(name);

    if (!TheFunction) {
        std::vector<Type *> Ints(args.size(), Type::getInt32Ty(*TheContext));
        FunctionType *FT =
                FunctionType::get(Type::getInt32Ty(*TheContext), Ints, false);

        TheFunction =
                Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());

        // Set names for all arguments.
        unsigned Idx = 0;
        for (auto &Arg : TheFunction->args())
            Arg.setName(reinterpret_cast<Id_node*>(args[Idx++].get())->name);
    }

    if (!TheFunction)
        throw std::runtime_error("Some troubles with processes");

    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    // Record the function arguments in the NamedValues map.
    for (auto &Arg : TheFunction->args())
    {
        // Create an alloca for this variable.
        Value *Alloca = Builder->CreateAlloca(Type::getInt32Ty(*TheContext), ConstantInt::get(*TheContext, APInt(32, 0, true)),
                                              Arg.getName());

        // Store the initial value int o the alloca.
        Builder->CreateStore(&Arg, Alloca);

        // Add arguments to variable symbol table.
        NamedValues[name][Arg.getName()] = Alloca;
    }

    //Put return value into NamedValues
    Value *Alloca = Builder->CreateAlloca(Type::getInt32Ty(*TheContext), ConstantInt::get(*TheContext, APInt(32, 0, true)),
                                          name);
    Builder->CreateStore(ConstantInt::get(*TheContext, APInt(32, 0, true)), Alloca);
    NamedValues[name][name] = Alloca;

    BasicBlock *Ret = BasicBlock::Create(*TheContext, "return", TheFunction);

    for (auto & i: body)
        i->codegen();

    for (auto & i: statement)
    {
        if (i != statement.back())
            i->codegen();
        else if (i->codegen())
        {
            Builder->CreateBr(Ret);
            // Finish off the function.
            Builder->SetInsertPoint(Ret);
            Builder->CreateRet(Builder->CreateLoad(NamedValues[name][name]));

            // Validate the generated code, checking for consistency.
            verifyFunction(*TheFunction);

            Builder->SetInsertPoint(mainBlock);
            return TheFunction;
        }
        else break;
    }

    // Error reading body, remove function.
    TheFunction->eraseFromParent();
    throw std::runtime_error("Function parsing failed");
}
//OK

Value *Parser::For_loop::codegen()  {
    // Get the current function.
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

// Create an alloca for the variable in the entry block.
    Value *Alloca = reinterpret_cast<Binary_expr*>(from.get())->left->Alloca();
    if (!Alloca)
        throw std::runtime_error("Unknown variable in for cycle");

    Value *Val = reinterpret_cast<Binary_expr*>(from.get())->left->codegen();

    //Store the old value -> Probably that's an overkill
    //Value *OldVal = Alloca;

    //Codegen the initial assignment
    from->codegen();

// Make the new basic block for the loop header, inserting after the current block.
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

// Insert an explicit fall through from the current block to the LoopBB.
    Builder->CreateBr(LoopBB);

// Start insertion in LoopBB.
    Builder->SetInsertPoint(LoopBB);

// Within the loop, the variable is defined equal to the PHI node. If it shadows an existing variable, we have to restore it, so save it now. -> Probably that's an overkill
    //NamedValues[Builder->GetInsertBlock()->getParent()->getName()][var_name] = Alloca;

    for (auto & i: statement)
        i->codegen();

    Value *change;
    if (increase)
        change = ConstantInt::get(*TheContext, APInt(32, 1, true));
    else
        change = ConstantInt::get(*TheContext, APInt(32, -1, true));

    Builder->CreateStore(Builder->CreateAdd(Builder->CreateLoad(Alloca), change, "change_loopvar"), Alloca);

// Compute the end condition.
    Value *EndCond;
    if (increase) {
        EndCond = Builder->CreateICmpSGE(
                to->codegen(),
                Builder->CreateLoad(Alloca, "for_endcond"),
                "forcond"
        );
    } else {
        EndCond = Builder->CreateICmpSLE(
                to->codegen(),
                Builder->CreateLoad(Alloca, "for_endcond"),
                "forcond"
        );
    }
    if (!EndCond)
        throw std::runtime_error("No ending condition");

// Create the "after loop" block and insert it.
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);

// Insert the conditional branch into the end of LoopBB.
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

// Any new code will be inserted in AfterBB.
    Builder->SetInsertPoint(AfterBB);

// Restore the unshadowed variable. -> Overkill
    /*if (OldVal)
        NamedValues[sugar] = OldVal;
    else
        NamedValues.erase(sugar);*/

// The 'for' expression always returns 0
    return Constant::getNullValue(Type::getInt32Ty(*TheContext));
}
//OK

Value *Parser::Writeln_node::codegen()  {
    Value * Expr = expr->codegen();

    TheModule->getOrInsertFunction(
            "printf", FunctionType::get(Builder->getInt32Ty(),
                                        Builder->getInt8Ty()->getPointerTo(), true));

    auto printFn = TheModule->getFunction("printf");

    Value *formatStr = Builder->CreateGlobalStringPtr("%d\n");
    std::vector<Value *> args{formatStr, Expr};

    return Builder->CreateCall(printFn, args);
}
//OK

Value *Parser::If_node::codegen()  {
    Value *CondV = cond->codegen();
    if (!CondV)
        throw std::runtime_error("Error in if condition");

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else", TheFunction);
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "cont", TheFunction);

    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // Emit then value.
    Builder->SetInsertPoint(ThenBB);

    for (auto & i: tr_branch)
        i->codegen();

    if (!tr_branch.empty() && tr_branch.back().get()->get_type() != "exit")
        Builder->CreateBr(MergeBB);

    // Emit else block.
    Builder->SetInsertPoint(ElseBB);

    for (auto & i: fl_branch)
        i->codegen();

    if (fl_branch.empty() || fl_branch.back().get()->get_type() != "exit")
        Builder->CreateBr(MergeBB);

    // Emit merge block.
    Builder->SetInsertPoint(MergeBB);

    return Constant::getNullValue(Type::getInt32Ty(*TheContext));
}
//OK

Value *Parser::Readln_node::codegen()  {
    Value * Expr = expr->Alloca();

    TheModule->getOrInsertFunction(
            "scanf", FunctionType::get(Builder->getInt32Ty(),
                                       Builder->getInt8Ty()->getPointerTo(), true));

    auto Fn = TheModule->getFunction("scanf");

    Value *formatStr = Builder->CreateGlobalStringPtr("%d");
    std::vector<Value *> args{formatStr, Expr};

    return Builder->CreateCall(Fn, args);
}
//OK

Value *Parser::While_loop::codegen()  {
    // Get the current function.
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

// Make the new basic block for the loop header, inserting after the current block.
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

// Create the "after loop" block and insert it.
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);

    // Compute the end condition.
    Value *EndCond = cond->codegen();
    if (!EndCond)
        throw std::runtime_error("No ending condition");

// Insert an explicit fall through from the current block to the LoopBB.
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

// Start insertion in LoopBB.
    Builder->SetInsertPoint(LoopBB);

    for (auto & i: statement)
        i->codegen();

    EndCond = cond->codegen();

// Insert the conditional branch into the end of LoopBB.
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

// Any new code will be inserted in AfterBB.
    Builder->SetInsertPoint(AfterBB);

// The 'while' expression always returns 0 (I hope so)
    return Constant::getNullValue(Type::getInt32Ty(*TheContext));
}
//OK

Value *Parser::Dec_node::codegen()  {
    // Generate code for the expression to be decreased
    Value *ExprValue = expr->codegen();
    if (!ExprValue)
        return nullptr;
    Value *Alloca = expr->Alloca();

    // Decrease the expression value by one
    Value *DecValue = Builder->CreateSub(ExprValue, ConstantInt::get(*TheContext, APInt(32, 1, true)), "dec");
    Builder->CreateStore(DecValue, Alloca);

    // Return the decreased value
    return ExprValue;
}
//OK

Value *Parser::Exit_node::codegen()  {
    BasicBlock *RetBl = Builder->GetInsertBlock()->getParent()->getBasicBlockList().front().getNextNode();

    return Builder->CreateBr(RetBl);
}
//OK

Value *Parser::Write_node::codegen()  {
    Value * quot_expr = expr->codegen();

    TheModule->getOrInsertFunction(
            "printf", FunctionType::get(Builder->getInt32Ty(),
                                        Builder->getInt8Ty()->getPointerTo(), true));

    auto printFn = TheModule->getFunction("printf");

    Value *formatStr = Builder->CreateGlobalStringPtr("%s\n");
    std::vector<Value *> args{formatStr, quot_expr};

    return Builder->CreateCall(printFn, args);
}
//OK

Value *Parser::Int_node::codegen()  {
    return ConstantInt::get(*TheContext, APInt(32, val, true));
}
//OK

Value *Parser::String_node::codegen()  {
    return Builder->CreateGlobalStringPtr(quot);
}
//OK

Value *Parser::Id_node::codegen()  {
    Value *V = NamedValues[Builder->GetInsertBlock()->getParent()->getName()][name];
    if (!V)
        throw std::runtime_error("Unknown identifier");
    return Builder->CreateLoad(V);
}
//OK

Value *Parser::Arr_access::codegen()  {
    Value * _arr = arr->Alloca();
    Value *ptr = Builder->CreateGEP(Type::getInt32Ty(*TheContext), _arr, index->codegen());
    return Builder->CreateLoad(ptr);
}
//OK

Value *Parser::Func_call::codegen()  {
    // Look up the name in the global module table.
    Function *CalleeF = TheModule->getFunction(reinterpret_cast<Id_node *>(func.get())->name);
    if (!CalleeF)
        throw std::runtime_error("Unknown function referenced");

    int cnt = 0;
    args->count_args(cnt);
    // If argument mismatch error.
    if (CalleeF->arg_size() != cnt)
        throw std::runtime_error("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;

    args->codegen_into(ArgsV);

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}
//OK

Value *Parser::Neg_node::codegen()  {
    // Generate code for the expression value
    Value *ExprValue = val->codegen();
    if (!ExprValue)
        return nullptr;

    // Negate the expression value by multiplying it by -1
    Value *NegatedValue = Builder->CreateMul(ExprValue, ConstantInt::get(*TheContext, APInt(32, -1, true)));

    return NegatedValue;
}
//OK

Value *Parser::Not_node::codegen()  {
    // Generate code for the expression value
    Value *ExprValue = val->codegen();
    if (!ExprValue)
        return nullptr;

    //Reinterpret to be a bool -> probably an overkill
    //ExprValue = Builder->CreateICmpNE(ExprValue, llvm::ConstantInt::get(ExprValue->getType(), 0), "BoolReinterpret");

    // Perform logical NOT operation
    Value *NotValue = Builder->CreateNot(ExprValue);

    return NotValue;
}
//OK

Value *Parser::Binary_expr::codegen()  {
    if (op == ":=") {
        Value *var = left->Alloca();
        Value *val = right->codegen();
        Builder->CreateStore(val, var);
        return val;
    }

    Value * L = left->codegen(), *R = right->codegen();
    if (!L || !R)
        throw std::runtime_error("Binary expr->missing part(-s)");

    if (op == "=")
        return Builder->CreateICmpEQ(L, R, "eqtmp");
    if (op == "<")
        return Builder->CreateICmpSLT(L, R, "lstmp");
    if (op == "-")
        return Builder->CreateSub(L, R, "minustmp");
    if (op == "+")
        return Builder->CreateAdd(L, R, "plustmp");
    if (op == "*")
        return Builder->CreateMul(L, R, "multtmp");
    if (op == "div")
        return Builder->CreateSDiv(L, R, "divtmp");
    if (op == "mod")
        return Builder->CreateSRem(L, R, "modtmp");
    if (op == ">")
        return Builder->CreateICmpSGT(L, R, "grtmp");
    if (op == ">=")
        return Builder->CreateICmpSGE(L, R, "gr_eqtmp");
    if (op == "<=")
        return Builder->CreateICmpSLE(L, R, "ls_eqtmp");
    if (op == "<>")
        return Builder->CreateICmpNE(L, R, "netmp");
    if (op == "and")
        return Builder->CreateAnd(L, R, "andtmp");
    if (op == "or")
        return Builder->CreateOr(L, R, "ortmp");
    if (op == "xor")
        return Builder->CreateXor(L, R, "xortmp");

    throw std::runtime_error("Binary expressions failure (probably, connected with commas..)");
}
//OK






































