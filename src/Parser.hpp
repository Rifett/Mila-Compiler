#ifndef PJPPROJECT_PARSER_HPP
#define PJPPROJECT_PARSER_HPP

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <utility>

#include "Lexer.hpp"

using namespace llvm;

extern std::unique_ptr<LLVMContext> TheContext;
extern std::unique_ptr<Module> TheModule;
extern std::unique_ptr<IRBuilder<>> Builder;
extern std::map<std::string, std::map<std::string, Value *>> NamedValues;
extern BasicBlock *mainBlock;

struct Parser {
    Parser();
    ~Parser() = default;

    bool Parse();                    // parse
    std::unique_ptr<Module> Generate();  // generate

    int getNextToken();

    Lexer m_Lexer;                   // lexer is used to read tokens
    int cur_tok;                     // to keep the current token

    //----------------------------------------NODE TYPES-------------------------------------------

    struct Node {
        virtual std::string get_type() { return ""; };
        virtual Value *codegen() = 0;
        virtual void count_args (int & cnt) { cnt++; };
        virtual void codegen_into (std::vector<Value *> &ArgsV){
            ArgsV.push_back(codegen());
            if (!ArgsV.back()) throw std::runtime_error("Something is wrong in codegen_into");
        };
        virtual Value *Alloca() { throw std::runtime_error("Wrong node called alloca"); }
    };

    struct Program_node : public Node {
        std::string name;
        std::vector<std::unique_ptr<Node>> body, statement;

        Program_node(std:: string Name, std::vector<std::unique_ptr<Node>> Body, std::vector<std::unique_ptr<Node>> Statement)
                : name(std::move(Name)), body(std::move(Body)), statement(std::move(Statement)) {}

        std::string get_type() override {return "program"; }

        Value * codegen() override;
    };

    struct Const_def : public Node {
        std::string name;
        int val;

        Const_def (std::string Name, int Val)
                : name(std::move(Name)), val(Val) {}

        std::string get_type() override { return "const"; }

        Value * codegen() override;
    };

    struct Int_var_decl : public Node {
        std::string name;

        Int_var_decl (std::string Name)
                : name(std::move(Name)) {}

        std::string get_type() override { return "int_var"; }

        Value * codegen() override;
    };

    struct Arr_var_decl : public Node {
        std::string name;

        int st_index, end_index;

        Arr_var_decl(std::string Name, int St_index, int End_index)
                : name(std::move(Name)), st_index(St_index), end_index(End_index) {}

        std::string get_type() override { return "arr_var"; }

        Value * codegen() override;
    };

    struct Func_def : public Node {
        std::string name;
        std::vector<std::unique_ptr<Node>> args, body, statement;

        Func_def(std::string Name, std::vector<std::unique_ptr<Node>> Args, std::vector<std::unique_ptr<Node>> Body, std::vector<std::unique_ptr<Node>> Statement)
                : name(std::move(Name)), args(std::move(Args)), body(std::move(Body)), statement(std::move(Statement)) {}

        std::string get_type() override { return "func_def"; }

        Value * codegen() override;
    };

    struct Func_decl : public Node {
        std::string name;
        std::vector<std::unique_ptr<Node>> args;

        Func_decl(std::string Name, std::vector<std::unique_ptr<Node>> Args)
                : name(std::move(Name)), args(std::move(Args)) {}

        std::string get_type() override { return "func_decl"; }

        Value * codegen() override;
    };

    struct Proc_def : public Node {
        std::string name;
        std::vector<std::unique_ptr<Node>> args, body, statement;

        Proc_def(std::string Name, std::vector<std::unique_ptr<Node>> Args, std::vector<std::unique_ptr<Node>> Body, std::vector<std::unique_ptr<Node>> Statement)
                : name(std::move(Name)), args(std::move(Args)), body(std::move(Body)), statement(std::move(Statement)) {}

        std::string get_type() override { return "proc_def"; }

        Value * codegen() override;
    };

    struct For_loop : public Node {
        std::unique_ptr<Node> from, to;
        bool increase;//true -> increase, false -> decrease
        std::vector<std::unique_ptr<Node>> statement;

        For_loop (std::unique_ptr<Node> From, std::unique_ptr<Node> To, std::vector<std::unique_ptr<Node>> Statement, bool to_inc)
                : from(std::move(From)), to(std::move(To)), increase(to_inc), statement(std::move(Statement)) {}

        std::string get_type() override { return "for"; }

        Value * codegen() override;
    };

    struct Writeln_node : public Node {
        std::unique_ptr<Node> expr;

        Writeln_node(std::unique_ptr<Node> Expr)
                : expr(std::move(Expr)) {}

        std::string get_type() override { return "writeln"; }

        Value * codegen() override;
    };

    struct If_node : public Node {
        std::unique_ptr<Node> cond;
        std::vector<std::unique_ptr<Node>> tr_branch, fl_branch;

        If_node(std::unique_ptr<Node> Cond, std::vector<std::unique_ptr<Node>> Tr, std::vector<std::unique_ptr<Node>> Fl)
                : cond(std::move(Cond)), tr_branch(std::move(Tr)), fl_branch(std::move(Fl)) {}

        std::string get_type() override { return "if"; }

        Value * codegen() override;
    };

    struct Readln_node : public Node {
        std::unique_ptr<Node> expr;

        Readln_node(std::unique_ptr<Node> Expr)
                : expr(std::move(Expr)) {}

        std::string get_type() override { return "readln"; }

        Value * codegen() override;
    };

    struct While_loop : public Node {
        std::unique_ptr<Node> cond;
        std::vector<std::unique_ptr<Node>> statement;

        While_loop(std::unique_ptr<Node> Cond, std::vector<std::unique_ptr<Node>> St)
                : cond(std::move(Cond)), statement(std::move(St)) {}

        std::string get_type() override { return "while"; }

        Value * codegen() override;
    };

    struct Dec_node : public Node {
        std::unique_ptr<Node> expr;

        Dec_node(std::unique_ptr<Node> Expr)
                : expr(std::move(Expr)) {}

        std::string get_type() override { return "dec"; }

        Value * codegen() override;
    };

    struct Exit_node : public Node {
        std::string get_type() override { return "exit"; }

        Value * codegen() override;
    };

    struct Write_node : public Node {
        std::unique_ptr<Node> expr;

        Write_node(std::unique_ptr<Node> Expr)
                : expr(std::move(Expr)) {}

        std::string get_type() override { return "write"; }

        Value * codegen() override;
    };

    struct Int_node : public Node {
        int val;

        Int_node(int Val)
                : val(Val) {}

        std::string get_type() override { return "int"; }

        Value * codegen() override;
    };

    struct String_node : public Node {
        std::string quot;

        String_node(std::string Str)
                : quot(std::move(Str)) {}

        std::string get_type() override { return "string"; }

        Value * codegen() override;
    };

    struct Id_node : public Node {
        std::string name;

        Id_node(std::string Name)
                : name(std::move(Name)) {}

        std::string get_type() override {return "id"; }

        Value * codegen() override;

        Value * Alloca() override;
    };

    struct Arr_access : public Node {
        std::unique_ptr<Node> arr, index;

        Arr_access(std::unique_ptr<Node> Arr, std::unique_ptr<Node> Index)
                : arr(std::move(Arr)), index(std::move(Index)) {}

        std::string get_type() override {return "arr_access"; }

        Value * codegen() override;

        Value * Alloca() override;
    };

    struct Func_call : public Node {
        std::unique_ptr<Node> func, args;

        Func_call(std::unique_ptr<Node> Func, std::unique_ptr<Node> Args)
                : func(std::move(Func)), args(std::move(Args)) {}

        std::string get_type() override { return "func_call"; }

        Value * codegen() override;
    };

    struct Neg_node : public Node {
        std::unique_ptr<Node> val;

        Neg_node(std::unique_ptr<Node> Val)
                : val(std::move(Val)) {}

        std::string get_type() override { return "neg"; }

        Value * codegen() override;
    };

    struct Not_node : public Node {
        std::unique_ptr<Node> val;

        Not_node(std::unique_ptr<Node> Val)
                : val(std::move(Val)) {}

        std::string get_type() override { return "not"; }

        Value * codegen() override;
    };

    struct Binary_expr : public Node {
        std::string op;
        std::unique_ptr<Node> left, right;

        Binary_expr(std::string Op, std::unique_ptr<Node> Left, std::unique_ptr<Node> Right)
                : op(std::move(Op)), left(std::move(Left)), right(std::move(Right)) {}

        std::string get_type() override { return op; }

        Value * codegen() override;

        void count_args(int &cnt) override;

        void codegen_into(std::vector<Value *> &ArgsV) override;
    };

    //----------------------------------------AST ROOT---------------------------------------------

    std::unique_ptr<Node> AST_root = nullptr;

    //----------------------------------------SUPPORTING METHODS-----------------------------------

    void tokenMatch(Token token);

    std::vector<std::unique_ptr<Node>> parse_consts();

    std::vector<std::unique_ptr<Node>> parse_vars();

    std::unique_ptr<Node> parse_function();

    std::unique_ptr<Node> parse_process();

    std::vector<std::unique_ptr<Node>> parse_statement();

    std::vector<std::unique_ptr<Node>> parse_body();

    std::vector<std::unique_ptr<Node>> parse_begin_end();

    std::unique_ptr<Node> parse_for_loop();

    std::unique_ptr<Node> parse_single_expr();//The one in the grammar, not a regular expression

    std::unique_ptr<Node> parse_writeln();

    std::unique_ptr<Node> parse_if_statement();

    std::unique_ptr<Node> parse_readln();

    std::unique_ptr<Node> parse_while_loop();

    std::unique_ptr<Node> parse_dec();

    std::unique_ptr<Node> parse_exit();

    std::unique_ptr<Node> parse_write();

    std::unique_ptr<Node> parse_expression();

    std::unique_ptr<Node> E11();
    std::unique_ptr<Node> E10();
    std::unique_ptr<Node> E9();
    std::unique_ptr<Node> E8();
    std::unique_ptr<Node> E7();
    std::unique_ptr<Node> E6();
    std::unique_ptr<Node> E5();
    std::unique_ptr<Node> E4();
    std::unique_ptr<Node> E3();
    std::unique_ptr<Node> E2();
    std::unique_ptr<Node> E1();
    std::unique_ptr<Node> E0();
    std::string parse_string();
};

#endif //PJPPROJECT_PARSER_HPP
