//
// Created by Oshwiciqwq on 2022/10/27.
//

#ifndef COMPILER_COMPILER_H
#define COMPILER_COMPILER_H

#include "bits/stdc++.h"

using namespace std;

enum class WordType {
    IDENFR, INTCON, STRCON, MAINTK, CONSTTK, INTTK, BREAKTK, CONTINUETK, IFTK, ELSETK,
    NOT, AND, OR, WHILETK, GETINTTK, PRINTFTK, RETURNTK, PLUS, MINU, VOIDTK,
    MULT, DIV, MOD, LSS, LEQ, GRE, GEQ, EQL, NEQ,
    ASSIGN, SEMICN, COMMA, LPARENT, RPARENT, LBRACK, RBRACK, LBRACE, RBRACE,
    END
};
const string WordTypeToString[] = {
        "IDENFR", "INTCON", "STRCON", "MAINTK", "CONSTTK", "INTTK", "BREAKTK", "CONTINUETK", "IFTK", "ELSETK",
        "NOT", "AND", "OR", "WHILETK", "GETINTTK", "PRINTFTK", "RETURNTK", "PLUS", "MINU", "VOIDTK",
        "MULT", "DIV", "MOD", "LSS", "LEQ", "GRE", "GEQ", "EQL", "NEQ",
        "ASSIGN", "SEMICN", "COMMA", "LPARENT", "RPARENT", "LBRACK", "RBRACK", "LBRACE", "RBRACE"
};
typedef pair<WordType, string> Word;


namespace Lexer {
    extern vector<Word> words;
    extern vector<int> lineNumbers;
    
    void getChar();
    
    void addWord(const Word &word);
    
    Word getOthers();
    
    Word getSTRCON();
    
    Word getINTCON();
    
    void getCommentLine();
    
    void getCommentBlock();
    
    void work(const string &inputFile, const string &outputFile = "");
}


enum class SyntaxType {
    CompUnit, Decl, ConstDecl, /*BType,*/ ConstDef, ConstInitVal, VarDecl, VarDef, InitVal, FuncDef,
    MainFuncDef, FuncType, FuncFParams, FuncFParam, Block, BlockItem, Stmt, Exp, Cond, LVal,
    PrimaryExp, Number, UnaryExp, UnaryOp, FuncRParams, MulExp, AddExp, RelExp, EqExp, LAndExp,
    LOrExp, ConstExp,
    AssignStmt, IfStmt, WhileStmt, BreakStmt, ContinueStmt, ReturnStmt, GetintStmt, PrintfStmt,
    UnaryExpIdent,
    Op
};

namespace Parser {
    void nextWord(int step = 1);
    
    void prevWord(int step = 1);

//    Number → IntConst
    int getNumber();

//    PrimaryExp → '(' Exp ')' | LVal | Number
    int getPrimaryExp();

//    UnaryOp → '+' | '−' | '!'
    int getUnaryOp();

//    FuncRParams → Exp { ',' Exp }
    int getFuncRParams();

//    Ident '(' [FuncRParams] ')'
    int getUnaryExpIdent();

//    UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
    int getUnaryExp();
    
    int getOp();

//    MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
    int getMulExp();

//    AddExp → MulExp | AddExp ('+' | '−') MulExp
    int getAddExp();

//    RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
    int getRelExp();

//    EqExp → RelExp | EqExp ('==' | '!=') RelExp
    int getEqExp();

//    ConstExp → AddExp
    int getConstExp();

//    Exp → AddExp
    int getExp();

//    LAndExp → EqExp | LAndExp '&&' EqExp
    int getLAndExp();

//    LOrExp → LAndExp | LOrExp '||' LAndExp
    int getLOrExp();

//    ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
    int getConstInitVal();

//    ConstDef → Ident { '[' ConstExp ']' } '=' ConstInitVal
    int getConstDef();

//    ConstDecl → 'const' BType ConstDef { ',' ConstDef } ';'
    int getConstDecl();

//    InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
    int getInitVal();

//    VarDef → Ident { '[' ConstExp ']' } | Ident { '[' ConstExp ']' } '=' InitVal
    int getVarDef();

//    VarDecl → BType VarDef { ',' VarDef } ';'
    int getVarDecl();

//    Decl → ConstDecl | VarDecl
    int getDecl();

//    BlockItem → Decl | Stmt
    int getBlockItem();

//    Block → '{' { BlockItem } '}'
    int getBlock();

//    Cond → LOrExp
    int getCond();

//    'if' '(' Cond ')' Stmt [ 'else' Stmt ]
    int getIfStmt();

//    'while' '(' Cond ')' Stmt
    int getWhileStmt();

//    'break' ';'
    int getBreakStmt();

//    'continue' ';'
    int getContinueStmt();

//    'return' [Exp] ';'
    int getReturnStmt();

//    'printf''('FormatString{','Exp}')'';'
    int getPrintfStmt();

//    LVal → Ident {'[' Exp ']'}
    int getLVal();

//    LVal '=' 'getint''('')'';'
    int getGetintStmt();

//    LVal '=' Exp ';'
    int getAssignStmt();

//    Stmt → LVal '=' Exp ';'
//    | [Exp] ';'
//    | Block
//    | 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
//    | 'while' '(' Cond ')' Stmt
//    | 'break' ';' | 'continue' ';'
//    | 'return' [Exp] ';'
//    | LVal '=' 'getint''('')'';'
//    | 'printf''('FormatString{','Exp}')'';'
    int getStmt();

//    FuncFParam → BType Ident ['[' ']' { '[' ConstExp ']' }]
    int getFuncFParam();

//    FuncFParams → FuncFParam { ',' FuncFParam }
    int getFuncFParams();

//    FuncType → 'void' | 'int'
    int getFuncType();

//    FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
    int getFuncDef();

//    MainFuncDef → 'int' 'main' '(' ')' Block
    int getMainFuncDef();

//    CompUnit → {Decl} {FuncDef} MainFuncDef
    int getCompUnit();
    
    void work(const string &outputFile = "");
}


namespace SyntaxTree {
    extern int varCnt;
    extern int funCnt;
    
    struct Symbol {
        Symbol() = default;
        
        Symbol(string ident, bool isFunc, bool isVoid, bool isConst, int dim, int scope, vector<int> params) {
            this->ident = std::move(ident);
            this->isFunc = isFunc;
            this->isVoid = isVoid;
            this->isConst = isConst;
            this->dim = dim;
            this->scope = scope;
            this->params = std::move(params);
            if (isFunc) {
                name = "func" + to_string(funCnt++);
            } else {
                name = "var" + to_string(varCnt++);
            }
        }
        
        string ident;
        bool isFunc = false;
        bool isVoid = false;
        bool isConst = false;
        int dim = 0;
        int scope = 0;
        string name;
        vector<int> params;
    };
    
    int newNode(SyntaxType type);
    
    void addIdent(int id, string ident);
    
    void addEdge(int id, int son);
    
    void addLineNumber(int id, int lineNumber);
    
    void checkErrorM(int x);
    
    void getReturns(int x);
    
    void checkErrorFG(int x);
    
    Symbol getSymbol(const string &ident);
    
    void checkErrorC(int x);
    
    void checkErrorH(int x);
    
    int getDim(int x);
    
    void checkErrorDE(int x);
    
    void addSymbol(int x);
    
    void dfs(int x, int f);

//=======================================================================================================//
//================================================CodeGen================================================//
//=======================================================================================================//
    
    string getTmp();
    
    string getLVal(int x);
    
    string getPrimaryExp(int x);
    
    string getUnaryExpIdent(int x);
    
    string getUnaryExp(int x);
    
    string getMulExp(int x);
    
    string getAddExp(int x);
    
    string getExp(int x);
    
    void getParams(int x);
    
    void codeGen(int x);
    
    void work();
}


enum class ErrorType {
    a, b, c, d, e, f, g, h, i, j, k, l, m
};
const string ErrorTypeToString[] = {
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m"
};

namespace ErrorHandler {
    void addError(ErrorType type, int lineNumber);
    
    bool checkFormatString(string s);
    
    void output(const string &outputFile = "");
}


enum class IRType {
    VAR,
    ADD, SUB, MUL, DIV, MOD,
    EQ, NEQ, LSS, LEQ, GRE, GEQ,
    AND, OR, NOT,
    GETINT, PRINT,
    LABEL, JUMP, BEQ,
    FUNC, PARAM,
    PUSH, CALL, SAVE, RESTORE,
    RETURN, GETRET
};

const string IRTypeToString[] = {
        "VAR",
        "ADD", "SUB", "MUL", "DIV", "MOD",
        "EQ", "NEQ", "LSS", "LEQ", "GRE", "GEQ",
        "AND", "OR", "NOT",
        "GETINT", "PRINT",
        "LABEL", "JUMP", "BEQ",
        "FUNC", "PARAM",
        "PUSH", "CALL", "SAVE", "RESTORE",
        "RETURN", "GETRET"
};

struct IR {
    IRType type;
    string res, num1, num2;
};

namespace Medium {
    string addStr(const string &s);
    
    void addIR(IRType type, string res = "", string num1 = "", string num2 = "");
    
    void output();
    
    extern map<string,int> paramCnt;
    
    void getGpMem(const string &var, int offset = 4);
    
    void getSpMem(const string &var, int offset = -4);
    
    void load(const string &reg, string s);
    
    void load(const string &reg, const string& s, int offset);
    
    void store(const string &reg, const string &s);
    
    void store(const string &reg, const string &s, int offset);
    
    void translate(const IR &ir);
    
    void codeGen();
}

#endif //COMPILER_COMPILER_H
