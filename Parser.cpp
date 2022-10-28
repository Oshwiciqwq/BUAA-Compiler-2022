//
// Created by Oshwiciqwq on 2022/10/27.
//

#include "Compiler.h"

namespace Parser {
    int pos = 0;
    Word curWord;
    WordType curWordType;
    string curWordString;
    int curLineNumber;
    int lasLineNumber;
    vector<string> outputs;
    
    void nextWord(int step) {
        while (step--) {
            if (pos > 0) {
                outputs.emplace_back(WordTypeToString[(int) curWordType] + " " + curWordString);
            }
            pos++;
            curWord = Lexer::words[pos - 1];
            curWordType = curWord.first;
            curWordString = curWord.second;
            lasLineNumber = curLineNumber;
            curLineNumber = Lexer::lineNumbers[pos - 1];
        }
    }
    
    void prevWord(int step) {
        while (step--) {
            pos--;
            curWord = Lexer::words[pos - 1];
            curWordType = curWord.first;
            curWordString = curWord.second;
            curLineNumber = lasLineNumber;
            lasLineNumber = (pos > 1 ? Lexer::lineNumbers[pos - 2] : 1);
            if (pos > 0) {
                outputs.pop_back();
            }
        }
    }

//    Number → IntConst
    int getNumber() {
        int id = SyntaxTree::newNode(SyntaxType::Number);
        
        SyntaxTree::addIdent(id,curWordString);
        nextWord();
        
        outputs.emplace_back("<Number>");
        return id;
    }

//    PrimaryExp → '(' Exp ')' | LVal | Number
    int getPrimaryExp() {
        int id = SyntaxTree::newNode(SyntaxType::PrimaryExp);
        
        if (curWordType == WordType::LPARENT) {
            nextWord();
            SyntaxTree::addEdge(id, getExp());
            nextWord();
        } else if (curWordType == WordType::INTCON) {
            SyntaxTree::addEdge(id, getNumber());
        } else {
            SyntaxTree::addEdge(id, getLVal());
        }
        
        outputs.emplace_back("<PrimaryExp>");
        return id;
    }

//    UnaryOp → '+' | '−' | '!'
    int getUnaryOp() {
        int id = SyntaxTree::newNode(SyntaxType::UnaryOp);
        
        SyntaxTree::addIdent(id, curWordString);
        nextWord();
        
        outputs.emplace_back("<UnaryOp>");
        return id;
    }

//    FuncRParams → Exp { ',' Exp }
    int getFuncRParams() {
        int id = SyntaxTree::newNode(SyntaxType::FuncRParams);
        
        SyntaxTree::addEdge(id, getExp());
        while (curWordType == WordType::COMMA) {
            nextWord();
            SyntaxTree::addEdge(id, getExp());
        }
        
        outputs.emplace_back("<FuncRParams>");
        return id;
    }

//    Ident '(' [FuncRParams] ')'
    int getUnaryExpIdent() {
        int id = SyntaxTree::newNode(SyntaxType::UnaryExpIdent);
        
        SyntaxTree::addIdent(id, curWordString);
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord(2);
        if (curWordType == WordType::PLUS || curWordType == WordType::MINU ||
            curWordType == WordType::NOT || curWordType == WordType::IDENFR ||
            curWordType == WordType::LPARENT || curWordType == WordType::INTCON) {
            SyntaxTree::addEdge(id, getFuncRParams());
        }
        if (curWordType != WordType::RPARENT) {
            ErrorHandler::addError(ErrorType::j, lasLineNumber);
        } else {
            nextWord();
        }
        return id;
    }

//    UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
    int getUnaryExp() {
        int id = SyntaxTree::newNode(SyntaxType::UnaryExp);
        
        if (curWordType == WordType::PLUS || curWordType == WordType::MINU || curWordType == WordType::NOT) {
            SyntaxTree::addEdge(id, getUnaryOp());
            SyntaxTree::addEdge(id, getUnaryExp());
        } else if (curWordType == WordType::IDENFR) {
            nextWord();
            if (curWordType == WordType::LPARENT) {
                prevWord();
                SyntaxTree::addEdge(id, getUnaryExpIdent());
            } else {
                prevWord();
                SyntaxTree::addEdge(id, getPrimaryExp());
            }
        } else {
            SyntaxTree::addEdge(id, getPrimaryExp());
        }
        
        outputs.emplace_back("<UnaryExp>");
        return id;
    }
    
    int getOp() {
        int id = SyntaxTree::newNode(SyntaxType::Op);
        
        SyntaxTree::addIdent(id, curWordString);
        nextWord();
        
        return id;
    }

//    MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
    int getMulExp() {
        int id = SyntaxTree::newNode(SyntaxType::MulExp);
        
        SyntaxTree::addEdge(id, getUnaryExp());
        outputs.emplace_back("<MulExp>");
        while (curWordType == WordType::MULT || curWordType == WordType::DIV || curWordType == WordType::MOD) {
            SyntaxTree::addEdge(id, getOp());
            SyntaxTree::addEdge(id, getUnaryExp());
            outputs.emplace_back("<MulExp>");
        }
        
        return id;
    }

//    AddExp → MulExp | AddExp ('+' | '−') MulExp
    int getAddExp() {
        int id = SyntaxTree::newNode(SyntaxType::AddExp);
        
        SyntaxTree::addEdge(id, getMulExp());
        outputs.emplace_back("<AddExp>");
        while (curWordType == WordType::PLUS || curWordType == WordType::MINU) {
            SyntaxTree::addEdge(id, getOp());
            SyntaxTree::addEdge(id, getMulExp());
            outputs.emplace_back("<AddExp>");
        }
        
        return id;
    }

//    RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
    int getRelExp() {
        int id = SyntaxTree::newNode(SyntaxType::RelExp);
        
        SyntaxTree::addEdge(id, getAddExp());
        outputs.emplace_back("<RelExp>");
        while (curWordType == WordType::LSS || curWordType == WordType::GRE ||
               curWordType == WordType::LEQ || curWordType == WordType::GEQ) {
            SyntaxTree::addEdge(id, getOp());
            SyntaxTree::addEdge(id, getAddExp());
            outputs.emplace_back("<RelExp>");
        }
        
        return id;
    }

//    EqExp → RelExp | EqExp ('==' | '!=') RelExp
    int getEqExp() {
        int id = SyntaxTree::newNode(SyntaxType::EqExp);
        
        SyntaxTree::addEdge(id, getRelExp());
        outputs.emplace_back("<EqExp>");
        while (curWordType == WordType::EQL || curWordType == WordType::NEQ) {
            SyntaxTree::addEdge(id, getOp());
            SyntaxTree::addEdge(id, getRelExp());
            outputs.emplace_back("<EqExp>");
        }
        
        return id;
    }

//    ConstExp → AddExp
    int getConstExp() {
        int id = SyntaxTree::newNode(SyntaxType::ConstExp);
        
        SyntaxTree::addEdge(id, getAddExp());
        
        outputs.emplace_back("<ConstExp>");
        return id;
    }

//    Exp → AddExp
    int getExp() {
        int id = SyntaxTree::newNode(SyntaxType::Exp);
        
        SyntaxTree::addEdge(id, getAddExp());
        
        outputs.emplace_back("<Exp>");
        return id;
    }

//    LAndExp → EqExp | LAndExp '&&' EqExp
    int getLAndExp() {
        int id = SyntaxTree::newNode(SyntaxType::LAndExp);
        
        SyntaxTree::addEdge(id, getEqExp());
        outputs.emplace_back("<LAndExp>");
        while (curWordType == WordType::AND) {
            SyntaxTree::addEdge(id, getOp());
            SyntaxTree::addEdge(id, getEqExp());
            outputs.emplace_back("<LAndExp>");
        }
        
        return id;
    }

//    LOrExp → LAndExp | LOrExp '||' LAndExp
    int getLOrExp() {
        int id = SyntaxTree::newNode(SyntaxType::LOrExp);
        
        SyntaxTree::addEdge(id, getLAndExp());
        outputs.emplace_back("<LOrExp>");
        while (curWordType == WordType::OR) {
            SyntaxTree::addEdge(id, getOp());
            SyntaxTree::addEdge(id, getLAndExp());
            outputs.emplace_back("<LOrExp>");
        }
        
        return id;
    }

//    ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
    int getConstInitVal() {
        int id = SyntaxTree::newNode(SyntaxType::ConstInitVal);
        
        if (curWordType == WordType::LBRACE) {
            nextWord();
            while (curWordType != WordType::RBRACE) {
                if (curWordType == WordType::COMMA) {
                    nextWord();
                }
                SyntaxTree::addEdge(id, getConstInitVal());
            }
            nextWord();
        } else {
            SyntaxTree::addEdge(id, getConstExp());
        }
        
        outputs.emplace_back("<ConstInitVal>");
        return id;
    }

//    ConstDef → Ident { '[' ConstExp ']' } '=' ConstInitVal
    int getConstDef() {
        int id = SyntaxTree::newNode(SyntaxType::ConstDef);
        
        SyntaxTree::addIdent(id, curWordString);
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord();
        while (curWordType == WordType::LBRACK) {
            nextWord();
            SyntaxTree::addEdge(id, getConstExp());
            if (curWordType != WordType::RBRACK) {
                ErrorHandler::addError(ErrorType::k, lasLineNumber);
            } else {
                nextWord();
            }
        }
        nextWord();
        SyntaxTree::addEdge(id, getConstInitVal());
        
        outputs.emplace_back("<ConstDef>");
        return id;
    }


//    ConstDecl → 'const' BType ConstDef { ',' ConstDef } ';'
    int getConstDecl() {
        int id = SyntaxTree::newNode(SyntaxType::ConstDecl);
        
        nextWord(2);
        SyntaxTree::addEdge(id, getConstDef());
        while (curWordType == WordType::COMMA) {
            nextWord();
            SyntaxTree::addEdge(id, getConstDef());
        }
        if (curWordType != WordType::SEMICN) {
            ErrorHandler::addError(ErrorType::i, lasLineNumber);
        } else {
            nextWord();
        }
        
        outputs.emplace_back("<ConstDecl>");
        return id;
    }

//    InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
    int getInitVal() {
        int id = SyntaxTree::newNode(SyntaxType::InitVal);
        
        if (curWordType == WordType::LBRACE) {
            nextWord();
            while (curWordType != WordType::RBRACE) {
                if (curWordType == WordType::COMMA) {
                    nextWord();
                }
                SyntaxTree::addEdge(id, getInitVal());
            }
            nextWord();
        } else {
            SyntaxTree::addEdge(id, getExp());
        }
        
        outputs.emplace_back("<InitVal>");
        return id;
    }

//    VarDef → Ident { '[' ConstExp ']' } | Ident { '[' ConstExp ']' } '=' InitVal
    int getVarDef() {
        int id = SyntaxTree::newNode(SyntaxType::VarDef);
        
        SyntaxTree::addIdent(id, curWordString);
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord();
        while (curWordType == WordType::LBRACK) {
            nextWord();
            SyntaxTree::addEdge(id, getConstExp());
            if (curWordType != WordType::RBRACK) {
                ErrorHandler::addError(ErrorType::k, lasLineNumber);
            } else {
                nextWord();
            }
        }
        if (curWordType == WordType::ASSIGN) {
            nextWord();
            SyntaxTree::addEdge(id, getInitVal());
        }
        
        outputs.emplace_back("<VarDef>");
        return id;
    }

//    VarDecl → BType VarDef { ',' VarDef } ';'
    int getVarDecl() {
        int id = SyntaxTree::newNode(SyntaxType::VarDecl);
        
        nextWord();
        SyntaxTree::addEdge(id, getVarDef());
        while (curWordType == WordType::COMMA) {
            nextWord();
            SyntaxTree::addEdge(id, getVarDef());
        }
        if (curWordType != WordType::SEMICN) {
            ErrorHandler::addError(ErrorType::i, lasLineNumber);
        } else {
            nextWord();
        }
        outputs.emplace_back("<VarDecl>");
        return id;
    }

//    Decl → ConstDecl | VarDecl
    int getDecl() {
        int id = SyntaxTree::newNode(SyntaxType::Decl);
        
        if (curWordType == WordType::CONSTTK) {
            SyntaxTree::addEdge(id, getConstDecl());
        } else if (curWordType == WordType::INTTK) {
            SyntaxTree::addEdge(id, getVarDecl());
        }
        
        return id;
    }

//    BlockItem → Decl | Stmt
    int getBlockItem() {
        int id = SyntaxTree::newNode(SyntaxType::BlockItem);
        
        if (curWordType == WordType::CONSTTK || curWordType == WordType::INTTK) {
            SyntaxTree::addEdge(id, getDecl());
        } else {
            SyntaxTree::addEdge(id, getStmt());
        }
        
        return id;
    }

//    Block → '{' { BlockItem } '}'
    int getBlock() {
        int id = SyntaxTree::newNode(SyntaxType::Block);
        
        nextWord();
        while (curWordType != WordType::RBRACE) {
            SyntaxTree::addEdge(id, getBlockItem());
        }
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord();
        
        outputs.emplace_back("<Block>");
        return id;
    }

//    Cond → LOrExp
    int getCond() {
        int id = SyntaxTree::newNode(SyntaxType::Cond);
        
        SyntaxTree::addEdge(id, getLOrExp());
        
        outputs.emplace_back("<Cond>");
        return id;
    }

//    'if' '(' Cond ')' Stmt [ 'else' Stmt ]
    int getIfStmt() {
        int id = SyntaxTree::newNode(SyntaxType::IfStmt);
        
        nextWord(2);
        SyntaxTree::addEdge(id, getCond());
        if (curWordType != WordType::RPARENT) {
            ErrorHandler::addError(ErrorType::j, lasLineNumber);
        } else {
            nextWord();
        }
        SyntaxTree::addEdge(id, getStmt());
        if (curWordType == WordType::ELSETK) {
            nextWord();
            SyntaxTree::addEdge(id, getStmt());
        }
        
        return id;
    }

//    'while' '(' Cond ')' Stmt
    int getWhileStmt() {
        int id = SyntaxTree::newNode(SyntaxType::WhileStmt);
        
        nextWord(2);
        SyntaxTree::addEdge(id, getCond());
        if (curWordType != WordType::RPARENT) {
            ErrorHandler::addError(ErrorType::j, lasLineNumber);
        } else {
            nextWord();
        }
        SyntaxTree::addEdge(id, getStmt());
        
        return id;
    }

//    'break' ';'
    int getBreakStmt() {
        int id = SyntaxTree::newNode(SyntaxType::BreakStmt);
        
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord();
        if (curWordType != WordType::SEMICN) {
            ErrorHandler::addError(ErrorType::i, lasLineNumber);
        } else {
            nextWord();
        }
        
        return id;
    }

//    'continue' ';'
    int getContinueStmt() {
        int id = SyntaxTree::newNode(SyntaxType::ContinueStmt);
        
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord(1);
        if (curWordType != WordType::SEMICN) {
            ErrorHandler::addError(ErrorType::i, lasLineNumber);
        } else {
            nextWord();
        }
        
        return id;
    }

//    'return' [Exp] ';'
    int getReturnStmt() {
        int id = SyntaxTree::newNode(SyntaxType::ReturnStmt);
        
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord();
        if (curWordType == WordType::PLUS || curWordType == WordType::MINU ||
            curWordType == WordType::NOT || curWordType == WordType::IDENFR ||
            curWordType == WordType::LPARENT || curWordType == WordType::INTCON) {
            SyntaxTree::addEdge(id, getExp());
        }
        if (curWordType != WordType::SEMICN) {
            ErrorHandler::addError(ErrorType::i, lasLineNumber);
        } else {
            nextWord();
        }
        
        return id;
    }

//    'printf''('FormatString{','Exp}')'';'
    int getPrintfStmt() {
        int id = SyntaxTree::newNode(SyntaxType::PrintfStmt);
        
        nextWord(2);
        if (!ErrorHandler::checkFormatString(curWordString)) {
            ErrorHandler::addError(ErrorType::a, curLineNumber);
        }
        int cnt = (int) count(curWordString.begin(), curWordString.end(), '%');
        SyntaxTree::addIdent(id, curWordString);
        nextWord();
        while (curWordType == WordType::COMMA) {
            nextWord();
            SyntaxTree::addEdge(id, getExp());
            cnt--;
        }
        if (cnt != 0) {
            ErrorHandler::addError(ErrorType::l, lasLineNumber);
        }
        if (curWordType != WordType::RPARENT) {
            ErrorHandler::addError(ErrorType::j, lasLineNumber);
        } else {
            nextWord();
        }
        if (curWordType != WordType::SEMICN) {
            ErrorHandler::addError(ErrorType::i, lasLineNumber);
        } else {
            nextWord();
        }
        
        return id;
    }

//    LVal → Ident {'[' Exp ']'}
    int getLVal() {
        int id = SyntaxTree::newNode(SyntaxType::LVal);
        
        SyntaxTree::addIdent(id, curWordString);
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord();
        while (curWordType == WordType::LBRACK) {
            nextWord();
            SyntaxTree::addEdge(id, getExp());
            if (curWordType != WordType::RBRACK) {
                ErrorHandler::addError(ErrorType::k, lasLineNumber);
            } else {
                nextWord();
            }
        }
        
        outputs.emplace_back("<LVal>");
        return id;
    }

//    LVal '=' 'getint''('')'';'
    int getGetintStmt() {
        int id = SyntaxTree::newNode(SyntaxType::GetintStmt);
        
        SyntaxTree::addEdge(id, getLVal());
        nextWord(3);
        if (curWordType != WordType::RPARENT) {
            ErrorHandler::addError(ErrorType::j, lasLineNumber);
        } else {
            nextWord();
        }
        if (curWordType != WordType::SEMICN) {
            ErrorHandler::addError(ErrorType::i, lasLineNumber);
        } else {
            nextWord();
        }
        
        return id;
    }

//    LVal '=' Exp ';'
    int getAssignStmt() {
        int id = SyntaxTree::newNode(SyntaxType::AssignStmt);
        
        SyntaxTree::addEdge(id, getLVal());
        nextWord();
        SyntaxTree::addEdge(id, getExp());
        if (curWordType != WordType::SEMICN) {
            ErrorHandler::addError(ErrorType::i, lasLineNumber);
        } else {
            nextWord();
        }
        
        return id;
    }

//    Stmt → LVal '=' Exp ';'
//    | [Exp] ';'
//    | Block
//    | 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
//    | 'while' '(' Cond ')' Stmt
//    | 'break' ';' | 'continue' ';'
//    | 'return' [Exp] ';'
//    | LVal '=' 'getint''('')'';'
//    | 'printf''('FormatString{','Exp}')'';'
    int getStmt() {
        int id = SyntaxTree::newNode(SyntaxType::Stmt);
        
        if (curWordType == WordType::LBRACE) {
            SyntaxTree::addEdge(id, getBlock());
        } else if (curWordType == WordType::IFTK) {
            SyntaxTree::addEdge(id, getIfStmt());
        } else if (curWordType == WordType::WHILETK) {
            SyntaxTree::addEdge(id, getWhileStmt());
        } else if (curWordType == WordType::BREAKTK) {
            SyntaxTree::addEdge(id, getBreakStmt());
        } else if (curWordType == WordType::CONTINUETK) {
            SyntaxTree::addEdge(id, getContinueStmt());
        } else if (curWordType == WordType::RETURNTK) {
            SyntaxTree::addEdge(id, getReturnStmt());
        } else if (curWordType == WordType::PRINTFTK) {
            SyntaxTree::addEdge(id, getPrintfStmt());
        } else if (curWordType == WordType::IDENFR) {
            nextWord();
            if (curWordType == WordType::LPARENT) {
                prevWord();
                if (curWordType != WordType::SEMICN) {
                    SyntaxTree::addEdge(id, getExp());
                }
                if (curWordType != WordType::SEMICN) {
                    ErrorHandler::addError(ErrorType::i, lasLineNumber);
                } else {
                    nextWord();
                }
            } else {
                prevWord();
                int tmp1 = pos;
                auto tmp2 = outputs.size();
                getLVal();
                if (curWordType == WordType::ASSIGN) {
                    nextWord();
                    if (curWordType == WordType::GETINTTK) {
                        prevWord(pos - tmp1);
                        while (outputs.size() > tmp2) {
                            outputs.pop_back();
                        }
                        SyntaxTree::addEdge(id, getGetintStmt());
                    } else {
                        prevWord(pos - tmp1);
                        while (outputs.size() > tmp2) {
                            outputs.pop_back();
                        }
                        SyntaxTree::addEdge(id, getAssignStmt());
                    }
                } else {
                    prevWord(pos - tmp1);
                    while (outputs.size() > tmp2) {
                        outputs.pop_back();
                    }
                    if (curWordType != WordType::SEMICN) {
                        SyntaxTree::addEdge(id, getExp());
                    }
                    if (curWordType != WordType::SEMICN) {
                        ErrorHandler::addError(ErrorType::i, lasLineNumber);
                    } else {
                        nextWord();
                    }
                }
            }
        } else {
            if (curWordType != WordType::SEMICN) {
                SyntaxTree::addEdge(id, getExp());
            }
            if (curWordType != WordType::SEMICN) {
                ErrorHandler::addError(ErrorType::i, lasLineNumber);
            } else {
                nextWord();
            }
        }
        
        outputs.emplace_back("<Stmt>");
        return id;
    }

//    FuncFParam → BType Ident ['[' ']' { '[' ConstExp ']' }]
    int getFuncFParam() {
        int id = SyntaxTree::newNode(SyntaxType::FuncFParam);
        
        nextWord();
        SyntaxTree::addIdent(id, curWordString);
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord();
        if (curWordType == WordType::LBRACK) {
            nextWord();
            SyntaxTree::addEdge(id, -1);
            if (curWordType != WordType::RBRACK) {
                ErrorHandler::addError(ErrorType::k, lasLineNumber);
            } else {
                nextWord();
            }
            while (curWordType == WordType::LBRACK) {
                nextWord();
                SyntaxTree::addEdge(id, getConstExp());
                if (curWordType != WordType::RBRACK) {
                    ErrorHandler::addError(ErrorType::k, lasLineNumber);
                } else {
                    nextWord();
                }
            }
        }
        
        outputs.emplace_back("<FuncFParam>");
        return id;
    }

//    FuncFParams → FuncFParam { ',' FuncFParam }
    int getFuncFParams() {
        int id = SyntaxTree::newNode(SyntaxType::FuncFParams);
        
        SyntaxTree::addEdge(id, getFuncFParam());
        while (curWordType == WordType::COMMA) {
            nextWord();
            SyntaxTree::addEdge(id, getFuncFParam());
        }
        
        outputs.emplace_back("<FuncFParams>");
        return id;
    }

//    FuncType → 'void' | 'int'
    int getFuncType() {
        int id = SyntaxTree::newNode(SyntaxType::FuncType);
        SyntaxTree::addIdent(id, curWordString);
        
        nextWord();
        
        outputs.emplace_back("<FuncType>");
        return id;
    }

//    FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
    int getFuncDef() {
        int id = SyntaxTree::newNode(SyntaxType::FuncDef);
        
        SyntaxTree::addEdge(id, getFuncType());
        SyntaxTree::addIdent(id, curWordString);
        SyntaxTree::addLineNumber(id, curLineNumber);
        nextWord(2);
        if (curWordType == WordType::INTTK) {
            SyntaxTree::addEdge(id, getFuncFParams());
        }
        if (curWordType != WordType::RPARENT) {
            ErrorHandler::addError(ErrorType::j, lasLineNumber);
        } else {
            nextWord();
        }
        SyntaxTree::addEdge(id, getBlock());
        
        outputs.emplace_back("<FuncDef>");
        return id;
    }

//    MainFuncDef → 'int' 'main' '(' ')' Block
    int getMainFuncDef() {
        int id = SyntaxTree::newNode(SyntaxType::MainFuncDef);
        
        SyntaxTree::addIdent(id, "main");
        nextWord(3);
        if (curWordType != WordType::RPARENT) {
            ErrorHandler::addError(ErrorType::j, lasLineNumber);
        } else {
            nextWord();
        }
        SyntaxTree::addEdge(id, getBlock());
        
        outputs.emplace_back("<MainFuncDef>");
        return id;
    }

//    CompUnit → {Decl} {FuncDef} MainFuncDef
    int getCompUnit() {
        int id = SyntaxTree::newNode(SyntaxType::CompUnit);
        nextWord();
        
        while (curWordType == WordType::CONSTTK || curWordType == WordType::INTTK) {
            if (curWordType == WordType::CONSTTK) {
                SyntaxTree::addEdge(id, getDecl());
            } else {
                nextWord();
                if (curWordType == WordType::IDENFR) {
                    nextWord();
                    if (curWordType == WordType::COMMA || curWordType == WordType::LBRACK ||
                        curWordType == WordType::ASSIGN || curWordType == WordType::SEMICN) {
                        prevWord(2);
                        SyntaxTree::addEdge(id, getDecl());
                    } else {
                        prevWord(2);
                        break;
                    }
                } else {
                    prevWord();
                    break;
                }
            }
        }
        
        while (true) {
            nextWord();
            if (curWordType == WordType::MAINTK) {
                prevWord();
                SyntaxTree::addEdge(id, getMainFuncDef());
                break;
            } else {
                prevWord();
                SyntaxTree::addEdge(id, getFuncDef());
            }
        }
        
        outputs.emplace_back("<CompUnit>");
        return id;
        
    }
    
    void work(const string &outputFile) {
        getCompUnit();
        if (!outputFile.empty()) {
            ofstream output(outputFile);
            for (auto &i: outputs) {
                output << i << endl;
            }
            output.close();
        }
    }
}
