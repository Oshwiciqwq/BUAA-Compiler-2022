#include <bits/stdc++.h>

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
enum class SyntaxType {
    CompUnit, Decl, ConstDecl, /*BType,*/ ConstDef, ConstInitVal, VarDecl, VarDef, InitVal, FuncDef,
    MainFuncDef, FuncType, FuncFParams, FuncFParam, Block, BlockItem, Stmt, Exp, Cond, LVal,
    PrimaryExp, Number, UnaryExp, UnaryOp, FuncRParams, MulExp, AddExp, RelExp, EqExp, LAndExp,
    LOrExp, ConstExp,
    AssignStmt, IfStmt, WhileStmt, BreakStmt, ContinueStmt, ReturnStmt, GetintStmt, PrintfStmt,
    UnaryExpIdent,
    Op
};
enum class ErrorType {
    a, b, c, d, e, f, g, h, i, j, k, l, m
};
const string ErrorTypeToString[] = {
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m"
};

namespace Lexer {
    ifstream input;
    
    string code;
    vector<Word> words;
    vector<int> lineNumbers;
    int curLineNumber = 0;
    int pos = 0;
    char ch;
    
    void getChar() {
        if (pos == (int) code.size()) {
            if (!(getline(input, code))) {
                ch = -1;
                return;
            }
            code += '\n';
            pos = 0;
            curLineNumber++;
        }
        ch = code[pos++];
    }
    
    void addWord(const Word &word) {
        words.emplace_back(word);
        lineNumbers.emplace_back(curLineNumber);
    }
    
    Word getOthers() {
        string str;
        while (isalnum(ch) || ch == '_') {
            str += ch;
            getChar();
        }
        if (str == "main") {
            return {WordType::MAINTK, str};
        } else if (str == "const") {
            return {WordType::CONSTTK, str};
        } else if (str == "int") {
            return {WordType::INTTK, str};
        } else if (str == "break") {
            return {WordType::BREAKTK, str};
        } else if (str == "continue") {
            return {WordType::CONTINUETK, str};
        } else if (str == "if") {
            return {WordType::IFTK, str};
        } else if (str == "else") {
            return {WordType::ELSETK, str};
        } else if (str == "while") {
            return {WordType::WHILETK, str};
        } else if (str == "getint") {
            return {WordType::GETINTTK, str};
        } else if (str == "printf") {
            return {WordType::PRINTFTK, str};
        } else if (str == "return") {
            return {WordType::RETURNTK, str};
        } else if (str == "void") {
            return {WordType::VOIDTK, str};
        } else {
            return {WordType::IDENFR, str};
        }
    }
    
    Word getSTRCON() {
        getChar();
        string str;
        while (ch != '"') {
            str += ch;
            getChar();
        }
        getChar();
        str = '"' + str + '"';
        return {WordType::STRCON, str};
    }
    
    Word getINTCON() {
        string str;
        while (isdigit(ch)) {
            str += ch;
            getChar();
        }
        return {WordType::INTCON, str};
    }
    
    void getCommentLine() {
        getChar();
        while (ch != '\r' && ch != '\n') {
            getChar();
        }
        getChar();
    }
    
    void getCommentBlock() {
        getChar();
        while (true) {
            if (ch == '*') {
                getChar();
                if (ch == '/') {
                    break;
                }
            } else {
                getChar();
            }
        }
        getChar();
    }
    
    void work(const string &inputFile, const string &outputFile = "") {
        input.open(inputFile);
        getChar();
        while (ch != -1) {
            if (isspace(ch)) {
                getChar();
            } else if (ch == '"') {
                addWord(getSTRCON());
            } else if (ch == '/') {
                getChar();
                if (ch == '/') {
                    getCommentLine();
                } else if (ch == '*') {
                    getCommentBlock();
                } else {
                    addWord({WordType::DIV, "/"});
                }
            } else if (ch == '!') {
                getChar();
                if (ch == '=') {
                    addWord({WordType::NEQ, "!="});
                    getChar();
                } else {
                    addWord({WordType::NOT, "!"});
                }
            } else if (ch == '&') {
                addWord({WordType::AND, "&&"});
                getChar();
                getChar();
            } else if (ch == '|') {
                addWord({WordType::OR, "||"});
                getChar();
                getChar();
            } else if (ch == '+') {
                addWord({WordType::PLUS, "+"});
                getChar();
            } else if (ch == '-') {
                addWord({WordType::MINU, "-"});
                getChar();
            } else if (ch == '*') {
                addWord({WordType::MULT, "*"});
                getChar();
            } else if (ch == '%') {
                addWord({WordType::MOD, "%"});
                getChar();
            } else if (ch == '<') {
                getChar();
                if (ch == '=') {
                    addWord({WordType::LEQ, "<="});
                    getChar();
                } else {
                    addWord({WordType::LSS, "<"});
                }
            } else if (ch == '>') {
                getChar();
                if (ch == '=') {
                    addWord({WordType::GEQ, ">="});
                    getChar();
                } else {
                    addWord({WordType::GRE, ">"});
                }
            } else if (ch == '=') {
                getChar();
                if (ch == '=') {
                    addWord({WordType::EQL, "=="});
                    getChar();
                } else {
                    addWord({WordType::ASSIGN, "="});
                }
            } else if (ch == ';') {
                addWord({WordType::SEMICN, ";"});
                getChar();
            } else if (ch == ',') {
                addWord({WordType::COMMA, ","});
                getChar();
            } else if (ch == '(') {
                addWord({WordType::LPARENT, "("});
                getChar();
            } else if (ch == ')') {
                addWord({WordType::RPARENT, ")"});
                getChar();
            } else if (ch == '[') {
                addWord({WordType::LBRACK, "["});
                getChar();
            } else if (ch == ']') {
                addWord({WordType::RBRACK, "]"});
                getChar();
            } else if (ch == '{') {
                addWord({WordType::LBRACE, "{"});
                getChar();
            } else if (ch == '}') {
                addWord({WordType::RBRACE, "}"});
                getChar();
            } else if (isdigit(ch)) {
                addWord(getINTCON());
            } else if (isalpha(ch) || ch == '_') {
                addWord(getOthers());
            }
        }
        input.close();
        if (!outputFile.empty()) {
            ofstream output(outputFile);
            for (auto &i: words) {
                output << WordTypeToString[(int) i.first] << " " << i.second << endl;
            }
            output.close();
        }
        words.emplace_back(make_pair(WordType::END, ""));
    }
}

namespace ErrorHandler {
    map<int, ErrorType> errors;
    
    void addError(ErrorType type, int lineNumber) {
        if (errors.find(lineNumber) == errors.end()) {
            errors[lineNumber] = type;
        }
    }
    
    bool checkFormatString(string s) {
        int len = (int) s.size();
        for (int i = 1; i < len - 1; i++) {
            if (s[i] == '\\') {
                if (i + 1 == len - 1 || s[i + 1] != 'n') {
                    return false;
                }
                i++;
            } else if (s[i] == '%') {
                if (i + 1 == len - 1 || s[i + 1] != 'd') {
                    return false;
                }
                i++;
            } else if (s[i] != 32 && s[i] != 33 && !(40 <= s[i] && s[i] <= 126)) {
                return false;
            }
        }
        return true;
    }
    
    void output(const string &outputFile = "") {
        if (!outputFile.empty()) {
            ofstream output(outputFile);
            for (auto &i: errors) {
                output << i.first << " " << ErrorTypeToString[(int) i.second] << endl;
            }
            output.close();
        }
    }
}

namespace SyntaxTree {
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
        }
        
        string ident;
        bool isFunc = false;
        bool isVoid = false;
        bool isConst = false;
        int dim = 0;
        int scope = 0;
        vector<int> params;
    };
    
    vector<SyntaxType> types;
    vector<string> idents;
    vector<vector<int>> edges;
    vector<int> lineNumbers;
    int total;
    vector<int> fa;
    vector<int> returns;
    vector<Symbol> symbols;
    int curScope;
    
    int newNode(SyntaxType type) {
        int id = total++;
        types.emplace_back(type);
        idents.emplace_back("");
        edges.emplace_back(vector<int>(0));
        lineNumbers.emplace_back(0);
        return id;
    }
    
    void addIdent(int id, string ident) {
        idents[id] = std::move(ident);
    }
    
    void addEdge(int id, int son) {
        edges[id].emplace_back(son);
    }
    
    void addLineNumber(int id, int lineNumber) {
        lineNumbers[id] = lineNumber;
    }
    
    void checkErrorM(int x) {
        if (types[x] != SyntaxType::BreakStmt && types[x] != SyntaxType::ContinueStmt) {
            return;
        }
        int y = x;
        while (types[y] != SyntaxType::FuncDef && types[y] != SyntaxType::MainFuncDef) {
            if (types[y] == SyntaxType::WhileStmt) {
                return;
            }
            y = fa[y];
        }
        ErrorHandler::addError(ErrorType::m, lineNumbers[x]);
    }
    
    void getReturns(int x) {
        if (types[x] == SyntaxType::ReturnStmt) {
            returns.emplace_back(x);
        }
        for (auto to: edges[x]) {
            if (to != -1) {
                getReturns(to);
            }
        }
    }
    
    void checkErrorFG(int x) {
        if (types[x] != SyntaxType::FuncDef && types[x] != SyntaxType::MainFuncDef) {
            return;
        }
        returns.clear();
        if (types[x] == SyntaxType::FuncDef && idents[edges[x][0]] == "void") {
            returns.clear();
            getReturns(x);
            for (auto z: returns) {
                if (!edges[z].empty()) {
                    ErrorHandler::addError(ErrorType::f, lineNumbers[z]);
                }
            }
        } else {
            int z = edges[x].back();   //Block
            if (edges[z].empty()) {
                ErrorHandler::addError(ErrorType::g, lineNumbers[z]);
                return;
            }
            int w = edges[z].back();   //BlockItem
            w = edges[w][0];            //Stmt
            w = edges[w][0];            //return
            if (types[w] != SyntaxType::ReturnStmt) {
                ErrorHandler::addError(ErrorType::g, lineNumbers[z]);
            }
        }
    }
    
    Symbol getSymbol(const string &ident) {
        if (symbols.empty()) {
            return (Symbol) {"", false, false, false, 0, 0, vector<int>(0)};
        }
        for (int i = (int) symbols.size() - 1; i >= 0; i--) {
            if (symbols[i].ident == ident) {
                return symbols[i];
            }
        }
        return (Symbol) {"", false, false, false, 0, 0, vector<int>(0)};
    }
    
    void checkErrorC(int x) {
        if (types[x] != SyntaxType::LVal && types[x] != SyntaxType::UnaryExpIdent) {
            return;
        }
        Symbol symbol = getSymbol(idents[x]);
        if (symbol.ident.empty()) {
            ErrorHandler::addError(ErrorType::c, lineNumbers[x]);
        }
    }
    
    void checkErrorH(int x) {
        if (types[x] != SyntaxType::LVal ||
            (types[fa[x]] != SyntaxType::AssignStmt && types[fa[x]] != SyntaxType::GetintStmt)) {
            return;
        }
        Symbol symbol = getSymbol(idents[x]);
        if (symbol.isConst) {
            ErrorHandler::addError(ErrorType::h, lineNumbers[x]);
        }
    }
    
    int getDim(int x) {
        if (types[x] == SyntaxType::LVal) {
            Symbol symbol = getSymbol(idents[x]);
            return symbol.dim - (int) edges[x].size();
        }
        if (types[x] == SyntaxType::UnaryExpIdent) {
            Symbol symbol = getSymbol(idents[x]);
            if (symbol.isVoid) {
                return INT_MAX;
            }
            return 0;
        }
        int res = 0;
        for (auto y: edges[x]) {
            res = max(res, getDim(y));
        }
        return res;
    }
    
    void checkErrorDE(int x) {
        if (types[x] != SyntaxType::UnaryExpIdent) {
            return;
        }
        Symbol symbol = getSymbol(idents[x]);
        if (edges[x].empty()) {
            if (!symbol.params.empty()) {
                ErrorHandler::addError(ErrorType::d, lineNumbers[x]);
            }
            return;
        }
        int y = edges[x][0];    //FuncRParams
        if (edges[y].size() != symbol.params.size()) {
            ErrorHandler::addError(ErrorType::d, lineNumbers[x]);
            return;
        }
        for (int i = 0; i < (int) edges[y].size(); i++) {
            if (getDim(edges[y][i]) != symbol.params[i]) {
                ErrorHandler::addError(ErrorType::e, lineNumbers[x]);
                return;
            }
        }
    }
    
    void addSymbol(int x) {
        Symbol now;
        if (types[x] == SyntaxType::ConstDef) {
            now = (Symbol) {idents[x], false, false, true, 0, curScope, vector<int>(0)};
            for (auto z: edges[x]) {
                if (types[z] == SyntaxType::ConstExp) {
                    now.dim++;
                }
            }
        } else if (types[x] == SyntaxType::VarDef) {
            now = (Symbol) {idents[x], false, false, false, 0, curScope, vector<int>(0)};
            for (auto z: edges[x]) {
                if (types[z] == SyntaxType::ConstExp) {
                    now.dim++;
                }
            }
        } else if (types[x] == SyntaxType::FuncDef) {
            bool isVoid = (idents[edges[x][0]] == "void");
            now = (Symbol) {idents[x], false, isVoid, false, 0, curScope, vector<int>(0)};
            int y = edges[x][1];
            if (types[y] == SyntaxType::FuncFParams) {
                for (auto z: edges[y]) {
                    now.params.emplace_back(edges[z].size());
                }
            }
        } else if (types[x] == SyntaxType::FuncFParam) {
            int y = fa[fa[x]];
            int z = edges[y].back();
            now = (Symbol) {idents[x], false, false, false, (int) edges[x].size(), z, vector<int>(0)};
        } else {
            return;
        }
        for (const auto &sym: symbols) {
            if (sym.ident == now.ident && sym.scope == now.scope) {
                ErrorHandler::addError(ErrorType::b, lineNumbers[x]);
                return;
            }
        }
        symbols.emplace_back(now);
    }
    
    void dfs(int x, int f) {
        fa[x] = f;
        int lasScope = curScope;
        if (types[x] == SyntaxType::Block) {
            curScope = x;
        }
        checkErrorM(x);
        checkErrorFG(x);
        checkErrorC(x);
        checkErrorH(x);
        checkErrorDE(x);
        addSymbol(x);
        for (auto to: edges[x]) {
            if (to != -1) {
                dfs(to, x);
            }
        }
        if (types[x] == SyntaxType::Block) {
            while (!symbols.empty() && symbols.back().scope == x) {
                symbols.pop_back();
            }
        }
        curScope = lasScope;
    }
    
    void work() {
        fa.resize(total);
        dfs(0, -1);
    }
}

namespace Parser {
    int pos = 0;
    Word curWord;
    WordType curWordType;
    string curWordString;
    int curLineNumber;
    int lasLineNumber;
    vector<string> outputs;
    
    void nextWord(int step = 1) {
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
    
    void prevWord(int step = 1) {
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
    
    int getStmt();
    
    int getExp();
    
    int getLVal();

//    Number → IntConst
    int getNumber() {
        int id = SyntaxTree::newNode(SyntaxType::Number);
        
        SyntaxTree::idents[id] = curWordString;
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
    
    void work(const string &outputFile = "") {
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

int main() {
    Lexer::work("testfile.txt");
    Parser::work();
    SyntaxTree::work();
    ErrorHandler::output("error.txt");
    
    return 0;
}