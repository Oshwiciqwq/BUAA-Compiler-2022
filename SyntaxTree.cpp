//
// Created by Oshwiciqwq on 2022/10/27.
//

#include "Compiler.h"

namespace SyntaxTree {
    int varCnt = 0;
    int funCnt = 0;
    
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
            int z = edges[x].back();    //Block
            if (edges[z].empty()) {
                ErrorHandler::addError(ErrorType::g, lineNumbers[z]);
                return;
            }
            int w = edges[z].back();    //BlockItem
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
            now = (Symbol) {idents[x], true, isVoid, false, 0, curScope, vector<int>(0)};
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
        Symbol symbol = getSymbol(idents[x]);
        if (!symbol.ident.empty()) {
            idents[x] = symbol.name;
        }
        if (types[x] == SyntaxType::Block) {
            while (!symbols.empty() && symbols.back().scope == x) {
                symbols.pop_back();
            }
        }
        curScope = lasScope;
    }

//=======================================================================================================//
//================================================CodeGen================================================//
//=======================================================================================================//
    
    map<string, string> funcType;
    vector<pair<string, string>> labels;
    map<string, vector<int>> arrayDim;
    map<string, vector<int>> constVal;
    vector<string> initVals;
    
    int tmpCnt = 0;
    int labelCnt = 0;
    
    string getTmp() {
        return "tmp" + to_string(tmpCnt++);
    }
    
    string getLabel() {
        return "LABEL" + to_string(labelCnt++);
    }
    
    bool isNumber(string s) {
        return !isalpha(s[0]);
    }
    
    string getLVal(int x) {
        if (edges[x].empty()) {
            if (constVal.find(idents[x]) != constVal.end()) {
                return to_string(constVal[idents[x]].front());
            }
            return idents[x];
        }
        vector<string> exps;
        bool allNumber = true;
        for (auto to: edges[x]) {
            exps.emplace_back(getExp(to));
            allNumber &= isNumber(exps.back());
        }
        vector<int> &dim = arrayDim[idents[x]];
        if (exps.size() < dim.size()) {
            if ((int) exps.size() == 0) {
                return idents[x];
            } else {
                string tmp = getTmp();
                Medium::addIR(IRType::MUL, tmp, exps.front(), to_string(dim.back() * 4));
                string tmp1 = getTmp();
                Medium::addIR(IRType::ADD, tmp1, idents[x], tmp);
                return tmp1;
            }
        }
        if (constVal.find(idents[x]) != constVal.end() && allNumber) {
            vector<int> &arr = constVal[idents[x]];
            int index = 0;
            if ((int) exps.size() == 2) {
                index += stoi(exps.front()) * dim.back();
            }
            if ((int) exps.size() >= 1) {
                index += stoi(exps.back());
            }
            return to_string(arr[index]);
        }
        string tmp = getTmp();
        string index = getTmp();
        if ((int) exps.size() == 2) {
            Medium::addIR(IRType::MUL, index, exps.front(), to_string(dim.back()));
            Medium::addIR(IRType::ADD, index, index, exps.back());
        }
        if ((int) exps.size() == 1) {
            Medium::addIR(IRType::ADD, index, exps.back(), "0");
        }
        Medium::addIR(IRType::ARR_LOAD, tmp, idents[x], index);
        return tmp;
    }
    
    void saveLVal(int x, const string &val) {
        if (edges[x].empty()) {
            Medium::addIR(IRType::ADD, idents[x], val, "0");
            return;
        }
        vector<string> exps;
        for (auto to: edges[x]) {
            exps.emplace_back(getExp(to));
        }
        vector<int> &dim = arrayDim[idents[x]];
        string index = getTmp();
        if ((int) exps.size() == 2) {
            Medium::addIR(IRType::MUL, index, exps.front(), to_string(dim.back()));
            Medium::addIR(IRType::ADD, index, index, exps.back());
        }
        if ((int) exps.size() == 1) {
            Medium::addIR(IRType::ADD, index, exps.back(), "0");
        }
        Medium::addIR(IRType::ARR_SAVE, val, idents[x], index);
    }
    
    string getPrimaryExp(int x) {
        int y = edges[x].front();
        if (types[y] == SyntaxType::Exp) {
            return getExp(y);
        } else if (types[y] == SyntaxType::LVal) {
            return getLVal(y);
        } else {//Number
            return idents[y];
        }
    }
    
    string getUnaryExpIdent(int x) {
        vector<string> params;
        if (!edges[x].empty()) {
            int y = edges[x].front();
            for (auto to: edges[y]) {
                params.emplace_back(getExp(to));
            }
        }
        Medium::addIR(IRType::SAVE);
        for (const auto &tmp: params) {
            Medium::addIR(IRType::PUSH, tmp);
        }
        Medium::addIR(IRType::CALL, idents[x]);
        Medium::addIR(IRType::RESTORE);
        if (funcType[idents[x]] == "void") {
            return "";
        }
        string res = getTmp();
        Medium::addIR(IRType::GETRET, res);
        return res;
    }
    
    string getUnaryExp(int x) {
        int y = edges[x].front();
        if (types[y] == SyntaxType::PrimaryExp) {
            return getPrimaryExp(y);
        } else if (types[y] == SyntaxType::UnaryExpIdent) {
            return getUnaryExpIdent(y);
        } else {
            string now = getUnaryExp(edges[x][1]);
            if (isNumber(now)) {
                int num = stoi(now);
                if (idents[y] == "+") {
                    num = +num;
                } else if (idents[y] == "-") {
                    num = -num;
                } else {
                    num = !num;
                }
                return to_string(num);
            }
            string tmp = getTmp();
            if (idents[y] == "+") {
                Medium::addIR(IRType::ADD, tmp, "0", now);
            } else if (idents[y] == "-") {
                Medium::addIR(IRType::SUB, tmp, "0", now);
            } else {
                Medium::addIR(IRType::EQ, tmp, now, "0");
            }
            return tmp;
        }
    }
    
    string getMulExp(int x) {
        string now = getUnaryExp(edges[x].front());
        bool allNumber = isNumber(now);
        for (int i = 2; i < (int) edges[x].size(); i += 2) {
            string nxt = getUnaryExp(edges[x][i]);
            string op = idents[edges[x][i - 1]];
            allNumber &= isNumber(nxt);
            if (allNumber) {
                if (op == "*") {
                    now = to_string(stoi(now) * stoi(nxt));
                } else if (op == "/") {
                    now = to_string(stoi(now) / stoi(nxt));
                } else if (op == "%") {
                    now = to_string(stoi(now) % stoi(nxt));
                }
            } else {
                string tmp = getTmp();
                if (op == "*") {
                    Medium::addIR(IRType::MUL, tmp, now, nxt);
                } else if (op == "/") {
                    Medium::addIR(IRType::DIV, tmp, now, nxt);
                } else if (op == "%") {
                    Medium::addIR(IRType::MOD, tmp, now, nxt);
                }
                now = tmp;
            }
        }
        return now;
    }
    
    string getAddExp(int x) {
        string now = getMulExp(edges[x].front());
        bool allNumber = isNumber(now);
        for (int i = 2; i < (int) edges[x].size(); i += 2) {
            string nxt = getMulExp(edges[x][i]);
            string op = idents[edges[x][i - 1]];
            allNumber &= isNumber(nxt);
            if (allNumber) {
                if (op == "+") {
                    now = to_string(stoi(now) + stoi(nxt));
                } else if (op == "-") {
                    now = to_string(stoi(now) - stoi(nxt));
                }
            } else {
                string tmp = getTmp();
                if (op == "+") {
                    Medium::addIR(IRType::ADD, tmp, now, nxt);
                } else if (op == "-") {
                    Medium::addIR(IRType::SUB, tmp, now, nxt);
                }
                now = tmp;
            }
        }
        return now;
    }
    
    string getExp(int x) {
        return getAddExp(edges[x].front());
    }
    
    pair<string, bool> getRelExp(int x) {
        string now = getAddExp(edges[x].front());
        bool isBool = false;
        for (int i = 2; i < (int) edges[x].size(); i += 2) {
            isBool = true;
            string nxt = getAddExp(edges[x][i]);
            string op = idents[edges[x][i - 1]];
            string tmp = getTmp();
            if (op == "<") {
                Medium::addIR(IRType::LSS, tmp, now, nxt);
            } else if (op == "<=") {
                Medium::addIR(IRType::LEQ, tmp, now, nxt);
            } else if (op == ">") {
                Medium::addIR(IRType::LSS, tmp, nxt, now);
            } else {
                Medium::addIR(IRType::LEQ, tmp, nxt, now);
            }
            now = tmp;
        }
        return {now, isBool};
    }
    
    string getEqExp(int x) {
        auto t = getRelExp(edges[x].front());
        string now = t.first;
        if (!t.second && (int) edges[x].size() == 1) {
            string tmp = getTmp();
            Medium::addIR(IRType::NEQ, tmp, now, "0");
            return tmp;
        }
        for (int i = 2; i < (int) edges[x].size(); i += 2) {
            string nxt = getRelExp(edges[x][i]).first;
            string op = idents[edges[x][i - 1]];
            string tmp = getTmp();
            if (op == "==") {
                Medium::addIR(IRType::EQ, tmp, now, nxt);
            } else {
                Medium::addIR(IRType::NEQ, tmp, now, nxt);
            }
            now = tmp;
        }
        return now;
    }
    
    string getLAndExp(int x, const string &noLabel) {
        string tmp = getTmp();
        for (int i = 0; i < (int) edges[x].size(); i += 2) {
            string now = getEqExp(edges[x][i]);
            Medium::addIR(IRType::OR, tmp, now, "0");
            Medium::addIR(IRType::BEQ, noLabel, tmp, "0");
        }
        return tmp;
    }
    
    string getLOrExp(int x, const string &yesLabel, const string &noLabel) {
        string tmp = getTmp();
        for (int i = 0; i < (int) edges[x].size(); i += 2) {
            string label = getLabel();
            string now = getLAndExp(edges[x][i], label);
            Medium::addIR(IRType::LABEL, label);
            Medium::addIR(IRType::OR, tmp, now, "0");
            Medium::addIR(IRType::BNE, yesLabel, tmp, "0");
        }
        Medium::addIR(IRType::JUMP, noLabel);
        return tmp;
    }
    
    string getCond(int x, const string &yesLabel, const string &noLabel) {
        return getLOrExp(edges[x].front(), yesLabel, noLabel);
    }
    
    void getParams(int x) {
        for (auto y: edges[x]) {
            Medium::addIR(IRType::PARAM, idents[y]);
            if (!edges[y].empty()) {
                arrayDim.insert({idents[y], vector<int>(0)});
                for (auto z: edges[y]) {
                    if (z == -1) {
                        arrayDim[idents[y]].emplace_back(z);
                    } else {
                        arrayDim[idents[y]].emplace_back(stoi(getExp(z)));
                    }
                }
            }
        }
        Medium::paramCnt[idents[fa[x]]] = (int) edges[x].size();
    }
    
    void getInitVals(int x) {
        for (auto y: edges[x]) {
            if (types[y] == SyntaxType::ConstExp || types[y] == SyntaxType::Exp) {
                initVals.emplace_back(getExp(y));
            } else {
                getInitVals(y);
            }
        }
    }
    
    void codeGen(int x) {
        if (types[x] == SyntaxType::ConstDef || types[x] == SyntaxType::VarDef) {
            if (!edges[x].empty() && types[edges[x].front()] == SyntaxType::ConstExp) {
                int size = 1;
                arrayDim.insert({idents[x], vector<int>(0)});
                for (auto y: edges[x]) {
                    if (types[y] == SyntaxType::ConstExp) {
                        int s = stoi(getExp(y));
                        arrayDim[idents[x]].emplace_back(s);
                        size *= s;
                    }
                }
                Medium::addIR(IRType::ARR, idents[x], to_string(size * 4));
                if (types[edges[x].back()] == SyntaxType::InitVal ||
                    types[edges[x].back()] == SyntaxType::ConstInitVal) {
                    int a1 = edges[x].back();   //InitVal
                    initVals.clear();
                    getInitVals(a1);
                    for (int i = 0; i < (int) initVals.size(); i++) {
                        Medium::addIR(IRType::ARR_SAVE, initVals[i], idents[x], to_string(i));
                    }
                    if (types[x] == SyntaxType::ConstDef) {
                        vector<int> val = vector<int>(0);
                        for (const auto &i: initVals) {
                            val.emplace_back(stoi(i));
                        }
                        constVal[idents[x]] = val;
                    }
                    
                }
            } else {
                Medium::addIR(IRType::VAR, idents[x], "4");
                if (!edges[x].empty() &&
                    (types[edges[x].back()] == SyntaxType::InitVal ||
                     types[edges[x].back()] == SyntaxType::ConstInitVal)) {
                    int a1 = edges[x].back();   //InitVal
                    int a2 = edges[a1].front(); //Exp
                    string initVal = getExp(a2);
                    Medium::addIR(IRType::ADD, idents[x], initVal, "0");
                    if (types[x] == SyntaxType::ConstDef) {
                        constVal[idents[x]] = vector<int>{stoi(initVal)};
                    }
                }
            }
        } else if (types[x] == SyntaxType::FuncDef || types[x] == SyntaxType::MainFuncDef) {
            funcType[idents[x]] = idents[edges[x].front()];
            Medium::addIR(IRType::FUNC, idents[x]);
            if ((int) edges[x].size() == 3) {
                getParams(edges[x][1]);
            }
            codeGen(edges[x].back());
            if (funcType[idents[x]] == "void") {
                Medium::addIR(IRType::RETURN);
            }
        } else if (types[x] == SyntaxType::AssignStmt) {
            saveLVal(edges[x].front(), getExp(edges[x].back()));
        } else if (types[x] == SyntaxType::ReturnStmt) {
            if (!edges[x].empty()) {
                Medium::addIR(IRType::RETURN, getExp(edges[x].front()));
            } else {
                Medium::addIR(IRType::RETURN);
            }
        } else if (types[x] == SyntaxType::GetintStmt) {
            string tmp = getTmp();
            Medium::addIR(IRType::GETINT, tmp);
            saveLVal(edges[x].front(), tmp);
        } else if (types[x] == SyntaxType::PrintfStmt) {
            if ((int) idents[x].size() == 2) {
                return;
            }
            queue<string> exps;
            for (auto y: edges[x]) {
                exps.push(getExp(y));
            }
            string str = idents[x].substr(1, idents[x].size() - 2);
            int las = 0;
            while (str.find('%', las) != string::npos) {
                int now = (int) str.find('%', las);
                if (las < now) {
                    Medium::addIR(IRType::PRINT, Medium::addStr(str.substr(las, now - las)));
                }
                Medium::addIR(IRType::PRINT, exps.front());
                exps.pop();
                las = now + 2;
            }
            if (las < (int) str.size()) {
                Medium::addIR(IRType::PRINT, Medium::addStr(str.substr(las)));
            }
        } else if (types[x] == SyntaxType::Exp) {
            getExp(x);
        } else if (types[x] == SyntaxType::IfStmt) {
            string yesLabel = getLabel();
            string noLabel = getLabel();
            string now = getCond(edges[x].front(), yesLabel, noLabel);
            
            Medium::addIR(IRType::BEQ, noLabel, now, "0");
            Medium::addIR(IRType::LABEL, yesLabel);
            codeGen(edges[x][1]);
            if ((int) edges[x].size() == 2) {
                Medium::addIR(IRType::LABEL, noLabel);
            } else {
                string endLabel = getLabel();
                Medium::addIR(IRType::JUMP, endLabel);
                Medium::addIR(IRType::LABEL, noLabel);
                codeGen(edges[x][2]);
                Medium::addIR(IRType::LABEL, endLabel);
            }
        } else if (types[x] == SyntaxType::WhileStmt) {
            string startLabel = getLabel();
            string yesLabel = getLabel();
            string noLabel = getLabel();
            labels.emplace_back(startLabel, noLabel);
            Medium::addIR(IRType::LABEL, startLabel);
            string now = getCond(edges[x].front(), yesLabel, noLabel);
            Medium::addIR(IRType::BEQ, noLabel, now, "0");
            Medium::addIR(IRType::LABEL, yesLabel);
            codeGen(edges[x][1]);
            Medium::addIR(IRType::JUMP, startLabel);
            Medium::addIR(IRType::LABEL, noLabel);
            labels.pop_back();
        } else if (types[x] == SyntaxType::ContinueStmt) {
            Medium::addIR(IRType::JUMP, labels.back().first);
        } else if (types[x] == SyntaxType::BreakStmt) {
            Medium::addIR(IRType::JUMP, labels.back().second);
        } else {
            for (auto to: edges[x]) {
                codeGen(to);
            }
        }
    }
    
    void work() {
        fa.resize(total);
        dfs(0, -1);
        if (!ErrorHandler::error()) {
            codeGen(0);
        }
    }
}
