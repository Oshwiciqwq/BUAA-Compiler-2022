//
// Created by Oshwiciqwq on 2022/10/27.
//

#include "Compiler.h"

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
    
    void work(const string &inputFile, const string &outputFile) {
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
