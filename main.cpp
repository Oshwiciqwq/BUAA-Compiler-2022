#include "Compiler.h"

using namespace std;

int main() {
    Lexer::work("testfile.txt");
    Parser::work();
    SyntaxTree::work();
//    ErrorHandler::output("error.txt");
//    Medium::output();
    Medium::codeGen();
    return 0;
}