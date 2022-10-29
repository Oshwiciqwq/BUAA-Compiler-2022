//
// Created by Oshwiciqwq on 2022/10/27.
//

#include "Compiler.h"

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
    
    void output(const string &outputFile) {
        if (!outputFile.empty()) {
            ofstream output(outputFile);
            for (auto &i: errors) {
                output << i.first << " " << ErrorTypeToString[(int) i.second] << endl;
            }
            output.close();
        }
    }
}
