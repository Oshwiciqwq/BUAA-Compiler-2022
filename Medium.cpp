//
// Created by Oshwiciqwq on 2022/10/27.
//

#include "Compiler.h"

namespace Medium {
    vector<string> strs;
    
    vector<IR> IRs;
    
    string addStr(const string &s) {
        strs.emplace_back(s);
        return "str" + to_string((int) strs.size() - 1);
    }
    
    void addIR(IRType type, string res, string num1, string num2) {
        IRs.emplace_back((IR) {type, std::move(res), std::move(num1), std::move(num2)});
    }
    
    void output() {
        ofstream output("ir.txt");
        for (int i = 0; i < (int) strs.size(); i++) {
            output << "str" << i << ":" << strs[i] << endl;
        }
        output << endl;
        for (auto &i: IRs) {
            output << IRTypeToString[(int) i.type] << " " << i.res << " " << i.num1 << " " << i.num2 << endl;
        }
        output.close();
    }
    
    map<string, int> paramCnt;
    
    ofstream codeOutput;
    map<string, pair<string, int>> mem;
    int gpOff = 0, spOff = 0, rec = 0;
    
    void getGpMem(const string &var) {
        if (var.substr(0, 3) != "var" && var.substr(0, 3) != "tmp") {
            return;
        }
        if (mem.find(var) != mem.end()) {
            return;
        }
        mem[var] = {"$gp", gpOff};
        gpOff += 4;
    }
    
    void getGpMemArr(const string &var, int offset) {
        if (var.substr(0, 3) != "var" && var.substr(0, 3) != "tmp") {
            return;
        }
        if (mem.find(var) != mem.end()) {
            return;
        }
        getGpMem(var);
        codeOutput << "addu $t0,$gp," << gpOff << endl;
        store("$t0", var);
        gpOff += offset;
    }
    
    void getSpMem(const string &var) {
        if (var.substr(0, 3) != "var" && var.substr(0, 3) != "tmp") {
            return;
        }
        if (mem.find(var) != mem.end()) {
            return;
        }
        spOff -= 4;
        mem[var] = {"$sp", spOff};
    }
    
    void getSpMemArr(const string &var, int offset) {
        if (var.substr(0, 3) != "var" && var.substr(0, 3) != "tmp") {
            return;
        }
        if (mem.find(var) != mem.end()) {
            return;
        }
        getSpMem(var);
        spOff -= offset;
        codeOutput << "addu $t0,$sp," << spOff << endl;
        store("$t0", var);
    }
    
    void load(const string &reg, string s) {
        if (SyntaxTree::isNumber(s)) {
            codeOutput << "li " << reg << "," << s << endl;
        } else {
            codeOutput << "lw " << reg << "," << mem[s].second << "(" << mem[s].first << ")" << endl;
        }
    }
    
    void load(const string &reg, const string &s, int offset) {
        codeOutput << "lw " << reg << "," << offset << "(" << s << ")" << endl;
    }
    
    void store(const string &reg, const string &s) {
        codeOutput << "sw " << reg << "," << mem[s].second << "(" << mem[s].first << ")" << endl;
    }
    
    void store(const string &reg, const string &s, int offset) {
        codeOutput << "sw " << reg << "," << offset << "(" << s << ")" << endl;
    }
    
    void calc(const IR &ir, const string &op) {
        load("$t0", ir.num1);
        load("$t1", ir.num2);
        codeOutput << op << " $t2,$t0,$t1" << endl;
        store("$t2", ir.res);
    }
    
    string getIndex(const string &arr, const string &index) {
        load("$t0", arr);
        load("$t1", index);
        codeOutput << "sll $t1,$t1,2" << endl;
        codeOutput << "addu $t0,$t0,$t1" << endl;
        return "$t0";
    }
    
    void translate(const IR &ir) {
        if (ir.type == IRType::ARR_SAVE) {
            load("$t2", ir.res);
            store("$t2", getIndex(ir.num1, ir.num2), 0);
        } else if (ir.type == IRType::ARR_LOAD) {
            load("$t2", getIndex(ir.num1, ir.num2), 0);
            store("$t2", ir.res);
        } else if (ir.type == IRType::ADD) {
            calc(ir, "addu");
        } else if (ir.type == IRType::SUB) {
            calc(ir, "subu");
        } else if (ir.type == IRType::MUL) {
            //TODO 移位优化
            load("$t0", ir.num1);
            load("$t1", ir.num2);
            codeOutput << "mult $t0,$t1" << endl;
            codeOutput << "mflo $t2" << endl;
            store("$t2", ir.res);
        } else if (ir.type == IRType::DIV) {
            load("$t0", ir.num1);
            load("$t1", ir.num2);
            codeOutput << "div $t0,$t1" << endl;
            codeOutput << "mflo $t2" << endl;
            store("$t2", ir.res);
        } else if (ir.type == IRType::MOD) {
            load("$t0", ir.num1);
            load("$t1", ir.num2);
            codeOutput << "div $t0,$t1" << endl;
            codeOutput << "mfhi $t2" << endl;
            store("$t2", ir.res);
        } else if (ir.type == IRType::GETINT) {
            codeOutput << "li $v0,5" << endl;
            codeOutput << "syscall" << endl;
            store("$v0", ir.res);
        } else if (ir.type == IRType::PRINT) {
            if (ir.res.substr(0, 3) == "str") {
                codeOutput << "la $a0," << ir.res << endl;
                codeOutput << "li $v0,4" << endl;
                codeOutput << "syscall" << endl;
            } else {
                load("$a0", ir.res);
                codeOutput << "li $v0,1" << endl;
                codeOutput << "syscall" << endl;
            }
        } else if (ir.type == IRType::RETURN) {
            if (!ir.res.empty()) {
                load("$v0", ir.res);
            }
            codeOutput << "jr $ra" << endl;
        } else if (ir.type == IRType::GETRET) {
            store("$v0", ir.res);
        } else if (ir.type == IRType::SAVE) {
            store("$ra", "$sp", spOff - 4);
            rec = 1;
        } else if (ir.type == IRType::PUSH) {
            rec++;
            load("$t0", ir.res);
            store("$t0", "$sp", spOff - rec * 4);
        } else if (ir.type == IRType::RESTORE) {
            load("$ra", "$sp", spOff - 4);
        } else if (ir.type == IRType::CALL) {
            codeOutput << "add $sp,$sp," << spOff - rec * 4 << endl;
            codeOutput << "jal " << ir.res << endl;
            codeOutput << "add $sp,$sp," << -(spOff - rec * 4) << endl;
        } else if (ir.type == IRType::FUNC) {
            codeOutput << endl << ir.res << ":" << endl;
            rec = paramCnt[ir.res];
        } else if (ir.type == IRType::PARAM) {
            rec--;
            load("$t0", "$sp", 4 * rec);
            store("$t0", ir.res);
        } else if (ir.type == IRType::EQ) {
            calc(ir, "seq");
        } else if (ir.type == IRType::NEQ) {
            calc(ir, "sne");
        } else if (ir.type == IRType::LSS) {
            calc(ir, "slt");
        } else if (ir.type == IRType::LEQ) {
            calc(ir, "sle");
        } else if (ir.type == IRType::AND) {
            calc(ir, "and");
        } else if (ir.type == IRType::OR) {
            calc(ir, "or");
        } else if (ir.type == IRType::JUMP) {
            codeOutput << "j " << ir.res << endl;
        } else if (ir.type == IRType::LABEL) {
            codeOutput << ir.res << ":" << endl;
        } else if (ir.type == IRType::BEQ) {
            load("$t0", ir.num1);
            load("$t1", ir.num2);
            codeOutput << "beq $t0,$t1," << ir.res << endl;
        } else if (ir.type == IRType::BNE) {
            load("$t0", ir.num1);
            load("$t1", ir.num2);
            codeOutput << "bne $t0,$t1," << ir.res << endl;
        }
    }
    
    void codeGen() {
        codeOutput.open("mips.txt");
        codeOutput << ".data" << endl;
        for (int i = 0; i < (int) strs.size(); i++) {
            codeOutput << "str" << i << ": .asciiz \"" << strs[i] << "\"" << endl;
        }
        
        codeOutput << endl << ".text" << endl;
        int i = 0;
        for (; i < (int) IRs.size(); i++) {
            if (IRs[i].type == IRType::FUNC) {
                break;
            }
            if (IRs[i].type == IRType::ARR) {
                getGpMemArr(IRs[i].res, stoi(IRs[i].num1));
            } else {
                getGpMem(IRs[i].res);
            }
            translate(IRs[i]);
        }
        codeOutput << "jal main" << endl;
        codeOutput << "jal end" << endl;
        
        for (; i < (int) IRs.size(); i++) {
            if (IRs[i].type == IRType::FUNC) {
                spOff = 0;
            }
            if (IRs[i].type == IRType::ARR) {
                getSpMemArr(IRs[i].res, stoi(IRs[i].num1));
            } else {
                getSpMem(IRs[i].res);
            }
            translate(IRs[i]);
        }
        
        codeOutput << "end:" << endl;
        codeOutput.close();
    }
}
