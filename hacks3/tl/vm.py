# TL Virtual Machine

from bytecode import *

class StackEmptyException(Exception): pass

class UnknownBytecodeException(Exception): pass

class VM:
    def __init__(self):
        self.stack = []
        self.bytecode = []
        self.pc = 0
        self.opcode = 0

        self.jmp_table = {
            ADD: self.op_add,
            SUB: self.op_sub,
            MULT: self.op_mult,
            DIVIDE: self.op_divide,
            PRINT_TOS: self.op_print_tos,
            PUSH_INT: self.op_push_int,
            POP: self.op_pop,
            }
        
    def push(self, val):
        self.stack.append(val)

    def pop(self):
        return self.stack.pop()

    def tos(self):
        return self.stack[-1]

    def op_add(self):
        op1 = self.pop()
        op2 = self.pop()
        op3 = op1 + op2
        self.push(op3)
        
    def op_sub(self):
        op1 = self.pop()
        op2 = self.pop()
        op3 = op1 - op2
        self.push(op3)
        
    def op_mult(self):
        op1 = self.pop()
        op2 = self.pop()
        op3 = op1 * op2
        self.push(op3)

    def op_divide(self):
        op1 = pop(self)
        op2 = pop(self)
        op3 = op1 / op2
        self.push(op3)

    def op_print_tos(self):
        if len(self.stack) <= 0:
            raise StackEmptyException, (self.opcode, self.pc)
        
        print self.tos()

    def op_push_int(self):
        self.push(int(self.bytecode[self.pc]))
        self.pc += 1

    def op_pop(self):
        self.pop()

    def interpret(self):
        
        while 1:
            if self.pc >= len(self.bytecode):
                return
            
            self.opcode = self.bytecode[self.pc]
            self.pc += 1
            
            if self.jmp_table.has_key(self.opcode):
                self.jmp_table[self.opcode]()
            else:
                raise UnknownBytecodeException, (self.opcode, self.pc)

if __name__ == "__main__":
    vm = VM()
    vm.pc = 0
    vm.bytecode = [PUSH_INT, 1,
                   PUSH_INT, 2,
                   ADD,
                   PUSH_INT, 47,
                   MULT,
                   PRINT_TOS,
                   PUSH_INT, 142,
                   SUB,
                   PRINT_TOS]
    
    vm.interpret()
#     f = open(sys.argv[1], "r")
#     pc = 0
#     while 1:
#         bc = f.read(1)
#         if len(bc) > 0:
#             bytecode[pc] = bc
#         else:
#             break
#     f.close()
#     interpret()

    
