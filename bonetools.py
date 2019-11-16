# some nice tools for coding boneless asm

from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *

# help assign registers to names
class RegisterManager:
    # we could instantiate a new Reg each time, but looking up here is faster
    # and also ensures each register is the same object, if that's ever useful.
    BREGS = [R0, R1, R2, R3, R4, R5, R6, R7]

    def __init__(self, reglist=None):
        self._name2reg = {}
        self._reg2name = {}
        if reglist is not None:
            self += reglist

    # add some registers to be managed.
    # other is a string representing the new register names. each assignment
    # is the form of Rn:name such that after this operation, self.name is Rn.
    # assignments are separated by whitespace.
    def __iadd__(self, other):
        if not isinstance(other, str):
            raise TypeError("expected str, not '{}'".format(type(other)))
        add = {} # add all at once so we don't half-assign on exception
        for assignment in other.split():
            reg, name = assignment.split(":")
            if len(reg) != 2 or reg[0] != "R" or reg[1] not in "0123456789":
                raise ValueError("register '{}' is not 'Rn' form".format(reg))
            reg = int(reg[1])
            if reg > 7:
                raise ValueError("register R{} does not exist".format(reg))
            if not name.isidentifier():
                raise ValueError("name '{}' is not a valid "
                    "Python identifier".format(name))
            if reg in self._reg2name or reg in add:
                try:
                    name = self._reg2name[reg]
                except KeyError:
                    name = add[reg]
                raise ValueError("register R{} already assigned "
                    "to name '{}'".format(reg, name))
            if name in self._name2reg:
                raise ValueError("name '{}' already assigned "
                    "to register R{}".format(name, self._name2reg[name]))
            add[reg] = name

        for reg, name in add.items():
            self._name2reg[name] = reg
            self._reg2name[reg] = name

        return self

    # remove some registers to be managed.
    # other is a string representing register names to remove. removals are
    # separated by whitespace. if the first character is !, all registers
    # except those given are removed
    def __isub__(self, other):
        if not isinstance(other, str):
            raise TypeError("expected str, not '{}'".format(type(other)))
        if other.startswith("!"):
            invert = True
            other = other[1:]
        else:
            invert = False
        rem = [] # remove all at once so we don't half-remove on exception
        if not invert:
            for name in other.split():
                if name not in self._name2reg:
                    raise KeyError(
                        "name '{}' not currently assigned".format(name))
                rem.append(name)
        else:
            # assume we're removing everything
            rem = set(self._name2reg.keys())
            # then remove things from that list that we want to keep
            for name in other.split():
                if name not in self._name2reg:
                    raise KeyError(
                        "name '{}' not currently assigned".format(name))
                del rem[name]
            rem = list(rem)

        for name in rem:
            reg = self._name2reg[name]
            del self._name2reg[name]
            del self._reg2name[reg]

        return self

    # return a register assignment if it's available
    def __getattr__(self, name):
        ri = self._name2reg.get(name)
        if ri is None:
            raise KeyError("name '{}' not currently assigned".format(name))
        return RegisterManager.BREGS[ri]