# the boneless register allocator!
# absolutely not time efficient in any way

from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *
Label = L

__all__ = ["RegisterAllocator", "R_USE"]

# special pseudo-instruction to force a register use
class R_USE:
    def __init__(self, rsd):
        self.rsd = rsd

# the register allocate itself. instantiate one, then reference arbitrary
# registers in your code as attributes of the tracker. then add code with
# add_code, and finally return allocated code with allocate.
class RegisterAllocator:
    def __init__(self):
        # create a tracker so we can monitor how the user uses variables
        self.tracker = RegisterTracker()
        # remember all the code the user has added. this is a list of Insns,
        # NOT boneless Instrs!
        self.code = []

    def add_code(self, code):
        # the code can be an arbitrarily nested list, so we want to flatten it
        def flat_iter(l):
            for item in l:
                # assume we only get lists so we don't flatten something by
                # accident
                if isinstance(item, list):
                    yield from flat_iter(item)
                else:
                    yield item

        # convert from boneless Instrs to our Insns
        for instr in flat_iter([code]):
            if isinstance(instr, Label):
                self.code.append(instr)
            else:
                self.code.append(Insn(instr))

        print(self.code)

# assign each property a unique number so we know where a variable is used
class RegisterTracker:
    def __init__(self):
        # initialize with machine registers
        self._regs = {"R{}".format(r): r for r in range(8)}

    def __getattr__(self, name):
        if name.startswith("_"): # forward non-variable accesses
            return super().__getattr__(name)

        # assume any non-underscore attribute is trying to be a register.
        # get the number from the name, or automatically assign the next
        # available number to the name (and return that number).
        return self._regs.setdefault(name, len(self._regs))

# decoded boneless instruction
class Insn:
    # instructions that branch to a label (not including aliases)
    INSTRS_BRANCH = {BZ1, BZ0, BS1, BS0, BC1, BC0, BV1, BV0,
        BGTS, BGTU, BGES, BLES, BLEU, BLTS}
    # instructions where rsd is a source
    INSTRS_SOURCE = {ST, STR, STX, STXA}

    def __init__(self, instr):
        # what we should init:
        # OTHER PEOPLE'S PROPERTIES
        # r_in: tuple of register numbers used by this insn
        # r_out: tuple of register numbers defined by ths insn
        # target: tuple of possible control flow targets.
        #         None = next instruction
        #         str = label with that name
        #         int = register with that number

        # OUR PROPERTIES
        # we save the information required to reconstruct the Instr that we were
        # built from so that we can generate unique ones with different register
        # mappings as necessary, instead of just modifying the same one.
        # instr_type: type object of the instr that made this insn
        # instr_fields: dict mapping insn field names to register numbers (rN)
        #               or values (imm)

        # hack for R_USE cause it's not a boneless instr
        if isinstance(instr, R_USE):
            self.instr_type = R_USE
            self.instr_fields = {"rsd": instr.rsd}
            self.r_in = (instr.rsd,)
            self.r_out = ()
            self.target = (None,)
            return

        # first, take apart the instr into its component parts
        self.instr_type = type(instr)
        instr_fields = {}
        for field in instr._field_types:
            v = getattr(instr, "_"+field)
            if field.startswith("r"):
                # convert register objects back to numbers
                instr_fields[field] = int(v)
            else:
                # convert immediates to strings if possible
                instr_fields[field] = v.value
        self.instr_fields = instr_fields

        # second, un-alias the instruction so we have a common type
        instr_type = type(instr)
        while instr_type.alias is True:
            # the only subclass of an alias is the original instruction class
            instr_type = instr_type.__bases__[0]

        # default flow properties
        self.r_in = ()
        self.r_out = ()
        self.target = (None,)
        
        # third, figure out what exactly that type means
        if instr_type in Insn.INSTRS_BRANCH:
            dest = instr_fields["imm"]
            if not isinstance(dest, str):
                raise ValueError(
                    "branch target must be str, which '{}' is not".format(dest))
            # target is the next instruction or the branch's destination
            self.target = (None, dest)
        elif instr_type is J:
            # unconditional jump, no registers, and target is label
            dest = instr_fields["imm"]
            if not isinstance(dest, str):
                raise ValueError(
                    "jump target must be str, which '{}' is not".format(dest))
            self.target = (dest,)
        elif instr_type is JR:
            # register-based jump
            if instr_fields["imm"] != 0:
                raise ValueError(
                    "JR offset must be 0, which '{}' is not".format(dest))
            # we depend on the jump target register
            self.r_in = (instr_fields["rsd"],)
            # and that's where we go always
            self.target = (instr_fields["rsd"],)
        elif instr_type is JRAL:
            # jump to register and save next pc in register
            self.r_in = (instr_fields["rb"],)
            self.r_out = (instr_fields["rsd"],)
            # for subroutine calls, we assume the call will eventually return
            # back to where it started. if we didn't have to allocate registers
            # for the target (e.g. it was a different procedure) then the user
            # would write that differently.
            self.target = (instr_fields["rb"], None)
        elif instr_type is JVT or instr_type is JST:
            raise ValueError("can't yet allocate JVT or JST")
        elif instr_type is JAL:
            # jump to target and save next pc in register
            self.r_out = (instr_fields["rsd"],)
            if not isinstance(instr_fields["imm"], str):
                raise ValueError(
                    "JAL target must be str, which '{}' is not".format(dest))
            # see commentary in JRAL
            self.target = (instr_fields["imm"], None)
        elif instr_type is NOP:
            # encoded as conditional jump, but it doesn't depend on anything.
            # the default is fine.
            pass
        else:
            # just a generic instruction with whatever function
            ir = [
                # ra and rb are always inputs
                instr_fields.get("ra"),
                instr_fields.get("rb"),
            ]
            # rsd might be for a few rare instructions
            if instr_type in Insn.INSTRS_SOURCE:
                ir.append(instr_fields.get("rsd"))
            self.r_in = tuple(r for r in ir if r is not None)
            # but rsd is probably an output
            if instr_type not in Insn.INSTRS_SOURCE:
                try:
                    self.r_out = (instr_fields["rsd"],)
                except KeyError:
                    pass
            
