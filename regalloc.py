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

    def allocate(self):
        # step 1: find basic blocks
        bb = self._to_basic_blocks()
        self._bb_render(bb)
        return bb

    # turn our code into a graph of basic blocks
    def _to_basic_blocks(self):
        # we name basic blocks with the integer number of the instruction
        # where they start.
        bbs = {} # all of the ones we've found
        bb_starts = {0} # set of places we might find to start a basic block

        # remove all the labels from the instructions and add their location
        # to the basic block candidate list.
        code = [] # instructions minus labels
        label_locs = {} # instruction location of each label
        insn_ptr = 0
        for insn in self.code:
            if isinstance(insn, Label):
                label_locs[insn.name] = insn_ptr
                bb_starts.add(insn_ptr)
            else:
                code.append(insn)
                insn_ptr += 1

        # search through all the starts and see if we can make a basic block
        while len(bb_starts) > 0:
            bb_start = bb_starts.pop()
            if bb_start in bbs:
                continue # probably going to happen. just ignore it.

            insn_ptr = bb_start
            insns = [] # instructions in this basic block
            targets = [] # where this basic block goes
            while True:
                # are we bumping into another basic block?
                if insn_ptr in bb_starts or insn_ptr in bbs:
                    # yes, end this one
                    targets = [insn_ptr]
                    break
                try:
                    insn = code[insn_ptr]
                except IndexError:
                    # end of code (and thus basic block)
                    break
                # this instruction is always in this basic block
                insns.append(insn)
                # but where does this one go?
                if len(insn.targets) != 1 or insn.targets[0] is not None:
                    # somewhere exciting!
                    for target in insn.targets:
                        if isinstance(target, int):
                            # a register. we can't understand that yet.
                            raise Exception("target reg is bad")
                        elif isinstance(target, str):
                            # a label. convert it to the pointer number.
                            targets.append(label_locs[target])
                        elif target is None:
                            # it just goes to the next instruction
                            targets.append(insn_ptr+1)
                    break # a change of flow ends the basic block
                insn_ptr += 1

            # remember the basic block
            bbs[bb_start] = (insns, targets)
            # anywhere this one goes might be another
            bb_starts.update(targets)

        return bbs

    def _bb_render(self, bbs):
        from graphviz import Digraph
        dot = Digraph()
        for bb_name, bb in bbs.items():
            # create a node for each basic block with its code inside
            cstr = "\n".join(str(insn) for insn in bb[0])
            dot.node(str(bb_name), cstr, shape="box")
        for bb_name, bb in bbs.items():
            # then connect all the nodes
            for target in bb[1]:
                dot.edge(str(bb_name), str(target))
        dot.render("/tmp/blah", view=True)


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
        # targets: tuple of possible control flow targets.
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
            self.targets = (None,)
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
        self.targets = (None,)
        
        # third, figure out what exactly that type means
        if instr_type in Insn.INSTRS_BRANCH:
            dest = instr_fields["imm"]
            if not isinstance(dest, str):
                raise ValueError(
                    "branch target must be str, which '{}' is not".format(dest))
            # target is the next instruction or the branch's destination
            self.targets = (None, dest)
        elif instr_type is J:
            # unconditional jump, no registers, and target is label
            dest = instr_fields["imm"]
            if not isinstance(dest, str):
                raise ValueError(
                    "jump target must be str, which '{}' is not".format(dest))
            self.targets = (dest,)
        elif instr_type is JR:
            # register-based jump
            if instr_fields["imm"] != 0:
                raise ValueError(
                    "JR offset must be 0, which '{}' is not".format(dest))
            # we depend on the jump target register
            self.r_in = (instr_fields["rsd"],)
            # and that's where we go always
            self.targets = (instr_fields["rsd"],)
        elif instr_type is JRAL:
            # jump to register and save next pc in register
            self.r_in = (instr_fields["rb"],)
            self.r_out = (instr_fields["rsd"],)
            # for subroutine calls, we assume the call will eventually return
            # back to where it started. if we didn't have to allocate registers
            # for the target (e.g. it was a different procedure) then the user
            # would write that differently.
            self.targets = (instr_fields["rb"], None)
        elif instr_type is JVT or instr_type is JST:
            raise ValueError("can't yet allocate JVT or JST")
        elif instr_type is JAL:
            # jump to target and save next pc in register
            self.r_out = (instr_fields["rsd"],)
            if not isinstance(instr_fields["imm"], str):
                raise ValueError(
                    "JAL target must be str, which '{}' is not".format(dest))
            # see commentary in JRAL
            self.targets = (instr_fields["imm"], None)
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
            
    def __repr__(self):
        return("Insn(type={}, r_in={}, r_out={}, targets={})".format(
            str(self.instr_type).split(".")[-1][:-2], # sorry
            self.r_in, self.r_out, self.targets))
