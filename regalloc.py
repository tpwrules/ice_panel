# the boneless register allocator!
# absolutely not time efficient in any way

# SOME RESOURCES I USED
# https://www.cs.umd.edu/class/spring2014/cmsc430/lectures/lec19.pdf
# http://www.cs.cmu.edu/afs/cs/academic/class/15745-s13
#       /public/lectures/L15-Register-Allocation.pdf
# https://web.cecs.pdx.edu/~apt/cs322/lecture10.pdf
# http://www.cs.utexas.edu/users/mckinley/380C/lecs/briggs-thesis-1992.pdf
# https://courses.cs.cornell.edu/cs412/2004sp/lectures/lec36.pdf
# https://www.cs.rice.edu/~keith/EMBED/dom.pdf
# https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf
# https://cseweb.ucsd.edu/classes/fa03/cse231/lec5seq.pdf
# https://sites.cs.ucsb.edu/~yufeiding/cs293s/slides/293S_06_SSA.pdf
# https://www.cs.colostate.edu/~mstrout/CS553/slides/lecture03.pdf

from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *
Label = L

from collections import namedtuple

__all__ = ["RegisterAllocator", "R_USE"]

# special pseudo-instruction to force a register use
class R_USE:
    def __init__(self, rsd):
        self.rsd = rsd

# the register allocator itself. instantiate one, then reference arbitrary
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
        bb = RegisterAllocator._make_basic_blocks(self.code)
        RegisterAllocator._bb_render(bb)
        return bb

    # turn some code into a graph of basic blocks
    @classmethod
    def _make_basic_blocks(cls, code_in):
        # a basic block's (BB) identifier is its integer index into the code.
        bbs = {} # dict from identifiers to the BB objects
        bb_starts = {0} # set of code inindices that might start a BB

        # labels always start a BB. they're also not real instructions. remove
        # all the labels from the instructions and add their location to the BB
        # candidate list.
        code = [] # the code, without labels. insn references index this.
        label_indices = {} # dict of label names to index of their first insn
        insn_idx = 0 # the number of the instruction that we're looking at
        for insn in code_in:
            if isinstance(insn, Label): # is this "instruction" a label?
                label_indices[insn.name] = insn_idx # remember where it starts
                bb_starts.add(insn_idx) # and queue it for BB search
            else: # it's just an instruction
                code.append(insn) # keep it
                insn_idx += 1 # advance insn index since we've added another

        # make a BB from each instruction that starts one
        while len(bb_starts) > 0:
            bb_start = bb_starts.pop()
            if bb_start in bbs: # is this already the start of a BB?
                continue # no need to process it again

            insn_idx = bb_start
            insns = [] # instructions in this BB
            targets = set() # identifiers of BBs this one may jump to
            while True:
                # are we bumping into another BB?
                if insn_idx in bb_starts or insn_idx in bbs:
                    # yes, end this one.
                    targets = {insn_idx} # it has to fall through to the bumpee
                    break
                if insn_idx == len(code): # finished with the instructions?
                    # this BB has to end. it doesn't have any targets.
                    break
                insn = code[insn_idx]
                # this instruction is always in this BB
                insns.append(insn)
                # but where does it go?
                if len(insn.targets) != 1 or None not in insn.targets:
                    # somewhere exciting!
                    for target in insn.targets:
                        if isinstance(target, int):
                            # a register. we can't understand that yet.
                            raise Exception("target reg is bad")
                        elif isinstance(target, str):
                            # a label. convert the name to a code index.
                            targets.add(label_indices[target])
                        elif target is None:
                            # it just goes to the next instruction.
                            targets.add(insn_idx+1)
                    break # a change of flow must end the BB
                insn_idx += 1

            bb = BasicBlock(
                insns=insns, targets=targets,
                # we calculate these later
                r_in=set(), r_out=set(), sources=set(),
            )
            bbs[bb_start] = bb

            # any code index that this BB targets must be another BB.
            bb_starts |= targets

        # from this point on, the instruction indices are meaningless. the basic
        # block identifier is now just an opaque number.

        return bbs

    @classmethod
    def _bb_render(cls, bbs):
        from graphviz import Digraph
        dot = Digraph()
        for bb_ident, bb in bbs.items():
            # create a node for each basic block with its code inside
            code = "\n".join(str(insn) for insn in bb.insns)
            dot.node(str(bb_ident), code, shape="box")
        for bb_ident, bb in bbs.items():
            # then connect all the nodes
            for target in bb.targets:
                dot.edge(str(bb_ident), str(target))
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
        # r_in: set of register numbers used by this insn
        # r_out: set of register numbers defined by ths insn
        # targets: set of possible control flow targets.
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
            self.r_in = {instr.rsd}
            self.r_out = set()
            self.targets = {None}
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
            # the only superclass of an alias is the original instruction class
            instr_type = instr_type.__bases__[0]

        # default flow properties
        self.r_in = set()
        self.r_out = set()
        self.targets = {None}
        
        # third, figure out what exactly that type means
        if instr_type in Insn.INSTRS_BRANCH:
            dest = instr_fields["imm"]
            if not isinstance(dest, str):
                raise ValueError(
                    "branch target must be str, which '{}' is not".format(dest))
            # target is the next instruction or the branch's destination
            self.targets = {None, dest}
        elif instr_type is J:
            # unconditional jump, no registers, and target is label
            dest = instr_fields["imm"]
            if not isinstance(dest, str):
                raise ValueError(
                    "jump target must be str, which '{}' is not".format(dest))
            self.targets = {dest}
        elif instr_type is JR:
            # register-based jump
            if instr_fields["imm"] != 0:
                raise ValueError(
                    "JR offset must be 0, which '{}' is not".format(dest))
            # we depend on the jump target register
            self.r_in = {instr_fields["rsd"]}
            # and that's where we go always
            self.targets = {instr_fields["rsd"]}
        elif instr_type is JRAL:
            # jump to register and save next pc in register
            self.r_in = {instr_fields["rb"]}
            self.r_out = {instr_fields["rsd"]}
            # for subroutine calls, we assume the call will eventually return
            # back to where it started. if we didn't have to allocate registers
            # for the target (e.g. it was a different procedure) then the user
            # would write that differently.
            self.targets = {instr_fields["rb"], None}
        elif instr_type is JVT or instr_type is JST:
            raise ValueError("can't yet allocate JVT or JST")
        elif instr_type is JAL:
            # jump to target and save next pc in register
            self.r_out = {instr_fields["rsd"]}
            if not isinstance(instr_fields["imm"], str):
                raise ValueError(
                    "JAL target must be str, which '{}' is not".format(dest))
            # see commentary in JRAL
            self.targets = {instr_fields["imm"], None}
        elif instr_type is NOP:
            # encoded as conditional jump, but it doesn't depend on anything.
            # the default is fine.
            pass
        else:
            # just a generic instruction with whatever function

            # ra and rb are always inputs
            ra = instr_fields.get("ra")
            if ra is not None: self.r_in.add(ra)
            rb = instr_fields.get("rb")
            if rb is not None: self.r_in.add(rb)
            # rsd might be an input too for a few rare instructions
            rsd = instr_fields.get("rsd")
            if instr_type in Insn.INSTRS_SOURCE and rsd is not None:
                self.r_in.add(rsd)

            # but rsd is probably an output
            if instr_type not in Insn.INSTRS_SOURCE and rsd is not None:
                self.r_out = {instr_fields["rsd"]}
            
    def __repr__(self):
        return("Insn(type={}, r_in={}, r_out={}, targets={})".format(
            str(self.instr_type).split(".")[-1][:-2], # sorry
            self.r_in, self.r_out, self.targets))

# basic block type
BasicBlock = namedtuple("BasicBlock", [
    "insns", # list of instructions in this basic block
    "r_in", # set of registers used by this basic block
    "r_out", # set of registers defined by this basic block
    "sources", # set of basic blocks that may jump to us
    "targets", # set of basic blocks that we may jump to
])