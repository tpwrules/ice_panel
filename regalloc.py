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
        bbs = RegisterAllocator._make_basic_blocks(self.code)
        bbs = RegisterAllocator._renumber_regs(bbs)
        interferences = RegisterAllocator._calculate_interferences(bbs)
        colors, uncolorable = \
            RegisterAllocator._color_interferences(interferences)
        if len(uncolorable) > 0:
            raise Exception("failed to color", uncolorable)
        RegisterAllocator._bb_render_code(bbs)
        RegisterAllocator._bb_render_interferences(interferences, colors)
        return bbs

    # turn some code into a graph of basic blocks
    @classmethod
    def _make_basic_blocks(cls, code_in):
        # a basic block's (BB) identifier is its integer index into the code.
        bbs = {} # dict from identifiers to the BB objects
        bb_starts = {0} # set of code indices that might start a BB

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
                r_use=set(), r_def=set(), sources=set(),
            )
            bbs[bb_start] = bb

            # any code index that this BB targets must be another BB.
            bb_starts |= targets

        # from this point on, the instruction indices are meaningless. the basic
        # block identifier is now just an opaque number.

        # now that we know each BB and where it goes, use that information to
        # calculate where each BB might come from.
        for bb_ident, bb in bbs.items():
            for target in bb.targets:
                bbs[target].sources.add(bb_ident)

        # calculate register usage for each BB as a whole
        for bb_ident, bb in bbs.items():
            for insn in bb.insns:
                # regs that this instruction uses must come from outside this BB
                # unless they are already defined by a previous instruction
                bb.r_use.update(insn.r_use - bb.r_def)
                # regs that this instruction defines are defined by this BB
                bb.r_def.update(insn.r_def)

        # freeze each BB so that other functions can't touch it and the return
        # of this function can always be reused. (it's ok that we didn't freeze
        # them earlier since we make them and know how we use them)
        for bb_ident, bb in bbs.items():
            bbs[bb_ident] = BasicBlock(
                insns=tuple(bb.insns),
                sources=frozenset(bb.sources),
                targets=frozenset(bb.targets),
                r_use=frozenset(bb.r_use),
                r_def=frozenset(bb.r_def),
            )

        return bbs

    # re"number" each register such that each returned register name represents
    # one virtual register that can't be assigned the same physical register as
    # another virtual register with an overlapping lifetime. instead of numbers,
    # registers are returned named as tuples.
    @classmethod
    def _renumber_regs(cls, bbs):
        # basically, we renumber the registers so that they're vaguely SSA

        # first thing is to give each register definition its own "generation".
        # i.e. if one register is defined by two basic blocks, each register
        # gets a different generation.
        gens = {} # dict from register number to latest generation number
        def add_generation(bb):
            r_def_genned = set()
            # replace each register number with (reg num, generation)
            for r_def in bb.r_def:
                gen = gens.get(r_def, 1)
                gens[r_def] = gen+1
                r_def_genned.add((r_def, gen))
            return bb._replace(r_def=frozenset(r_def_genned))
        # rebuild dict so we don't modify the one passed in
        bbs = {bb_ident: add_generation(bb) for bb_ident, bb in bbs.items()}

        # the second thing is to figure out which generations will be defined at
        # the point of each register use. we do this by computing the
        # "reachability": which generations of what registers will be defined by
        # the time each BB is entered.

        # dict from BB ident to set of (reg, gen) tuples that reach that BB
        reachable = {bb_ident: set() for bb_ident in bbs.keys()}
        # dict from BB ident to set of registers that that BB defines
        defined = {}
        for bb_ident, bb in bbs.items():
            defined[bb_ident] = set(r_def for r_def, r_def_gen in bb.r_def)
        changed = True # loop until we've stopped updating reachability
        while changed:
            changed = False
            for bb_ident, bb in bbs.items():
                reaching_defs = set()
                # look through all the BBs that lead to us
                for bb_pred_ident in bb.sources:
                    bb_pred = bbs[bb_pred_ident]
                    # all the generations defined by that BB must reach us
                    reaching_defs |= bb_pred.r_def
                    # as well as any generations that reach that BB, so long as
                    # that BB does not redefine them into a new generation!
                    for r_def, r_def_gen in reachable[bb_pred_ident]:
                        if r_def not in defined[bb_pred_ident]:
                            reaching_defs.add((r_def, r_def_gen))

                if len(reaching_defs - reachable[bb_ident]): # found new defs?
                    reachable[bb_ident] = reaching_defs
                    # changing what reaches us might also change what reaches
                    # other BBs, so we need to recalculate another time
                    changed = True

        # we can figure out which generations each BB might use based on what's
        # reachable to that BB
        for bb_ident, bb in bbs.items():
            r_use_genned = set()
            # we mark as used every generation of every register used by this
            # BB. note that we use multiple generations if available; those will
            # be merged later. we don't remember generations for registers this
            # BB doesn't use; its targets will remember if necessary.
            for r_use, r_use_gen in reachable[bb_ident]:
                if r_use in bb.r_use:
                    r_use_genned.add((r_use, r_use_gen))
            bbs[bb_ident] = bb._replace(r_use=frozenset(r_use_genned))

        # we've calculated what register generations each BB uses and defines,
        # but the insns within still don't know. fortunately, since they are
        # straight line code, it's straightforward to figure out.
        for bb_ident, bb in bbs.items():
            insns_genned = []
            # the current generation defined for each register. since we go
            # backwards through the insns, this starts out as the generation
            # that the BB defines.
            def_gen = {r: r_gen for r, r_gen in bb.r_def}
            for insn in bb.insns[::-1]:
                r_def = set()
                for r in insn.r_def:
                    # if this insn defines a register, it must define the
                    # current generation because that's what future insns are
                    # expecting to use
                    r_def.add((r, def_gen[r]))
                    # but! now that this insn has defined that generation, no
                    # other insn can, cause this is SSA. any other definitions
                    # have to be to a new generation. additionally, insns before
                    # this one (i.e. those later in the loop) don't have access
                    # to this generation, so all usages have to be from that new
                    # generation too.
                    gen = gens.get(r, 1)
                    gens[r] = gen+1
                    def_gen[r] = gen
                # this insn can only use the currently defined generations
                r_use = set()
                for r in insn.r_use:
                    if r not in def_gen:
                        # we haven't defined this register yet, which can only
                        # happen if it was defined earlier in this BB. that
                        # definition must be to a new generation, cause this is
                        # still SSA, so create and remember it for when we
                        # encounter that definition.
                        gen = gens.get(r, 1)
                        gens[r] = gen+1
                        def_gen[r] = gen
                    r_use.add((r, def_gen[r]))
                insns_genned.append(insn._replace(
                    r_use=frozenset(r_use), r_def=frozenset(r_def)))
            # at this point, the currently defined generation for each register
            # is the generation that must be defined outside this BB. since we
            # assume that generation will be used by insns within this BB, the
            # BB has to use them too. of course, if the BB doesn't use that
            # register, then the assumed insns can't exist, so the BB still
            # doesn't need to use any generations of that register.
            # set of registers used by this BB
            nums_used = set(r for r, r_gen in bb.r_use)
            r_use = bb.r_use.union(
                (r, r_gen) for r, r_gen in def_gen.items() if r in nums_used)
            # there is by definition another generation of each register that's
            # already used by this BB, but it's okay if we have multiple; that
            # gets fixed later.
            bbs[bb_ident] = bb._replace(
                r_use=r_use, insns=tuple(insns_genned[::-1]))

        # once the above is done, each BB ends up using a lot of generations of
        # the same register, each from a different definition. however, the
        # insns using that register (i.e. from the start of the BB to before any
        # definitions of it) can only use one particular generation. thus, all
        # generations of a each register used by this BB MUST be replaced by the
        # generation actually used by this BB's insns so that all those
        # definitions will define the one generation that gets used.

        # by the way, this makes our BBs definitely not SSA

        # dict from (reg, gen) to (reg, replaced_gen)
        gen_map = {}
        # first we go through and figure out which generations need to be
        # replaced, and by what
        for bb in bbs.values():
            # sort the (reg, gen) tuples used by this BB into dict of reg to set
            # of that reg's gens
            used_gens = {}
            for r, r_gen in bb.r_use:
                used_gens.setdefault(r, set()).add(r_gen)
            # figure out which registers have multiple available generations
            for r, r_used_gens in used_gens.items():
                if len(r_used_gens) == 1:
                    # if only one generation is available, no need to replace it
                    continue
                # but if there's more than one, we have to. figure out if any of
                # the generations already has a replacement.
                for r_gen in r_used_gens:
                    replace_gen = gen_map.get((r, r_gen))
                    if replace_gen is not None:
                        # this one does. use its replacement for the others too.
                        replace_gen = replace_gen[1]
                        break
                else:
                    # it doesn't, so create a new replacement generation
                    replace_gen = gens.get(r, 1)
                    gens[r] = replace_gen+1
                # we say that all the generations this BB uses get replaced by
                # the one selected above
                for r_gen in r_used_gens:
                    gen_map[(r, r_gen)] = (r, replace_gen)
        # now that we know what each generation is replaced by, apply that
        # knowledge to replace generations across the whole structure
        def replace(regs):
            # look up what each (reg, gen) is replaced by, or leave it alone if
            # we don't have a replacement
            return frozenset(
                gen_map.get((r, r_gen), (r, r_gen)) for r, r_gen in regs)
        for bb_ident, bb in bbs.items():
            insns = []
            bbs[bb_ident] = bb._replace(
                r_use=replace(bb.r_use),
                r_def=replace(bb.r_def),
                insns=tuple(insn._replace(r_use=replace(insn.r_use),
                    r_def=replace(insn.r_def)) for insn in bb.insns)
            )

        # at this point, each (reg, gen) tuple is a unique register in the
        # program such that it represents one virtual register that can't be
        # assigned the same physical register as another virtual register with
        # an overlapping lifetime.

        return bbs

    # calculate the interference of lifetimes in a BB
    @classmethod
    def _calculate_interferences(cls, bbs):
        # first thing to do is propagate register liveness so that we know which
        # registers are live at the start (in) and end (out) of each BB.

        # dict of BB ident to sets of such
        bb_in = {bb_ident: set() for bb_ident in bbs.keys()}
        bb_out = {bb_ident: set() for bb_ident in bbs.keys()}
        # we go "backward" through the dict because the information flows
        # backward, and thus we loop less times
        changed = True # loop until we've stopped updating reachability
        while changed:
            changed = False
            for bb_ident in sorted(bbs.keys(), reverse=True):
                bb = bbs[bb_ident]
                # a register has to be live coming into us if we use it, or it's
                # live going out of us and we don't define it ourselves (i.e. it
                # must be defined outside us, so it has to go through us)
                r_in = bb.r_use | (bb_out[bb_ident] - bb.r_def)
                # if a register is live going into any of our targets, it has to
                # be live coming out of us because that's where the target
                # expects to get it from
                r_out = frozenset().union(*(bb_in[t] for t in bb.targets))

                if len(r_in - bb_in[bb_ident]): # found new live-ins?
                    bb_in[bb_ident] = r_in
                    # changing what's live coming into us might change what's
                    # live going out of other BBs, so we need to recalculate
                    changed = True

                if len(r_out - bb_out[bb_ident]): # found new live-outs?
                    bb_out[bb_ident] = r_out
                    # changing what's live going out of us might change what's
                    # live coming into other BBs, so we need to recalculate
                    changed = True

        # show off our hard work
        RegisterAllocator._bb_render_in_out(bbs, bb_in, bb_out)

        # now that we know which registers we need to be concerned with in each
        # basic block, we have to extend that knowledge to the individual
        # instructions the live ranges of each register.

        # figure out what all the registers actually are
        all_regs = set()
        for bb in bbs.values():
            for insn in bb.insns:
                all_regs |= insn.r_use
                all_regs |= insn.r_def
        all_regs = frozenset(all_regs)

        # dict from register to set of (bb ident, insn idx) of instructions that
        # the register will be live for. insn idx can be len(insn) for registers
        # that are live after the last insn.
        live_ranges = {r: set() for r in all_regs}
        for bb_ident, bb in bbs.items():
            # since this is straight line code, we can easily figure out the
            # liveness of its registers. a register is live from the instruction
            # after its first definition to the instruction of its last use.

            # dict from register to insn idx of first definition. registers live
            # into this BB were effectively defined before the first insn.
            def_insns = {r: -1 for r in bb_in[bb_ident]}
            # dict from register to insn idx of last use. registers live out of
            # this bb are effectively used after the last insn.
            use_insns = {r: len(bb.insns) for r in bb_out[bb_ident]}

            for insn_idx, insn in enumerate(bb.insns):
                for r in insn.r_def:
                    # say that this register is defined at this index, as long
                    # as the register isn't already in the dict
                    def_insns.setdefault(r, insn_idx)
                for r in insn.r_use:
                    # is this use later than the use already in the dict?
                    if insn_idx > use_insns.get(r, -1):
                        # yes, update with our index
                        use_insns[r] = insn_idx

            # each register is live from the insn idx after its definition
            # through the insn idx of its last use.
            for r in def_insns.keys():
                # if insn n defines a register, it's live for insn n+1
                def_idx = def_insns[r]+1
                # but if insn is the last user of a register, it's dead after.
                # some registers aren't used cause they're trash outputs. those
                # we say are used only by the insn after the one that created
                # them. they have to be alive at least one insn so that they get
                # allocated.
                use_idx = use_insns.get(r, def_idx)
                live_ranges[r].update(
                    ((bb_ident, idx) for idx in range(def_idx, use_idx+1)))

        # we know precisely when a register is live, so we can now figure out
        # which others might interfere with it. this is currently probably
        # pretty inefficient.

        # dict of registers to set of registers that interfere with it
        interferences = {}
        for r in all_regs:
            these_interferences = set()
            for r_o in all_regs:
                # don't need to say that register interferes with itself
                if r == r_o:
                    continue
                if not live_ranges[r].isdisjoint(live_ranges[r_o]):
                    # they have a live point in common -> they interfere
                    these_interferences.add(r_o)
            interferences[r] = frozenset(these_interferences)

        return interferences

    # color the interferences with physical registers so that no physical
    # register interferes with itself
    @classmethod
    def _color_interferences(cls, interferences):
        # this isn't real graph data structures, so this is gonna get ugly

        # duplicate the input dict so we can slice and dice it. it doesn't need
        # to be a deep copy since it's only got frozensets inside.
        interferences = interferences.copy()

        phys_regs = 8

        nodes = []
        # first, we "simplify" the interference graph by pulling out registers
        # that could be colored first, then ones that definitely can't be.
        while len(interferences) > 0:
            for r, interferers in interferences.items():
                # we have to figure out how many registers this one currently
                # interferes with. some may have been removed, so we only count
                # the ones that are still here.
                num_interferers = 0
                for r_o in interferers:
                    if r_o in interferences:
                        num_interferers += 1
                # if this one interferes with less registers than there are
                # physical registers, it could be colored and we want to
                # schedule that
                if num_interferers < phys_regs:
                    nodes.append((r, interferers))
                    break
            else:
                # we didn't find a register that's not impossible to color. ah
                # well, let's hope the last one we looked at will be colorable
                # in the future.
                nodes.append((r, interferers))
            # the r left over is the one we just scheduled for coloring. it
            # doesn't need to be scheduled again.
            del interferences[r]

        # now we "select" nodes to color, starting with the ones we claimed
        # couldn't be and hoping that they can get a color anyway.
        colors = {} # dict from reg to its color
        uncolorable = []
        all_colors = set(range(phys_regs))
        for r, interferers in nodes[::-1]:
            # figure out the colors of registers that interfere with this one
            # (we let None mean no color)
            other_colors = set(colors.get(r_o, None) for r_o in interferers)
            other_colors.discard(None)
            possible_colors = all_colors - other_colors
            if len(possible_colors) == 0:
                # this register interferes with too many others
                uncolorable.append((r, interferers))
            else:
                # assign the register an arbitrary color
                colors[r] = possible_colors.pop()

        return colors, uncolorable

    @classmethod
    def _bb_render_code(cls, bbs):
        from graphviz import Digraph
        dot = Digraph()
        for bb_ident, bb in bbs.items():
            # create a node for each basic block with its code inside
            code = "\n".join(str(insn) for insn in bb.insns)
            dot.node(str(bb_ident), code, shape="box", xlabel=str(bb_ident),
                forcelabels="true")
        for bb_ident, bb in bbs.items():
            # then connect all the nodes
            for target in bb.targets:
                dot.edge(str(bb_ident), str(target))
        dot.render("/tmp/blah", view=True)

    @classmethod
    def _bb_render_use_def(cls, bbs):
        from graphviz import Digraph
        dot = Digraph()
        for bb_ident, bb in bbs.items():
            # create a node for each basic block with its input and output regs
            # inside
            regs = "R_USE: {}\nR_DEF: {}".format(bb.r_use, bb.r_def)
            dot.node(str(bb_ident), regs, shape="box", xlabel=str(bb_ident),
                forcelabels="true")
        for bb_ident, bb in bbs.items():
            # then connect all the nodes
            for target in bb.targets:
                dot.edge(str(bb_ident), str(target))
        dot.render("/tmp/blah2", view=True)

    @classmethod
    def _bb_render_in_out(cls, bbs, bb_in, bb_out):
        from graphviz import Digraph
        dot = Digraph()
        for bb_ident, bb in bbs.items():
            # create a node for each basic block with its regs inside
            regs = "R_IN: {}\nR_USE: {}\nR_DEF: {}\nR_OUT: {}".format(
                bb_in[bb_ident], bb.r_use, bb.r_def, bb_out[bb_ident])
            dot.node(str(bb_ident), regs, shape="box", xlabel=str(bb_ident),
                forcelabels="true")
        for bb_ident, bb in bbs.items():
            # then connect all the nodes
            for target in bb.targets:
                dot.edge(str(bb_ident), str(target))
        dot.render("/tmp/blah3", view=True)

    @classmethod
    def _bb_render_interferences(cls, interferences, colors=None):
        from graphviz import Graph
        dot = Graph()
        # create a node for each register with its name inside
        for r, interferers in interferences.items():
            dot.node(str(r), str(r),
                xlabel=str(len(interferers) if colors is None else colors[r]),
                forcelabels="true")
        # then connect all the nodes
        for r, interferers in interferences.items():
            for r_o in interferers:
                # only create edge in one direction
                if r <= r_o:
                    dot.edge(str(r), str(r_o))
        dot.render("/tmp/blah4", view=True)


# assign each property a unique number so we know where a register is used
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
_i_fields = [
    # OTHER PEOPLE'S PROPERTIES
    "r_use",   # frozenset of register numbers used by this insn
    "r_def",   # frozenset of register numbers defined by ths insn
    "targets", # frozenset of possible control flow targets:
               #     None = next instruction
               #     str = label with that name
               #     int = register with that number
    # OUR PROPERTIES
    # we save the information required to reconstruct the Instr that we were
    # built from so that we can generate unique objects with different register
    # mappings as necessary, instead of just modifying the same one.
    "instr_type",   # type object of the instr that made this insn
    "instr_fields", # dict mapping insn field names to register numbers (rN)
                    #     or values (imm)
]
class Insn(namedtuple("Insn", _i_fields)):
    # instructions that branch to a label (not including aliases)
    INSTRS_BRANCH = {BZ1, BZ0, BS1, BS0, BC1, BC0, BV1, BV0,
        BGTS, BGTU, BGES, BLES, BLEU, BLTS}
    # instructions where rsd is a source
    INSTRS_RSD_SOURCE = {ST, STR, STX, STXA}

    def __new__(cls, instr):
        # hack for R_USE cause it's not a boneless instr
        if isinstance(instr, R_USE):
            return super(Insn, cls).__new__(cls,
                r_use=frozenset((instr.rsd,)),
                r_def=frozenset(),
                targets=frozenset((None,)),
                instr_type=R_USE,
                instr_fields={"rsd": instr.rsd},
            )

        # first, take apart the instr into its component parts
        instr_type = type(instr)
        instr_fields = {}
        for field in instr._field_types:
            v = getattr(instr, "_"+field)
            if field.startswith("r"):
                # convert register objects back to numbers
                instr_fields[field] = int(v)
            else:
                # convert immediates to strings if possible
                instr_fields[field] = v.value

        # second, un-alias the instruction so we have a common type
        instr_base_type = instr_type
        while instr_base_type.alias is True:
            # the only superclass of an alias is the original instruction class
            instr_base_type = instr_base_type.__bases__[0]

        # default flow properties
        r_use = set()
        r_def = set()
        targets = {None}
        
        # third, figure out what exactly that type means
        if instr_base_type in Insn.INSTRS_BRANCH:
            dest = instr_fields["imm"]
            if not isinstance(dest, str):
                raise ValueError(
                    "branch target must be str, which '{}' is not".format(dest))
            # target is the next instruction or the branch's destination
            targets = {dest, None}
        elif instr_base_type is J:
            # unconditional jump, no registers, and target is label
            dest = instr_fields["imm"]
            if not isinstance(dest, str):
                raise ValueError(
                    "jump target must be str, which '{}' is not".format(dest))
            targets = {dest}
        elif instr_base_type is JR:
            # register-based jump
            if instr_fields["imm"] != 0:
                raise ValueError(
                    "JR offset must be 0, which '{}' is not".format(dest))
            # we depend on the jump target register
            r_use = {instr_fields["rsd"]}
            # and that's where we go always
            targets = {instr_fields["rsd"]}
        elif instr_base_type is JRAL:
            # jump to register and save next pc in register
            r_use = {instr_fields["rb"]}
            r_def = {instr_fields["rsd"]}
            # for subroutine calls, we assume the call will eventually return
            # back to where it started. if we didn't have to allocate registers
            # for the target (e.g. it was a different procedure) then the user
            # would write that differently.
            targets = {instr_fields["rb"], None}
        elif instr_base_type is JVT or instr_base_type is JST:
            raise ValueError("can't yet allocate JVT or JST")
        elif instr_base_type is JAL:
            # jump to target and save next pc in register
            r_def = {instr_fields["rsd"]}
            if not isinstance(instr_fields["imm"], str):
                raise ValueError(
                    "JAL target must be str, which '{}' is not".format(dest))
            # see commentary in JRAL
            targets = {instr_fields["imm"], None}
        elif instr_base_type is NOP:
            # encoded as conditional jump, but it doesn't depend on anything.
            # the default is fine.
            pass
        else:
            # just a generic instruction with whatever function.
            # ra and rb are always inputs.
            ra = instr_fields.get("ra")
            if ra is not None: r_use.add(ra)
            rb = instr_fields.get("rb")
            if rb is not None: r_use.add(rb)
            rsd = instr_fields.get("rsd")
            if rsd is not None:
                if instr_base_type in Insn.INSTRS_RSD_SOURCE:
                    # rsd might be an input too for a few rare instructions
                    r_use.add(rsd)
                else:
                    # but it's usually an output
                    r_def = {instr_fields["rsd"]}

        # freeze everything and return the data
        return super(Insn, cls).__new__(cls,
            r_use=frozenset(r_use),
            r_def=frozenset(r_def),
            targets=frozenset(targets),
            instr_type=instr_type,
            instr_fields=instr_fields,
        )
            
    def __repr__(self):
        # only include parts that actually exist to shorten representation
        s = [
            "Insn(",
            # get instruction name from its class
            "type={}".format(str(self.instr_type).split(".")[-1][:-2]),
            # convert frozensets to sets to avoid displaying frozenset()
            (", r_use={}".format(set(self.r_use)))
                if len(self.r_use) > 0 else "",
            (", r_def={}".format(set(self.r_def)))
                if len(self.r_def) > 0 else "",
            (", targets={}".format(set(self.targets)))
                if len(self.targets) > 1 or None not in self.targets else "",
            ")",
        ]
        return "".join(s)

del _i_fields # avoid cluttering namespace

BasicBlock = namedtuple("BasicBlock", [
    "insns", # tuple of instructions in this basic block
    "r_use", # frozenset of registers used by this basic block
    "r_def", # frozenset of registers defined by this basic block
    "sources", # frozenset of basic blocks that may jump to us
    "targets", # frozenset of basic blocks that we may jump to
])
