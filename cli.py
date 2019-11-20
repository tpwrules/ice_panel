import argparse

from nmigen.hdl.ir import Fragment
from nmigen.back import rtlil, verilog, pysim

# largely stolen from nmigen.cli

__all__ = ["main"]


def main_parser(parser=None):
    if parser is None:
        parser = argparse.ArgumentParser()

    p_action = parser.add_subparsers(dest="action", required=True)

    p_simulate = p_action.add_parser(
        "simulate", help="simulate the design")
    p_simulate.add_argument("-v", "--vcd-file",
        metavar="VCD-FILE", type=argparse.FileType("w"),
        help="write execution trace to VCD-FILE")
    p_simulate.add_argument("-w", "--gtkw-file",
        metavar="GTKW-FILE", type=argparse.FileType("w"),
        help="write GTKWave configuration to GTKW-FILE")
    p_simulate.add_argument("-p", "--period", dest="sync_period",
        metavar="TIME", type=float, default=1e-6,
        help="set 'sync' clock domain period to TIME (default: %(default)s)")
    p_simulate.add_argument("-c", "--clocks", dest="sync_clocks",
        metavar="COUNT", type=int, required=True,
        help="simulate for COUNT 'sync' clock periods")

    p_build = p_action.add_parser(
        "build", help="build the design using nMigen's build system")
    p_build.add_argument("-p", "--program", action="store_true",
        help="program the platform with the built design")

    p_boneload = p_action.add_parser(
        "boneload", help="boneload design firmware into device")
    p_boneload.add_argument("-p", "--port", type=str, required=True,
        help="serial port to program over")
    p_boneload.add_argument("-r", "--ram", action="store_true",
        help="download to ram only")

    return parser


def main_runner(parser, args, maker, fw=None, ports=(), build_args={}):
    if args.action == "simulate":
        design, platform = maker(simulating=True)
        fragment = Fragment.get(design, platform)
        with pysim.Simulator(fragment,
                vcd_file=args.vcd_file,
                gtkw_file=args.gtkw_file,
                traces=ports) as sim:
            sim.add_clock(args.sync_period)
            sim.run_until(args.sync_period * args.sync_clocks, run_passive=True)
    
    if args.action == "build":
        design, platform = maker(simulating=False)
        platform.build(design, do_program=args.program, **build_args)

    if args.action == "boneload":
        import boneload
        boneload.boneload(fw(), args.port, args.ram)

def main(*args, **kwargs):
    parser = main_parser()
    main_runner(parser, parser.parse_args(), *args, **kwargs)
