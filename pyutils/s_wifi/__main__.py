import sys

from .cli import parse_args
from . import fn

if __name__ == '__main__':
    args = parse_args()
    fn.activate(interface=args.interface)
    if args.cmd == 'scan':
        print(fn.run_scan(interface=args.interface))
    elif args.cmd == 'wpa':
        fn.wpa_conn(args.network, args.password,
                        timeout=args.timeout,
                        interface=args.interface)
    elif args.cmd == 'auto':
        fn.auto_conn(timeout=args.timeout,
                         interface=args.interface)
    else:
        sys.stderr.write(
            "unknown or absent subcommand. " +
            "run with the '--help' argument to see " +
            "a list of available subcommands." + '\n')
        quit(2)

