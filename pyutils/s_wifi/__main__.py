import sys

from .cli import parse_args
from . import fn

CONN_STATE_MSGS = {
    fn.ConnState.CONNECTED: "connected",
    fn.ConnState.AUTH_FAIL: "authorization fail.  check password",
    fn.ConnState.UNKNOWN: ("couldn't connect.  " +
                               "check network name, timeouts, etc."),
    fn.ConnState.EXITING: "\n closing connection..",
    fn.ConnState.CLOSED: "closed. ^C again to exit program.",
}

if __name__ == '__main__':
    conn_gen = None
    args = parse_args()
    fn.activate(interface=args.interface)
    if args.cmd == 'scan':
        print(fn.run_scan(interface=args.interface))
    elif args.cmd == 'open':
        print(fn.open_conn(args.network,
                               interface=args.interface,
                               ))
    elif args.cmd == 'wpa':
        conn_gen = fn.wpa_conn(args.network, args.password,
                                   timeout=args.timeout,
                                   interface=args.interface,
                                   print_wpa_raw=args.print_raw,
                                   )
    elif args.cmd == 'auto':
        conn_gen = fn.auto_conn(timeout=args.timeout,
                                    interface=args.interface,
                                    print_wpa_raw=args.print_raw,
                                    )
    else:
        sys.stderr.write(
            "unknown or absent subcommand. " +
            "run with the '--help' argument to see " +
            "a list of available subcommands." + '\n')
        quit(2)
    if conn_gen is not None:
        for conn_state in conn_gen:
            print(CONN_STATE_MSGS.get(
                conn_state,
                "no msg for conn state " + str(conn_state.value))
                      )
