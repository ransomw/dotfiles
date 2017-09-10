import argparse

class MyHelpFormatter(argparse.ArgumentDefaultsHelpFormatter):

    def __init__(self, *args, **kwargs):
        width = kwargs.pop('width', 60)
        super().__init__(*args, width=width, **kwargs)


def _config_parser_scan(parser):
    pass


def _config_parser_wpa_conn(parser):
    parser.add_argument(
        'network',
        help=("name of network (SSID)"),
        )
    parser.add_argument(
        'password',
        help=("plain-text password"),
        )
    parser.add_argument(
        '-t', '--timeout', dest='timeout',
        type=int,
        default=10,
        help=("timeout to result in seconds"),
        )


def _config_parser_auto_conn(parser):
    parser.add_argument(
        '-t', '--timeout', dest='timeout',
        type=int,
        default=10,
        help=("timeout to result in seconds"),
        )


# todo: command aliases?
def parse_args():
    """ parse script arguments """
    arg_parser = argparse.ArgumentParser(
        formatter_class=MyHelpFormatter,
        )
    arg_parser.add_argument(
        '-i', '--interface', dest='interface',
        default='wlan0',
        help=("network interface (wifi card) to use for connection"),
        )
    subParsers = arg_parser.add_subparsers(dest='cmd')
    _config_parser_scan(subParsers.add_parser(
        'scan',
        formatter_class=MyHelpFormatter,
        help=("list networks."),
    ))
    _config_parser_wpa_conn(subParsers.add_parser(
        'wpa',
        formatter_class=MyHelpFormatter,
        help=("create a wpa connection"),
    ))
    _config_parser_auto_conn(subParsers.add_parser(
        'auto',
        formatter_class=MyHelpFormatter,
        help=("automatically connect to a known network"),
    ))
    return arg_parser.parse_args()
