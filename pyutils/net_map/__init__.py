import socket
import ipaddress
from typing import (List,)


class NetmapError(Exception):
    pass


class NetmapParseError(ValueError, NetmapError):
    pass


def _get_own_ip():
    raise NotImplementedError()


def _is_port_open(
        ip_addr,
        port_no,
) -> bool:
    addr = (str(ip_addr), port_no,)
    try:
        conn = socket.create_connection(addr)
    except ConnectionRefusedError:
        return False
    conn.close()
    return True #...#
    sock = socket.socket(
        socket.AF_INET,
        socket.SOCK_STREAM | socket.SOCK_NONBLOCK)
    sock.connect(addr)


def _is_port_open_direct_lib(
        ip_addr,
        port_no,
) -> bool:
    addr = (str(ip_addr), port_no,)
    try:
        conn = socket.create_connection(addr)
    except ConnectionRefusedError:
        return False
    conn.close()
    return True #...#
    sock = socket.socket(
        socket.AF_INET,
        socket.SOCK_STREAM | socket.SOCK_NONBLOCK)
    sock.connect(addr)



def _port_scan_single_host(host, ports,
                           scan_method='high-lvl',):
    scan_method__create_connection = ['high-lvl', 'low-lvl',
                           'hi', 'lo',
                           'create_connection', 'direct_lib']
    scan_method__direct_lib = ['high-lvl', 'low-lvl',
                           'hi', 'lo',
                           'create_connection', 'direct_lib']
    assert scan_method in ['high-lvl', 'low-lvl',
                           'hi', 'lo',
                           'create_connection', 'direct_lib']
    rv = {}
    for port in ports:
        print("scanning port", port)
        single_port_res: bool = _is_port_open_direct_lib(host, port)
        single_port_res: bool = _is_port_open(host, port)

        rv.update({port: single_port_res})
    return rv


def port_scan(host_or_hosts, ports: List[int]):

    def _parse_host(maybe_host):
        try:
            host = str(ipaddress.ip_address(str(host_or_hosts)))
        except Exception as err:
            breakpoint() # get exception class
            raise NetmapParseError("", (err,))
        return host


    def _parse_hosts(hosts):
        if isinstance(hosts, (list, tuple, iter)):
            return [_parse_hosts(host)
                    for host in hosts]
        if isinstance(hosts, (str,)):
            pass


    def _coerce_ports(port_or_ports):
        ports = None
        if isinstance(port_or_ports, int):
            ports = [port_or_ports]
        if isinstance(port_or_ports, (list, tuple, set,)):
            ports = list(port_or_ports)
            if not all([isinstance(p, int) for p in ports]):
                raise NetmapParseError(
                    "recursive port coercion unimpl"
                )
        if ports is None:
            raise NetmapParseError()
        return ports


    single_host = None
    try:
        single_host = _parse_host(host_or_hosts)
    except NetmapParseError:
        pass
    if single_host is not None:
        hosts = [single_host]
    else:
        hosts = _parse_hosts(host_or_hosts)
    rv = {}
    for host in hosts:
        print("scanning host", host)
        single_host_res = _port_scan_single_host(host,
                                                 # ports,
                                                 _coerce_ports(ports),
                                                 )
        rv.update(**{str(host): single_host_res})

    breakpoint()

    return rv
