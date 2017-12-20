"""
functions generally corresponding to subcommands
"""
from time import sleep
from enum import Enum
from enum import unique

from . import exc
from . import networks_conf as nconf
from .subps_util import run
from .iw_data import ScanRes
from .iw_data import ConnectRes
from .wpa import WpaProc

WPA_POLL_INTERVAL = 2.0 # sec

def activate(interface='wlan0'):
    """ensure wifi card activated"""
    completed_process_linkup = run(
        ['ip', 'link', 'set', interface, 'up'])
    assert hasattr(completed_process_linkup, 'stderr')
    assert hasattr(completed_process_linkup, 'stdout')
    assert hasattr(completed_process_linkup, 'returncode')


def run_scan(interface='wlan0'):
    """ scan and show network info """
    completed_process_scan = run(
        ['iw', interface, 'scan'])
    assert completed_process_scan.returncode == 0
    return ScanRes(completed_process_scan.stdout.decode('utf-8'))


def open_conn(ssid,
                  interface='wlan0',
                  ):
    scan_res = run_scan(interface=interface)
    completed_process_connect = run(
        ['iw', 'dev', interface, 'connect', '-w', ssid])
    assert completed_process_connect.returncode == 0
    connect_res =  ConnectRes(completed_process_connect
                                  .stdout.decode('utf-8'))
    if connect_res.connected:
        run(['dhclient', '-v'])
    return connect_res


@unique
class ConnState(Enum):
    CONNECTED = 'connected'
    # iw via `run_scan` is more reliable than wpa_supplicant
    # to determine if a network with a given SSID exists
    NO_NETWORK = 'network not found'
    AUTH_FAIL = 'authorization failure'
    UNKNOWN = 'unknown state'
    EXITING = 'closing subprocesses'
    CLOSED = 'closed'


def wpa_conn(ssid, password,
                 timeout=None,
                 interface='wlan0',
                 print_wpa_raw=False,
                 ):
    wpa_proc = WpaProc(ssid, password,
                           interface=interface,
                           print_raw=print_wpa_raw,
                           )
    try:
        curr_wpa_state = wpa_proc.get_current_state(timeout=timeout)
        if curr_wpa_state == WpaProc.State.CONNECTED:
            run(['dhclient', '-v'])
            nconf.update(nconf.AuthType.WPA, ssid, password)
            yield ConnState.CONNECTED
            while (wpa_proc.get_current_state(timeout=timeout) in
                       set([
                           WpaProc.State.CONNECTED,
                           # includes non-'CTRL-EVENT-*' states
                           WpaProc.State.UNKNOWN,
                           ])):
                sleep(WPA_POLL_INTERVAL)
        elif curr_wpa_state == WpaProc.State.AUTH_FAIL:
            yield ConnState.AUTH_FAIL
        else:
            yield ConnState.UNKNOWN
    except KeyboardInterrupt:
        yield ConnState.EXITING
    finally:
        wpa_proc.stop()
        yield ConnState.CLOSED


def auto_conn(
        timeout=None,
        interface='wlan0',
        print_wpa_raw=False,
    ):
    present_ssids = set([
        network.ssid for network
        in run_scan(interface=interface).networks
        if network.ssid is not None
    ])
    found_known_network = False
    for ssid, conf in nconf.get().items():
        if ssid in present_ssids:
            found_known_network = True
            if conf['auth_type'] == nconf.AuthType.WPA.value:
                return wpa_conn(ssid, conf['password'],
                                    timeout=timeout,
                                    interface=interface,
                                    print_wpa_raw=print_wpa_raw,
                                    )
            else:
                raise Exception("auto connect unimpl for auth type")
    if not found_known_network:
        raise exc.NetworkNotFound
