"""
functions generally corresponding to subcommands
"""

from . import exc
from . import networks_conf as nconf
from .subps_util import run
from .iw_data import ScanRes
from .wpa import WpaProc


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


def wpa_conn(ssid, password,
                 timeout=None,
                 interface='wlan0',
                 ):
    wpa_proc = WpaProc(ssid, password,
                           interface=interface)
    try:
        print("trying to connect, Ctrl-c to exit")
        curr_wpa_state = wpa_proc.get_current_state(timeout=timeout)
        if curr_wpa_state == WpaProc.State.CONNECTED:
            run(['dhclient', '-v'])
            nconf.update(nconf.AuthType.WPA, ssid, password)
            print("connected")
            while True:
                pass
        elif curr_wpa_state == WpaProc.State.AUTH_FAIL:
            print("authorization fail.  check password")
        else:
            print("couldn't connect. check network name")
    except KeyboardInterrupt:
        print("\nexiting..")
    finally:
        wpa_proc.stop()


def auto_conn(
        timeout=None,
        interface='wlan0',
    ):
    present_ssids = [network.ssid for network
                         in run_scan(interface=interface).networks]
    found_known_network = False
    for ssid, conf in nconf.get().items():
        if ssid in present_ssids:
            found_known_network = True
            if conf['auth_type'] == nconf.AuthType.WPA.value:
                wpa_conn(ssid, conf['password'],
                             timeout=timeout,
                             interface=interface,
                             )
            else:
                raise Exception("auto connect unimpl for auth type")
    if not found_known_network:
        raise exc.NetworkNotFound
