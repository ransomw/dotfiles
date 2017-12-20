"""
data structures and parsing for `iw` command output
"""
import re

class NetworkRes:
    """ info about scan results for an individual network """

    def __init__(self, res_str):
        self._res_str = res_str

    def __str__(self):
        return '\n'.join([': '.join(pair) for pair in [
            ("network id", self.ssid or 'no network id'),
            ("signal", str(self.signal) + " dBm"),
            ("open network", ("Yes" if self.is_open else "No")),
            ]])

    @property
    def ssid(self):
        mo = re.search(
            r'SSID: (.+)($|\n)',
            self._res_str,
            )
        if mo is None:
            return None
        return mo.group(1)

    @property
    def signal(self):
        return -1 * float(re.search(
            r'signal: -(\d*.\d*) dBm',
            self._res_str,
            ).group(1))

    @property
    def is_open(self):
        return (
            re.search(
                r'(\n|^).*capability:.*Privacy.*(\n|$)',
                self._res_str,
                ) is None
            )

    @property
    def is_wpa(self):
        return (
            re.search(
                r'(\n|^).*WPA.*(\n|$)',
                self._res_str,
                ) is not None
            )

    @property
    def is_wpa2(self):
        return (
            re.search(
                # Robust Security Network
                r'(\n|^).*RSN.*(\n|$)',
                self._res_str,
                ) is not None
            )

    @property
    def is_wep(self):
        return not (self.is_open or self.is_wpa or self.is_wpa2)


class ScanRes:
    """ info about scan results for all networks """

    def __init__(self, str_scan_res,
                     omit_no_ssid=True,
                     ):
        self._networks = [ NetworkRes(res_str) for res_str in
            list(self._get_network_strs(str_scan_res)) ]
        self._omit_no_ssid = omit_no_ssid

    def __str__(self):
        return '\n----------\n'.join([
            str(network) for network in self.networks])

    def _get_network_strs(self, str_scan_res):
        res_split = re.split('(^BSS|\nBSS)', str_scan_res)
        return [ res for res in res_split[2::2] ]

    @property
    def networks(self):
        if self._omit_no_ssid:
            networks = filter(lambda n: n.ssid, self._networks)
        else:
            networks = self._networks
        return sorted(networks,
                          key=lambda n: n.signal,
                          reverse=True)


class ConnectRes:
    """ result of an connecting to an open network with iw """

    def __init__(self, str_conn_res):
        mo = re.search(r'^([^ ]+) \((.*)\): (.*)', str_conn_res)
        assert mo is not None
        (self._iface,
             _, # e.g. 'phy #0'
             self._msg,
        ) = [mo.group(i) for i in range(1, 4)]

    @property
    def connected(self):
        return re.search(r'^connected', self._msg)

    def __str__(self):
        if self.connected:
            return self._iface + " connected"
        else:
            return "connect failed:\n" + self._msg
