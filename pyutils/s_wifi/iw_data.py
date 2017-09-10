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
            ("network id", self.ssid),
            ("signal", str(self.signal) + " dBm"),
            ("open network", ("Yes" if self.is_open else "No")),
            ]])

    @property
    def ssid(self):
        return re.search(
            r'SSID: (.+)($|\n)',
            self._res_str,
            ).group(1)

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

    def __init__(self, str_scan_res):
        self._networks = [ NetworkRes(res_str) for res_str in
            list(self._get_network_strs(str_scan_res)) ]

    def __str__(self):
        return '\n----------\n'.join([
            str(network) for network in self.networks])

    def _get_network_strs(self, str_scan_res):
        res_split = re.split('(^BSS|\nBSS)', str_scan_res)
        return [ res for res in res_split[2::2] ]

    @property
    def networks(self):
        return sorted(self._networks,
                          key=lambda n: n.signal,
                          reverse=True)

