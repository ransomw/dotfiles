"""
data structures and utilities for interacting with
WPA command-line tools (for connecting to networks)
"""

import os.path
import re
import subprocess
import datetime
from warnings import warn
from selectors import DefaultSelector
from selectors import EVENT_READ
from enum import Enum
from enum import unique

from .subps_util import run

class WpaData(dict):
    """
    a single line of data out of wpa_supplicant
    """

    def __init__(self, line):
        self['line'] = line
        if line == 'Successfully initialized wpa_supplicant':
            self['is_init'] = True
            return
        mo = re.match(r'^([^:]+): (.*)$', line)
        if mo is None:
            warn(''.join([
                "WPA data line didn't match 'iface: message' format",
                "'" + line + "'"]))
            return
        self['iface'] = mo.group(1)
        content = mo.group(2)
        self._parse_ctrl_event(content)

    def _parse_ctrl_event(self, content):
        mo = re.match(r'^CTRL-EVENT-([^ ]*) (.*)$', content)
        if mo is None:
            return
        self['ctrl_event'] = mo.group(1)
        if self['ctrl_event'] == 'CONNECTED':
            self['mac_addr'] = re.search(
                r'Connection to (.*) completed',
                mo.group(2)).group(1)
            return
        params_str = mo.group(2).strip()
        params_dict = {}
        while params_str != '':
            params_str, _, val = params_str.rpartition('=')
            params_str, _, key = params_str.rpartition(' ')
            params_dict[key] = val
        self['ctrl_event_params'] = params_dict


class WpaBuffer:
    """
    buffered data reading of wpa_supplicant
    """

    SELECT_TIMEOUT = 0.5 # seconds

    def __init__(self, file_obj):
        self._data_raw = []
        self._data = []
        self._file_obj = file_obj
        self._selector = DefaultSelector()
        self._selector.register(self._file_obj, EVENT_READ, data=self)

    def read_new(self):
        """ read one one message from wpa_supplicant """
        events = self._selector.select(timeout=self.SELECT_TIMEOUT)
        for selectorKey, eventType in events:
            if selectorKey.data is not self:
                warn("unexpected event")
                continue
            assert selectorKey.fileobj is self._file_obj
            new_line = self._file_obj.readline().strip()
            self._data_raw.append(new_line)
            new_data = WpaData(new_line)
            self._data.append(new_data)
            return new_data, new_line
        return None, None

    def read_all(self):
        """ read all currently available messages """
        all_new_data = []
        all_new_lines = []

    @property
    def data(self):
        return self._data


class WpaProc:
    """
    wrapper around a Popen object for wpa_supplicant
    """

    @unique
    class State(Enum):
        STOPPED = 'wpa process stopped'
        CONNECTED = 'connected'
        # iw via `run_scan` is more reliable than wpa_supplicant
        # to determine if a network with a given SSID exists
        NO_NETWORK = 'network not found'
        AUTH_FAIL = 'authorization failure'
        UNKNOWN = 'unknown state'

    WPA_CONF_FILEPATH = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        'wpa_supplicant.conf')

    def __init__(self, ssid, password,
                     interface='wlan0'):
        # encrypt password
        str_conf = (run(
            ['wpa_passphrase', ssid, password])
            .stdout.decode('utf-8'))
        with open(self.WPA_CONF_FILEPATH, 'w') as f:
            f.write(str_conf)
        self._popen = subprocess.Popen([
            'wpa_supplicant', '-i', interface, '-c',
            self.WPA_CONF_FILEPATH
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True,
            bufsize=1,
        )
        self._buffer = WpaBuffer(self._popen.stdout)

    def poll(self):
        return self._popen.poll()

    def stop(self):
        self._popen.terminate()
        self._popen.wait()

    def get_current_state(self, timeout=None):
        start = datetime.datetime.now()
        while self._popen.poll() is None:
            if (timeout is not None and
                    (datetime.datetime.now() - start
                         ).total_seconds() > timeout):
                warn("current state lookup timeout")
                return self.State.UNKNOWN
            new_data, new_line = self._buffer.read_new()
            if new_data is None:
                last_data = self._buffer.data[-1]
                if last_data.get('ctrl_event', None) == 'CONNECTED':
                    return self.State.CONNECTED
                if last_data.get('ctrl_event', None) == 'SCAN-FAILED':
                    return self.State.NO_NETWORK
                continue
            # if new_data is connected or scan failed,
            # this loop will catch it on the next pass
            # b/c wpa_supplicant stops printing in these cases
            if (new_data.get('ctrl_event', None) == 'SSID-TEMP-DISABLED'
                    and
                    (new_data.get('ctrl_event_params', {})
                         .get('reason', None) == 'WRONG_KEY')):
                return self.State.AUTH_FAIL
        return self.State.STOPPED
