"""
information about networks is store in a config file, and
this module provides access to that info for the rest
of the package with a "module as singleton" pattern
"""

import os.path
import configparser
from enum import Enum
from enum import unique

_NETWORKS_CONF_FILEPATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    'networks.conf')
_NETWORKS_CONF = configparser.ConfigParser()
if os.path.isfile(_NETWORKS_CONF_FILEPATH):
    _NETWORKS_CONF.read(_NETWORKS_CONF_FILEPATH)

@unique
class AuthType(Enum):
    """ as used in config file to determine connection method """
    WPA = 'wpa'


def update(auth_type, network, password):
    _NETWORKS_CONF[network] = {
        'auth_type': auth_type.value,
        'password': password,
    }
    with open(_NETWORKS_CONF_FILEPATH, 'w') as f:
        _NETWORKS_CONF.write(f)


def get():
    return { network: dict(_NETWORKS_CONF[network])
                 for network in _NETWORKS_CONF.sections() }
