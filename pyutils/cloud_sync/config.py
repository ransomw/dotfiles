"""
read and parse config file 
"""
from pathlib import Path

import appdirs as ad
import yaml


try:
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader  # type: ignore


appname = 'dotfiles-cloud_sync'
appauthor = 'ransomw'

def read_config(file_path=None):
    if file_path:
        config_file_path = file_path
    else:
        config_dir_path = Path(ad.user_config_dir(appname, appauthor))
        config_file_path = config_dir_path / "config.yaml"
    with open(str(config_file_path)) as fh:
        doc = yaml.load(fh.read(), Loader=Loader)
    #todo: validation
    return doc
