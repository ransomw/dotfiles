"""
various utilities for running subprocesses
"""
import subprocess
from functools import partial

run = partial(
    subprocess.run,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    # shell=True,
    check=True,
    )

