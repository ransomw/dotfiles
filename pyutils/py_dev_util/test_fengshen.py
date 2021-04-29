from os import path as pth
from pprint import pp
import pytest
import yaml
from toolz.dicttoolz import (
    keymap,
)
from .fengshen import(
    partition_time_logs,
    keymap_aux,
)

@pytest.fixture
def static_data():
    with open(
            pth.join(pth.dirname(pth.realpath(__file__)), 'test_fengshen.yaml'),
    ) as f:
        return yaml.load(f)


_identity = lambda x: x

def test_partition_time_logs(
        static_data,
):
    for time_logs in static_data[
             'time-logs']:
        token_getter = (
            _identity if
            'token-attr'
            not in time_logs else
            lambda z: (
                z[time_logs[
                    'token-attr']]))
        parsed_logs = (
            partition_time_logs(
                time_logs['time-logs'],
                [('BA', 'EA'),
                 ('BB', 'EB'),
                ],
                token_getter,
            ))
        assert (
            parsed_logs
            ==
            time_logs['parsed']
        )

def test_keymap_aux(
    static_data,
):
    for input_dict in static_data['keymap-aux']:
        actual = keymap(
            (keymap_aux(input_dict['aux-input'])
             if 'fn' not in input_dict else
             keymap_aux(input_dict['aux-input'],
                        eval(input_dict['fn']),
             )
             ),
            input_dict['dict-to-map'],
        )
        assert(
            actual
            ==
            input_dict['expected-map']
            )
