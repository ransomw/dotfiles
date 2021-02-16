"""
"""
from functools import (
    reduce,
    wraps,
)
#
from toolz.dicttoolz import (
    update_in,
)
from toolz.functoolz import (
    compose,
    curry,
    flip,
    juxt,
)
from toolz.itertoolz import (
    concat,
    concatv,
    diff,
    isdistinct,
    nth,
    unique,
)

_identity = lambda x: x
_maplist = curry(compose(list, map))
_list_nth = lambda n: _maplist(
    curry(nth, n))


def _parse_wrapping_toks__(
    wrapping_toks,
):
    (begin_toks,
     end_toks,
    ) = juxt(
        _list_nth(0),
        _list_nth(1),
    )(wrapping_toks)
    all_toks = concatv(
        begin_toks,
        end_toks,
    )
    if None in all_toks:
        raise ValueError((
            "tokens may not be"
            "None"
        ))
    if not isdistinct(all_toks):
        raise ValueError((
            "begin and end tokens "
            "not all distinct"
        ))
    return (
        wrapping_toks,
        begin_toks,
        end_toks,
        all_toks,
    )


def _wrap_token_getter__(token_getter):
    @wraps(token_getter)
    def wrapper(*args, **kwargs):
        res = token_getter(
            *args, **kwargs)
        if res is None:
            raise ValueError((
                "tokens may not be "
                "None"
            ))
        return res
    return wrapper


def _begin_to_end_toks__(
    wrapping_toks,
    begin_tok,
):
    for (curr_begin, curr_end,
    ) in wrapping_toks:
        if begin_tok == curr_begin:
            return curr_end
    return None


def _validate_frame_with_end_tok__(
        wrapping_toks,
        stack_frame,
        tok,
):
    if ('begin' in stack_frame
        and
        _begin_to_end_toks__(
            wrapping_toks,
            stack_frame['begin_token'],
        ) != tok):
        raise ValueError((
            "unbalanced start/"
            "end tokens"))


class _TimeLogPartitioner:

    def __init__(
            self,
            wrapping_toks,
            token_getter,
    ):

        (self._wrapping_toks,
         self._begin_toks,
         self._end_toks,
         _,
        ) = _parse_wrapping_toks__(
            wrapping_toks,
        )

        self._token_getter = (
            _wrap_token_getter__(
                token_getter))
        self._stack = []
        self._parsed = []

    def get_res(self):
        while True:
            try:
                self._merge_frame(
                    self._stack.pop())
            except IndexError:
                break
        return self._parsed

    def parse_entry(
            self,
            time_log,
    ):
        tok = self._token_getter(
            time_log)
        proc_fn = (
            self._proc_begin_tok
            if tok in self._begin_toks
            else
            (self._proc_end_tok
             if tok in self._end_toks
             else self._proc_other_tok))
        proc_fn(
            time_log,
            tok)

    def _proc_begin_tok(
            self,
            time_log,
            tok):
        self._stack.append({
            'begin_token': tok,
            'begin': time_log,
            'entries': [],
        })

    def _proc_end_tok(
            self,
            time_log,
            tok):
        if len(self._stack) > 0:
            stack_frame = (
                self._stack.pop())
        else:
            self._parsed.append({
                'end_token': tok,
                'end': time_log,
            })
            return
        self._proc_end_tok_given_frame(
            time_log,
            tok,
            stack_frame,
        )

    def _proc_end_tok_given_frame(
            self,
            time_log,
            tok,
            stack_frame):
        _validate_frame_with_end_tok__(
            self._wrapping_toks,
            stack_frame,
            tok,
        )
        stack_frame[
            'end'] = time_log
        stack_frame[
            'end_token'] = tok
        self._merge_frame(stack_frame)

    def _merge_frame(
            self,
            stack_frame):
        if len(self._stack) > 0:
            (self._stack[-1]['entries']
             .append(stack_frame))
        else:
            self._parsed.append(
                stack_frame)

    def _proc_other_tok(
            self,
            time_log,
            tok):
        if len(self._stack) > 0:
            (self._stack[-1]['entries']
             .append(time_log))
        else:
            self._stack.append({
                'entries': [time_log],
            })



def partition_time_logs(
    time_logs,
    wrapping_toks,
    token_getter,
):
    time_log_partitioner = (
        _TimeLogPartitioner(
            wrapping_toks,
            token_getter,
        ))

    for time_log in time_logs:
        (time_log_partitioner
         .parse_entry(time_log))

    return (time_log_partitioner
            .get_res())


def keymap_aux(mapping_dict: dict, fn_c=_identity):
    def fn_b(x):
        if x in mapping_dict:
            return mapping_dict[x]
        return fn_c(x)
    return fn_b


# todo: re-define via
#    * arg splatter
#    * reverse
#    * curry
# in lieu of ramda's __
def _updater_in(keys, func):
    def updater_in(d):
        return update_in(d, k, f)
    return updater_in


def _mypy_fail():
    keymap_aux(1)
