import enum
import os

import urwid

import toolz.itertoolz as itlz

_CLIPBOARD = []


"""
चिया
आगो
संगीत
हावा
पानी
"""

dlt1 = {
    "I": chr(0x093F),
    "i": chr(0x0940),
    "o": chr(0x094B),
    "r": "र",
    "l": "ल",
    "k": "क",
    "K": "ख",
    "g": "ग",
    "m": "म",
    "s": "स",
    "n": "न",
    "a": "।",
    "A": "अ",
    "b": "ब",
    "v": "व",
    "h": "ह",
    "p": "प",
}


class IME_enum(enum.Enum):
    direct = 1
    devangari = 2
    # digraph = 3 # ..


def _ime_translate_keypress(
        ime, keypress
):
    if ime == IME_enum.devangari:
        if keypress in dlt1:
            return dlt1[keypress]
    return keypress


def _edit_box_cut_cp(edit_box, cut=False,):
    if edit_box.region_bdry is None:
        return
    if edit_box.edit.edit_pos <= edit_box.region_bdry:
        cut_cp_text = edit_box.edit.get_edit_text()[
            edit_box.edit.edit_pos:edit_box.region_bdry]
        if cut:
            edit_box.edit.set_edit_text(
                edit_box.edit.get_edit_text()[:edit_box.edit.edit_pos]
                +
                edit_box.edit.get_edit_text()[edit_box.region_bdry+1:])
    else:
        cut_cp_text = edit_box.edit.get_edit_text()[
            edit_box.region_bdry+1:edit_box.edit.edit_pos]
        if cut:
            edit_box.edit.set_edit_text(
                edit_box.edit.get_edit_text()[:edit_box.region_bdry]
                +
                edit_box.edit.get_edit_text()[edit_box.edit.edit_pos:])
            edit_box.edit.set_edit_pos(edit_box.region_bdry)
    clipboard.append(cut_text)
    if not cut:
        # cp
        edit_box.edit.set_edit_text(
            edit_box.edit.get_edit_text()[:edit_box.region_bdry]
            +
            edit_box.edit.get_edit_text()[edit_box.region_bdry+1:])
    edit_box.region_bdry = None



class EditBox(urwid.Filler):
    region_bdry_marker = '|'

    def __init__(self, *args, **kwargs):
        edit_text = kwargs.pop('edit_text', '')
        self.edit = urwid.Edit(
            edit_text=edit_text,
            multiline=True,
        )
        super(EditBox, self).__init__(self.edit, *args, **kwargs)
        self.region_bdry = None

    def keypress(self, size, key):
        clipboard = _CLIPBOARD
        if key in ["ctrl n",]:
            key = 'down'
        elif key in ["ctrl p",]:
            key = 'up'
        elif key in ["ctrl b",]:
            key = 'left'
        elif key in ["ctrl f",]:
            key = 'right'
        elif key in ["ctrl a",]:
            key = 'home'
            if False:
                edit_text_before = self.edit.get_edit_text()[:self.edit.edit_pos]
                try:
                    newline_before = edit_text_before.rindex('\n')
                except ValueError:
                    newline_before = -1
                self.edit.set_edit_pos(newline_before+1)
                return
        elif key in ["ctrl e",]:
            key = 'end'
            if False:
                edit_text_after = self.edit.get_edit_text()[self.edit.edit_pos:]
                try:
                    newline_after = edit_text_after.index('\n')
                except ValueError:
                    newline_after = len(edit_text_after)
                self.edit.set_edit_pos(self.edit.edit_pos+newline_after)
                return
        ###
        elif key in ["meta <",]:
            self.edit.set_edit_pos(0)
        elif key in ["meta >",]:
            self.edit.set_edit_pos(len(self.edit.get_edit_text()))
        elif key in ["ctrl k",]:
            pass
        elif key in ["ctrl d",]:
            key = "delete"
        elif key in ["ctrl _",]:
            pass
        ###
        elif key in [
                # "ctrl space"
                   "<0>",]:
            if self.region_bdry is None:
                self.region_bdry = self.edit.edit_pos
                self.edit.insert_text(
                    self.region_bdry_marker)
                return
        elif key in ["meta w",]:
            _edit_box_cut_cp(self, cut=False,)
            return
        elif key in ["ctrl w",]:
            _edit_box_cut_cp(self, cut=True,)
            return
        elif key in ["ctrl y",]:
            if clipboard:
                self.edit.insert_text(itlz.last(
                    clipboard))
            return
        elif key in ["ctrl u",]:
            try:
                clipboard.pop()
            except IndexError:
                pass
            return
        ###
        translated_key = _ime_translate_keypress(
            # IME_enum.direct,
            IME_enum.devangari,
            key
        )
        return super(EditBox, self).keypress(size, translated_key)


class UrwidComponents:

    def __init__(self,
                 main_box,
                 edit_text_box,
                 second_edit_text_box,):
        self.main_box = main_box
        self.edit_box = edit_text_box
        self.second_edit_box = second_edit_text_box



def _build_main_loop_box(
        edit_text,
        second_edit_text,
):
    _footer_text = (
        "<ctl-space> start copy <M-w> copy <C-w> cut -- <f9> quit <f10> save"
    )

    _footer_text = ('foot', [
        "Text Editor    ",
        ('key', "C-<space>"), " start copy ",
        ('key', "M-w"), " copy ",
        ('key', "C-w"), " cut ",
        ('key', "F9"), " quit ",
        ('key', "F10"), " save",
    ])

    footer = urwid.AttrWrap(urwid.Text(_footer_text), "foot")

    edit_box = EditBox(
        "top",
        edit_text=edit_text,
    )
    if second_edit_text is not None:
        second_edit_box = EditBox(
            "top",
            edit_text=second_edit_text,
        )
        view = urwid.Columns([
            urwid.LineBox(
                edit_box,
            ),
            urwid.LineBox(
                second_edit_box,
            ),
        ])
    else:
        second_edit_box = None
        view = urwid.LineBox(
            edit_box,
        )
    main_loop_box = urwid.Frame(
        urwid.AttrWrap(view, "body"),
        footer=footer,
    )
    return UrwidComponents(main_loop_box,
                    edit_box,
                    second_edit_box,
                    )


# todo:
#    * kill line
#    * maintain AST in parallel with text
#        - arrays of lines, lines of arrays
#    * i18n input methods, mathsym input method
#    * undo
#    * see Edit.highlight re. copy and paste impl
#    * additional cursor motion commands
#      - search text
#      - goto line
#    * use urwid Command Map
#    * allow editing in webbrowser
#      (see calc.py example)
def edit_text_terminal_urwid(edit_text, second_edit_text=None,
    word_re=r'[A-z]'
):

    exit_flags = []

    uc = urwid_components = _build_main_loop_box(
            edit_text,
            second_edit_text,
    )

    ###
    def unhandled_keypress(k):
        #
        if k in ["f10",]:
            raise urwid.ExitMainLoop()
        elif k in ["f9",]:
            exit_flags.append("save")
            raise urwid.ExitMainLoop()
        ###
    #
    loop = urwid.MainLoop(
        uc.main_box,
        unhandled_input=unhandled_keypress,
    )
    loop.run()
    _CLIPBOARD = []
    return {
        'text': uc.edit_box.edit.get_edit_text(),
        'second_text': (None if uc.second_edit_box is None
                        else uc.second_edit_box.edit.get_edit_text()),
        'exit_flags': exit_flags,
        'save': 'save' in exit_flags,
    }


###


def demo_edit_text():
    res = edit_text_terminal_urwid('Append..')
    breakpoint()
