import os

import curses

import urwid

import toolz.itertoolz as itlz

def edit_text_terminal_curses(text):
    # startup
    stdscr = curses.initscr()
    curses.noecho()
    curses.cbreak()
    stdscr.keypad(True)
    ###

    height, width = stdscr.getmaxyx()
    editwin = curses.newwin(height - 10, width - 10, 1, 1)
    editwin.scrollok(True)
    curses.textpad.rectangle(stdscr, 0, 0, height - 9, width - 9)
    stdscr.refresh()

    box = curses.textpad.Textbox(editwin, insert_mode=True)
    for char in text:
        box.do_command(char)
    # Let the user edit until Ctrl-G is struck.
    box.edit()

    # Get resulting contents
    message = box.gather()

    # exit
    curses.nocbreak()
    stdscr.keypad(False)
    curses.echo()
    curses.endwin()

    return message





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
            clipboard = edit_text_terminal_urwid.clipboard
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
                if self.region_bdry is None:
                    return
                if self.edit.edit_pos <= self.region_bdry:
                    cp_text = self.edit.get_edit_text()[
                        self.edit.edit_pos:self.region_bdry]
                else:
                    cp_text = self.edit.get_edit_text()[
                        self.region_bdry+1:self.edit.edit_pos]
                clipboard.append(cp_text)
                self.edit.set_edit_text(
                    self.edit.get_edit_text()[:self.region_bdry]
                    +
                    self.edit.get_edit_text()[self.region_bdry+1:])
                self.region_bdry = None
                return
            elif key in ["ctrl w",]:
                if self.region_bdry is None:
                    return
                if self.edit.edit_pos <= self.region_bdry:
                    cut_text = self.edit.get_edit_text()[
                        self.edit.edit_pos:self.region_bdry]
                    self.edit.set_edit_text(
                        self.edit.get_edit_text()[:self.edit.edit_pos]
                        +
                        self.edit.get_edit_text()[self.region_bdry+1:])
                else:
                    cut_text = self.edit.get_edit_text()[
                        self.region_bdry+1:self.edit.edit_pos]
                    self.edit.set_edit_text(
                        self.edit.get_edit_text()[:self.region_bdry]
                        +
                        self.edit.get_edit_text()[self.edit.edit_pos:])
                    self.edit.set_edit_pos(self.region_bdry)
                clipboard.append(cut_text)
                self.region_bdry = None
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
            return super(EditBox, self).keypress(size, key)


    edit_box = EditBox(
        "top",
        edit_text=edit_text,
    )
    if second_edit_text is not None:
        second_edit_box = EditBox(
            "top",
            edit_text=second_edit_text,
        )
        main_loop_box = urwid.Columns([
            urwid.LineBox(
                edit_box,
            ),
            urwid.LineBox(
                second_edit_box,
            ),
        ])
    else:
        second_edit_box = None
        main_loop_box = urwid.LineBox(
            edit_box,
        )
    ###
    def unhandled_keypress(k):
        #
        if k in ["f10",]:
            raise urwid.ExitMainLoop()
        elif k in ["f9",]:
            edit_text_terminal_urwid.exit_flags.append("save")
            raise urwid.ExitMainLoop()
        ###
    #
    loop = urwid.MainLoop(
        main_loop_box,
        unhandled_input=unhandled_keypress,
    )
    loop.run()
    exit_flags = edit_text_terminal_urwid.exit_flags
    edit_text_terminal_urwid.clipboard = []
    edit_text_terminal_urwid.exit_flags = []
    return {
        'text': edit_box.edit.get_edit_text(),
        'second_text': (None if second_edit_box is None
                        else second_edit_box.edit.get_edit_text()),
        'exit_flags': exit_flags,
        'save': 'save' in exit_flags,
    }
edit_text_terminal_urwid.clipboard = []
edit_text_terminal_urwid.exit_flags = []


def edit_file_terminal(filename):
    filename = os.path.realpath(filename)
    with open(filename) as f:
        text_before = f.read()
    res = edit_text_terminal_urwid(text_before)
    text_after = res['text']
    with open(filename, 'w') as f:
        f.write(text_after)
