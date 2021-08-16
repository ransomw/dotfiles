from pyutils.text_edit.urwid import (
    edit_text_terminal_urwid,
)

def edit_file_terminal(filename):
    filename = os.path.realpath(filename)
    with open(filename) as f:
        text_before = f.read()
    res = edit_text_terminal_urwid(text_before)
    text_after = res['text']
    with open(filename, 'w') as f:
        f.write(text_after)


def demo_edit_text():
    res = edit_text_terminal_urwid('Append..')
    breakpoint()


