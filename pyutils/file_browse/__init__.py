import os
import os.path as pth
from functools import partial
#
import urwid


# todo:
# * sieve/filter (name, time, etc.)
# * select-multiple/highlight items ['<0>']
# * optionally return filtered or selected list
# * scrollbar & mouse click to select
# retain cat-v-style minimalism, and
# implement separate mc clone otherwise.
def simple_file_browser_urwid(dirname='.'):
    if not pth.isdir(dirname):
        raise ValueError("not a directory",
                         (dirname,))
    ###
    list_walker = urwid.SimpleFocusListWalker([])
    def list_walker_modified_callback(
            list_walker, focus):
        for idx, ent in enumerate(list_walker):
            if isinstance(ent, urwid.AttrWrap):
                list_walker[idx] = ent.get_w()
        sel = list_walker[focus]
        hl_sel = urwid.AttrWrap(sel, 'focus')
        list_walker[focus] = hl_sel
    #
    def set_curr_dir(list_walker, name):
        simple_file_browser_urwid.curr_dir = (
            pth.abspath(name))
        filelist = (
            ['./', '../',] +
            [filename + (
                '/' if pth.isdir(
                    pth.join(name, filename))
                else '') for filename
             in os.listdir(name)]
        )
        list_walker.clear()
        list_walker += [
            urwid.Text(text) for text in filelist]
        list_walker_modified_callback(list_walker, 0)
    #
    set_curr_dir(list_walker, dirname)
    list_walker.set_focus_changed_callback(
        partial(list_walker_modified_callback,
                list_walker))
    def unhandled_keypress(key):
        if key in ["f10", "ctrl q"]:
            raise urwid.ExitMainLoop()
        elif key in ["enter"]:
            name, _ = (list_walker[list_walker.focus]
                       .get_w()
                       .get_text())
            if name[-1] == '/':
                set_curr_dir(
                    list_walker,
                    pth.join(
                        simple_file_browser_urwid.curr_dir,
                        name,
                    )
                )
        elif key in ["ctrl n"]:
            if list_walker.focus < len(list_walker) - 1:
                list_walker.focus += 1
        elif key in ["ctrl p"]:
            if list_walker.focus > 0:
                list_walker.focus -= 1
    ###
    listbox = urwid.ListBox(list_walker)
    view = urwid.Frame(listbox)
    palette = (
        ('focus', 'light gray', 'dark blue', 'standout'),
    )
    loop = urwid.MainLoop(
        view,
        palette,
        unhandled_input=unhandled_keypress,
    )
    loop.run()
    return pth.join(
        simple_file_browser_urwid.curr_dir,
        (list_walker[list_walker.focus]
         .get_w()
         .get_text())[0],
    )
simple_file_browser_urwid.curr_dir = None
