"""
`qtile shell` starts an interactive interface
    to lookup qtile commands
"""
from libqtile.command import lazy
# ?? equiv?
# from libqtile.lazy import lazy

###
# unusued
vim_style_movement_keys = [
    # Switch between windows
    # idea/default(?): vim-style
    lazy.layout.left(),
    lazy.layout.right(),
    lazy.layout.down(),
    lazy.layout.up(),
    ##
]




def investigate_names__qtile():
     icc = interactive_cmd_cli \
         = lazz  \
         = InteractiveCommandClient(
             LazyCommandInterface()
         )



class CreateDesktops:

    step_types = [
        "jmp", #desktop,..
        "create_window",
        ## idio[t|mas]
        "minimize_all_windows",
        "modify_terminal_window",
        "resize_window",
        #..
    ]

    def __init__(self, *args, **kwargs):
        self._steps =[]

    def steps(self):
        return self._stpes


create_float_theme_desktops = CreateDesktops([
    {"name": "jmp", "word": "3"},
    {"name": "create_window", "word": "st"},
    {"name": "jmp", "word": "8"},
    {"name": "create_window", "word": "chrome"},
    {"name": "jmp", "word": "9"},
    {"name": "create_window", "word": "st"},
    ])



def proceedural_create_desktops():
    for step in p.steps():
        step.do()
    # o otra
    for etape in p.etapas():
        if etapa.name == 'change_desktop':
            curr_desktop = etapa.word









###








##### looking for (1) introspection and (2) interactive interpreter use of what, in the qtile.py IoC config script for `qtile`, is `from libqtile.lazy import lazy`
from libqtile.lazy import LazyCommandInterface
from libqtile.command.interface import CommandInterface
from libqtile.command.client import InteractiveCommandClient
