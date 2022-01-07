import os
import curses


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


