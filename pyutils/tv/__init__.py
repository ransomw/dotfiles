import tkinter
import subprocess
import signal
import shlex
import threading
from subprocess import Popen


def mouse_only_ui(cmd_arg):
    cmd = (shlex.split(cmd_arg)
            if isinstance(cmd_arg, (str,))
            else cmd_arg)
    if not isinstance(cmd, (list,)):
        raise ValueError()
    #
    app_state = {}
    def stdio_reader(file_obj_name):
        if file_obj_name not in ['stdout', 'stderr']:
            raise ValueError()
        proc = app_state['proc']
        file_obj = getattr(proc, file_obj_name)
        for line in iter((file_obj.readline
                          if file_obj else lambda: b""), b""):
            app_state[file_obj_name].append(
                line.decode("utf-8"))

    def on_go():
        if 'proc' in app_state and app_state['proc'].poll() is None:
            return
        proc = Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        app_state['proc'] = proc
        # workaround "pipe buffer deadlock"
        # also consider
        # * .communicate() with a future (?)
        # * passing a file object to stdout and stderr
        app_state['stdout'] = []
        app_state['stderr'] = []
        app_state['reader_threads'] = [
            threading.Thread(
                target=stdio_reader,
                args=('stdout',),
            ),
            threading.Thread(
                target=stdio_reader,
                args=('stderr',),
            ),
        ]
        for t in app_state['reader_threads']:
            t.start()

    def on_stop():
        if 'proc' not in app_state:
            return
        proc = app_state['proc']
        #
        for sig in [None, signal.SIGINT,
                signal.SIGKILL,
                signal.SIGTERM,]:
            if sig is not None:
                proc.send_signal(sig)
            if sig is None:
                if proc.poll() is None:
                    continue
                else:
                    break
            try:
                if sig is not None:
                    proc.wait(timeout=3)
            except subprocess.TimeoutExpired as ex:
                continue
            break
        else:
            raise RuntimeError("process didn't exit")
        #
        for t in app_state['reader_threads']:
            t.join()
        if proc.poll() != 0:
            print("error exit")
            print("stdout")
            print(('\n'.join(app_state['stdout'])
                   if app_state['stdout']
                   else "<None>"))
            print("stderr")
            print(('\n'.join(app_state['stderr'])
                   if app_state['stderr']
                   else "<None>"))
        del app_state['proc']
        del app_state['reader_threads']
        del app_state['stdout']
        del app_state['stderr']
    ###
    # ui
    root = tkinter.Tk()
    left_frame = tkinter.Frame(
            root,
#            relief="raised",
#            color="#200",
            )
    left_frame["bg"] = "purple"
    right_frame = tkinter.Frame(
            root,
            )
    right_frame["bg"] = "pink"
    left_frame.pack(
            side="left",
            fill="both",
            anchor="center",
            expand=True,
            )
    right_frame.pack(
            side="right",
            fill="y",
            )
    ##
    # https://stackoverflow.com/questions/42579927/rounded-button-tkinter-python
    # suggests using canvas.
    # elsewhere suggested to use image/bitmap
    class RoundButton(tkinter.Button, tkinter.Canvas):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            # self.create_line(0, 0, 10, 10)
            self.create_line(*self.bbox("all"))
        #
        def _configure(self, *args, **kwargs):
            rv = super()._configure(*args, **kwargs)
            breakpoint()
            return rv
    ##
    go_btn = tkinter.Button(
            left_frame,
            text="Go!",
            command=on_go,
            relief="raised",
            )
    go_btn["bg"] = "#082"
    stop_btn = tkinter.Button(
            right_frame,
            text="stop",
            command=on_stop,
            )
    stop_btn["bg"] = "#086"
    go_btn.pack(
            side="bottom",
            # expand=True,
            fill="both",
            )
    stop_btn.pack(
            side="top",
            )
    root.mainloop()
