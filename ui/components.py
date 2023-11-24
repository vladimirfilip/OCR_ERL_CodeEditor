from typing import Optional, Callable
from PyQt6 import QtCore, QtWidgets
from PyQt6.QtGui import QTextCursor
from PyQt6.QtWidgets import QPlainTextEdit, QPushButton
from PyQt6 import QtGui


class Button(QPushButton):
    def __init__(self, *args, **kwargs):
        """
        A child class of the PyQt PushButton class that sets the following characteristics through
        the keyword arguments provided at instantiation:
         - fixed width
         - fixed height
         - onclick event
         - icon

        :param args: arguments given at instantiation
        :param kwargs: keyword arguments given at instantiation
        """
        reserved_kwargs = ["fixed_width", "fixed_height", "clicked", "icon"]
        filtered_kwargs = {}
        for k, v in kwargs.items():
            if k not in reserved_kwargs:
                filtered_kwargs[k] = v
        super().__init__(*args, **filtered_kwargs)
        arg_to_method = {
            "fixed_width": self.setFixedWidth,
            "fixed_height": self.setFixedHeight,
            "clicked": self.clicked.connect,
            "icon": self.setIcon,
        }
        assert list(arg_to_method.keys()) == reserved_kwargs, "No sync between reserved kwargs and associated methods"
        for k in arg_to_method.keys():
            if (proc_args := kwargs.pop(k, None)) is not None:
                if type(proc_args) != list:
                    proc_args = [proc_args]
                arg_to_method[k](*proc_args)


class InputTextBox(QPlainTextEdit):
    """
    Textbox in which source code input is added by the user. Contains some simple bindings
    so that the text input is constantly kept up-to-date
    """

    def __init__(self, *args, **kwargs):
        self.outer_text_changed = kwargs.pop("text_changed", None)
        super().__init__(*args, **kwargs)
        self.textChanged.connect(self.on_text_changed)
        self.__text = ""
        self.setFont(QtGui.QFont("Consolas"))
        self.setTabStopDistance(26)

    @property
    def text(self):
        return self.__text

    @text.setter
    def text(self, new_text: str):
        self.__text = new_text

    def on_text_changed(self):
        self.text = self.toPlainText()
        self.outer_text_changed()


class Terminal(QtWidgets.QPlainTextEdit):
    """
    The widget where user interacts with the interpreter. Runs the interpreter and output from the interpreter is kept read-only, along with previous
    inputs from the user.
    """

    def __init__(self, on_run_start: Optional[Callable] = None, on_run_end: Optional[Callable] = None, *args, **kwargs):
        super().__init__(*args, **kwargs)
        #
        # Fields storing the contents of the terminal that are read-only and all text in the terminal
        #
        self.fixed_contents: str = ""
        self.current_contents: str = ""
        self.cleared = False
        self.textChanged.connect(self.on_text_changed)
        #
        # When a program is not running, the terminal is kept entirely read-only
        #
        self.setReadOnly(True)
        self.process: Optional[QtCore.QProcess] = None
        self.on_run_start = on_run_start
        self.on_run_end = on_run_end

    def run(self, *args):
        """
        Runs the given command in a separate thread and binds std_output and str_err to itself so that the user can
        view outputs and error messages, along with events for starting and the end in execution.
        :param args: console arguments
        :return: None
        """
        self.process = QtCore.QProcess(self)
        program, rest_args = args[0], args[1:]
        self.process.setProgram(program)
        self.process.readyReadStandardOutput.connect(self.on_ready_read_std_output)
        self.process.readyReadStandardError.connect(self.on_ready_read_std_err)
        self.process.setArguments(rest_args)
        self.process.start()
        self.process.finished.connect(self.on_process_end)
        self.setReadOnly(False)
        if self.on_run_start is not None:
            self.on_run_start()

    @QtCore.pyqtSlot(int, QtCore.QProcess.ExitStatus)
    def on_process_end(self, exit_code):
        """
        Calls the on_run_end event and sets itself as read-only
        :param exit_code: exit code from execution
        :return:
        """
        msg = f"\n\nFinished execution with exit code {exit_code}"
        self.fixed_contents += msg
        self.insertPlainText(msg)
        self.setReadOnly(True)
        self.process = None
        if self.on_run_end:
            self.on_run_end()

    @QtCore.pyqtSlot()
    def on_ready_read_std_output(self):
        """
        Decodes standard output from the running program and adds it to the terminal contents, displaying it to the user
        :return:
        """
        out = self.process.readAllStandardOutput().data().decode().replace("\r", "")
        self.fixed_contents = self.toPlainText() + out
        self.insertPlainText(out)

    @QtCore.pyqtSlot()
    def on_ready_read_std_err(self):
        """
        Decodes standard error output from the running program and adds it to the terminal contents, displaying it to the user
        :return:
        """
        err = "\n" + self.process.readAllStandardError().data().decode()
        self.fixed_contents += self.toPlainText() + err
        self.insertPlainText(err)

    def send_input(self, val: str):
        """
        Sends user input to the running interpreter process
        :param val: value to send
        :return: None
        """
        self.process.write(val.encode())

    def on_text_changed(self):
        """
        Called whenever the contents of the terminal are changed. Enforces that read-only contents are retained and
        deduces when the user has finished typing input for the interpreter.
        :return:
        """
        #
        # Clearing the terminal overrides any enforcement of read-only contents.
        #
        if self.cleared:
            self.cleared = False
            return
        contents: str = self.toPlainText()
        prev_contents: str = self.current_contents
        cursor: QTextCursor = self.textCursor()
        cursor_pos: int = cursor.position()
        """
        The algorithm can detected whether read-only text has been edited based on the cursor position and the previous contents.
        If the read-only text is changed, that added or removed character is removed or added back, respectively.
        """
        if len(contents) < len(prev_contents):
            if cursor_pos < len(self.fixed_contents):
                cursor.insertText(self.fixed_contents[cursor_pos])
        elif len(contents) > len(prev_contents):
            if cursor_pos <= len(self.fixed_contents) and prev_contents and contents != self.fixed_contents:
                cursor.deletePreviousChar()
        self.current_contents = self.toPlainText()
        non_fixed_contents: str = self.current_contents[len(self.fixed_contents):]
        """
        If user inputted a new-line, then any non-read-only contents are input to the interpreter, which is sent.
        """
        if non_fixed_contents and non_fixed_contents[-1] == "\n":
            self.send_input(non_fixed_contents)
            self.fixed_contents += non_fixed_contents

    def stop_running(self):
        """
        Ends the running process
        :return:
        """
        self.process.close()
        self.process = None

    @property
    def is_running(self):
        return self.process is not None

    def clear(self):
        """
        Clears the terminal.
        :return:
        """
        self.cleared = True
        self.current_contents = ""
        self.fixed_contents = ""
        super().clear()
