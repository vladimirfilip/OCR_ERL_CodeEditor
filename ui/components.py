import sys
from typing import Optional, Callable

from PyQt6 import QtCore, QtWidgets
from PyQt6.QtCore import QEvent
from PyQt6.QtGui import QTextCursor, QWheelEvent
from PyQt6.QtWidgets import QPlainTextEdit, QHBoxLayout, QWidget


class InputTextBox(QPlainTextEdit):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.textChanged.connect(self.on_text_changed)
        self.__text = ""

    @property
    def text(self):
        return self.__text

    @text.setter
    def text(self, new_text: str):
        self.__text = new_text

    def on_text_changed(self):
        self.text = self.toPlainText()


class Terminal(QtWidgets.QPlainTextEdit):
    def __init__(self, on_run_start: Optional[Callable] = None, on_run_end: Optional[Callable] = None, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.fixed_contents: str = ""
        self.current_contents: str = ""
        self.cleared = False
        self.textChanged.connect(self.on_text_changed)
        self.setReadOnly(True)
        self.process: Optional[QtCore.QProcess] = None
        self.on_run_start = on_run_start
        self.on_run_end = on_run_end

    def run(self, *args):
        self.process = QtCore.QProcess(self)
        self.process.setProgram(sys.executable)
        self.process.readyReadStandardOutput.connect(self.on_ready_read_std_output)
        self.process.readyReadStandardError.connect(self.on_ready_read_std_err)
        self.process.setArguments(args)
        self.process.start()
        self.process.finished.connect(self.on_process_end)
        self.setReadOnly(False)
        if self.on_run_start is not None:
            self.on_run_start()

    @QtCore.pyqtSlot(int, QtCore.QProcess.ExitStatus)
    def on_process_end(self, exit_code):
        msg = f"\n\nFinished execution with exit code {exit_code}"
        self.fixed_contents += msg
        self.insertPlainText(msg)
        self.setReadOnly(True)
        self.process = None
        if self.on_run_end:
            self.on_run_end()

    @QtCore.pyqtSlot()
    def on_ready_read_std_output(self):
        out = self.process.readAllStandardOutput().data().decode().replace("\r", "")
        self.fixed_contents = self.toPlainText() + out
        self.insertPlainText(out)

    @QtCore.pyqtSlot()
    def on_ready_read_std_err(self):
        err = "\n" + self.process.readAllStandardError().data().decode()
        self.fixed_contents += self.toPlainText() + err
        self.insertPlainText(err)

    def send_input(self, val: str):
        self.process.write(val.encode())

    def on_text_changed(self):
        if self.cleared:
            self.cleared = False
            return
        contents: str = self.toPlainText()
        prev_contents: str = self.current_contents
        cursor: QTextCursor = self.textCursor()
        cursor_pos: int = cursor.position()
        if len(contents) < len(prev_contents):
            if cursor_pos < len(self.fixed_contents):
                cursor.insertText(self.fixed_contents[cursor_pos])
        elif len(contents) > len(prev_contents):
            if cursor_pos <= len(self.fixed_contents) and prev_contents and contents != self.fixed_contents:
                cursor.deletePreviousChar()
        self.current_contents = self.toPlainText()
        non_fixed_contents: str = self.current_contents[len(self.fixed_contents):]
        if non_fixed_contents and non_fixed_contents[-1] == "\n":
            self.send_input(non_fixed_contents)
            self.fixed_contents += non_fixed_contents

    def stop_running(self):
        self.process.close()
        self.process = None

    @property
    def is_running(self):
        return self.process is not None

    def clear(self):
        self.cleared = True
        self.current_contents = ""
        self.fixed_contents = ""
        super().clear()
