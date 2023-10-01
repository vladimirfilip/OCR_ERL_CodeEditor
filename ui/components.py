import sys
from PyQt6 import QtCore, QtWidgets
from PyQt6.QtGui import QTextCursor
from PyQt6.QtWidgets import QPlainTextEdit


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
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.fixed_contents: str = ""
        self.current_contents: str = ""
        self.cleared = False
        self.textChanged.connect(self.on_text_changed)
        self.setReadOnly(True)
        self.process = QtCore.QProcess(self)
        self.process.setProgram(sys.executable)
        self.process.readyReadStandardOutput.connect(self.on_ready_read_std_output)
        self.process.readyReadStandardError.connect(self.on_ready_read_std_err)

    def run(self, *args):
        self.process.setArguments(args)
        self.process.start()
        self.process.finished.connect(self.on_process_end)
        self.setReadOnly(False)

    @QtCore.pyqtSlot(int, QtCore.QProcess.ExitStatus)
    def on_process_end(self, exit_code):
        msg = f"\n\nProcess finished with exit code {exit_code}"
        self.fixed_contents += msg
        self.insertPlainText(msg)
        self.setReadOnly(True)

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

    def clear(self):
        super().clear()
        self.cleared = True
        self.current_contents = ""
        self.fixed_contents = ""
