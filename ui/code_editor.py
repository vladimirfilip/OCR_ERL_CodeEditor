import sys

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QFont
from PyQt6.QtWidgets import QWidget, QPushButton, QVBoxLayout, QHBoxLayout, QTextEdit, QPlainTextEdit
from subprocess import Popen, PIPE, STDOUT


#from components import RunBtn, InputTextbox, OutputTextbox
from ui.components import InputTextBox, Terminal

# Subclass QMainWindow to customize your application's main window
class CodeEditor(QWidget):
    TEMP_FILENAME: str = "t.txt"
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.layout = QVBoxLayout(self)
        self.setLayout(self.layout)
        self.textbox_layout = QHBoxLayout(self)
        self.text_edit = InputTextBox(self)
        self.textbox_layout.addWidget(self.text_edit)
        self.layout.addLayout(self.textbox_layout)
        self.terminal = Terminal()
        self.textbox_layout.addWidget(self.terminal)
        button = QPushButton("Run Me!")
        button.clicked.connect(self.on_run_btn_click)
        self.layout.addWidget(button)
        self.running = False

    def on_run_btn_click(self):
        with open(CodeEditor.TEMP_FILENAME, "w") as file:
            file.write(self.text_edit.text)
        self.terminal.clear()
        self.running = True
        self.terminal.setFocus()
        self.terminal.run("C:\\dev\\vlad\\Github\\OCR_ERL_CodeEditor\\interpreter\\interpreter.py", "t.txt")
        # self.subprocess = Popen('python "C:\\dev\\vlad\\Github\\OCR_ERL_CodeEditor\\interpreter\\interpreter.py" "t.txt"',
        #           stdout=PIPE, stderr=STDOUT, stdin=PIPE, shell=False, text=True, encoding="utf8")
        # for line in self.subprocess.stdout:
        #     self.output.append(line.rstrip("\n"))
        self.running = False

