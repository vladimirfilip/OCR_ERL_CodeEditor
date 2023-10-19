import sys

from PyQt6 import QtGui
from PyQt6.QtWidgets import QWidget, QPushButton, QVBoxLayout, QHBoxLayout, QGraphicsDropShadowEffect
from ui.components import InputTextBox, Terminal
from ui.styles import GLOBAL_STYLES
from tkinter import filedialog


class CodeEditor(QWidget):
    TEMP_FILENAME: str = "t.txt"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.interpreter_path = ""
        self.layout = QVBoxLayout(self)
        self.setLayout(self.layout)
        self.textbox_layout = QHBoxLayout()
        self.text_edit = InputTextBox(self)
        self.text_edit.setFont(QtGui.QFont("Consolas"))
        self.text_edit.setTabStopDistance(26)
        self.textbox_layout.addWidget(self.text_edit)
        self.layout.addLayout(self.textbox_layout)
        self.terminal = Terminal()
        self.textbox_layout.addWidget(self.terminal)
        self.btn_layout = QHBoxLayout()
        self.btn_layout.addStretch(0)
        self.run_btn = QPushButton(self)
        self.run_btn.setFixedWidth(50)
        self.run_btn.setFixedHeight(50)
        self.run_btn.clicked.connect(self.on_run_btn_click)
        self.run_btn.setIcon(QtGui.QIcon("ui\play.png"))
        self.terminal.setFont(QtGui.QFont("Consolas"))
        self.terminal.on_run_start = lambda: self.run_btn.setIcon(QtGui.QIcon("ui\stop.png"))
        self.terminal.on_run_end = lambda: self.run_btn.setIcon(QtGui.QIcon("ui\play.png"))
        CodeEditor.add_drop_shadow(self.run_btn)
        CodeEditor.add_drop_shadow(self.text_edit)
        CodeEditor.add_drop_shadow(self.terminal)
        self.btn_layout.addWidget(self.run_btn)
        self.btn_layout.addStretch()
        self.layout.addLayout(self.btn_layout)
        self.setStyleSheet(GLOBAL_STYLES)

    @staticmethod
    def add_drop_shadow(widget: QWidget):
        drop_shadow = QGraphicsDropShadowEffect()
        drop_shadow.setColor(QtGui.QColor("#ddd"))
        drop_shadow.setXOffset(0)
        drop_shadow.setYOffset(5)
        drop_shadow.setBlurRadius(15)
        widget.setGraphicsEffect(drop_shadow)

    def on_run_btn_click(self):
        if not self.terminal.is_running:
            self.run_code_input()
        else:
            self.terminal.stop_running()

    def run_code_input(self):
        with open(CodeEditor.TEMP_FILENAME, "w") as file:
            file.write(self.text_edit.text)
        self.terminal.clear()
        self.terminal.setFocus()
        if not self.interpreter_path:
            self.interpreter_path = filedialog.askopenfilename()
        if self.interpreter_path.split(".")[-1] == "py":
            run_args = (sys.executable, self.interpreter_path, CodeEditor.TEMP_FILENAME)
        else:
            run_args = (self.interpreter_path, CodeEditor.TEMP_FILENAME)
        self.terminal.run(*run_args)
