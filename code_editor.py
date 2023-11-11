import sys

from PyQt6 import QtGui
from PyQt6.QtWidgets import QWidget, QPushButton, QVBoxLayout, QHBoxLayout, QGraphicsDropShadowEffect
from ui.components import InputTextBox, Terminal
from ui.styles import GLOBAL_STYLES


class CodeEditor(QWidget):
    """
    Base widget for the all widgets such as buttons, input textbox and output terminal
    """
    #
    # file name of the text file to be used for passing input source code into the interpreter
    #
    TEMP_FILENAME: str = "t.txt"
    #
    # file path of interpreter
    #
    INTERPRETER_PATH: str = "interpreter/interpreter.py"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        #
        # Initialises base vertical layout where input and output textboxes are stored at the top
        # and buttons are placed at the bottom
        #
        self.layout = QVBoxLayout(self)
        self.setLayout(self.layout)
        #
        # Initialises horizontal layout for input and output textboxes, and initialises these textboxes
        #
        self.textbox_layout = QHBoxLayout()
        self.text_edit = InputTextBox(self)
        self.textbox_layout.addWidget(self.text_edit)
        self.layout.addLayout(self.textbox_layout)
        self.terminal = Terminal()
        self.textbox_layout.addWidget(self.terminal)
        #
        # Initialises button layout and buttons
        #
        self.btn_layout = QHBoxLayout()
        self.btn_layout.addStretch(0)
        self.run_btn = QPushButton(self)
        self.run_btn.setFixedWidth(50)
        self.run_btn.setFixedHeight(50)
        self.run_btn.clicked.connect(self.on_run_btn_click)
        self.run_btn.setIcon(QtGui.QIcon("ui\play.png"))
        self.terminal.setFont(QtGui.QFont("Consolas"))
        #
        # Adds binding so that when terminal starts and ends execution the icon,
        # the run button is set accordingly
        #
        self.terminal.on_run_start = lambda: self.run_btn.setIcon(QtGui.QIcon("ui\stop.png"))
        self.terminal.on_run_end = lambda: self.run_btn.setIcon(QtGui.QIcon("ui\play.png"))
        self.btn_layout.addWidget(self.run_btn)
        self.btn_layout.addStretch()
        #
        # Adds styling
        #
        CodeEditor.add_drop_shadow(self.run_btn)
        CodeEditor.add_drop_shadow(self.text_edit)
        CodeEditor.add_drop_shadow(self.terminal)
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
        """
        Writes the contents of the input textbox to the temporary file and runs the interpreter through the editor's terminal.
        :return: None
        """
        with open(CodeEditor.TEMP_FILENAME, "w+") as file:
            file.write(self.text_edit.text)
        self.terminal.clear()
        self.terminal.setFocus()
        run_args = (sys.executable, CodeEditor.INTERPRETER_PATH, CodeEditor.TEMP_FILENAME)
        self.terminal.run(*run_args)
