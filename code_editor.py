import sys
from tkinter import filedialog, messagebox
from PyQt6 import QtGui
from PyQt6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QGraphicsDropShadowEffect, QLabel
from components import InputTextBox, Terminal, Button
from styles import GLOBAL_STYLES


class CodeEditor(QWidget):
    """
    Base Code Editor UI widget, parent of all other widgets
    """
    #
    # file name of the intermediary text file to be used for passing input source code into the interpreter
    #
    TEMP_FILENAME: str = "t.txt"
    #
    # file path of interpreter
    #
    INTERPRETER_PATH: str = "interpreter.exe"
    #
    # Storage of the file path of the file currently opened and a flag indicating the presence of unsaved changes
    #
    OPEN_FILE_PATH: str = ""
    IS_UNSAVED: bool = False
    #
    # Default source file extension: '.orl' for (O)CR (R)eference (L)anguage
    #
    SOURCE_FILE_EXTENSION: str = ".orl"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        #
        # Initialises base vertical layout where open file path label, input and output textboxes
        # and buttons are placed
        #
        self.layout = QVBoxLayout(self)
        self.setLayout(self.layout)
        #
        # Initialising the label to show the file path of the currently opened file
        #
        self.open_file_label = QLabel(text=self.open_file_path)
        self.layout.addWidget(self.open_file_label)
        #
        # Initialises horizontal layout for input and output textboxes, and initialises these textboxes
        #
        self.textbox_layout = QHBoxLayout()
        self.text_edit = InputTextBox(self,
                                      text_changed=lambda: self.set_unsaved_true())
        self.textbox_layout.addWidget(self.text_edit)
        self.layout.addLayout(self.textbox_layout)
        self.terminal = Terminal()
        self.textbox_layout.addWidget(self.terminal)
        #
        # Initialises button layout and buttons
        #
        self.btn_layout = QHBoxLayout()
        self.btn_layout.addStretch(0)
        file_btn_size = 35
        padding = 5
        self.btn_layout.setContentsMargins(padding, padding, padding, padding)
        self.run_btn = Button(self,
                              fixed_width=50,
                              fixed_height=50,
                              clicked=lambda: self.on_run_btn_click(),
                              icon=QtGui.QIcon("assets\\play.png"))
        self.file_open_btn = Button(self,
                                    fixed_width=file_btn_size,
                                    fixed_height=file_btn_size,
                                    clicked=lambda: self.on_file_open_btn_click(),
                                    icon=QtGui.QIcon("assets\\open.png"))
        self.new_file_btn = Button(self,
                                   fixed_width=file_btn_size,
                                   fixed_height=file_btn_size,
                                   clicked=lambda: self.on_new_file_btn_click(),
                                   icon=QtGui.QIcon("assets\\new.png"))
        self.save_file_btn = Button(self,
                                    fixed_width=file_btn_size,
                                    fixed_height=file_btn_size,
                                    clicked=lambda: self.on_file_save_btn_click(),
                                    icon=QtGui.QIcon("assets\\save.png"))
        self.terminal.setFont(QtGui.QFont("Consolas"))
        self.btn_layout.addWidget(self.new_file_btn)
        self.btn_layout.addWidget(self.save_file_btn)
        self.btn_layout.addWidget(self.run_btn)
        self.btn_layout.addWidget(self.file_open_btn)
        self.btn_layout.addSpacing(file_btn_size + padding)
        self.btn_layout.addStretch()
        #
        # Adds binding so that when terminal starts and ends execution the icon of the
        # run button is set accordingly
        #
        self.terminal.on_run_start = lambda: self.run_btn.setIcon(QtGui.QIcon("assets\\stop.png"))
        self.terminal.on_run_end = lambda: self.run_btn.setIcon(QtGui.QIcon("assets\\play.png"))
        #
        # Adds styling
        #
        CodeEditor.set_button_styles([self.run_btn, self.file_open_btn, self.new_file_btn, self.save_file_btn])
        CodeEditor.add_drop_shadow(self.text_edit)
        CodeEditor.add_drop_shadow(self.terminal)
        self.layout.addLayout(self.btn_layout)
        self.setStyleSheet(GLOBAL_STYLES)

    @staticmethod
    def set_button_styles(buttons: list[Button]):
        for button in buttons:
            CodeEditor.add_drop_shadow(button)
            button.setStyleSheet(f"border-radius: {button.width() // 2}px;")

    @staticmethod
    def add_drop_shadow(widget: QWidget):
        drop_shadow = QGraphicsDropShadowEffect()
        drop_shadow.setColor(QtGui.QColor("#ddd"))
        drop_shadow.setXOffset(0)
        drop_shadow.setYOffset(5)
        drop_shadow.setBlurRadius(15)
        widget.setGraphicsEffect(drop_shadow)

    @property
    def open_file_path(self):
        return CodeEditor.OPEN_FILE_PATH

    @open_file_path.setter
    def open_file_path(self, new_path: str):
        #
        # Updates open file label with the new path as well as the static variable
        #
        self.open_file_label.setText(new_path + ("*" if self.is_unsaved else ""))
        CodeEditor.OPEN_FILE_PATH = new_path

    @property
    def is_unsaved(self):
        return CodeEditor.IS_UNSAVED

    @is_unsaved.setter
    def is_unsaved(self, unsaved: bool):
        #
        # Updates the open file label with a '*' if there are now unsaved changes
        #
        self.open_file_label.setText(self.open_file_path + ("*" if unsaved else ""))
        #
        # Sets static flag
        #
        CodeEditor.IS_UNSAVED = unsaved

    def set_unsaved_true(self):
        self.is_unsaved = True

    def run_code_input(self):
        """
        Writes the contents of the input textbox to the temporary file and runs the interpreter through the editor's terminal.
        :return: None
        """
        with open(CodeEditor.TEMP_FILENAME, "w+") as file:
            file.write(self.text_edit.text)
        self.terminal.clear()
        self.terminal.setFocus()
        run_args = (CodeEditor.INTERPRETER_PATH, CodeEditor.TEMP_FILENAME)
        self.terminal.run(*run_args)

    def on_run_btn_click(self):
        if not self.terminal.is_running:
            self.run_code_input()
        else:
            self.terminal.stop_running()

    def on_file_open_btn_click(self):
        """
        Function for file open functionality.
        Checks if user has unsaved changes and if they want to save them.
        Obtains the file path to open from a file dialog, opens the file, reads its contents and writes it to the input textbox.
        Unsaved changes flag is set to False and the open_file_path field is updated.
        :return: None
        """
        if self.is_unsaved:
            go_ahead: bool = self.ask_save_unsaved_changes(
                "Would you like to save unsaved changes before opening another file?"
            )
            if not go_ahead:
                return
        file_path: str = filedialog.askopenfilename(defaultextension=CodeEditor.SOURCE_FILE_EXTENSION)
        if not file_path:
            return
        self.text_edit.clear()
        with open(file_path, "r") as file:
            while True:
                line = file.readline()
                if line:
                    self.text_edit.appendPlainText(line.rstrip("\n"))
                else:
                    break
        self.open_file_path = file_path
        self.is_unsaved = False
        self.text_edit.setFocus()

    def on_file_save_btn_click(self):
        """
        Saves the contents of the input textbox to the file path in the open_file_path field.
        If open_file_path is empty, a file dialog opens to allow user to choose save location.
        :return: None
        """
        file_path = self.open_file_path
        if not self.open_file_path:
            file_path = filedialog.asksaveasfilename(defaultextension=CodeEditor.SOURCE_FILE_EXTENSION)
        if file_path is None:
            return
        with open(file_path, "w+") as file:
            file.write(self.text_edit.text)
        self.open_file_path = file_path
        self.is_unsaved = False

    def on_new_file_btn_click(self):
        if self.is_unsaved:
            go_ahead: bool = self.ask_save_unsaved_changes(
                "Would you like to save unsaved changes before creating a new file?"
            )
            if not go_ahead:
                return
        self.open_file_path = ""
        self.text_edit.clear()
        self.text_edit.setFocus()
        self.is_unsaved = False

    def ask_save_unsaved_changes(self, msg: str) -> bool:
        """
        Displays a message alert asking the user if they wish to save their unsaved changes.
        User has option for Yes, No or Cancel.
        :param msg: the exact message to display
        :return: a boolean flag indicating True if the user made a Yes/No choice,
        False if the user chose Cancel or closed the alert
        """
        save = messagebox.askyesnocancel(message=msg)
        if save is None:
            return False
        if save:
            self.on_file_save_btn_click()
        return True
