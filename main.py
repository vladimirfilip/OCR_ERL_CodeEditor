from code_editor import CodeEditor
import sys


from PyQt6.QtWidgets import QApplication, QMainWindow


app = QApplication(sys.argv)
window = QMainWindow()
code_editor = CodeEditor()
window.setStyleSheet("QMainWindow{background-color: #fff;}")
window.setCentralWidget(code_editor)
window.show()
app.exec()