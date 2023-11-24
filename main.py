from code_editor import CodeEditor
import sys
from PyQt6.QtWidgets import QApplication, QMainWindow
from PyQt6 import QtGui
import ctypes

#
# Tells Windows that the app currently running should not be grouped with all apps hosted by the python executable
# and so its taskbar icon should be the window icon, not the default python icon
#
myappid = u'mycompany.myproduct.subproduct.version' # arbitrary string
ctypes.windll.shell32.SetCurrentProcessExplicitAppUserModelID(myappid)
app = QApplication(sys.argv)
window = QMainWindow()
code_editor = CodeEditor()
window.setStyleSheet("QMainWindow{background-color: #fff;}")
window.setWindowIcon(QtGui.QIcon("ui\\appicon.ico"))
window.setWindowTitle("OCR ERL Code Editor")
window.setCentralWidget(code_editor)
window.setMinimumWidth(600)
window.setMinimumHeight(400)
window.show()
app.exec()
