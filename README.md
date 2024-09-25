# OCR_ERL_CodeEditor

## Introduction
This is a simple code editor program made for writing programs in the UK exam board OCR's Exam Reference Language (ERL). It comes with a built-in LL(1) interpreter and a simple UI made with PyQt6, through which the user can create, save and open text files containing ERL source code and run ERL code from the same interface. 

## For running interpreter on command-line                             
Have Python 3.11 installed on your device and added to environment variables. To test, open command prompt and type `python --version`.
I'm assuming you are all using Windows.

Click on 'Code' at top right, click 'download ZIP', extract files, right-click the `interpreter_dependencies` folder and click 'Open in Terminal'.
Type `python interpreter.py <file path of text file containing code>` and press Enter.
