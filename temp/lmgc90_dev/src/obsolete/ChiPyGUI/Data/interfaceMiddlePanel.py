# -*- coding: iso-8859-1 -*-

from Tkinter import *

from localisation import *
from commandFile import CommandFile
from newCommand import NewCommand
import os
from loader import Options

class ButtonSet:
	def __init__(self, parent, interfaceMiddle, textEditor):
		self.parent = interfaceMiddle
		self.textEditor = textEditor
		self.addButton = Button(parent, text = BUT_ADD_COMMAND, command = self.addCommand)
		self.addButton.pack(side = LEFT, pady = 10)
		
		self.delButton = Button(parent, text = BUT_DEL_COMMAND, command = self.delCommand)
		self.delButton.pack(side = LEFT, pady = 10)

		return

	def addCommand(self):
		dialog = NewCommand(self.parent)
		self.parent.wait_window(dialog)

		result = dialog.getResult()

		if(result == "COM"):
			text = dialog.getString()
			self.textEditor.addCommand("#" + text, "COM")
		elif(result == "PRINT"):
			text = dialog.getString()
			self.textEditor.addCommand("print " + text, "PRINT")
		elif(result == "WRAP"):
			module, fonction, opt = dialog.getString()
			line = module + "." + fonction + "(" + opt + ")"
			self.textEditor.addCommand(line, "WRAP")
		elif(result == "ENDLINE"):
			self.textEditor.addCommand("", "ENDLINE")
		elif(result == "ENDTAB"):
			self.textEditor.addCommand("", "ENDTAB")
		elif(result == "FOR"):
			var, min, max, step = dialog.getString()
			line = "for " + var + " in xrange(" + min + "," + max + "," + step + "):"
			self.textEditor.addCommand(line, "FOR")
		elif(result == "FROM"):
			module, fonctions = dialog.getString()
			text = "from %s import %s" %(module, fonctions)
			self.textEditor.addCommand(text, "FROM")
		elif(result == "IMPORT"):
			module = dialog.getString()
			text = "import %s" %module
			self.textEditor.addCommand(text, "IMPORT")
		elif(result == "PYTHON"):
			isIncrementer, ligne = dialog.getString()
			text = "%s" %ligne
			if(isIncrementer.get() == 0):
				isIncrementer = False
			else:
				isIncrementer = True
			self.textEditor.addCommand(text, "PYTHON", isIncrementer)

		dialog.destroy()
		return

	def delCommand(self):
		self.textEditor.deleteCommand()
		return

class ListBoxArea(Listbox):
	def __init__(self, parent, interfaceMiddle, fileName):
		self.interfaceMiddle = interfaceMiddle
		self.fileName = fileName
		loader = Options.getInstance()
		if(self.fileName == ""):
			color = loader.get("errorColor")
		else:
			color = loader.get("internalColor")

		Listbox.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 65, height = 35, selectmode = EXTENDED)

		self.textVariable = StringVar()
		self.textVariable.set(LAB_MIDDLEPANEL_EDITOR %self.fileName)
		self.title = Label(parent, textvariable = self.textVariable, anchor = "w", foreground = color)
		self.title.pack(side = TOP, fill = "x")

		scroll = Scrollbar(parent, command = self.yview)
		self.configure(yscrollcommand = scroll.set)
		scroll.pack(side = RIGHT, fill = "y")

		scroll2 = Scrollbar(parent, command = self.xview, orient=HORIZONTAL)
		self.configure(xscrollcommand = scroll2.set)
		scroll2.pack(side = BOTTOM, fill = "x")		

		self.selection = ()

		# BIND
		self.bind("<ButtonRelease-1>", self.selected)
		self.bind("<Control-c>", self.interfaceMiddle.copy)
		self.bind("<Control-v>", self.interfaceMiddle.paste)
		self.bind("<Control-a>", self.selectAll)
		self.bind("<Delete>", self.interfaceMiddle.deleteCommand)

		return

	def selectAll(self, ev):
		self.select_set(0, END)
		return

	def hasBeenSaved(self):
		self.textVariable.set(LAB_MIDDLEPANEL_EDITOR %self.fileName)
		loader = Options.getInstance()
		color = loader.get("internalColor")
		self.title.configure(foreground = color)
		return

	def hasBeenModified(self):
		self.textVariable.set(LAB_MIDDLEPANEL_EDITOR %self.fileName + " * ")
		loader = Options.getInstance()
		color = loader.get("errorColor")
		self.title.configure(foreground = color)
		return

	def fileNameChanged(self, newFileName):
		self.fileName = newFileName
		self.textVariable.set(LAB_MIDDLEPANEL_EDITOR %self.fileName)
		return

	def getActiveIndex(self):
		return self.active

	def setActive(self, index = ""):
		if(index == ""):
			if(self.get(END) == TXT_ENDFILE):
				self.active = int(self.index(END)) - 1
				if(self.active < 0):
					self.active = 0
			else:
				self.active = int(self.index(END))
		else:
			self.active = index
		return

	def getSelection(self):
		return self.selection

	def selected(self, ev):
		index = self.curselection()

		if(index == ()):
			return
		self.selection = index
		self.activate(index[0])
		self.active = int(index[0])

		self.interfaceMiddle.selectionChanged(int(index[0]))

		self.focus_set()

		return

class TextBoxEditor:
	def __init__(self, parent, statusBar, interfaceCMD, interface, fileName, template = False):
		self.parent = parent
		self.interface = interface

		self.interfaceCMD = interfaceCMD

		self.lineToCopy = []

		if(template):
			self.fileName = ""
		else:
			self.fileName = fileName

		self.listBoxFrame = Frame(self.parent, width = 500, height = 520)
		self.listBoxFrame.pack_propagate(0)
		self.listBoxFrame.pack(side = TOP)
		self.buttonSetFrame = Frame(self.parent)
		self.buttonSetFrame.pack(side = TOP)

		self.listBox = ListBoxArea(self.listBoxFrame, self, self.fileName)
		self.listBox.pack(side = TOP, fill = "both")

		self.buttonSet = ButtonSet(self.buttonSetFrame, self.interface, self)

		self.commandFile = CommandFile()

		if(fileName != ""):
			self.commandFile.openFile(fileName)
		else:
			self.commandFile.newFile()

		self.opened = True

		self.miseAJour(True)
		self.listBox.setActive()

		return

	def getFileName(self):
		return self.fileName

	def closeFile(self):
		self.opened = False
		return

	def isOpened(self):
		return self.opened

	def saveMacro(self, fileName):
		if(fileName != self.fileName):
			self.fileName = fileName
			self.listBox.fileNameChanged(self.fileName)

		listeCommands = self.commandFile.getCommands()

		errors = []

		fichier = ""
		fichier = fichier + "##MACRO NAME : " + self.fileName.split(os.sep)[-1].split(".")[0].strip() + "\n"

		for num in range(len(listeCommands)):
			com = listeCommands[num]
			tabulation = ""
			if(com.getType() == "ENDTAB"):
				fichier = fichier + "ENDTAB\n"
				continue
			if(com.getType() == "ENDFILE"):
				continue
			if(com.getType() == "ENDLINE"):
				fichier = fichier + "ENDLINE\n"
				continue

			if(com.getType() == "PYTHON"):
				if(com.isIncrementer()):
					fichier = fichier + " "
				tab, line = com.getString()
				fichier = fichier + line + "\n"
				continue

			tab, text = com.getString()
			line = text.strip() + "\n"
			if(com.getValidity() == "Invalide"):
				errors.append(ERR_SAVE_LINE %(num, line.strip()))
			if(com.getValidity() == "Inexistante"):
				errors.append(ERR_SAVE_LINE %(num, line.strip()))

			fichier = fichier + line

		self.listBox.hasBeenSaved()
		self.interfaceCMD.hasBeenSaved()

		return errors, fichier

	def saveFile(self, fileName):
		if(fileName != self.fileName):
			self.fileName = fileName
			self.listBox.fileNameChanged(self.fileName)

		listeCommands = self.commandFile.getCommands()

		errors = []

		fichier = ""

		for num in range(len(listeCommands)):
			com = listeCommands[num]
			if(com.getType() == "ENDTAB" or com.getType() == "ENDFILE"):
				continue

			tab, text = com.getString()

			tabulation = "\t" * tab
			line = tabulation + text.strip() + "\n"

			if(com.getValidity() == "Invalide"):
				errors.append(ERR_SAVE_LINE %(num, line.strip()))
			if(com.getValidity() == "Inexistante"):
				errors.append(ERR_SAVE_LINE %(num, line.strip()))

			fichier = fichier + line

		self.listBox.hasBeenSaved()
		self.interfaceCMD.hasBeenSaved()

		return errors, fichier

	def deleteCommand(self, ev = ""):
		if(self.listBox.curselection() == ()):
			return

		activeIndex = self.listBox.getActiveIndex()

		elements = self.listBox.curselection()
		ajustement = 0
		miseAJour = False

		for num in range(len(elements)):
			numElement = int(elements[num])
			deleted = self.commandFile.deleteCommand(numElement - ajustement)
			if(deleted):
				miseAJour = True
			ajustement = ajustement + 1

		self.listBox.activate(activeIndex)

		if(miseAJour):
			self.miseAJour(delete = True)
		self.listBox.selected(self)
		return

	def addCommand(self, commande, type = "WRAP", isIncrementer = False):
		index = self.listBox.index(ACTIVE)
		self.commandFile.addCommand(index, type, commande, isIncrementer)
		self.miseAJour()

		self.listBox.activate(index + 1)
		self.listBox.select_set(ACTIVE)
		return

	def copy(self, ev = ""):
		if(self.listBox.curselection() == ()):
			return False

		self.lineToCopy = []
		selection = self.listBox.curselection()
		for element in selection:
			element = int(element)
			command = self.commandFile.getCommand(element)
			numTab, text = command.getString()
			type = command.getType()
			incrementer = command.isIncrementer()

			theCommand = [type, incrementer, text]
			if(theCommand[2] != TXT_ENDFILE):
				self.lineToCopy.insert(0, theCommand)

		if(self.lineToCopy == []):
			return False

		if(ev != ""):
			self.copyFromAccel()

		return True

	def copyFromAccel(self):
		self.interfaceCMD.copyFromAccel()
		return

	def paste(self, ev = ""):
		if(self.listBox.curselection() == ()):
			return

		if(self.lineToCopy == []):
			return

		selection = self.listBox.curselection()
		if(len(selection) > 1):
			self.deleteCommand()
			
		for line in self.lineToCopy:
			self.addCommand(line[2], line[0], line[1])
		return

	def miseAJour(self, loading = False, delete = False, complete = False):
		if(not loading):
			self.interfaceCMD.hasBeenModified()
			self.listBox.hasBeenModified()

		if(complete):
			self.commandFile.miseAJourTabulation()

		listeCommands = self.commandFile.getCommands()
		index = self.listBox.index(ACTIVE)

		for num in range(len(listeCommands)):
			com = listeCommands[num]
			tabulation = ""
			tab, text = com.getString()
			isValid = com.getValidity()
			for i in range(tab):
				tabulation = tabulation + "   "

			finalLine = tabulation + text
			if(finalLine == self.listBox.get(num)):
				pass
			else:
				self.listBox.delete(num, num)
				self.listBox.insert(num, finalLine)
				loader = Options.getInstance()
				color = "Black"
				if(isValid == "Valide"):
					color = loader.get("validColor")
					bgcolor = loader.bgget("validColor")
				elif(isValid == "Incomplet"):
					color = loader.get("optionalColor")
					bgcolor = loader.bgget("optionalColor")
				elif(isValid == "Invalide"):
					color = loader.get("errorColor")
					bgcolor = loader.bgget("errorColor")
				elif(isValid == "Internal command"):
					color = loader.get("internalColor")
					bgcolor = loader.bgget("internalColor")
				elif(isValid == "Inexistante"):
					color = loader.get("noexistColor")
					bgcolor = loader.bgget("noexistColor")
				elif(isValid == "Ignore"):
					color = loader.get("ignoreColor")
					bgcolor = loader.bgget("ignoreColor")
				if(bgcolor != ""):
					self.listBox.itemconfigure(num, foreground = color, background = bgcolor)
				else:
					self.listBox.itemconfigure(num, foreground = color)

		self.listBox.delete(len(listeCommands), END)
		
		self.listBox.activate(index)
		if(delete):
			self.listBox.select_set(ACTIVE)
		return

	def selectionChanged(self, newSelection):
		self.interfaceCMD.textEditorSelectionChanged(self.commandFile.getCommand(newSelection))
		return
