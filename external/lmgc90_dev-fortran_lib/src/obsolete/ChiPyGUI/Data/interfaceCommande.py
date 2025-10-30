# -*- coding: iso-8859-1 -*-

from Tkinter import *

from interfaceLeftPanel import ListBoxes
from interfaceMiddlePanel import TextBoxEditor
from interfaceRightPanel import ParamGroup

class LeftPanel(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 224, height = 600)
		self.pack_propagate(0)
		self.pack(side = LEFT)
		return

class MiddlePanel(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 500, height = 600)
		self.pack_propagate(0)
		self.pack(side = LEFT)
		return

class RightPanel(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 300, height = 600)
		self.pack_propagate(0)
		self.pack(side = LEFT)
		return

class InterfaceCMD:
	def __init__(self, parent, interface, fileName, template = False):
		self.interface = interface
		self.parent = parent
		self.leftPanel = LeftPanel(self.parent)

		self.modified = False

		self.middlePanel = MiddlePanel(self.parent)

		self.rightPanel = RightPanel(self.parent)

		self.listBoxes = ListBoxes(self.leftPanel, parent.getStatusBar(), self)
		self.textBoxEditor = TextBoxEditor(self.middlePanel, parent.getStatusBar(), self, self.interface, fileName, template = template)
		self.parametre = ParamGroup(self.rightPanel, self)

		return

	def copyFromAccel(self):
		self.interface.copyFromAccel()
		return

	def copy(self):
		return self.textBoxEditor.copy()

	def paste(self):
		self.textBoxEditor.paste()
		return

	def getFileName(self):
		return self.textBoxEditor.getFileName()

	def closeFile(self):
		self.textBoxEditor.closeFile()
		return

	def isOpened(self):
		return self.textBoxEditor.isOpened()

	def reloadLeftPanel(self):
		self.listBoxes.reloadLeftPanel()
		return

	def miseAJourMacros(self):
		self.listBoxes.miseAJourMacros()
		return

	def saveFile(self, fileName):
		return self.textBoxEditor.saveFile(fileName)

	def saveMacro(self, fileName):
		return self.textBoxEditor.saveMacro(fileName)

	def destroy(self):
		self.leftPanel.destroy()
		self.middlePanel.destroy()
		self.rightPanel.destroy()
		return

	def hasBeenModified(self):
		self.modified = True
		return

	def isModified(self):
		return self.modified

	def hasBeenSaved(self):
		self.modified = False
		return

	def textEditorSelectionChanged(self, commande):
		self.parametre.loadInterior(commande)
		return

	def miseAJour(self, complete = False):
		self.textBoxEditor.miseAJour(complete = complete)
		return

	def addCommand(self, commande, type = "", isIncrementer = False):
		if(type == ""):
			self.textBoxEditor.addCommand(commande, isIncrementer = isIncrementer)
		else:
			self.textBoxEditor.addCommand(commande, type = type, isIncrementer = isIncrementer)
		return
