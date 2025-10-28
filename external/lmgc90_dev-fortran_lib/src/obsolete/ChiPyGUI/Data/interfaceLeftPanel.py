# -*- coding: iso-8859-1 -*-

from Tkinter import *

from loader import ModuleLoader
from loader import FunctionLoader
from loader import MacroLoader
from loader import Options
from localisation import *

class InterfaceFonction(Listbox):
	def __init__(self, parent, listBoxModule, mode = "FuncMod"):
		self.interfaceModule = listBoxModule
		self.mode = mode
		self.selection = ""

		# CREATION INTERFACE
		Listbox.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 24, height = 10)

		loader = Options.getInstance()
		color = loader.get("internalColor")

		if(self.mode == "FuncMod"):
			self.label = Label(parent, text = LAB_LEFTPANEL_FONCTION, anchor = "w", foreground = color)
		else:
			self.label = Label(parent, text = LAB_LEFTPANEL_MODULE, anchor = "w", foreground = color)
		self.label.pack(side = TOP, fill = "x")

		self.scroll = Scrollbar(parent, command = self.yview)
		self.configure(yscrollcommand = self.scroll.set)
		self.scroll.pack(side = RIGHT, fill = "y")

		if(self.mode == "FuncMod"):
			## Recuperation de la liste des fonctions
			loader = FunctionLoader.getInstance()
			liste = loader.getAllFunctions()
		else:
			## Recuperation de la liste des modules
			loader = ModuleLoader.getInstance()
			liste = loader.getModules()

		for element in liste:
			self.insert(END, element)
			if(element == WRAP_OVERALL_MODULE or element == WRAP_AFTERALL_MODULE):
				loader = Options.getInstance()
				color = loader.get("warningColor")
				bgcolor = loader.bgget("warningColor")
				self.itemconfigure(END, foreground = color, background = bgcolor)

		# BIND
		self.bind("<ButtonRelease-1>", self.selected)
		return

	def vider(self):
		self.label.destroy()
		self.scroll.destroy()
		return

	def selected(self, ev):
		if(self.curselection() == ()):
			self.selection = ""
			return

		self.selection = self.get(self.curselection())
		self.interfaceModule.elementSelectionChanged(self.selection)
		return
	pass

class InterfaceModule(Listbox):
	def __init__(self, parent, statusBar, listBoxes, mode = "FuncMod"):
		self.statusBar = statusBar
		self.mode = mode
		if(self.mode == "FuncMod"):
			self.fonction = ""
		else:
			self.module = ""

		self.listBoxes = listBoxes

		# CREATION INTERFACE
		Listbox.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 24, height = 10)

		loader = Options.getInstance()
		color = loader.get("internalColor")

		if(self.mode == "FuncMod"):
			self.label = Label(parent, text = LAB_LEFTPANEL_MODULE, anchor = "w", foreground = color)
		else:
			self.label = Label(parent, text = LAB_LEFTPANEL_FONCTION, anchor = "w", foreground = color)
		self.label.pack(side = TOP, fill = "x")

		self.scroll = Scrollbar(parent, command = self.yview)
		self.configure(yscrollcommand = self.scroll.set)
		self.scroll.pack(side = RIGHT, fill = "y")

		# BIND
		self.bind("<ButtonRelease-1>", self.selected)
		self.bind("<Double-Button-1>", self.doubleClic)
		return

	def vider(self):
		self.label.destroy()
		self.scroll.destroy()
		return

	def doubleClic(self, ev):
		if(self.mode == "FuncMod"):
			self.listBoxes.addCommand(self.selection + "." + self.fonction + "()")
		else:
			self.listBoxes.addCommand(self.module + "." + self.selection + "()")
		return

	def selected(self, ev):
		if(self.curselection() == ()):
			self.selection = ""
			return

		self.selection = self.get(self.curselection())

		funcLoader = FunctionLoader.getInstance()

		if(self.mode == "FuncMod"):
			liste = funcLoader.getFunctionsOf(self.selection)
			message = STATUS_OTHER_FUNCTIONS
		else:
			liste = funcLoader.getModulesOf(self.selection)
			message = STATUS_OTHER_MODULES

		for element in liste:
			if(message.endswith(":")):
				message = message + " "
			else:
				message = message + ", "
			message = message + element

		self.statusBar.message('state', message)
		return

	def elementSelectionChanged(self, newElement):
		funcLoader = FunctionLoader.getInstance()
		if(self.mode == "FuncMod"):
			self.fonction = newElement
			liste = funcLoader.getModulesOf(newElement)
		else:
			self.module = newElement
			liste = funcLoader.getFunctionsOf(newElement)

		self.delete(0, END)
		for element in liste:
			self.insert(END, element)
			if(element == WRAP_OVERALL_MODULE or element == WRAP_AFTERALL_MODULE):
				loader = Options.getInstance()
				color = loader.get("warningColor")
				bgcolor = loader.bgget("warningColor")
				self.itemconfigure(END, foreground = color, background = bgcolor)

		return

	pass

class InterfaceMacro(Listbox):
	def __init__(self, parent, listBoxes):
		self.listBoxes = listBoxes

		# CREATION INTERFACE
		Listbox.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 24, height = 9)

		loader = Options.getInstance()
		color = loader.get("internalColor")

		label = Label(parent, text = LAB_LEFTPANEL_MACRO, anchor = "w", foreground = color)
		label.pack(side = TOP, fill = "x")

		scroll = Scrollbar(parent, command = self.yview)
		self.configure(yscrollcommand = scroll.set)
		scroll.pack(side = RIGHT, fill = "y")

		loader = MacroLoader.getInstance()
		macros = loader.getNames()

		for macro in macros:
			self.insert(END, macro)

		# BIND
		self.bind("<ButtonRelease-1>", self.selected)
		self.bind("<Double-Button-1>", self.doubleClic)
		return

	def miseAJour(self):
		self.delete(0, END)
		loader = MacroLoader.getInstance()
		macros = loader.getNames()

		for macro in macros:
			self.insert(END, macro)
		return

	def doubleClic(self, ev):
		loader = MacroLoader.getInstance()
		macro = loader.getMacro(self.selection)
		self.listBoxes.addMacro(macro)
		return

	def selected(self, ev):
		if(self.curselection() == ()):
			self.selection = ""
			return

		self.selection = self.get(self.curselection())
		return

	pass

class ListBoxes:
	def __init__(self, parent, statusBar, interfaceCMD):
		self.parent = parent
		self.statusBar = statusBar

		self.interfaceCMD = interfaceCMD

		self.listBoxUpFrame = Frame(self.parent)
		self.listBoxUpFrame.pack(side = TOP)
		self.listBoxDownFrame = Frame(self.parent)
		self.listBoxDownFrame.pack(side = TOP)
		self.listBoxMacroFrame = Frame(self.parent)
		self.listBoxMacroFrame.pack(side = TOP)

		loader = Options.getInstance()
		mode = loader.getOption("LISTBOXMODE")
		self.listBoxDown = InterfaceModule(self.listBoxDownFrame, self.statusBar, self, mode)
		self.listBoxDown.pack(side = TOP)
		self.listBoxUp = InterfaceFonction(self.listBoxUpFrame, self.listBoxDown, mode)
		self.listBoxUp.pack(side = TOP)

		self.listBoxMacro = InterfaceMacro(self.listBoxMacroFrame, self)
		self.listBoxMacro.pack(side = TOP)

		return

	def reloadLeftPanel(self):
		self.listBoxDown.vider()
		self.listBoxUp.vider()
		self.listBoxUp.destroy()
		self.listBoxDown.destroy()

		loader = Options.getInstance()
		mode = loader.getOption("LISTBOXMODE")
		self.listBoxDown = InterfaceModule(self.listBoxDownFrame, self.statusBar, self, mode)
		self.listBoxDown.pack(side = TOP)
		self.listBoxUp = InterfaceFonction(self.listBoxUpFrame, self.listBoxDown, mode)
		self.listBoxUp.pack(side = TOP)

		return

	def miseAJourMacros(self):
		self.listBoxMacro.miseAJour()
		return

	def addMacro(self, macro):
		moduleLoader = ModuleLoader.getInstance()
		functionLoader = FunctionLoader.getInstance()

		for line in macro.getLines():
			isIncrementer = False
			type = ""
			if(line.startswith("print")):
				type = "PRINT"
			elif(line.startswith("from")):
				type = "FROM"
			elif(line.startswith("import")):
				type = "IMPORT"
			elif(line.startswith("#")):
				type = "COM"
			elif(line.startswith("ENDLINE")):
				type = "ENDLINE"
			elif(line.startswith("ENDTAB")):
				type = "ENDTAB"
			elif(line.startswith("ENDFILE")):
				type = "ENDFILE"
			elif(line.startswith("for")):
				type = "FOR"
			elif(line.startswith(" ")):
				type = "PYTHON"
				isIncrementer = True
			else:
				try:
					module = line.split(".")[0].strip()
					fonction = line.split(".")[1].split("(")[0].strip()
					if(moduleLoader.hasModule(module) and functionLoader.hasFunction(module, fonction)):
						type = "WRAP"
					else:
						type = "PYTHON"
				except:
					type = "PYTHON"

			self.interfaceCMD.addCommand(line, type, isIncrementer)
		return

	def addCommand(self, commande):
		self.interfaceCMD.addCommand(commande)
		return
