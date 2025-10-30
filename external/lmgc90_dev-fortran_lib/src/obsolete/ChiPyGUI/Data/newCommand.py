# -*- coding: iso-8859-1 -*-

from Tkinter import *
import Pmw

from localisation import *
from loader import ModuleLoader, FunctionLoader, ArgumentsLoader

class Commentaire(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, width = 400, height = 300)
		self.pack_propagate(0)

		fr = Frame(self, width = 400, height = 100)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		lab = Label(self, text = LAB_TOP_COMMENTAIRE)
		lab.pack(side = TOP, anchor = "w")

		self.inputText = Entry(self, width = 50)
		self.inputText.pack(side = TOP)

		return

	def getString(self):
		return self.inputText.get()

	def getType(self):
		return "COM"

class Python(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, width = 400, height = 300)
		self.pack_propagate(0)

		fr = Frame(self, width = 400, height = 100)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		fr = Frame(self, width = 400, height = 150)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		frame = Frame(fr, width = 400, height = 40)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		lab = Label(frame, text = LAB_TOP_PYTHON)
		lab.pack(side = TOP, anchor = "w")

		self.inputText = Entry(frame, width = 50)
		self.inputText.pack(side = TOP)

		frame = Frame(fr, width = 400, height = 40)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		self.checkVar = IntVar()

		self.checkBox = Checkbutton(frame, text = CHK_LAB_INCREMENT, variable = self.checkVar)
		self.checkBox.pack(side = LEFT)

		return

	def getString(self):
		return self.checkVar, self.inputText.get()

	def getType(self):
		return "PYTHON"

class Import(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, width = 400, height = 300)
		self.pack_propagate(0)

		fr = Frame(self, width = 400, height = 100)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		lab = Label(self, text = LAB_TOP_IMPORT)
		lab.pack(side = TOP, anchor = "w")

		self.inputText = Entry(self, width = 50)
		self.inputText.pack(side = TOP)

		return

	def getString(self):
		return self.inputText.get()

	def getType(self):
		return "IMPORT"

class From(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, width = 400, height = 300)
		self.pack_propagate(0)

		fr = Frame(self, width = 400, height = 100)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		fr = Frame(self, width = 400, height = 50)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		lab = Label(fr, text = LAB_TOP_IMPORT)
		lab.pack(side = TOP, anchor = "w")

		self.inputModule = Entry(fr, width = 50)
		self.inputModule.pack(side = TOP)


		fr = Frame(self, width = 400, height = 50)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		lab = Label(fr, text = LAB_TOP_FROM)
		lab.pack(side = TOP, anchor = "w")

		self.inputFonctions = Entry(fr, width = 50)
		self.inputFonctions.pack(side = TOP)

		return

	def getString(self):
		return self.inputModule.get(), self.inputFonctions.get()

	def getType(self):
		return "FROM"

class Print(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, width = 400, height = 300)
		self.pack_propagate(0)

		fr = Frame(self, width = 400, height = 100)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		lab = Label(self, text = LAB_TOP_PRINT)
		lab.pack(side = TOP, anchor = "w")

		self.inputText = Entry(self, width = 50)
		self.inputText.pack(side = TOP)

		return

	def getString(self):
		return self.inputText.get()

	def getType(self):
		return "PRINT"

class EndLine(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, width = 600, height = 400)
		self.pack_propagate(0)
		return

	def getString(self):
		return "ENDLINE"

	def getType(self):
		return "ENDLINE"

class EndTab(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent)
		return

	def getString(self):
		return "ENDTAB"

	def getType(self):
		return "ENDTAB"

class ChiPyFunc(Frame):
	def __init__(self, parent):
		self.selectedModule = ""
		self.selectedFonction = ""
		self.reqEntry = []
		self.reqLabel = []
		self.optEntry = []
		self.optLabel = []

		Frame.__init__(self, parent, width = 400, height = 300)
		self.pack_propagate(0)

		frameTop = Frame(self, width = 400, height = 60)
		frameTop.pack_propagate(0)
		frameTop.pack(side = TOP)

		frameTopLeft = Frame(frameTop, width = 200, height = 60)
		frameTopLeft.pack_propagate(0)
		frameTopLeft.pack(side = LEFT)

		self.module = Pmw.ComboBox(frameTopLeft, label_text = LAB_TOP_MODULE, labelpos = 'nw', selectioncommand = self.moduleChanged)
		self.module.pack(side = 'left', anchor = 'n', fill = 'x', expand = 1, padx = 8, pady = 8)

		frameTopRight = Frame(frameTop, width = 200, height = 60)
		frameTopRight.pack_propagate(0)
		frameTopRight.pack(side = RIGHT)

		self.fonction = Pmw.ComboBox(frameTopRight, label_text = LAB_TOP_FONCTION, labelpos = 'nw', selectioncommand = self.fonctionChanged)
		self.fonction.pack(side = 'left', anchor = 'n', fill = 'x', expand = 1, padx = 8, pady = 8)

		## Creation du champs des arguments
		frameBottom = Frame(self, width = 400, height = 240)
		frameBottom.pack_propagate(0)
		frameBottom.pack(side = TOP)

		self.group = Pmw.Group(frameBottom, tag_text = LAB_TOP_ARGUMENTS)
		self.group.pack(fill = 'both', expand = 1, padx = 6, pady = 6)
		self.group.interior().pack_propagate(0)

		self.leftCol = Frame(self.group.interior(), width = 200, height = 220)
		self.leftCol.pack_propagate(0)
		self.leftCol.pack(side = LEFT)

		self.rightCol = Frame(self.group.interior(), width = 200, height = 220)
		self.rightCol.pack_propagate(0)
		self.rightCol.pack(side = RIGHT)

		label = Label(self.leftCol, text = LAB_TOP_REQUIRED)
		label.pack(side = TOP)

		label = Label(self.rightCol, text = LAB_TOP_OPTIONAL)
		label.pack(side = TOP)

		## Remplissage comboBox
		loader = ModuleLoader.getInstance()
		modules = loader.getModules()
		modules.sort()

		for module in modules:
			self.module.insert(END, module)

		if(len(modules) > 0):
			self.module.selectitem(modules[0])
			self.moduleChanged(modules[0])
		return

	def moduleChanged(self, item):
		self.selectedModule = item
		loader = FunctionLoader.getInstance()
		listeFonction = loader.getFunctionsOf(item)
		listeFonction.sort()

		self.fonction.clear()
		self.selectedFonction = ""
		for fonction in listeFonction:
			self.fonction.insert(END, fonction)

		if(len(listeFonction) > 0):
			self.fonction.selectitem(listeFonction[0])
			self.fonctionChanged(listeFonction[0])
		return

	def fonctionChanged(self, item):
		self.selectedFonction = item

		loader = ArgumentsLoader.getInstance()
		listeReq = loader.getRequiredArgsOf(self.selectedModule, self.selectedFonction)
		listeOpt = loader.getOptionalArgsOf(self.selectedModule, self.selectedFonction)

		for label in self.reqLabel:
			label.destroy()
		for entry in self.reqEntry:
			entry.destroy()
		for label in self.optLabel:
			label.destroy()
		for entry in self.optEntry:
			entry.destroy()

		self.reqLabel = []
		self.reqEntry = []
		self.optLabel = []
		self.optEntry = []

		for arg in listeReq:
			fr = Frame(self.leftCol, width = 200, height = 40)
			fr.pack_propagate(0)
			fr.pack(side = TOP, anchor = "w")

			label = Label(fr, text = arg, anchor = "w")
			label.pack(side = LEFT, padx = 10)
			self.reqLabel.append(label)
			entry = Entry(fr, width = 10)
			entry.pack(side = LEFT, padx = 10)
			self.reqEntry.append(entry)

		for arg in listeOpt:
			fr = Frame(self.rightCol, width = 200, height = 40)
			fr.pack_propagate(0)
			fr.pack(side = TOP, anchor = "w")

			label = Label(fr, text = arg, anchor = "w")
			label.pack(side = LEFT, padx = 10)
			self.optLabel.append(label)
			entry = Entry(fr, width = 10)
			entry.pack(side = LEFT, padx = 10)
			self.optEntry.append(entry)

		return

	def getString(self):
		opt = ""
		for numArg in range(len(self.reqLabel)):
			if(self.reqEntry[numArg].get() != ""):
				ligne = self.reqLabel[numArg].cget("text") + "=" + self.reqEntry[numArg].get()
				if(opt != ""):
					opt = opt + ", "
				opt = opt + ligne

		for numArg in range(len(self.optLabel)):
			if(self.optEntry[numArg].get() != ""):
				ligne = self.optLabel[numArg].cget("text") + "=" + self.optEntry[numArg].get()
				if(opt != ""):
					opt = opt + ", "
				opt = opt + ligne

		return self.selectedModule, self.selectedFonction, opt

	def getType(self):
		return "WRAP"

class BoucleFor(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, width = 400, height = 300)
		self.pack_propagate(0)

		fr = Frame(self, width = 400, height = 100)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		fr = Frame(self, width = 250, height = 25)
		fr.pack_propagate(0)
		fr.pack(side = TOP)
		lab = Label(fr, text = LAB_TOP_VARIABLE, anchor = "w")
		lab.pack(side = LEFT)
		self.varEntry = Entry(fr, width = 20)
		self.varEntry.pack(side = RIGHT)

		fr = Frame(self, width = 250, height = 25)
		fr.pack_propagate(0)
		fr.pack(side = TOP)
		lab = Label(fr, text = LAB_TOP_MINRANGE, anchor = "w")
		lab.pack(side = LEFT)
		self.minEntry = Entry(fr, width = 20)
		self.minEntry.pack(side = RIGHT)

		fr = Frame(self, width = 250, height = 25)
		fr.pack_propagate(0)
		fr.pack(side = TOP)
		lab = Label(fr, text = LAB_TOP_MAXRANGE, anchor = "w")
		lab.pack(side = LEFT)
		self.maxEntry = Entry(fr, width = 20)
		self.maxEntry.pack(side = RIGHT)

		fr = Frame(self, width = 250, height = 25)
		fr.pack_propagate(0)
		fr.pack(side = TOP)
		lab = Label(fr, text = LAB_TOP_STEP, anchor = "w")
		lab.pack(side = LEFT)
		self.stepEntry = Entry(fr, width = 20)
		self.stepEntry.pack(side = RIGHT)

		fr = Frame(self, width = 300, height = 100)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		return

	def getString(self):
		return self.varEntry.get(), self.minEntry.get(), self.maxEntry.get(), self.stepEntry.get()

	def getType(self):
		return "FOR"

class ListBox(Listbox):
	def __init__(self, parent, interfaceTopLevel):
		Listbox.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 20, height = 20)

		self.TopLevel = interfaceTopLevel
		self.insert(END, LISTBOX_COMMAND_COM)
		self.insert(END, LISTBOX_COMMAND_IMPORT)
		self.insert(END, LISTBOX_COMMAND_FROM)
		self.insert(END, LISTBOX_COMMAND_FOR)
		self.insert(END, LISTBOX_COMMAND_PYTHON)
		self.insert(END, LISTBOX_COMMAND_NONE)
		#self.insert(END, LISTBOX_COMMAND_PRINT)
		self.insert(END, LISTBOX_COMMAND_WRAP)
		self.insert(END, LISTBOX_COMMAND_NONE)
		self.insert(END, LISTBOX_COMMAND_ENDTAB)
		self.insert(END, LISTBOX_COMMAND_ENDLINE)

		self.bind("<ButtonRelease-1>", self.selected)
		return

	def selected(self, ev):
		if(self.curselection() == ()):
			return

		text = self.get(self.curselection())

		self.TopLevel.deleteOldFrame()
		if(text == LISTBOX_COMMAND_NONE):
			self.TopLevel.newFrame(type = "NONE")
		if(text == LISTBOX_COMMAND_COM):
			self.TopLevel.newFrame(type = "COM")
		if(text == LISTBOX_COMMAND_FROM):
			self.TopLevel.newFrame(type = "FROM")
		if(text == LISTBOX_COMMAND_IMPORT):
			self.TopLevel.newFrame(type = "IMPORT")
		if(text == LISTBOX_COMMAND_PYTHON):
			self.TopLevel.newFrame(type = "PYTHON")
		if(text == LISTBOX_COMMAND_PRINT):
			self.TopLevel.newFrame(type = "PRINT")
		if(text == LISTBOX_COMMAND_WRAP):
			self.TopLevel.newFrame(type = "WRAP")
		if(text == LISTBOX_COMMAND_FOR):
			self.TopLevel.newFrame(type = "FOR")
		if(text == LISTBOX_COMMAND_ENDTAB):
			self.TopLevel.newFrame(type = "ENDTAB")
		if(text == LISTBOX_COMMAND_ENDLINE):
			self.TopLevel.newFrame(type = "ENDLINE")
		return


class NewCommand(Toplevel):
	def __init__(self, parent):
		Toplevel.__init__(self, parent)
		self.transient(parent)
		self.resizable(width=NO,height=NO)

		self.theFrame = None
		self.cancel = False

		self.grab_set()

		self.frame1 = Frame(self)
		self.frame1.pack(side = LEFT)

		self.frame2 = Frame(self)
		self.frame2.pack(side = RIGHT)

		self.listBox = ListBox(self.frame1, self)
		self.listBox.pack(side = TOP, fill = "y", expand = 1)

		self.container = Frame(self.frame2, width = 400, height = 300)
		self.container.pack(side = TOP)
		self.container.pack_propagate(0)

		self.buttonFrame = Frame(self.frame2)
		self.buttonFrame.pack(side = BOTTOM)

		self.okButton = Button(self.buttonFrame, text = BUT_OK, command = self.okClic)
		self.okButton.pack(side = LEFT)

		self.canButton = Button(self.buttonFrame, text = BUT_CANCEL, command = self.canClic)
		self.canButton.pack(side = LEFT)		

		self.listBox.select_set(END)
		self.listBox.selected(self)

		return

	def deleteOldFrame(self):
		if(self.theFrame != None):
			self.theFrame.destroy()
			self.theFrame = None
		return

	def newFrame(self, type = ""):
		if(type == "COM"):
			self.theFrame = Commentaire(self.container)
		elif(type == "PYTHON"):
			self.theFrame = Python(self.container)
		elif(type == "FROM"):
			self.theFrame = From(self.container)
		elif(type == "IMPORT"):
			self.theFrame = Import(self.container)
		elif(type == "PRINT"):
			self.theFrame = Print(self.container)
		elif(type == "WRAP"):
			self.theFrame = ChiPyFunc(self.container)
		elif(type == "FOR"):
			self.theFrame = BoucleFor(self.container)
		elif(type == "ENDLINE"):
			self.theFrame = EndLine(self.container)
		elif(type == "ENDTAB"):
			self.theFrame = EndTab(self.container)
		elif(type == "NONE"):
			self.theFrame = None
			return
		else:
			self.theFrame = None
			return

		self.theFrame.pack()
		return

	def canClic(self):
		self.cancel = True
		self.result = False
		self.type = False
		self.grab_release()
		self.destroy()
		return

	def okClic(self):
		if(self.theFrame == None):
			self.cancel = True
			self.result = False
			self.type = False
		else:
			self.result = self.theFrame.getString()
			self.type = self.theFrame.getType()
		self.grab_release()
		self.destroy()
		return

	def getString(self):
		if(self.cancel == False):
			return self.result
		else:
			return False

	def getResult(self):
		if(self.cancel == False):
			return self.type
		else:
			return False

