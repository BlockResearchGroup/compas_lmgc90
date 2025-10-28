# -*- coding: iso-8859-1 -*-

from Tkinter import *
from localisation import *
from loader import Options
import Pmw

class ColorTab:
	def __init__(self, parent):
		##
		frame = Frame(parent, width = 500, height = 30)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		inframe = Frame(frame, width = 200, height = 30)
		inframe.pack_propagate(0)
		inframe.pack(side = LEFT)

		label = Label(inframe, text = LAB_OPTION_OPTION, anchor = "w")
		label.pack(side = LEFT)

		inframe = Frame(frame, width = 125, height = 30)
		inframe.pack_propagate(0)
		inframe.pack(side = LEFT)

		label = Label(inframe, text = LAB_OPTION_FONT, anchor = "w")
		label.pack(side = LEFT)

		inframe = Frame(frame, width = 125, height = 30)
		inframe.pack_propagate(0)
		inframe.pack(side = LEFT)

		label = Label(inframe, text = LAB_OPTION_BACKGROUND, anchor = "w")
		label.pack(side = LEFT)

		##
		loader = Options.getInstance()

		self.options = []
		listeCouleur = ["Black", "Brown", "VioletRed", "Darkblue", "Blue", "Lightblue", "Darkgreen", "Green", "Darkred", "Red", "Orange", "Yellow", "White", "Grey"]

		######################
		## Option 1
		color = loader.get("validColor")
		bgcolor = loader.bgget("validColor")
		opt = ComboLabel(parent, "validColor", OPT_CMD_VAL, listeCouleur, color, bgcolor)
		opt.pack(side = TOP)
		self.options.append(opt)

		## Option 2
		color = loader.get("errorColor")
		opt = ComboLabel(parent, "invalidColor", OPT_CMD_PARREQ, listeCouleur, color)
		opt.pack(side = TOP)
		self.options.append(opt)

		## Option 3
		color = loader.get("optionalColor")
		bgcolor = loader.bgget("optionalColor")
		opt = ComboLabel(parent, "optionalColor", OPT_CMD_PAROPT, listeCouleur, color, bgcolor)
		opt.pack(side = TOP)
		self.options.append(opt)

		## Option 4
		color = loader.get("internalColor")
		bgcolor = loader.bgget("internalColor")
		opt = ComboLabel(parent, "internalColor", OPT_CMD_INF, listeCouleur, color, bgcolor)
		opt.pack(side = TOP)
		self.options.append(opt)

		## Option 5
		color = loader.get("noexistColor")
		bgcolor = loader.bgget("noexistColor")
		opt = ComboLabel(parent, "noexistColor", OPT_CMD_NOEXIST, listeCouleur, color, bgcolor)
		opt.pack(side = TOP)
		self.options.append(opt)

		## Option 6
		color = loader.get("warningColor")
		bgcolor = loader.bgget("warningColor")
		opt = ComboLabel(parent, "warningColor", OPT_CMD_WARN, listeCouleur, color, bgcolor)
		opt.pack(side = TOP)

		## Option 7
		color = loader.get("ignoreColor")
		bgcolor = loader.bgget("ignoreColor")
		opt = ComboLabel(parent, "ignoreColor", OPT_CMD_IGNORE, listeCouleur, color, bgcolor)
		opt.pack(side = TOP)

		self.options.append(opt)

		return

	def validate(self):
		loader = Options.getInstance()
		for i in self.options:
			nom, value, bgvalue = i.getSelected()

			loader.set(nom, value)
			if(bgvalue != None):
				loader.bgset(nom, bgvalue)
		return

class EditionTab:
	def __init__(self, parent):
		loader = Options.getInstance()

		## Option 1 : langue
		self.languageList = getLanguages()

		frame = Frame(parent, width = 500, height = 30)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		fr = Frame(frame, width = 200, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		label = Label(fr, text = OPT_LANGUAGE)
		label.pack(side = LEFT)

		fr = Frame(frame, width = 150, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		self.langBox = Pmw.ComboBox(fr, scrolledlist_items = self.languageList.keys(), selectioncommand = self.langChanged)
		self.langBox.pack(side = LEFT)
		self.language = self.languageList[getCurrentLanguage()]
		self.langBox.selectitem(getCurrentLanguage())

		fr = Frame(frame, width = 150, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		## Option 2 : mode listbox
		loader = Options.getInstance()
		self.mode = loader.getOption("LISTBOXMODE")
		modeList = [OPT_MODE_FUNCMOD, OPT_MODE_MODFUNC]

		frame = Frame(parent, width = 500, height = 30)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		fr = Frame(frame, width = 200, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		label = Label(fr, text = OPT_MODE)
		label.pack(side = LEFT)

		fr = Frame(frame, width = 150, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		self.modeBox = Pmw.ComboBox(fr, scrolledlist_items = modeList, selectioncommand = self.modeChanged)
		self.modeBox.pack(side = LEFT)
		if(self.mode == "FuncMod"):
			self.modeBox.selectitem(OPT_MODE_FUNCMOD)
		else:
			self.modeBox.selectitem(OPT_MODE_MODFUNC)

		fr = Frame(frame, width = 150, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		## Option 3 : chemin navigateur web
		frame = Frame(parent, width = 500, height = 30)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		fr = Frame(frame, width = 200, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		label = Label(fr, text = OPT_EXPLORATEUR)
		label.pack(side = LEFT)

		fr = Frame(frame, width = 150, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		self.explorateurEntry = Entry(fr, width = 30)
		self.explorateurEntry.pack(side = LEFT)
		self.explorateurEntry.insert(END, loader.getOption("EXPLORATEUR"))

		fr = Frame(frame, width = 150, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		## Option 4 : chemin doc
		frame = Frame(parent, width = 500, height = 30)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		fr = Frame(frame, width = 200, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		label = Label(fr, text = OPT_DOC)
		label.pack(side = LEFT)

		fr = Frame(frame, width = 150, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		self.docEntry = Entry(fr, width = 30)
		self.docEntry.pack(side = LEFT)
		self.docEntry.insert(END, loader.getOption("DOC"))

		fr = Frame(frame, width = 150, height = 30)
		fr.pack_propagate(0)
		fr.pack(side = LEFT)

		return

	def langChanged(self, item):
		self.language = self.languageList[item]
		return

	def modeChanged(self, item):
		if(item == OPT_MODE_FUNCMOD):
			self.mode = "FuncMod"
		else:
			self.mode = "ModFunc"
		return

	def validate(self):
		loader = Options.getInstance()
		loader.setOption("LANGUAGE", self.language)
		loader.setOption("LISTBOXMODE", self.mode)
		loader.setOption("EXPLORATEUR", self.explorateurEntry.get())
		loader.setOption("DOC", self.docEntry.get())
		return

class ComboLabel(Frame):
	def __init__(self, parent, option, text, comboList, selected = "", selected2 = ""):
		Frame.__init__(self, parent, width = 500, height = 30)
		self.pack_propagate(0)
		self.option = option

		if(selected == ""):
			color = "Black"
		else:
			color = selected

		frame = Frame(self, width = 200, height = 30)
		frame.pack_propagate(0)
		frame.pack(side = LEFT)

		self.label = Label(frame, text = text, foreground = color)
		self.label.pack(side = LEFT)

		self.selected = selected
		self.selected2 = selected2
		self.comboBox2 = None

		frame = Frame(self, width = 125, height = 30)
		frame.pack_propagate(0)
		frame.pack(side = LEFT)

		self.backColor = frame.cget("background")

		self.comboBox = Pmw.ComboBox(frame, selectioncommand = self.selectionChanged, scrolledlist_items = comboList)
		self.comboBox.pack(side = LEFT)

		frame = Frame(self, width = 125, height = 30)
		frame.pack_propagate(0)
		frame.pack(side = LEFT)

		if(selected2 == ""):
			selected2 = COMBO_NO_COLOR

		self.comboBox2 = Pmw.ComboBox(frame, selectioncommand = self.selection2Changed, scrolledlist_items = [COMBO_NO_COLOR] + comboList)
		self.comboBox2.pack(side = LEFT)
		self.comboBox2.selectitem(selected2)

		if(selected2 != COMBO_NO_COLOR):
			self.label.configure(background = selected2)

		if(selected != ""):
			self.comboBox.selectitem(selected)

		return

	def selectionChanged(self, selected):
		self.selected = selected
		self.label.configure(foreground = selected)
		return

	def selection2Changed(self, selected):
		self.selected2 = selected
		if(selected == COMBO_NO_COLOR):
			self.label.configure(background = self.backColor)
		else:
			self.label.configure(background = selected)
		return

	def setSelected(self, selected):
		self.selected = selected
		self.comboBox.selectitem(self.selected)
		return

	def getSelected(self):
		if(self.comboBox2 == None):
			return self.option, self.selected, None
		else:
			if(self.selected2 == COMBO_NO_COLOR):
				self.selected2 = ""
			return self.option, self.selected, self.selected2


class OptionWindow(Toplevel):
	def __init__(self, parent):
		Toplevel.__init__(self, parent)
		self.transient(parent)
		self.resizable(width=NO,height=NO)

		self.title(LG_MENU_PREFERENCE)

		self.parent = parent

		self.grab_set()

		self.container = Frame(self, width = 500, height = 320)
		self.container.pack(side = TOP)
		self.container.pack_propagate(0)

		notebook = Pmw.NoteBook(self.container)
        	notebook.pack(fill = 'both', expand = 1, padx = 10, pady = 10)

		editionTab = notebook.add(NTB_PAGE_EDITION)
		colorTab = notebook.add(NTB_PAGE_COLOR)

		self.colorClass = ColorTab(colorTab)
		self.editionClass = EditionTab(editionTab)

		self.buttonFrame = Frame(self, width = 400, height = 50)
		self.buttonFrame.pack(side = TOP)

		self.okButton = Button(self.buttonFrame, text = BUT_OK, command = self.okClic)
		self.okButton.pack(side = LEFT)

		self.canButton = Button(self.buttonFrame, text = BUT_CANCEL, command = self.canClic)
		self.canButton.pack(side = LEFT)		

		return

	def okClic(self):
		self.colorClass.validate()
		self.editionClass.validate()

		self.parent.reloadLeftPanel()

		self.grab_release()
		self.destroy()
		return

	def canClic(self):
		self.grab_release()
		self.destroy()
		return
