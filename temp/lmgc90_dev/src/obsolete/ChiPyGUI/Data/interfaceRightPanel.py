# -*- coding: iso-8859-1 -*-

from Tkinter import *
from localisation import *
from loader import ArgumentsLoader, Options, DOCReader
import Pmw
import threading

####################
class FROMModifier:
	def __init__(self, parent, commande, ParamGroup):
		self.ParamGroup = ParamGroup
		self.commande = commande
		self.module = commande.get("module")
		self.text = commande.get("text")

		loader = Options.getInstance()
		color = loader.get("internalColor")

		##
		fr = Frame(parent, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_RIGHT_FROMTITLE, anchor = "w")
		label.pack()

		##
		fr = Frame(parent, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_RIGHT_FROMMODULE, anchor = "w", foreground = color)
		label.pack(side = LEFT)

		##
		fr = Frame(parent, width = 350, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		self.moduleEntry = Entry(fr, width = 30)
		self.moduleEntry.pack(side = TOP)
		self.moduleEntry.insert(END, self.module)

		##
		fr = Frame(parent, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		##
		fr = Frame(parent, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_RIGHT_IMPORTMODULE, anchor = "w", foreground = color)
		label.pack(side = LEFT)

		##
		fr = Frame(parent, width = 350, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		self.textEntry = Entry(fr, width = 30)
		self.textEntry.pack(side = TOP)
		self.textEntry.insert(END, self.text)

		self.textEntry.bind("<FocusOut>", self.validate)
		self.moduleEntry.bind("<FocusOut>", self.validate)

		return

	def save(self):
		self.commande.set("text", self.text)
		self.commande.set("module", self.module)
		self.ParamGroup.miseAJour()
		return

	def validate(self, ev):
		hasChanged = False

		if(ev.widget == self.textEntry):
			newText = self.textEntry.get()
			try:
				int(newText)
			except:
				if(newText != self.text):
					self.text = newText
					hasChanged = True
			else:
				self.textEntry.delete(0, END)
				self.textEntry.insert(END, self.text)

		if(ev.widget == self.moduleEntry):
			newModule = self.moduleEntry.get()
			try:
				int(newModule)
			except:
				if(newModule != self.module):
					self.module = newModule
					hasChanged = True
			else:
				self.moduleEntry.delete(0, END)
				self.moduleEntry.insert(END, self.module)

		if(hasChanged):
			self.save()
		return

	def validateAll(self):
		hasChanged = False

		newModule = self.moduleEntry.get()
		try:
			int(newModule)
		except:
			if(newModule != self.module):
				self.module = newModule
				hasChanged = True
		else:
			pass

		newText = self.textEntry.get()
		try:
			int(newText)
		except:
			if(newText != self.text):
				self.text = newText
				hasChanged = True
		else:
			pass

		return hasChanged

	def destroy(self):
		hasChanged = self.validateAll()
		if(hasChanged):
			self.save()
		return

####################
class PYTHONModifier:
	def __init__(self, parent, commande, ParamGroup):
		self.ParamGroup = ParamGroup
		self.commande = commande
		self.text = commande.get("text")
		self.isIncrementer = commande.get("isAnIncrementer")

		loader = Options.getInstance()
		color = loader.get("internalColor")

		frame = Frame(parent, width = 250, height = 50)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		##
		fr = Frame(frame, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_RIGHT_PYTHON, anchor = "w", foreground = color)
		label.pack(side = LEFT)

		##
		fr = Frame(frame, width = 350, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		self.textEntry = Entry(fr, width = 35)
		self.textEntry.pack(side = TOP)
		self.textEntry.insert(END, self.text)

		frame = Frame(parent, width = 250, height = 50)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		self.checkVar = IntVar()
		self.check = Checkbutton(frame, text = CHK_LAB_INCREMENT, variable = self.checkVar, command = self.checkChanged)
		self.check.pack(side = TOP)

		if(self.isIncrementer):
			self.checkVar.set(1)
		else:
			self.checkVar.set(0)

		self.textEntry.bind("<FocusOut>", self.validate)

		return

	def checkChanged(self):
		if(self.checkVar.get() == 0):
			self.isIncrementer = False
		else:
			self.isIncrementer = True

		self.commande.set("isAnIncrementer", self.isIncrementer)
		self.ParamGroup.miseAJour(complete = True)
		return

	def save(self):
		self.commande.set("text", self.text)
		self.ParamGroup.miseAJour()
		return

	def validate(self, ev):
		text = self.textEntry.get()
		if(text != self.text):
			self.text = self.textEntry.get()
			self.save()

		return

	def validateAll(self):
		hasChanged = False
		text = self.textEntry.get()

		if(text != self.text):
			self.text = self.textEntry.get()
			hasChanged = True

		return hasChanged

	def destroy(self):
		hasChanged = self.validateAll()
		if(hasChanged):
			self.save()
		return

####################
class IMPORTModifier:
	def __init__(self, parent, commande, ParamGroup):
		self.ParamGroup = ParamGroup
		self.commande = commande
		self.module = commande.get("module")

		loader = Options.getInstance()
		color = loader.get("internalColor")

		##
		fr = Frame(parent, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_RIGHT_IMPORTTITLE, anchor = "w")
		label.pack()

		##
		fr = Frame(parent, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_RIGHT_IMPORTMODULE, anchor = "w", foreground = color)
		label.pack(side = LEFT)

		##
		fr = Frame(parent, width = 350, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		self.moduleEntry = Entry(fr, width = 30)
		self.moduleEntry.pack(side = TOP)
		self.moduleEntry.insert(END, self.module)

		self.moduleEntry.bind("<FocusOut>", self.validate)

		return

	def save(self):
		self.commande.set("module", self.module)
		self.ParamGroup.miseAJour()
		return

	def validate(self, ev):
		module = self.moduleEntry.get()
		try:
			int(module)
		except:
			if(module != self.module):
				self.module = self.moduleEntry.get()
				self.save()
		else:
			self.moduleEntry.delete(0, END)
			self.moduleEntry.insert(END, self.module)
		return

	def validateAll(self):
		hasChanged = False
		module = self.moduleEntry.get()
		try:
			int(module)
		except:
			if(module != self.module):
				self.module = self.moduleEntry.get()
				hasChanged = True
		else:
			self.moduleEntry.delete(0, END)
			self.moduleEntry.insert(END, self.module)

		return hasChanged

	def destroy(self):
		hasChanged = self.validateAll()
		if(hasChanged):
			self.save()
		return

####################
class PRINTModifier:
	def __init__(self, parent, commande, ParamGroup):
		self.ParamGroup = ParamGroup
		self.commande = commande
		self.text = commande.get("text")

		loader = Options.getInstance()
		color = loader.get("internalColor")

		##
		fr = Frame(parent, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_RIGHT_PRINT, anchor = "w", foreground = color)
		label.pack(side = LEFT)

		##
		fr = Frame(parent, width = 350, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		self.textEntry = Entry(fr, width = 35)
		self.textEntry.pack(side = TOP)
		self.textEntry.insert(END, self.text)

		self.textEntry.bind("<FocusOut>", self.validate)

		return

	def save(self):
		self.commande.set("text", self.text)
		self.ParamGroup.miseAJour()
		return

	def validate(self, ev):
		text = self.textEntry.get()
		if(text != self.text):
			self.text = self.textEntry.get()
			self.save()
		return

	def validateAll(self):
		hasChanged = False
		text = self.textEntry.get()
		if(text != self.text):
			self.text = self.textEntry.get()
			hasChanged = True
		return hasChanged

	def destroy(self):
		hasChanged = self.validateAll()
		if(hasChanged):
			self.save()
		return

####################

class WRAPModifier:
	def __init__(self, parent, commande, ParamGroup):
		self.ParamGroup = ParamGroup
		self.commande = commande
		self.parent = parent

		loader = Options.getInstance()
		color = loader.get("internalColor")

		self.module = commande.get("module")
		self.fonction = commande.get("fonction")

		loader = ArgumentsLoader.getInstance()
		self.listeReq = loader.getRequiredArgsOf(self.module, self.fonction)
		self.listeOpt = loader.getOptionalArgsOf(self.module, self.fonction)

		self.reqArgs = commande.getRequiredArgs()
		self.optArgs = commande.getOptionalArgs()

		requiredGroupHeight = len(self.listeReq) * 30 + 50
		optionalGroupHeight = len(self.listeOpt) * 30 + 50

		if(requiredGroupHeight == 50):
			requiredGroupHeight = 70
		if(optionalGroupHeight == 50):
			optionalGroupHeight = 70

		frame = Frame(self.parent, width = 350, height = 25)
		frame.pack_propagate(0)
		frame.pack(side = TOP)
		label = Label(frame, text = LAB_RIGHT_MODULE, anchor = "w", foreground = color)
		label.pack(side = LEFT)
		label = Label(frame, text = self.module, anchor = "w")
		label.pack(side = LEFT)

		frame = Frame(self.parent, width = 350, height = 25)
		frame.pack_propagate(0)
		frame.pack(side = TOP)
		label = Label(frame, text = LAB_RIGHT_FONCTION, anchor = "w", foreground = color)
		label.pack(side = LEFT)
		label = Label(frame, text = self.fonction, anchor = "w")
		label.pack(side = LEFT)

		frame = Frame(self.parent, width = 350, height = requiredGroupHeight)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		self.requiredGroup = Pmw.Group(frame, tag_text = LAB_RIGHT_REQUIRED)
		self.requiredGroup.pack(fill = "both", expand = 1, padx = 6, pady = 6)
		self.requiredGroup.interior().pack_propagate(0)

		frame = Frame(self.parent, width = 350, height = optionalGroupHeight)
		frame.pack_propagate(0)
		frame.pack(side = TOP)

		self.optionalGroup = Pmw.Group(frame, tag_text = LAB_RIGHT_OPTIONAL)
		self.optionalGroup.pack(fill = "both", expand = 1, padx = 6, pady = 6)
		self.optionalGroup.interior().pack_propagate(0)

		if(len(self.listeReq) == 0):
			label = Label(self.requiredGroup.interior(), text = LAB_RIGHT_NOREQARG)
			label.pack(pady = 5)
		if(len(self.listeOpt) == 0):
			label = Label(self.optionalGroup.interior(), text = LAB_RIGHT_NOOPTARG)
			label.pack(pady = 5)

		## Creation des champs de chaque parametre
		self.entriesDictionnary = {}
		self.argsDictionnary = {}
		self.reverseDictionnary = {}
		for arg in self.listeReq:
			fr = Frame(self.requiredGroup.interior(), width = 350, height = 30)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			label = Label(fr, text = arg, anchor = "w")
			label.pack(side = LEFT, padx = 10)
			entry = Entry(fr, width = 30)
			entry.pack(side = RIGHT, padx = 10)
			entry.bind("<FocusOut>", self.validate)

			self.argsDictionnary[arg] = ""
			self.entriesDictionnary[entry] = arg
			self.reverseDictionnary[arg] = entry

		for arg in self.listeOpt:
			fr = Frame(self.optionalGroup.interior(), width = 350, height = 30)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			label = Label(fr, text = arg, anchor = "w")
			label.pack(side = LEFT, padx = 10)
			entry = Entry(fr, width = 30)
			entry.pack(side = RIGHT, padx = 10)
			entry.bind("<FocusOut>", self.validate)

			self.argsDictionnary[arg] = ""
			self.entriesDictionnary[entry] = arg
			self.reverseDictionnary[arg] = entry

		## On rempli les champs
		for opt in self.reqArgs:
			self.argsDictionnary[opt.getName()] = opt.getValue()
			entry = self.reverseDictionnary[opt.getName()]
			entry.insert(END, opt.getValue())

		for opt in self.optArgs:
			self.argsDictionnary[opt.getName()] = opt.getValue()
			entry = self.reverseDictionnary[opt.getName()]
			entry.insert(END, opt.getValue())
		return

	def save(self):
		for opt in self.reqArgs:
			opt.setValue(self.argsDictionnary[opt.getName()])

		for opt in self.optArgs:
			opt.setValue(self.argsDictionnary[opt.getName()])
		self.ParamGroup.miseAJour()
		return

	def validate(self, ev):
		arg = self.entriesDictionnary[ev.widget]
		newVal = ev.widget.get()
		if(newVal != self.argsDictionnary[arg]):
			self.argsDictionnary[arg] = newVal
			self.save()
		return

	def validateAll(self):
		hasChanged = False
		entries = self.entriesDictionnary.keys()
		for entry in entries:
			arg = self.entriesDictionnary[entry]
			oldValue = self.argsDictionnary[arg]
			newValue = entry.get()
			if(oldValue != newValue):
				hasChanged = True
				self.argsDictionnary[arg] = newValue
		return hasChanged

	def destroy(self):
		hasChanged = self.validateAll()
		if(hasChanged):
			self.save()
		return

####################
class COMModifier:
	def __init__(self, parent, commande, ParamGroup):
		self.ParamGroup = ParamGroup
		self.commande = commande
		self.text = commande.get("text")

		loader = Options.getInstance()
		color = loader.get("internalColor")

		##
		fr = Frame(parent, width = 250, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_TOP_COMMENTAIRE, anchor = "w", foreground = color)
		label.pack(side = LEFT)

		##
		fr = Frame(parent, width = 350, height = 20)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		self.textEntry = Entry(fr, width = 35)
		self.textEntry.pack(side = TOP)
		self.textEntry.insert(END, self.text)

		self.textEntry.bind("<FocusOut>", self.validate)

		return

	def save(self):
		self.commande.set("text", self.text)
		self.ParamGroup.miseAJour()
		return

	def validateAll(self):
		hasChanged = False
		text = self.textEntry.get()
		if(text != self.text):
			self.text = self.textEntry.get()
			hasChanged = True
		return hasChanged

	def validate(self, ev):
		text = self.textEntry.get()
		if(text != self.text):
			self.text = self.textEntry.get()
			self.save()
		return

	def destroy(self):
		hasChanged = self.validateAll()
		if(hasChanged):
			self.save()
		return

####################
class FORModifier:
	def __init__(self, parent, commande, ParamGroup):
		self.ParamGroup = ParamGroup
		self.minRange = commande.get("minRange")
		self.maxRange = commande.get("maxRange")
		self.step = commande.get("step")
		self.variable = commande.get("variable")
		self.commande = commande


		##
		fr = Frame(parent, width = 250, height = 25)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		loader = Options.getInstance()
		color = loader.get("internalColor")

		label = Label(fr, text = LAB_TOP_BOUCLEFOR, foreground = color)
		label.pack(side = LEFT)

		##
		fr = Frame(parent, width = 250, height = 50)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_TOP_VARIABLE, anchor = "w", foreground = color)
		label.pack(side = LEFT)
		self.varEntry = Entry(fr, width = 20)
		self.varEntry.pack(side = RIGHT)
		self.varEntry.insert(END, self.variable)

		##
		fr = Frame(parent, width = 250, height = 50)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_TOP_MINRANGE, anchor = "w", foreground = color)
		label.pack(side = LEFT)
		self.minEntry = Entry(fr, width = 20)
		self.minEntry.pack(side = RIGHT)
		self.minEntry.insert(END, self.minRange)

		##
		fr = Frame(parent, width = 250, height = 50)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_TOP_MAXRANGE, anchor = "w", foreground = color)
		label.pack(side = LEFT)
		self.maxEntry = Entry(fr, width = 20)
		self.maxEntry.pack(side = RIGHT)
		self.maxEntry.insert(END, self.maxRange)

		##
		fr = Frame(parent, width = 250, height = 50)
		fr.pack_propagate(0)
		fr.pack(side = TOP)

		label = Label(fr, text = LAB_TOP_STEP, anchor = "w", foreground = color)
		label.pack(side = LEFT)
		self.stepEntry = Entry(fr, width = 20)
		self.stepEntry.pack(side = RIGHT)
		self.stepEntry.insert(END, self.step)

		self.stepEntry.bind("<FocusOut>", self.validate)
		self.maxEntry.bind("<FocusOut>", self.validate)
		self.minEntry.bind("<FocusOut>", self.validate)
		self.varEntry.bind("<FocusOut>", self.validate)

		return

	def save(self):
		self.commande.set("step", self.step)
		self.commande.set("maxRange", self.maxRange)
		self.commande.set("minRange", self.minRange)
		self.commande.set("variable", self.variable)

		self.ParamGroup.miseAJour()
		return

	def validateAll(self):
		hasChanged = False

		## step
		newStep = self.stepEntry.get()
		try:
			int(newStep)
		except:
			pass
		else:
			if(int(newStep) != self.step):
				self.step = int(newStep)
				hasChanged = True

		## max
		newMax = self.maxEntry.get()
		try:
			int(newMax)
		except:
			pass
		else:
			if(int(newMax) != self.maxRange):
				self.maxRange = int(newMax)
				hasChanged = True

		## min
		newMin = self.minEntry.get()
		try:
			int(newMin)
		except:
			pass
		else:
			if(int(newMin) != self.minRange):
				self.minRange = int(newMin)
				hasChanged = True

		## var
		newVar = self.varEntry.get()
		if(newVar.count(" ") > 0):
			pass
		else:
			try:
				int(newVar)
			except:
				if(newVar != self.variable):
					self.variable = newVar
					hasChanged = True
			else:
				self.varEntry.delete(0, END)
				self.varEntry.insert(END, self.variable)

		return hasChanged

	def validate(self, ev):
		hasChanged = False

		if(ev.widget == self.stepEntry):
			newStep = self.stepEntry.get()
			try:
				int(newStep)
			except:
				self.stepEntry.delete(0, END)
				self.stepEntry.insert(END, self.step)
			else:
				if(int(newStep) != self.step):
					self.step = int(newStep)
					hasChanged = True

		if(ev.widget == self.maxEntry):
			newMax = self.maxEntry.get()
			try:
				int(newMax)
			except:
				self.maxEntry.delete(0, END)
				self.maxEntry.insert(END, self.maxRange)
			else:
				if(int(newMax) != self.maxRange):
					self.maxRange = int(newMax)
					hasChanged = True

		if(ev.widget == self.minEntry):
			newMin = self.minEntry.get()
			try:
				int(newMin)
			except:
				self.minEntry.delete(0, END)
				self.minEntry.insert(END, self.minRange)
			else:
				if(int(newMin) != self.minRange):
					self.minRange = int(newMin)
					hasChanged = True

		if(ev.widget == self.varEntry):
			newVar = self.varEntry.get()
			if(newVar.count(" ") > 0):
				self.varEntry.delete(0, END)
				self.varEntry.insert(END, self.variable)
			else:
				try:
					int(newVar)
				except:
					if(newVar != self.variable):
						self.variable = newVar
						hasChanged = True
				else:
					self.varEntry.delete(0, END)
					self.varEntry.insert(END, self.variable)

		if(hasChanged):
			self.save()
		return

	def destroy(self):
		hasChanged = self.validateAll()

		if(hasChanged):
			self.save()
		return

####################
class ParamGroup:
	def __init__(self, parent, interface):
		self.interface = interface
		self.parent = parent
		self.enCours = None

		self.group = Pmw.Group(self.parent, tag_text = LAB_RIGHT_PARAM)
		self.group.pack(fill = 'both', expand = 1, padx = 6, pady = 6)
		self.group.interior().pack_propagate(0)

		self.interior = Frame(self.group.interior())
		self.interior.pack(fill = 'both', expand = 1)
		self.interior.pack_propagate(0)

		return

	def validate(self):
		if(self.enCours != None):
			hasChanged = self.enCours.validateAll()
			if(hasChanged):
				self.enCours.save()
		return

	def miseAJour(self, complete = False):
		self.interface.miseAJour(complete = complete)
		return

	def loadInterior(self, commande):
		if(self.enCours != None):
			self.enCours.destroy()
		self.enCours = None
		self.interior.destroy()
		self.interior = Frame(self.group.interior())
		self.interior.pack(fill = 'both', expand = 1)
		self.interior.pack_propagate(0)

		if(commande.getType() == "ENDLINE"):
			fr = Frame(self.interior, height = 100)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			label = Label(self.interior, text = LAB_RIGHT_ENDLINE)
			label.pack(side = TOP)
			self.enCours = None

		if(commande.getType() == "ENDTAB"):
			fr = Frame(self.interior, height = 100)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			label = Label(self.interior, text = LAB_RIGHT_ENDTAB)
			label.pack(side = TOP)
			self.enCours = None

		if(commande.getType() == "FOR"):
			fr = Frame(self.interior, height = 100)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 250)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			self.enCours = FORModifier(fr, commande, self)

			fr = Frame(self.interior, height = 160)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 50)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			button = Button(fr, text = BUT_RIGHT_VALIDATE, command = self.validate)
			button.pack()

		if(commande.getType() == "COM"):
			fr = Frame(self.interior, height = 100)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 250)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			self.enCours = COMModifier(fr, commande, self)

			fr = Frame(self.interior, height = 160)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 50)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			button = Button(fr, text = BUT_RIGHT_VALIDATE, command = self.validate)
			button.pack()

		if(commande.getType() == "PYTHON"):
			fr = Frame(self.interior, height = 100)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 250)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			self.enCours = PYTHONModifier(fr, commande, self)

			fr = Frame(self.interior, height = 160)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 50)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			button = Button(fr, text = BUT_RIGHT_VALIDATE, command = self.validate)
			button.pack()

		if(commande.getType() == "IMPORT"):
			fr = Frame(self.interior, height = 100)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 250)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			self.enCours = IMPORTModifier(fr, commande, self)

			fr = Frame(self.interior, height = 160)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 50)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			button = Button(fr, text = BUT_RIGHT_VALIDATE, command = self.validate)
			button.pack()

		if(commande.getType() == "FROM"):
			fr = Frame(self.interior, height = 100)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 250)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			self.enCours = FROMModifier(fr, commande, self)

			fr = Frame(self.interior, height = 160)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 50)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			button = Button(fr, text = BUT_RIGHT_VALIDATE, command = self.validate)
			button.pack()

		if(commande.getType() == "PRINT"):
			fr = Frame(self.interior, height = 100)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 250)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			self.enCours = PRINTModifier(fr, commande, self)

			fr = Frame(self.interior, height = 160)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 50)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			button = Button(fr, text = BUT_RIGHT_VALIDATE, command = self.validate)
			button.pack()

		if(commande.getType() == "WRAP"):
			#fr = Frame(self.interior, height = 30)
			#fr.pack_propagate(0)
			#fr.pack(side = TOP)

			fr = Frame(self.interior, width = 350, height = 510)
			fr.pack_propagate(0)
			fr.pack(side = TOP)

			self.enCours = WRAPModifier2(fr, commande, self)

			fr = Frame(self.interior, width = 350, height = 50)
			fr.pack_propagate(0)
			fr.pack(side = TOP)
			button = Button(fr, text = BUT_RIGHT_VALIDATE, command = self.validate)
			button.pack()

		return


class WRAPModifier2:
	def __init__(self, parent, commande, ParamGroup):
		self.ParamGroup = ParamGroup
		self.commande = commande
		self.parent = parent
		self.argsDictionnary = {}
		self.entry = None

		loader = Options.getInstance()
		color = loader.get("internalColor")

		self.module = commande.get("module")
		self.fonction = commande.get("fonction")

		loader = ArgumentsLoader.getInstance()
		self.listeReq = loader.getRequiredArgsOf(self.module, self.fonction)
		self.listeOpt = loader.getOptionalArgsOf(self.module, self.fonction)

		self.reqArgs = commande.getRequiredArgs()
		self.optArgs = commande.getOptionalArgs()

		frame = Frame(self.parent, width = 350, height = 25)
		frame.pack_propagate(0)
		frame.pack(side = TOP)
		label = Label(frame, text = LAB_RIGHT_MODULE, anchor = "w", foreground = color)
		label.pack(side = LEFT)
		label = Label(frame, text = self.module, anchor = "w")
		label.pack(side = LEFT)

		frame = Frame(self.parent, width = 350, height = 25)
		frame.pack_propagate(0)
		frame.pack(side = TOP)
		label = Label(frame, text = LAB_RIGHT_FONCTION, anchor = "w", foreground = color)
		label.pack(side = LEFT)
		label = Label(frame, text = self.fonction, anchor = "w")
		label.pack(side = LEFT)


		### GROUPES PARAMETRES

		frame = Frame(self.parent, width = 350, height = 160)
		frame.pack(side = TOP)
		frame.pack_propagate(0)

		paramGroup = Pmw.Group(frame, tag_text = LAB_TOP_ARGUMENTS)
		paramGroup.pack(side = TOP, fill = "both", expand = 1, padx = 6, pady = 6)
		paramGroup.interior().pack_propagate(0)
		self.paramGroup = paramGroup.interior()

		frame = Frame(self.parent, width = 350, height = 300)
		frame.pack(side = TOP)
		frame.pack_propagate(0)

		#docGroup = Pmw.Group(frame, tag_text = LAB_RIGHT_DOC)
		#docGroup.pack(side = TOP, fill = "both", expand = 1, padx = 6, pady = 6)
		#docGroup.interior().pack_propagate(0)
		#self.docGroup = docGroup.interior()
		self.docGroup = frame

		fr = Frame(self.paramGroup, height = 60, width = 350)
		fr.pack(side = TOP)
		fr.pack_propagate(0)

		fr1 = Frame(fr, height = 30, width = 350)
		fr1.pack(side = TOP)
		fr1.pack_propagate(0)

		label = Label(fr1, text = LAB_RIGHT_ARGUMENT)
		label.pack(side = LEFT)

		fr1 = Frame(fr, height = 30, width = 200)
		fr1.pack(side = TOP)
		fr1.pack_propagate(0)

		comboList = [COMBO_SELECT, COMBO_REQUIRED] + self.listeReq + [COMBO_OPTIONAL] + self.listeOpt

		self.comboBox = Pmw.ComboBox(fr1, selectioncommand = self.selectionChanged, scrolledlist_items = comboList, history = 0)
		self.comboBox.pack(side = TOP, fill = "x")
		self.comboBox.selectitem(comboList[0])

		self.argFrame = None

		self.docFrame = Frame(self.docGroup, height = 260, width = 350)
		self.docFrame.pack_propagate(0)
		self.docFrame.pack(side = TOP)

		button = Button(self.docFrame, text = LAB_RIGHT_DOC, command = self.searchDoc)
		button.pack()

		## On rempli les champs
		for opt in self.reqArgs:
			self.argsDictionnary[opt.getName()] = opt.getValue()

		for opt in self.optArgs:
			self.argsDictionnary[opt.getName()] = opt.getValue()
		return

	def selectionChanged(self, item):
		if(self.argFrame != None):
			self.validate()
			self.entry.destroy()
			self.argFrame.destroy()

		if(item == COMBO_REQUIRED or item == COMBO_OPTIONAL or item == COMBO_SELECT):
			self.argFrame = None
			self.docFrame = None
			self.entry = None
			return

		self.argFrame = Frame(self.paramGroup, height = 60, width = 350)
		self.argFrame.pack_propagate(0)
		self.argFrame.pack(side = TOP)

		if(item in self.listeReq):
			liste = self.reqArgs

		if(item in self.listeOpt):
			liste = self.optArgs

		for opt in liste:
			if(opt.getName() == item):
				self.optEnCours = opt

				fr = Frame(self.argFrame, width = 350, height = 30)
				fr.pack(side = TOP)
				fr.pack_propagate(0)

				label = Label(fr, text = LAB_RIGHT_VALUE)
				label.pack(side = LEFT)

				self.entry = Entry(self.argFrame, width = 30)
				self.entry.pack(side = TOP)
				self.entry.insert(END, opt.getValue())
				self.entry.bind("<FocusOut>", self.validate)

		## Documentation
		#loader = DOCReader.getInstance()
		#laDoc = loader.getDocOf(self.module, self.fonction, item)
		#if(laDoc != None):
		#	textBox = Text(self.docFrame, width = 35, height = 20)
		#	textBox.pack(side = TOP)
		#	for line in laDoc:
		#		textBox.insert(END, line)
		#	textBox.configure(state = "disabled")

		

		return

	def searchDoc(self):
		loader = Options.getInstance()
		explorateur = loader.getOption("EXPLORATEUR")
		cheminDoc = loader.getOption("DOC")
		if(explorateur != "" and cheminDoc != ""):
			if(cheminDoc.endswith(os.sep) == False):
				cheminDoc = cheminDoc + os.sep

			cheminDoc = cheminDoc + "ChiPy" + os.sep + "src" + os.sep + "shared" + os.sep
			cheminDoc = cheminDoc + self.module + "_f90.html"
			goto = ""

			try:
				fic = open(cheminDoc)
			except:
				return
			else:
				nextIsTheOne = False
				for line in fic:
					if(nextIsTheOne):
						goto = "\#" + line.split("<a name=\"")[1].split("\"></a>")[0]
						break

					line = line.lower()
					module = self.module.split("_")[1].lower()
					
					theLine = "<a name=\"chipy2e" + module + "2f" + self.fonction.lower() + "\">"
					if(line.startswith(theLine)):
						nextIsTheOne = True
				fic.close()

			command = explorateur + " file://" + cheminDoc + goto
			t = threading.Thread(target=self.launchNav, args=command)
			t.start()
		return

	def launchNav(self, *command):
		line = ""
		for char in command:
			line = line + char
		if(os.name == "posix"):
			os.system(line + " 2>/dev/null")
		else:
			os.system(line)
		return

	def save(self):
		for opt in self.reqArgs:
			opt.setValue(self.argsDictionnary[opt.getName()])

		for opt in self.optArgs:
			opt.setValue(self.argsDictionnary[opt.getName()])
		self.ParamGroup.miseAJour()
		return

	def validate(self, ev = ""):
		arg = self.optEnCours.getName()
		newVal = self.entry.get()
		if(newVal != self.argsDictionnary[arg]):
			self.argsDictionnary[arg] = newVal
			self.save()
		return

	def destroy(self):
		if(self.entry != None):
			self.validate()
			self.entry.destroy()
			self.entry = None
		return
