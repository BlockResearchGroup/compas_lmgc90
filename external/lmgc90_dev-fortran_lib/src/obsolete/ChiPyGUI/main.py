#!/bin/env python
# -*- coding: iso-8859-1 -*-

## Import externe
from Tkinter import *

## Import logiciel
from Data.localisation import *
from Data.interfaceCommande import InterfaceCMD
from Data.loader import MacroLoader, Options
from Data.loader import readLibrary
from Data.optionWin import OptionWindow
from Data.aboutWin import AboutDialog

import tkFileDialog
import os
import optparse
import sys

try:
	import Pmw
except:
	print ERR_PMW
	sys.exit(0)

class Container(Frame):
	""" Class Container(Frame):
		Definit la zone d'affichage de l'application

		METHODES :
		  - getStatusBar()
	"""
	def __init__(self, parent):
		Frame.__init__(self, parent, borderwidth = 2, relief = "sunken", width = 1020, height = 610)
		self.pack_propagate(0)

		self.messageBar = Pmw.MessageBar(self, entry_width = 150, entry_relief='groove', labelpos = 'w')
		self.messageBar.pack(side = BOTTOM, fill = 'x', padx = 0, pady = 0)
		self.messageBar.message('state', '')

		return

	def getStatusBar(self):
		"""getStatusBar(self)
			SORTIES:
			  - self.messageBar : Widget de la barre de statut
		"""
		return self.messageBar

def detectNextChar(string, usedChars):
	"""detectNextChar(string, usedChars)
		Retourne le premier caractere de la chaine 'string' non present dans le tableau 'usedChars'
		Le tableau usedChars est egalement mis a jour en ajoutant le caractere trouvé a la suite
	"""
	for numChar in range(len(string)):
			if(string[numChar] in usedChars):
				continue
			usedChars.append(string[numChar])
			raccourci = numChar
			break

	return usedChars, raccourci

class MenuBar(Frame):
	"""MenuBar(Frame)
		Class decrivant le menu de l'application
	"""
	def __init__(self, parent):
		Frame.__init__(self, parent, borderwidth = 2, relief = "raised", width = 1024, height = 33)
		self.pack_propagate(0)

		usedCharsMenu = []

		## MENU FILE
		usedCharsMenu, raccourci = detectNextChar(LG_MENU_FILE, usedCharsMenu)
		self.File = Menubutton(self, text = LG_MENU_FILE, underline = raccourci)
		self.File.pack(side = LEFT)

		self.FileMenu = Menu(self.File, tearoff = 0)

		cascade = Menu(self.File, tearoff = 0)

		usedCharsCascade = []
		usedCharsCascade, raccourci = detectNextChar(LG_MENU_VIERGE, usedCharsCascade)
		cascade.add_command(label = LG_MENU_VIERGE, underline = raccourci, command = parent.newFile, accelerator = "Ctrl+N")

		usedCharsCascade, raccourci = detectNextChar(LG_MENU_TEMPLATE, usedCharsCascade)
		cascade.add_command(label = LG_MENU_TEMPLATE, underline = raccourci, command = parent.newTemplate, accelerator = "Ctrl+Shift+N")

		usedChars = []
		usedChars, raccourci = detectNextChar(LG_MENU_NEWFILE, usedChars)
		self.FileMenu.add_cascade(label = LG_MENU_NEWFILE, underline = raccourci, menu = cascade)

		#self.FileMenu.add_command(label = LG_MENU_NEWFILE, underline = 0, command = parent.newFile)

		usedChars, raccourci = detectNextChar(LG_MENU_OPENFILE, usedChars)
		self.FileMenu.add_command(label = LG_MENU_OPENFILE, underline = raccourci, command = parent.openFile, accelerator = "Ctrl+O")

		self.FileMenu.add_separator()

		usedChars, raccourci = detectNextChar(LG_MENU_SAVE, usedChars)
		self.FileMenu.add_command(label = LG_MENU_SAVE, underline = raccourci, command = parent.saveFile, accelerator = "Ctrl+S")

		usedChars, raccourci = detectNextChar(LG_MENU_SAVEAS, usedChars)
		self.FileMenu.add_command(label = LG_MENU_SAVEAS, underline = raccourci, command = parent.saveFileAs, accelerator = "Ctrl+Shift+S")

		self.FileMenu.add_separator()

		usedChars, raccourci = detectNextChar(LG_MENU_CLOSE, usedChars)
		self.FileMenu.add_command(label = LG_MENU_CLOSE, underline = raccourci, command = parent.closeFile, accelerator = "Ctrl+W")

		usedChars, raccourci = detectNextChar(LG_MENU_QUIT, usedChars)
		self.FileMenu.add_command(label = LG_MENU_QUIT, underline = raccourci, command = parent.quitEditor, accelerator = "Ctrl+Q")

		self.File.configure(menu = self.FileMenu)

		## MENU MACRO
		usedCharsMenu, raccourci = detectNextChar(LG_MENU_MACRO, usedCharsMenu)
		self.Macro = Menubutton(self, text = LG_MENU_MACRO, underline = raccourci)
		self.Macro.pack(side = LEFT)

		self.MacroMenu = Menu(self.Macro, tearoff = 0)
		usedChars = []
		usedChars, raccourci = detectNextChar(LG_MENU_LOAD, usedChars)
		self.MacroMenu.add_command(label = LG_MENU_LOAD, underline = raccourci, command = parent.loadMacro, accelerator = "Ctrl+L")

		usedChars, raccourci = detectNextChar(LG_MENU_LOADREP, usedChars)
		self.MacroMenu.add_command(label = LG_MENU_LOADREP, underline = raccourci, command = parent.loadRep, accelerator = "Ctrl+Shift+L")

		self.MacroMenu.add_separator()

		usedChars, raccourci = detectNextChar(LG_MENU_SAVE, usedChars)
		self.MacroMenu.add_command(label = LG_MENU_SAVE, underline = raccourci, command = parent.saveMacro, accelerator = "Ctrl+Z")

		usedChars, raccourci = detectNextChar(LG_MENU_SAVEAS, usedChars)
		self.MacroMenu.add_command(label = LG_MENU_SAVEAS, underline = raccourci, command = parent.saveMacroAs, accelerator = "Ctrl+Shift+Z")

		self.Macro.configure(menu = self.MacroMenu)

		## MENU EDITION
		usedCharsMenu, raccourci = detectNextChar(LG_MENU_EDITION, usedCharsMenu)
		self.Edition = Menubutton(self, text = LG_MENU_EDITION, underline = raccourci)
		self.Edition.pack(side = LEFT)

		self.EditionMenu = Menu(self.Edition, tearoff = 0)
		usedChars = []
		usedChars, raccourci = detectNextChar(LG_MENU_COPY, usedChars)
		self.EditionMenu.add_command(label = LG_MENU_COPY, underline = raccourci, command = parent.copy, accelerator = "Ctrl+C")

		usedChars, raccourci = detectNextChar(LG_MENU_PASTE, usedChars)
		self.EditionMenu.add_command(label = LG_MENU_PASTE, underline = raccourci, command = parent.paste, accelerator = "Ctrl+V")

		self.Edition.configure(menu = self.EditionMenu)

		## MENU OPTION
		usedCharsMenu, raccourci = detectNextChar(LG_MENU_OPTION, usedCharsMenu)
		self.Option = Menubutton(self, text = LG_MENU_OPTION, underline = raccourci)
		self.Option.pack(side = LEFT)

		self.OptionMenu = Menu(self.Option, tearoff = 0)

		usedChars = []
		usedChars, raccourci = detectNextChar(LG_MENU_PREFERENCE, usedChars)
		self.OptionMenu.add_command(label = LG_MENU_PREFERENCE, underline = raccourci, command = parent.openOption)

		self.Option.configure(menu = self.OptionMenu)


		## MENU AIDE
		usedCharsMenu, raccourci = detectNextChar(LG_MENU_AIDE, usedCharsMenu)
		self.Aide = Menubutton(self, text = LG_MENU_AIDE, underline = raccourci)
		self.Aide.pack(side = LEFT)

		self.AideMenu = Menu(self.Aide, tearoff = 0)

		usedChars = []
		usedChars, raccourci = detectNextChar(LG_MENU_ABOUT, usedChars)
		self.AideMenu.add_command(label = LG_MENU_ABOUT, underline = raccourci, command = parent.about)

		self.Aide.configure(menu = self.AideMenu)

		self.pack(side = TOP)

		return

	def enablePaste(self):
		"""enablePaste(self)
			Active certains menus
		"""
		self.EditionMenu.entryconfigure(2, state = "active")
		return

	def disablePaste(self):
		"""disablePaste(self)
			Desactive certains menus
		"""
		self.EditionMenu.entryconfigure(2, state = "disabled")
		return

	def enableSave(self):
		"""enableSave(self)
			Active certains menus
		"""
		self.FileMenu.entryconfigure(3, state = "active")
		self.FileMenu.entryconfigure(4, state = "active")
		self.FileMenu.entryconfigure(6, state = "active")

		self.Macro.configure(state = "active")
		self.Edition.configure(state = "active")
		return

	def disableSave(self):
		"""disableSave(self)
			Desactive certains menus
		"""
		self.FileMenu.entryconfigure(3, state = "disabled")
		self.FileMenu.entryconfigure(4, state = "disabled")
		self.FileMenu.entryconfigure(6, state = "disabled")

		self.Macro.configure(state = "disabled")
		self.disablePaste()
		self.Edition.configure(state = "disabled")
		return

class Interface(Tk):
	"""class Interface(Tk)
		Fenetre mere de l'application
		Elle est definit en singleton

		METHODES:
		  - disableSave()
		  - enableSave()
		  - quitEditor()
		  - closeFile()
		  - saveFile()
		  - saveFileAs()
		  - openFile()
		  - newFile()
		  - save()
		  - getInstance()
		  - getContainer()
	"""
	_instance = None

	def __init__(self, fileName = ""):
		Tk.__init__(self)
		self.title(LG_TITLE_MAIN)
		self.resizable(width = YES, height = YES)
		self.wm_grid(1024, 650, 1, 1)
		self.pack_propagate(0)

		self.wm_protocol('WM_DELETE_WINDOW', self.quitEditor)

		self.balloon = Pmw.Balloon(self)
		self.balloon.configure(state = 'balloon')

		self.menuBar = MenuBar(self)

		self.disableSave()

		#self.menuBar.addcascademenu(LG_MENU_FILE, 'Example', '', traverseSpec = 'z', tearoff = 1)
		#self.menuBar.addmenuitem('Example', 'command', '', command = self.quit, label = LG_MENU_QUIT)

		self.configure(menu = self.menuBar)

		self.container = Container(self)
		#self.container.grid(row = 1, column = 0, rowspan = 12, columnspan = 10)
		self.container.pack(side = TOP, pady = 5)

		self.interfaceCommande = None
		self.interfaceListeFile = None

		self.update()
       		self.geometry(self.geometry())

		self.grid()

		## Creation de la classe Options :
		Options.getInstance()

		if(fileName != ""):
			self.openFile(False, fileName)

		self.bind_all("<Control-n>", self.newFile)
		self.bind_all("<Control-N>", self.newTemplate)
		self.bind_all("<Control-o>", self.openFileFromAccel)

		self.bind_all("<Control-s>", self.saveFile)
		self.bind_all("<Control-S>", self.saveFileAs)

		self.bind_all("<Control-z>", self.saveMacro)
		self.bind_all("<Control-Z>", self.saveMacroAs)

		self.bind_all("<Control-l>", self.loadMacro)
		self.bind_all("<Control-L>", self.loadRep)

		self.bind_all("<Control-w>", self.closeFile)
		self.bind_all("<Control-q>", self.quitEditor)

		return

	def disableSave(self):
		"""disableSave(self):
			desactive les menus enregistrer, enregistrer sous et fermer
		"""

		#self.menuBar.entryconfigure(0, state = "disabled")

		self.menuBar.disableSave()

		return

	def enableSave(self):
		"""enableSave(self):
			active les menus enregistrer, enregistrer sous et fermer
		"""
		self.menuBar.enableSave()

		return

	def about(self):
		"""about(self)
			Appele la fenetre TopLevel about
		"""
		dialog = AboutDialog(self)
		self.wait_window(dialog)
		return

	def copy(self, ev = ""):
		"""copy(self, ev = "")
			Essaye de copier en memoire les elements
		"""
		if(self.interfaceCommande.copy()):
			self.menuBar.enablePaste()
		return

	def copyFromAccel(self):
		"""copyFromAccel(self)
		"""
		self.menuBar.enablePaste()
		return

	def paste(self, ev = ""):
		"""paste(self, ev = "")
			Colle les elements en memoire
		"""
		self.interfaceCommande.paste()
		return

	def openOption(self):
		"""openOption(self)
			Ouvre la fenetre TopLevel options
		"""
		dialog = OptionWindow(self)
		self.wait_window(dialog)
		return

	def reloadLeftPanel(self):
		"""reloadLeftPanel(self)
			Recharge l'interface gauche de l'application
		"""
		if(self.interfaceCommande != None):
			self.interfaceCommande.reloadLeftPanel()
		return

	def saveMacro(self, ev = ""):
		"""saveMacro
			Verifie que le fichier peut etre sauver et le sauve
			Si aucun nom de fichier n'est disponible dans la class (fileName),
			appelle saveMacroAs()
		"""
		if(self.interfaceCommande == None or self.interfaceCommande.isOpened() == False):
			return

		## On verifie que le fichier a ete definie par l'utilisateur (en cas de "nouveau", il n'y a pas de nom)
		fileName = self.interfaceCommande.getFileName()

		if(fileName != "" and fileName.endswith(".mac")):
			## On verifie qu'on peut ouvrir le fichier en ecriture
			try:
				fic = open(fileName, 'a')
			except:
				dialog = Pmw.MessageDialog(self, title = DLG_TITLE_SAVEERROR, defaultbutton = 0, message_text = DLG_LAB_ERROROPENING %fileName)
				dialog.activate()
				self.saveMacroAs()
			else:
				fic.close()
				self.exportMacro(fileName)
		else:
			self.saveMacroAs()
		return

	def saveMacroAs(self, ev = ""):
		"""saveMacroAs(self)
			demande a l'utilisateur un emplacement et un nom de fichier,
			verifie l'emplacement et le nom de fichier et
			enregistre le fichier
		"""
		if(self.interfaceCommande == None or self.interfaceCommande.isOpened() == False):
			return

		## Fichier a enregistrer :
		fileName = self.interfaceCommande.getFileName()
		if(fileName != ""):
			cheminFinal = os.sep
			chemin = fileName.split(os.sep)[1:-1]
			for boutDeChemin in chemin:
				cheminFinal = cheminFinal + boutDeChemin + os.sep
		else:
			cheminFinal = os.getcwd()

		fileName = tkFileDialog.asksaveasfilename(filetypes=[(FILETYPE_MACRO, ".mac")], parent = self, initialdir = cheminFinal, title = DLG_TITLE_FILESAVEAS)
		if(fileName == () or fileName == ""):
				return

		if(fileName.endswith(".mac") == False):
			fileName = fileName + ".mac"
		self.exportMacro(fileName)
		return

	def exportMacro(self, fileName):
		"""exportMacro(self, fileName)
			enregistre le contenu de l'application dans le fichier fileName sous forme de macro
		"""
		errors, fichier = self.interfaceCommande.saveMacro(fileName)
		if(len(errors) > 0):
			allErr = ""
			for error in errors:
				allErr = allErr + error + "\n"
			message = DLG_LAB_LINEERROR %allErr
			
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_WARNING, defaultbutton = DLG_BUT_SAVE, buttons = (DLG_BUT_SAVE, DLG_BUT_CANCEL), message_text = message)
			result = dialog.activate()

			if(result == DLG_BUT_CANCEL):
				return

		fic = open(fileName, "w")
		fic.write(fichier)
		fic.close()
		return

	def loadRep(self, ev = ""):
		"""loadRep(self, ev = "")
			Demande un repertoire a l'utilisateur et charge tous les fichiers macro de ce repertoire
		"""
		if(self.interfaceCommande == None):
			return

		loader = Options.getInstance()
		initialDir = loader.getOption("INSTALLPATH")

		path = tkFileDialog.askdirectory(parent = self, mustexist = 1, initialdir = initialDir, title = DLG_TITLE_REPTOOPEN)
		if(path == () or path == ""):
			return

		if(path.endswith(os.sep) == False):
			path = path + os.sep

		if(path.startswith(os.sep) == False):
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_ERROR, defaultbutton = 0, message_text = DLG_LAB_MUSTBEABSOLUTE)
			dialog.activate()
		else:
			loader = MacroLoader.getInstance()
			nom = loader.loadDir(path)

			self.interfaceCommande.miseAJourMacros()
		return

	def loadMacro(self, ev = ""):
		"""loadMacro(self)
			ouvre un fichier macro et le charge
		"""
		if(self.interfaceCommande == None):
			return

		loader = Options.getInstance()
		initialDir = loader.getOption("INSTALLPATH") + "Macros"

		## Fichier a ouvrir :
		fileName = tkFileDialog.askopenfilename(filetypes=[(FILETYPE_MACRO, ".mac")], parent = self, initialdir = initialDir, title = DLG_TITLE_FILETOOPEN)
		if(fileName == () or fileName == ""):
			return		

		if(fileName.startswith(os.sep) == False):
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_ERROR, defaultbutton = 0, message_text = DLG_LAB_MUSTBEABSOLUTE)
			dialog.activate()
		else:

			## On verifie que le fichier peut bien etre lu
			try:
				fic = open(fileName, 'r')
			except:
				dialog = Pmw.MessageDialog(self, title = DLG_TITLE_ERROR, defaultbutton = 0, message_text = DLG_LAB_ERROROPENING %fileName)
				dialog.activate()
			else:
				fic.close()
				loader = MacroLoader.getInstance()
				nom = loader.loadMacro(fileName)

				self.interfaceCommande.miseAJourMacros()
		return

	def quitEditor(self, ev = ""):
		"""quitEditor(self)
			demande si l'utilisateur veut enregistrer son fichier avant de quitter l'application
		"""
		## Si un fichier est ouvert, on demande a l'utilisateur ce qu'il veut faire
		if(self.interfaceCommande != None and self.interfaceCommande.isOpened() and self.interfaceCommande.isModified()):
			fileName = self.interfaceCommande.getFileName()
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_WARNING, defaultbutton = DLG_BUT_SAVE, buttons = (DLG_BUT_SAVE, DLG_BUT_DONTSAVE, DLG_BUT_CANCEL), message_text = DLG_LAB_FILEOPENED %fileName)
			result = dialog.activate()

			if(result == DLG_BUT_CANCEL):
				return

			if(result == DLG_BUT_SAVE):
				self.saveFile()

		## on sauve le fichier option
		loader = Options.getInstance()
		loader.saveOptions()
		self.quit()
		return

	def closeFile(self, ev = ""):
		"""closeFile(self)
			demande si l'utilisateur veut enregistrer son fichier puis le ferme
		"""
		if(self.interfaceCommande == None or self.interfaceCommande.isOpened() == False):
			return

		if(self.interfaceCommande != None and self.interfaceCommande.isOpened() and self.interfaceCommande.isModified()):
			fileName = self.interfaceCommande.getFileName()
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_WARNING, defaultbutton = DLG_BUT_SAVE, buttons = (DLG_BUT_SAVE, DLG_BUT_DONTSAVE, DLG_BUT_CANCEL), message_text = DLG_LAB_FILEOPENED %fileName)
			result = dialog.activate()

			if(result == DLG_BUT_CANCEL):
				return

			if(result == DLG_BUT_SAVE):
				self.saveFile()

		self.interfaceCommande.closeFile()

		if(self.interfaceCommande != None):
			self.interfaceCommande.destroy()
		self.interfaceCommande = None
		self.disableSave()
		return

	def saveFile(self, ev = ""):
		"""saveFile
			Verifie que le fichier peut etre sauver et le sauve
			Si aucun nom de fichier n'est disponible dans la class (self.fileName),
			appelle saveFileAs()
		"""

		if(self.interfaceCommande == None or self.interfaceCommande.isOpened() == False):
			return

		fileName = self.interfaceCommande.getFileName()
		## On verifie que le fichier a ete definie par l'utilisateur (en cas de "nouveau", il n'y a pas de nom)
		if(fileName != ""):
			## On verifie qu'on peut ouvrir le fichier en ecriture
			try:
				fic = open(fileName, 'a')
			except:
				dialog = Pmw.MessageDialog(self, title = DLG_TITLE_SAVEERROR, defaultbutton = 0, message_text = DLG_LAB_ERROROPENING %fileName)
				dialog.activate()
				self.saveFileAs()
			else:
				fic.close()
				self.save(fileName)
		else:
			self.saveFileAs()
		return

	def saveFileAs(self, ev = ""):
		"""saveFileAs(self)
			demande a l'utilisateur un emplacement et un nom de fichier,
			verifie l'emplacement et le nom de fichier et
			enregistre le fichier
		"""
		if(self.interfaceCommande == None or self.interfaceCommande.isOpened() == False):
			return

		fileName = self.interfaceCommande.getFileName()

		## Fichier a enregistrer :
		if(fileName != ""):
			cheminFinal = os.sep
			chemin = fileName.split(os.sep)[1:-1]
			for boutDeChemin in chemin:
				cheminFinal = cheminFinal + boutDeChemin + os.sep
		else:
			cheminFinal = os.getcwd()

		fileName = tkFileDialog.asksaveasfilename(filetypes=[(FILETYPE_PYTHON, ".py")], parent = self, initialdir = cheminFinal, title = DLG_TITLE_FILESAVEAS)
		if(fileName == () or fileName == ""):
				return

		self.save(fileName)
		return

	def save(self, fileName):
		"""save(self, fileName)
			enregistre le contenu de l'application dans le fichier fileName
		"""
		errors, fichier = self.interfaceCommande.saveFile(fileName)
		if(len(errors) > 0):
			allErr = ""
			for error in errors:
				allErr = allErr + error + "\n"
			message = DLG_LAB_LINEERROR %allErr
			
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_WARNING, defaultbutton = DLG_BUT_SAVE, buttons = (DLG_BUT_SAVE, DLG_BUT_CANCEL), message_text = message)
			result = dialog.activate()

			if(result == DLG_BUT_CANCEL):
				return

		fic = open(fileName, "w")
		fic.write(fichier)
		fic.close()
		return

	def newFile(self, ev = ""):
		"""newFile(self)
			ouvre un nouveau fichier
		"""
		## Si un fichier est deja ouvert, on demande a l'utilisateur ce qu'il veut faire
		if(self.interfaceCommande != None and self.interfaceCommande.isOpened() and self.interfaceCommande.isModified()):
			fileName = self.interfaceCommande.getFileName()
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_WARNING, defaultbutton = DLG_BUT_SAVE, buttons = (DLG_BUT_SAVE, DLG_BUT_DONTSAVE, DLG_BUT_CANCEL), message_text = DLG_LAB_FILEOPENED %fileName)
			result = dialog.activate()

			if(result == DLG_BUT_CANCEL):
				return

			if(result == DLG_BUT_SAVE):
				self.saveFile()

		if(self.interfaceCommande != None):
			self.interfaceCommande.destroy()
		self.interfaceCommande = InterfaceCMD(self.container, self, "")

		self.enableSave()
		return

	def newTemplate(self, ev = ""):
		"""newTemplate(self)
			ouvre un nouveau fichier a partir d'un modele
		"""
		## Si un fichier est deja ouvert, on demande a l'utilisateur ce qu'il veut faire
		if(self.interfaceCommande != None and self.interfaceCommande.isOpened() and self.interfaceCommande.isModified()):
			fileName = self.interfaceCommande.getFileName()
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_WARNING, defaultbutton = DLG_BUT_SAVE, buttons = (DLG_BUT_SAVE, DLG_BUT_DONTSAVE, DLG_BUT_CANCEL), message_text = DLG_LAB_FILEOPENED %fileName)
			result = dialog.activate()

			if(result == DLG_BUT_CANCEL):
				return

			if(result == DLG_BUT_SAVE):
				self.saveFile()

		loader = Options.getInstance()
		path = loader.getOption("INSTALLPATH")
		path = path + "Templates"

		## Fichier a ouvrir :
		fileName = tkFileDialog.askopenfilename(filetypes=[(FILETYPE_TEMPLATE, ".pyt")], parent = self, initialdir = path, title = DLG_TITLE_TEMPLATETOOPEN)
		if(fileName == () or fileName == ""):
			return	

		if(fileName.startswith(os.sep) == False):
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_ERROR, defaultbutton = 0, message_text = DLG_LAB_MUSTBEABSOLUTE)
			dialog.activate()
		else:
			## On verifie que le fichier peut bien etre lu
			try:
				fic = open(fileName, 'r')
			except:
				dialog = Pmw.MessageDialog(self, title = DLG_TITLE_ERROR, defaultbutton = 0, message_text = DLG_LAB_ERROROPENING %fileName)
				dialog.activate()
			else:
				fic.close()

				if(self.interfaceCommande != None):
					self.interfaceCommande.destroy()
			
				self.interfaceCommande = InterfaceCMD(self.container, self, fileName, template = True)
				self.fileIsOpen = True
				self.enableSave()

		return

	def openFileFromAccel(self, ev = ""):
		self.openFile()
		return

	def openFile(self, fromMenu = True, fileName = ""):
		"""openFile(self, fromMenu)
			ouvre un fichier et le charge

			ENTREES:
			  - fromMenu : variable indiquant si la commande a été lancée depuis le menu
		"""
		if(fromMenu):
			## Si un fichier est deja ouvert, on demande a l'utilisateur ce qu'il veut faire
			if(self.interfaceCommande != None and self.interfaceCommande.isOpened() and self.interfaceCommande.isModified()):
				fileName = self.interfaceCommande.getFileName()
				dialog = Pmw.MessageDialog(self, title = DLG_TITLE_WARNING, defaultbutton = DLG_BUT_SAVE, buttons = (DLG_BUT_SAVE, DLG_BUT_DONTSAVE, DLG_BUT_CANCEL), message_text = DLG_LAB_FILEOPENED %fileName)
				result = dialog.activate()

				if(result == DLG_BUT_CANCEL):
					return

				if(result == DLG_BUT_SAVE):
					self.saveFile()

			repertoire = os.path.expanduser('~')

			## Fichier a ouvrir :
			fileName = tkFileDialog.askopenfilename(filetypes=[(FILETYPE_PYTHON, ".py")], parent = self, initialdir = repertoire, title = DLG_TITLE_FILETOOPEN)
			if(fileName == () or fileName == ""):
					return

		if(fileName.startswith(os.sep) == False):
			dialog = Pmw.MessageDialog(self, title = DLG_TITLE_ERROR, defaultbutton = 0, message_text = DLG_LAB_MUSTBEABSOLUTE)
			dialog.activate()
		else:

			## On verifie que le fichier peut bien etre lu
			try:
				fic = open(fileName, 'r')
			except:
				dialog = Pmw.MessageDialog(self, title = DLG_TITLE_ERROR, defaultbutton = 0, message_text = DLG_LAB_ERROROPENING %fileName)
				dialog.activate()
			else:
				fic.close()

				if(self.interfaceCommande != None):
					self.interfaceCommande.destroy()
			
				self.interfaceCommande = InterfaceCMD(self.container, self, fileName)
				self.enableSave()
		return

	def getInstance(self, fileName = ""):
		"""getInstance(self, fileName)
			renvoie l'instance de la classe. Si elle n'existe pas,
			elle est crée avec en parametre fileName (le fichier indiqué sera ouvert)
		"""
		if(self._instance == None):
			self._instance = Interface(fileName)
		return self._instance

	def getContainer(self):
		"""getContainer(self)
			renvoie le widget container
		"""
		return self.container.interior()

	getInstance = classmethod(getInstance)

if(__name__ == "__main__"):
	parser = optparse.OptionParser()
	parser.add_option("-g", "--generate-catalog", action = "store_true", dest = "generate", help = HELP_GENERATE_CATALOG)
	parser.add_option("-f", "--file", action = "store", type = "string", dest = "file", help = HELP_OPEN_FILE)
	(options, args) = parser.parse_args()

	fileName = ""
	
	if(options.generate):
		print INFO_GENERATE_CATALOG
		readLibrary(False)
	else:
		print INFO_LOADING_CATALOG
		readLibrary(True)

	if(options.file):
		fileName = options.file

	inter = Interface.getInstance(fileName)
	inter.mainloop()
