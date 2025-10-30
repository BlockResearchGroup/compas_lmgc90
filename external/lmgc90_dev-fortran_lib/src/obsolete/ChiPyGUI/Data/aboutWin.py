# -*- coding: iso-8859-1 -*-

from Tkinter import *
from localisation import *

class AboutDialog(Toplevel):
	"""AboutDialog(Toplevel)
		Definit une fenetre about
	"""
	def __init__(self, parent):
		Toplevel.__init__(self, parent)
		self.transient(parent)
		self.resizable(width=NO,height=NO)

		self.parent = parent

		self.title(LG_MENU_ABOUT)

		self.grab_set()

		self.container = Frame(self, width = 300, height = 100)
		self.container.pack(side = TOP)
		self.container.pack_propagate(0)

		label = Label(self.container, text = DLG_ABOUT_TEXT)
		label.pack()

		self.buttonFrame = Frame(self, width = 300, height = 50)
		self.buttonFrame.pack(side = TOP)

		self.okButton = Button(self.buttonFrame, text = BUT_OK, command = self.okClic)
		self.okButton.pack(side = LEFT)		

		return

	def okClic(self):
		self.grab_release()
		self.destroy()
		return
