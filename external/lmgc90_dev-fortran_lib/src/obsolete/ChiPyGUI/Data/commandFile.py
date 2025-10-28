# -*- coding: iso-8859-1 -*-

from localisation import *
from loader import ArgumentsLoader, ModuleLoader, FunctionLoader

class Option:
	def __init__(self, nom = "", valeur = "", required = False):
		self.nom = nom
		self.valeur = valeur
		self.required = required
		return

	def setValue(self, valeur):
		self.valeur = valeur
		return

	def setName(self, nom):
		self.nom = nom
		return

	def getName(self):
		return self.nom

	def getValue(self):
		return self.valeur

	def isRequired(self):
		return self.required

	def getString(self):
		if(self.nom != ""):
			return "%s = %s" %(self.nom, self.valeur)
		return "%s" %self.valeur

class Commande:
	def __init__(self, text = "", type = "", module = "", fonction = "", variable = "", minRange = "", maxRange = "", step = "", numTab = 0, incrementer = False):
		self.numTab = numTab
		self.text = text
		self.type = type
		self.module = module
		self.fonction = fonction
		self.variable = variable
		self.minRange = minRange
		self.maxRange = maxRange
		self.step = step
		self.options = []
		self.isAnIncrementer = incrementer

		if(type == "WRAP"):
			if(text.strip() != ""):
				listeArgs = text.split(",")
			else:
				listeArgs = []

			loader = ArgumentsLoader.getInstance()
			listeReq = loader.getRequiredArgsOf(module, fonction)
			listeOpt = loader.getOptionalArgsOf(module, fonction)

			## On cree tous les champs Option
			for arg in listeReq:
				option = Option(nom = arg, valeur = "", required = True)
				self.options.append(option)
			for arg in listeOpt:
				option = Option(nom = arg, valeur = "", required = False)
				self.options.append(option)

			numArg = 0
			for arg in listeArgs:
				arg = arg.strip()

				if(arg.count("=") > 0):
					nom = arg.split("=")[0].strip()
					value = arg.split("=")[1].strip()
					for opt in self.options:
						if(opt.getName() == nom):
							opt.setValue(value)
				else:
					## On essaye de coller les arguments dans les options
					## En toute logique, les arguments apparaissent dans
					## l'ordre recuperer dans le prototype
					## print WARN_PARAM_ERROR %(module, fonction, text)
					opt = self.options[numArg]
					opt.setValue(arg)
					numArg = numArg + 1
		return

	def setIsIncrementer(self):
		self.isAnIncrementer = True
		return

	def isIncrementer(self):
		return self.isAnIncrementer

	def getValidity(self):
		resultat = "Valide"

		if(self.type == "WRAP"):
			## Verification de la validité de la fonction
			loader = ModuleLoader.getInstance()
			if(self.module not in loader.getModules()):
				return "Inexistante"
			loader = FunctionLoader.getInstance()
			if(self.fonction not in loader.getFunctionsOf(self.module)):
				return "Inexistante"

			for opt in self.options:
				if(opt.isRequired()):
					if(opt.getValue() == ""):
						return "Invalide"
				else:
					if(opt.getValue() == ""):
						resultat = "Incomplet"
		if(self.type == "PYTHON"):
			return "Ignore"
		if(self.type == "FOR"):
			if(self.step == "" or self.minRange == "" or self.variable == "" or self.maxRange == ""):
				return "Invalide"
		if(self.type == "PRINT"):
			if(self.text == ""):
				return "Invalide"
		if(self.type == "IMPORT"):
			if(self.module == ""):
				return "Invalide"
		if(self.type == "FROM"):
			if(self.module == "" or self.text == ""):
				return "Invalide"

		if(self.type == "ENDTAB"):
			resultat = "Internal command"
		if(self.type == "ENDFILE"):
			resultat = "Internal command"
		return resultat

	def getRequiredArgs(self):
		liste = []
		for arg in self.options:
			if(arg.isRequired()):
				liste.append(arg)
		return liste

	def getOptionalArgs(self):
		liste = []
		for arg in self.options:
			if(arg.isRequired() == False):
				liste.append(arg)
		return liste

	def get(self, key):
		if(self.__dict__.has_key(key)):
			return self.__dict__[key]
		return

	def set(self, key, value):
		if(self.__dict__.has_key(key)):
			self.__dict__[key] = value
		return

	def setTab(self, tab):
		self.numTab = tab
		return

	def getNumTab(self):
		return self.numTab

	def getString(self):
		if(self.type == "PYTHON"):
			return self.numTab, self.text
		if(self.type == "ENDFILE"):
			return 0, TXT_ENDFILE
		if(self.type == "ENDTAB"):
			return self.numTab, TXT_ENDTAB
		if(self.type == "ENDLINE"):
			return self.numTab, " "
		if(self.type == "COM"):
			return self.numTab, "#%s" %self.text
		if(self.type == "IMPORT"):
			return self.numTab, "import %s" %self.module
		if(self.type == "FROM"):
			return self.numTab, "from %s import %s" %(self.module, self.text)
		if(self.type == "FOR"):
			return self.numTab, "for %s in xrange(%s, %s, %s):" %(self.variable, self.minRange, self.maxRange, self.step)
		if(self.type == "WRAP"):
			args = ""
			for arg in self.options:
				if(arg.getValue() != ""):
					if(args != ""):
						args = args + ", "
					args = args + arg.getString()
			return self.numTab, "%s.%s(%s)" %(self.module, self.fonction, args)
		if(self.type == "PRINT"):
			return self.numTab, "print %s" %self.text

		return self.numTab, "ERROR"

	def getType(self):
		return self.type

class CommandFile:
	def __init__(self):
		self.listeCommande = []
		return

	def newFile(self):
		commande = Commande(type = "ENDFILE")
		self.listeCommande.append(commande)
		return

	def detectCommandType(self, ligne, numberOfTabulations):
		command = None

		if(ligne.strip().startswith("#")):
			commande = Commande(type = "COM", text = ligne.strip()[1:], numTab = numberOfTabulations)
		elif(ligne.strip().startswith("for ")):
			value = ligne.strip().split("(")[1].split(")")[0].strip()
			if(value.count(",") == 2):
				minRange = value.split(",")[0].strip()
				maxRange = value.split(",")[1].strip()
				step = value.split(",")[2].strip()
			else:
				minRange = 0
				maxRange = value
				step = 1
			variable = ligne.strip().split("for")[1].split("in")[0].strip()
			commande = Commande(type = "FOR", variable = variable, minRange = minRange, maxRange = maxRange, step = step, numTab = numberOfTabulations)
			#numberOfTabulations = numberOfTabulations + 1
		elif(ligne.strip().startswith("from ")):
			module = ligne.strip().split("from")[1].split("import")[0].strip()
			text = ligne.strip().split("import")[1].strip()
			commande = Commande(type = "FROM", text = text, module = module, numTab = numberOfTabulations)
		elif(ligne.strip().startswith("import ")):
			module = ligne.strip().split("import")[1].strip()
			commande = Commande(type = "IMPORT", module = module , numTab = numberOfTabulations)
		#elif(ligne.strip().startswith("print ")):
		#	text = ligne.strip().split("print")[1].strip()
		#	commande = Commande(type = "PRINT", text = text, numTab = numberOfTabulations)
		else:
			try:
				module = ligne.strip().split(".")[0]
				fonction = ligne.strip().split(".")[1].split("(")[0].strip()
				text = ligne.strip().split("(")[1].split(")")[0].strip()

				moduleLoader = ModuleLoader.getInstance()
				functionLoader = FunctionLoader.getInstance()

				if(moduleLoader.hasModule(module) and functionLoader.hasFunction(module, fonction)):
					commande = Commande(type = "WRAP", module = module, fonction = fonction, text = text, numTab = numberOfTabulations)
				else:
					## ce n'est pas une ligne ChiPy
					commande = Commande(type = "PYTHON", text = ligne.strip(), numTab = numberOfTabulations)
			except:
				## ce n'est pas une ligne ChiPy
				commande = Commande(type = "PYTHON", text = ligne.strip(), numTab = numberOfTabulations)

		return commande, numberOfTabulations

	def openFile(self, fileName):
		try:
			fic = open(fileName, 'r')
		except:
			print ERR_FICHIER_NON_TROUVE %fileName
			return

		#if(fic.readline() != MOD_FIRST_LINE):
		#	fic.close()
		#	print ERR_FORMAT_INCONNU %fileName
		#	return

		ligne = fic.readline()
		numberOfTabulations = 0
		addTab = False
		lastCommand = None

		indentationParEspace = False
		indentationParTabulation = False

		spaceTabulation = []
		totalSpaceTabulation = 0

		while(ligne != ""):
			commande = None
			if(ligne.endswith("\n")):
				ligne = ligne[:-1]

			if(ligne.strip() == "" or ligne.strip() == "\n"):
				commande = Commande(type = "ENDLINE", numTab = numberOfTabulations)
				self.listeCommande.append(commande)
				ligne = fic.readline()
				continue

			if(ligne.startswith(" ") and not indentationParTabulation):
				indentationParEspace = True
			if(ligne.startswith("\t") and not indentationParEspace):
				indentationParTabulation = True

			## On verifie l'indentation de type /t
			if(indentationParTabulation):
				while(ligne.count("\t") < numberOfTabulations):
					numberOfTabulations = numberOfTabulations - 1
					commande = Commande(type = "ENDTAB", numTab = numberOfTabulations)
					self.listeCommande.append(commande)
					commande = None

				if(ligne.count("\t") > numberOfTabulations):
					if(lastCommand.getType() == "FOR"):
						numberOfTabulations = numberOfTabulations + 1
					if(lastCommand.getType() == "PYTHON"):
						lastCommand.setIsIncrementer()
						numberOfTabulations = numberOfTabulations + 1

			if(indentationParEspace):
				## On verifie l'indentation de type " "
				lesEspaces = ""
				for char in ligne:
					if(char == " "):
						lesEspaces = lesEspaces + " "
					else:
						break

				while(lesEspaces.count(" ") < totalSpaceTabulation):
					numberOfTabulations = numberOfTabulations - 1
					## Mise a jour des variables
					## On retire au nombre total d'espace, le nombre d'espace defini pour
					## la derniere indentation du tableau
					totalSpaceTabulation = totalSpaceTabulation - spaceTabulation[-1]
					del spaceTabulation[-1]

					commande = Commande(type = "ENDTAB", numTab = numberOfTabulations)
					self.listeCommande.append(commande)
					commande = None

				if(lesEspaces.count(" ") > totalSpaceTabulation):
					if(lastCommand.getType() == "FOR"):
						sp = 0
						for char in ligne:
							if(char == " "):
								sp = sp + 1
							else:
								break
						totalSpaceTabulation = totalSpaceTabulation + sp
						spaceTabulation.append(sp)
						numberOfTabulations = numberOfTabulations + 1

					if(lastCommand.getType() == "PYTHON"):
						sp = 0
						for char in ligne:
							if(char == " "):
								sp = sp + 1
							else:
								break
						totalSpaceTabulation = totalSpaceTabulation + sp
						spaceTabulation.append(sp)
						lastCommand.setIsIncrementer()
						numberOfTabulations = numberOfTabulations + 1

			commande, numberOfTabulations = self.detectCommandType(ligne, numberOfTabulations)

			if(commande != None):
				self.listeCommande.append(commande)
				lastCommand = commande
			ligne = fic.readline()

		fic.close()

		commande = Commande(type = "ENDFILE")
		self.listeCommande.append(commande)

		return

	def addCommand(self, index = 0, type = "", ligne = "", isIncrementer = False):
		if(index > 0):
			numberOfTabulations = self.listeCommande[index-1].getNumTab()
			if(self.listeCommande[index-1].getType() == "FOR"):
				numberOfTabulations = numberOfTabulations + 1
		else:
			numberOfTabulations = 0

		if(type == "WRAP"):
			module = ligne.split(".")[0]
			fonction = ligne.split(".")[1].split("(")[0]
			text = ligne.split("(")[1].split(")")[0]
			commande = Commande(type = type, module = module, fonction = fonction, text = text, numTab = numberOfTabulations)
		elif(type == "COM"):
			text = ligne[1:]
			commande = Commande(type = type, text = text, numTab = numberOfTabulations)
		elif(type == "IMPORT"):
			module = ligne.strip().split("import")[1].strip()
			commande = Commande(type = type, module = module, numTab = numberOfTabulations)
		elif(type == "FROM"):
			module = ligne.strip().split("from")[1].split("import")[0].strip()
			text = ligne.strip().split("import")[1].strip() 
			commande = Commande(type = type, module = module, text = text, numTab = numberOfTabulations)
		elif(type == "PRINT"):
			text = ligne.split("print")[1].strip()
			commande = Commande(type = type, text = text, numTab = numberOfTabulations)
		elif(type == "FOR"):
			variable = ligne.split("for")[1].split("in")[0].strip()
			xran = ligne.split("(")[1].split(")")[0].strip()
			minRange = xran.split(",")[0].strip()
			maxRange = xran.split(",")[1].strip()
			step = xran.split(",")[2].strip()
			commande = Commande(type = "FOR", variable = variable, minRange = minRange, maxRange = maxRange, step = step, numTab = numberOfTabulations)
		elif(type == "ENDLINE"):
			commande = Commande(type = "ENDLINE", numTab = numberOfTabulations)
		elif(type == "ENDTAB"):
			numberOfTabulations = numberOfTabulations - 1
			commande = Commande(type = "ENDTAB", numTab = numberOfTabulations)
		elif(type == "PYTHON"):
			commande = Commande(type = "PYTHON", numTab = numberOfTabulations, text = ligne, incrementer = isIncrementer)

		self.listeCommande.insert(index, commande)

		self.miseAJourTabulation()

		return commande

	def deleteCommand(self, index):
		if(self.listeCommande[index].getType() == "ENDFILE"):
			return False
		del self.listeCommande[index]
		self.miseAJourTabulation()
		return True

	def miseAJourTabulation(self):
		tabulation = 0
		for commande in self.listeCommande:
			commande.setTab(tabulation)
			if(commande.getType() == "ENDTAB"):
				tabulation = tabulation - 1
			if(commande.getType() == "FOR"):
				tabulation = tabulation + 1
			if(commande.getType() == "PYTHON" and commande.isIncrementer()):
				tabulation = tabulation + 1
			if(tabulation < 0):
				tabulation = 0

		return

	def getCommands(self):
		return self.listeCommande

	def getCommand(self, index):
		return self.listeCommande[index]

