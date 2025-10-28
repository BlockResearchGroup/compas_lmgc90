# -*- coding: iso-8859-1 -*-

import os, sys
from localisation import *

CHIPY = "Data.ChiPy"
EXT = ""
if(os.name == "nt"):
	EXT = ".dll"
elif(os.name == "posix"):
	EXT = ".so"

def readLibrary(fromCatalog = True):
	if(fromCatalog == False):
		try:
			exec("import " + CHIPY)
		except:
			CHIPYPATH = CHIPY.replace(".", os.sep)
			print ERR_LIB %(CHIPYPATH + EXT)
			sys.exit(0)

	ModuleLoader.getInstance(fromCatalog)
	FunctionLoader.getInstance(fromCatalog)
	ArgumentsLoader.getInstance(fromCatalog)
	if(fromCatalog == False):
		writeCatalog()
	return

def writeCatalog():
	loader = ArgumentsLoader.getInstance()
	dictionnary = loader.getDictionnary()

	modules = dictionnary.keys()

	fichier = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
	fichier = fichier + "\n"
	fichier = fichier + "<Library>\n"

	for module in modules:
		fichier = fichier + "\t<Module>\n"
		fichier = fichier + "\t\t<Name>" + module + "</Name>\n"
		
		functions = dictionnary[module].keys()
		for function in functions:
			fichier = fichier + "\t\t<Function>\n"
			fichier = fichier + "\t\t\t<Name>" + function + "</Name>\n"

			reqArgs = dictionnary[module][function]["REQUIRED"]
			optArgs = dictionnary[module][function]["OPTIONAL"]

			for arg in reqArgs:
				fichier = fichier + "\t\t\t<Required>" + arg + "</Required>\n"
			for arg in optArgs:
				fichier = fichier + "\t\t\t<Optional>" + arg + "</Optional>\n"

			fichier = fichier + "\t\t</Function>\n"
		fichier = fichier + "\t</Module>\n"
	fichier = fichier + "</Library>\n"

	loader = Options.getInstance()
	chemin = loader.getOption("INSTALLPATH")

	try:
		fic = open(chemin + "Data" + os.sep + "catalog.xml", "w")
	except:
		pass
	else:
		fic.write(fichier)
		fic.close()
	return

class Options:
	_instance = None
	_intern = False

	_dictionnary = {}
	_bgdictionnary = {}

	_options = {}

	def __init__(self):
		self.ModuleList = None

		if(self.__class__._intern == False):
			print WARN_PLEASE_USE %"'Options.getInstance()'"
			return

		filePath = __file__
		if(__name__ != "__main__"):
			# On split le chemin sur le nom du fichier (on ne garde donc que la partie "repertoires" du chemin
			# et on selectionne [0] ([1] est une chaine vide par logique)
			# /home0/xxx/ChiPyGUI/loader.py  ->  ["/home0/xxx/ChiPyGUI/", ""]
			leRep = ""
			leNom = __name__

			if(__name__.find(".") > 0):
				leNom = __name__.split(".")[-1].strip()
				leRep = __name__.split(".")[0].strip()

			EDITORPATH = filePath.split(__name__ + ".py")[0]
			if(leRep != ""):
				EDITORPATH = EDITORPATH.split(leRep)[0]			
		else:
			# Theoriquement impossible de passer ici. Ce module ne sera jamais executé en tant que main
			EDITORPATH = os.getenv("CHIPYGUI_INSTALL")

		if(EDITORPATH == "" or EDITORPATH == None):
			print WARN_PLEASE_SET
			return

		if(EDITORPATH.endswith(os.sep) == False):
			EDITORPATH = EDITORPATH + os.sep

		self.__class__._options["INSTALLPATH"] = EDITORPATH
		self.__class__._options["LANGUAGE"] = "fr"
		self.__class__._options["LISTBOXMODE"] = "FuncMod"
		self.__class__._options["EXPLORATEUR"] = "firefox"
		self.__class__._options["DOC"] = ""

		self.__class__._dictionnary["validColor"] = "Darkgreen"
		self.__class__._dictionnary["optionalColor"] = "Orange"
		self.__class__._dictionnary["errorColor"] = "Red"
		self.__class__._dictionnary["internalColor"] = "Blue"
		self.__class__._dictionnary["noexistColor"] = "White"
		self.__class__._dictionnary["ignoreColor"] = "Grey"
		self.__class__._dictionnary["warningColor"] = "Black"

		self.__class__._bgdictionnary["validColor"] = ""
		self.__class__._bgdictionnary["optionalColor"] = ""
		self.__class__._bgdictionnary["errorColor"] = ""
		self.__class__._bgdictionnary["internalColor"] = ""
		self.__class__._bgdictionnary["noexistColor"] = "Black"
		self.__class__._bgdictionnary["warningColor"] = "Orange"
		self.__class__._bgdictionnary["ignoreColor"] = ""

		try:
			fic = open(EDITORPATH + "Data" + os.sep + "options.dat", "r")
		except:
			pass
		else:
			line = fic.readline()
			while(line != ""):
				if(line.startswith("COLOR")):
					liste = line.split(":")
					nom = liste[1].strip()
					color = liste[2].strip()
					self.__class__._dictionnary[nom] = color
				elif(line.startswith("BGCOLOR")):
					liste = line.split(":")
					nom = liste[1].strip()
					color = liste[2].strip()
					self.__class__._bgdictionnary[nom] = color
				else:
					liste = line.split(":")
					key = liste[0].strip()
					value = liste[1].strip()
					self.__class__._options[key] = value
				line = fic.readline()

			fic.close()
		
		return

	def saveOptions(self):
		EDITORPATH = self.getOption("INSTALLPATH")

		try:
			fic = open(EDITORPATH + "Data" + os.sep + "options.dat", "w")
		except:
			pass
		else:
			for key in self.__class__._dictionnary.keys():
				fic.write("COLOR:" + key + ":" + self.__class__._dictionnary[key] + "\n")
			for key in self.__class__._bgdictionnary.keys():
				fic.write("BGCOLOR:" + key + ":" + self.__class__._bgdictionnary[key] + "\n")
			for key in self.__class__._options.keys():
				if(key != "INSTALLPATH"):
					fic.write(key + ":" + self.__class__._options[key] + "\n")
			fic.close()

		return

	def getOption(self, key):
		if(self.__class__._options.has_key(key)):
			return self.__class__._options[key]
		return

	def setOption(self, key, value):
		self.__class__._options[key] = value
		return

	def set(self, key, value):
		self.__class__._dictionnary[key] = value
		return

	def get(self, key):
		if(self.__class__._dictionnary.has_key(key)):
			return self.__class__._dictionnary[key]
		return False

	def bgset(self, key, value):
		self.__class__._bgdictionnary[key] = value
		return

	def bgget(self, key):
		if(self.__class__._bgdictionnary.has_key(key)):
			return self.__class__._bgdictionnary[key]
		return False

	def getInstance(self):
		if(self._instance == None):
			self._intern = True
			self._instance = Options()
			self._intern = False
		return self._instance

	getInstance = classmethod(getInstance)


class Macro:
	def __init__(self):
		self.lignes = []
		return

	def addLine(self, line):
		self.lignes.append(line)
		return

	def getLines(self):
		return self.lignes

class MacroLoader:
	_instance = None
	_intern = False

	_dictionnary = {}

	def __init__(self):
		self.ModuleList = None

		if(self.__class__._intern == False):
			print WARN_PLEASE_USE %"'MacroLoader.getInstance()'"
			return

		EDITORPATH = Options.getInstance().getOption("INSTALLPATH")

		MACROPATH = EDITORPATH + "Macros" + os.sep

		self.loadDir(MACROPATH)
		return

	def loadDir(self, path):
		liste = os.listdir(path)
		for fichier in liste:
			if(fichier.startswith(".")):
				continue
			if(fichier.endswith(".mac") == False):
				continue

			## C'est un fichier macro !
			valide, name, macro = self.readFile(path, fichier)
			if(valide):
				finalName = name
				num = 0
				while(finalName in self.__class__._dictionnary.keys()):
					finalName = name + str(num)
					num = num + 1
				self.__class__._dictionnary[finalName] = macro
		return

	def loadMacro(self, path):
		fichier = path.split(os.sep)[-1].strip()
		leChemin = path.split(os.sep)[:-1]
		chemin = ""
		for bout in leChemin:
			chemin = chemin + bout + os.sep

		if(fichier.startswith(".")):
			return
		if(fichier.endswith(".mac") == False):
			return

		valide, name, macro = self.readFile(chemin, fichier)
		finalName = ""

		if(valide):
			finalName = name
			num = 0
			while(finalName in self.__class__._dictionnary.keys()):
				finalName = name + str(num)
				num = num + 1
			self.__class__._dictionnary[finalName] = macro
		return finalName

	def readFile(self, chemin, fichier):
		name = ""
		valide = True
		macro = Macro()
		fic = open(chemin + fichier, "r")

		line = fic.readline()
		if(line.startswith("##MACRO NAME : ")):
			name = line.split(":")[1].strip()
		else:
			valide = False

		if(valide):
			line = fic.readline()
			while(line != ""):
				macro.addLine(line)
				line = fic.readline()

		fic.close()
		return valide, name, macro

	def getNames(self):
		return self.__class__._dictionnary.keys()

	def getMacro(self, nom):
		if(self.__class__._dictionnary.has_key(nom)):
			return self.__class__._dictionnary[nom]
		return

	def getInstance(self):
		if(self._instance == None):
			self._intern = True
			self._instance = MacroLoader()
			self._intern = False
		return self._instance

	getInstance = classmethod(getInstance)

class ArgumentsLoader:
	_instance = None
	_intern = False

	_dictionnary = {}

	def __init__(self, fromCatalog):
		if(self.__class__._intern == False):
			print WARN_PLEASE_USE %"'ArgumentsLoader.getInstance()'"
			return

		if(fromCatalog):
			reader = XMLReader.getInstance()
			modules = reader.getModules()

			for module in modules:
				fonctions = reader.getFunctionsOf(module)
				dictFunc = {}

				for fonction in fonctions:
					dictArg = {}

					reqArgs = reader.getRequiredArgsOf(module, fonction)
					optArgs = reader.getOptionalArgsOf(module, fonction)
					dictArg["REQUIRED"] = reqArgs
					dictArg["OPTIONAL"] = optArgs

					dictFunc[fonction] = dictArg

				self.__class__._dictionnary[module] = dictFunc

		else:
			exec("import " + CHIPY + " as ChiPy")

			loader = ModuleLoader.getInstance()
			listeModule = loader.getModules()

			loader = FunctionLoader.getInstance()
			for module in listeModule:
				listeFonction = loader.getFunctionsOf(module)
				dictFunc = {}
				for func in listeFonction:
					dictArg = {}

					exec("fonction = ChiPy." + module + "." + func)
					listReq, listOpt, listeRet = self.findArgs(fonction)

					dictArg["REQUIRED"] = listReq
					dictArg["OPTIONAL"] = listOpt
					dictArg["RESULT"] = listeRet

					dictFunc[func] = dictArg

				self.__class__._dictionnary[module] = dictFunc

		return

	def getDictionnary(self):
		return self.__class__._dictionnary

	def getRequiredArgsOf(self, module, fonction):
		if(self.__class__._dictionnary.has_key(module)):
			if(self.__class__._dictionnary[module].has_key(fonction)):
				return self.__class__._dictionnary[module][fonction]["REQUIRED"]
		return []

	def getOptionalArgsOf(self, module, fonction):
		if(self.__class__._dictionnary.has_key(module)):
			if(self.__class__._dictionnary[module].has_key(fonction)):
				return self.__class__._dictionnary[module][fonction]["OPTIONAL"]
		return []

	def getResultArgsOf(self, module, fonction):
		if(self.__class__._dictionnary.has_key(module)):
			if(self.__class__._dictionnary[module].has_key(fonction)):
				return self.__class__._dictionnary[module][fonction]["RESULT"]
		return []

	def getArgsOf(self, module, fonction):
		if(self.__class__._dictionnary.has_key(module)):
			if(self.__class__._dictionnary[module].has_key(fonction)):
				return self.__class__._dictionnary[module][fonction]
		return {}

	def findArgs(self, func):
		req = False
		opt = False
		ret = False
		listeReq = []
		listeOpt = []
		listeRet = []

		funcDoc = func.__doc__
		listeDoc = funcDoc.split("\n")
		for ligne in listeDoc:
			if(ligne.strip() == ""):
				continue
			if(ligne.startswith("Required arguments")):
				req = True
				opt = False
				ret = False
				continue
			if(ligne.startswith("Optional arguments")):
				opt = True
				req = False
				ret = False
				continue
			if(ligne.startswith("Return objects")):
				req = False
				ret = True
				opt = False
				continue
			if(req == True):
				listeReq.append(ligne.split(":")[0].strip())
				continue
			if(opt == True):
				listeOpt.append(ligne.split(":")[0].strip())
			if(ret == True):
				listeRet.append(ligne.split(":")[0].strip())
	
		return listeReq, listeOpt, listeRet

	def getInstance(self, fromCatalog = True):
		if(self._instance == None):
			self._intern = True
			self._instance = ArgumentsLoader(fromCatalog)
			self._intern = False
		return self._instance

	getInstance = classmethod(getInstance)

class FunctionLoader:
	_instance = None
	_intern = False

	_dictionnary = {}
	_reverseDictionnary = {}
	_functionList = []

	def __init__(self, fromCatalog):
		self.ModuleList = None

		if(self.__class__._intern == False):
			print WARN_PLEASE_USE %"'FunctionLoader.getInstance()'"
			return

		loader = ModuleLoader.getInstance()
		listeModule = loader.getModules()

		if(fromCatalog):
			reader = XMLReader.getInstance()
			for module in listeModule:
				fonctions = reader.getFunctionsOf(module)
				fonctions.sort()
				self.__class__._dictionnary[module] = fonctions

				for func in fonctions:
					if(func not in self.__class__._functionList):
						self.__class__._functionList.append(func)

					if(self.__class__._reverseDictionnary.has_key(func)):
						liste = self.__class__._reverseDictionnary[func]
					else:
						liste = []

					if(module not in liste):
						liste.append(module)
						self.__class__._reverseDictionnary[func] = liste
		else:
			exec("import " + CHIPY + " as ChiPy")
			for module in listeModule:
				exec("listeFonctions = dir(ChiPy." + module + ")")
				listeFonctions.sort()
				self.__class__._dictionnary[module] = listeFonctions

				for func in listeFonctions:
					if(func not in self.__class__._functionList):
						self.__class__._functionList.append(func)

					if(self.__class__._reverseDictionnary.has_key(func)):
						liste = self.__class__._reverseDictionnary[func]
					else:
						liste = []

					if(module not in liste):
						liste.append(module)
						self.__class__._reverseDictionnary[func] = liste

		self.__class__._functionList.sort()
		return

	def hasFunction(self, module, func):
		if(self.__class__._dictionnary.has_key(module) and func in self.__class__._dictionnary[module]):
			return True
		return False

	def getAllFunctions(self):
		return self.__class__._functionList

	def getFunctionsOf(self, module):
		if(self.__class__._dictionnary.has_key(module)):
			return self.__class__._dictionnary[module]

		print ERR_MODULE_INEXISTANT %module
		return []

	def getModulesOf(self, fonction):
		if(self.__class__._reverseDictionnary.has_key(fonction)):
			return self.__class__._reverseDictionnary[fonction]

		print ERR_FONCTION_INEXISTANT %fonction
		return []

	def getInstance(self, fromCatalog = True):
		if(self._instance == None):
			self._intern = True
			self._instance = FunctionLoader(fromCatalog)
			self._intern = False
		return self._instance

	getInstance = classmethod(getInstance)

class ModuleLoader:
	_instance = None
	_intern = False

	def __init__(self, fromCatalog):
		self.ModuleList = None

		if(self.__class__._intern == False):
			print WARN_PLEASE_USE %"'ModuleLoader.getInstance()'"
			return

		if(fromCatalog):
			## Recuperation de la liste des modules a partir du catalog xml
			reader = XMLReader.getInstance()
			self.ModuleList = reader.getModules()
		else:
			## Recuperation de la liste des modules a partir de la librairie .so
			self.ModuleList = []
			exec("import " + CHIPY + " as ChiPy")
			
			liste = dir(ChiPy)
			for module in liste:
				if(module.startswith("_") == False):
					self.ModuleList.append(module)

		self.ModuleList.sort()
		return

	def hasModule(self, module):
		if(module in self.ModuleList):
			return True
		return False

	def getModules(self):
		if(self.ModuleList != None):
			return self.ModuleList
		print "PLEASE USE 'ModuleLoader.getInstance()'"
		return

	def getInstance(self, fromCatalog = True):
		if(self._instance == None):
			self._intern = True
			self._instance = ModuleLoader(fromCatalog)
			self._intern = False
		return self._instance

	getInstance = classmethod(getInstance)

class XMLReader(object):
	_instance = None
	_intern = False
	_dictionnary = {}

	def __init__(self):
		if(self.__class__._intern == False):
			print WARN_PLEASE_USE %"'XMLReader.getInstance()'"
			return

		loader = Options.getInstance()
		chemin = loader.getOption("INSTALLPATH")

		dictionnary = {}

		try:
			fic = open(chemin + "Data" + os.sep + "catalog.xml", "r")
			line = fic.readline()
	
			module = False
			function = False

			moduleName = ""
			functionName = ""
			listOpt = []
			listReq = []

			while(line != ""):
				line = line.strip()
				if(line == ""):
					pass
				elif(line.startswith("<Module>")):
					module = True
				elif(line.startswith("<Function>")):
					listOpt = []
					listReq = []
					function = True
				elif(line.startswith("<Name>")):
					if(function and module):
						functionName = line.split("</Name>")[0].split("<Name>")[1].strip()
						dictionnary[moduleName][functionName] = {}
					elif(module):
						moduleName = line.split("</Name>")[0].split("<Name>")[1].strip()
						dictionnary[moduleName] = {}
				elif(line.startswith("<Required>")):
					if(function and module):
						argName = line.split("</Required>")[0].split("<Required>")[1].strip()
						listReq.append(argName)
				elif(line.startswith("<Optional>")):
					if(function and module):
						argName = line.split("</Optional>")[0].split("<Optional>")[1].strip()
						listOpt.append(argName)
				elif(line.startswith("</Function>")):
					function = False
					dictionnary[moduleName][functionName]["OPTIONAL"] = listOpt
					dictionnary[moduleName][functionName]["REQUIRED"] = listReq
				elif(line.startswith("</Module>")):
					module = False

				line = fic.readline()

		except:
			print WARN_NO_CATALOG
			print WARN_CONTINUE
			choix = raw_input()
			if(choix.lower() not in WARN_YES):
				sys.exit(0)

		self.__class__._dictionnary = dictionnary
		return

	def getDictionnary(self):
		return self.__class__._dictionnary

	def getModules(self):
		return self.__class__._dictionnary.keys()

	def getFunctionsOf(self, module):
		return self.__class__._dictionnary[module].keys()

	def getRequiredArgsOf(self, module, fonction):
		return self.__class__._dictionnary[module][fonction]["REQUIRED"]

	def getOptionalArgsOf(self, module, fonction):
		return self.__class__._dictionnary[module][fonction]["OPTIONAL"]

	def getInstance(self):
		if(self._instance == None):
			self._intern = True
			self._instance = XMLReader()
			self._intern = False
		return self._instance

	getInstance = classmethod(getInstance)


class DOCReader(object):
	_instance = None
	_intern = False
	_dictionnary = {}

	def __init__(self):
		if(self.__class__._intern == False):
			print WARN_PLEASE_USE %"'DOCReader.getInstance()'"
			return

		loader = Options.getInstance()
		chemin = loader.getOption("INSTALLPATH")

		dictionnary = {}

		try:
			fic = open(chemin + "Data" + os.sep + "doc.xml", "r")
			line = fic.readline()
	
			module = False
			function = False
			param = False
			doc = False

			nbChar = 0

			moduleName = ""
			functionName = ""
			paramName = ""
			listOpt = []
			listReq = []

			while(line != ""):
				oriLine = line
				line = line.strip()
				if(line == ""):
					pass
				elif(line.startswith("<Module>")):
					module = True
				elif(line.startswith("<Function>")):
					function = True
				elif(line.startswith("<Param>")):
					param = True
				elif(line.startswith("<Name>")):
					if(module and function and param):
						paramName = line.split("</Name>")[0].split("<Name>")[1].strip()
						dictionnary[moduleName][functionName][paramName] = []
					elif(function and module):
						functionName = line.split("</Name>")[0].split("<Name>")[1].strip()
						dictionnary[moduleName][functionName] = {}
					elif(module):
						moduleName = line.split("</Name>")[0].split("<Name>")[1].strip()
						dictionnary[moduleName] = {}
				elif(line.startswith("<Doc>")):
					nbChar = len(oriLine) - len(line) -1
					doc = True
				elif(line.startswith("</Doc>")):
					doc = False
				elif(line.startswith("</Param>")):
					param = False
				elif(line.startswith("</Function>")):
					function = False
				elif(line.startswith("</Module>")):
					module = False
				elif(module and function and param and doc):
					dictionnary[moduleName][functionName][paramName].append(oriLine[nbChar:])

				line = fic.readline()

		except:
			pass

		self.__class__._dictionnary = dictionnary
		return

	def getDocOf(self, module, function, arg):
		if(self.__class__._dictionnary.has_key(module)):
			if(self.__class__._dictionnary[module].has_key(function)):
				if(self.__class__._dictionnary[module][function].has_key(arg)):
					return self.__class__._dictionnary[module][function][arg]
		return None

	def getInstance(self):
		if(self._instance == None):
			self._intern = True
			self._instance = DOCReader()
			self._intern = False
		return self._instance

	getInstance = classmethod(getInstance)
