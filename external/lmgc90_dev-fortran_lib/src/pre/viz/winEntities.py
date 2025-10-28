from tkinter import *
from vtk     import *
from vtk.tk.vtkTkRenderWidget import *


class winBodies2D():
    """ classe winBodies2D()
        __init__
        mainloop
        quit
    """
    def __init__(self,entities,height=400,width=400):
        """ __init__(self,entities,height=400,width=400,factor=200)
            permet en fonctions d un conteneur d entites ayant une methode
            'vtkObject' definie de tracer les entites dans la fenetre principale
        """
        self._tk = root = Tk()
        ren=vtkRenderer()
        winWidget = vtkTkRenderWidget(root) #,width=width,height=height)
        winWidget.pack(expand=YES,fill=BOTH)
        renWin = winWidget.GetRenderWindow()
        renWin.AddRenderer(ren)

        for entity in entities:
            inside = entity.vtkObject(1000.,width/2,height/2)
            ren.AddActor2D(inside)

        button = Button(root,text="Quit",command=self.quit)
        
        button.pack(expand=YES,fill=BOTH)    

        self.mainloop()
        
    def mainloop(self):
        self._tk.mainloop()

    def quit(self):
        self._tk.destroy()

        
        
