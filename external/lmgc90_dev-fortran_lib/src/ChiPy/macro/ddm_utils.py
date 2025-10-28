
import os
import shutil
import glob

from . import lmgc90

def create_working_directory(nb_sdm):
  """
  Create the needed DATBOX directories for
  a number of subdomain.
  """

  working_dir = lmgc90.overall_GetWorkingDirectory()

  for directory in glob.glob( os.path.join(working_dir,'DDM_WD_*') ):
     shutil.rmtree( directory )
  
  for i in range(nb_sdm):
     working_directory=os.path.join(working_dir,"DDM_WD_" + "%(#)05d" % {"#": i+1})
     os.mkdir(working_directory)
     shutil.copytree(os.path.join(working_dir,"DATBOX"), os.path.join(working_directory,"DATBOX"))
     os.mkdir(os.path.join(working_directory,"OUTBOX"))
     os.mkdir(os.path.join(working_directory, "DISPLAY"))
     os.mkdir(os.path.join(working_directory, "POSTPRO"))

def concatenate_OUTBOX():
  """
  Concatenate contents of DDM_WD_*/OUTBOX directories in a single
  OUTBOX directory
  """

  working_dir = lmgc90.overall_GetWorkingDirectory()

  nb_SDM=0
  
  # Calcul du nombre de sous-domaines pour lesquels des sorties
  # .OUT sont effectuees en parallele.
  for root, dirs, files in os.walk(working_dir, topdown=False):
      for myfile in dirs:
          if not myfile.startswith("DDM_WD_"):
             continue
          T=myfile.split("_")
          try:
             n=int(T[2])
          except Exception:
             continue
          nb_SDM=max(nb_SDM,n)
  
  import glob
  list_file  = glob.glob( os.path.join(working_dir,"DDM_WD_00001","OUTBOX","DOF.*" ) )
  list_file += glob.glob( os.path.join(working_dir,"DDM_WD_00001","OUTBOX","Vloc_Rloc.*" ) )
  list_file += glob.glob( os.path.join(working_dir,"DDM_WD_00001","OUTBOX","GPV.*" ) )
  begin = len( os.path.join(working_dir,"DDM_WD_00001","OUTBOX") ) + 1
  for f in list_file :
      filename = f[begin:]
      #Concatenation des filename par sous-domaines en un seul filename
      my_inputs=[]
      for j in range(nb_SDM):
          my_inputs.append(os.path.join("DDM_WD_"+"%(#)05d" % {"#": j+1},"OUTBOX",filename))
      
      my_output=open(os.path.join(working_dir,"OUTBOX",filename),"w")
      for my_file in my_inputs:
          my_input=open(my_file, "r")
      
          for line in my_input:
              my_output.write(line)
          my_input.close()
      
      my_output.close()

def concatenate_DISPLAY():
  """
  Create a paraview pvd format file in /DISPLAY 
  from all vtu or vtp display files located in /DDM_WD_*/DISPLAY
  """

  try:
    import vtk
    is_vtk_display = True
  except ImportError:
    is_vtk_display = False

  import glob
  
  if not is_vtk_display:
    return

  working_dir = lmgc90.overall_GetWorkingDirectory()

  # Calcul du nombre de pas de temps pour lesquels des sorties
  # .vtu/.vtp sont effectuees en parallele.
  nb_DOF = len( glob.glob("DDM_WD_00001/DISPLAY/mecafe_*") )
  
  # Calcul du nombre de sous-domaines pour lesquels des sorties
  # paraview sont effectuees en parallele.
  nb_SDM = len( glob.glob("DDM_WD_*") )
      
  #Recherche des differents fichiers a concatener : mecafe, inters, etc.
  my_fics = []
  pvd_list = glob.glob("DDM_WD_00001/DISPLAY/*.pvd")
  for f in pvd_list :
     my_fics.append( f.replace("DDM_WD_00001/DISPLAY/","").replace(".pvd","") )

  for fic in my_fics :    
     # Calcul du nombre de pas de temps pour lesquels des sorties
     # .vtu/.vtp sont effectuees en parallele.
     nb_out = len( glob.glob("DDM_WD_00001/DISPLAY/"+fic+"_*") )
     if nb_out > 0 :
        my_inputs=[]
        my_output=open(os.path.join(working_dir,"DISPLAY",fic+".pvd"),"w")
        my_output.write("<?xml version=\"1.0\"?>\n")
        my_output.write("<VTKFile type=\"Collection\" version=\"0.1\" byte_order=\"LittleEndian\">\n")  
        my_output.write("<Collection>\n")
        extension = "." + glob.glob("DDM_WD_00001/DISPLAY/"+fic+"_*")[0].split(".")[1]
        for j in range(nb_SDM):
           my_file = os.path.join(working_dir,"DDM_WD_"+"%(#)05d" % {"#": j+1},'DISPLAY',fic+".pvd")
           my_input=open(my_file, "r")
           iline=0
           for line in my_input:
              iline += 1
              if iline <=2:
                 #rien a faire
                 pass
              elif iline ==3:
                 #on enleve les 12 premiers caracteres
                 line = line[12:]
                 line = line.replace("./","../DDM_WD_"+"%(#)05d" % {"#": j+1} + "/DISPLAY/")
                 my_output.write(line)
              elif iline > nb_DOF+2:
                 #rien a faire
                 pass
              else:
                 #on copie la ligne
                 line = line.replace("./","../DDM_WD_"+"%(#)05d" % {"#": j+1} + "/DISPLAY/")
                 my_output.write(line)
           # Pour concatener les fichiers meme si le calcul s'est arrete brutalement et que les fichier .pvd 
           # de chaque processeur ne sont pas generes.
           if( iline < 2 ):
              for k in range(nb_out) :
                 f = my_file.replace(".pvd", "")
                 line = '<DataSet timestep="' + "%(#)05d" % {"#": k} + '.0" group="" part="0" file=".' + f + '_' + str(k+1) + extension + '"/>\n'
                 my_output.write(line)
        # Pour generer des fichiers .pvtu et .pvtp a partir des fichiers .vtu et .vtp
        nb_step = len( glob.glob("DDM_WD_00001/DISPLAY/"+fic+"_*"+extension) )
        for k in range(nb_step):
           vtxFile = None
           pvtxFile = None
           if( extension == ".vtu" ) :
              vtxFile = vtk.vtkXMLUnstructuredGridReader()
              pvtxFile = vtk.vtkXMLPUnstructuredGridWriter()
           elif( extension == ".vtp" ) :
              vtxFile = vtk.vtkXMLPolyDataReader()
              pvtxFile = vtk.vtkXMLPPolyDataWriter()
           vtxFile.SetFileName("./DDM_WD_00001/DISPLAY/"+fic+"_"+str(k+1)+extension)
           
           pvtxFile.SetNumberOfPieces(nb_SDM)
           pvtxFile.SetFileName("./DISPLAY/TEMP.tmp")
           pvtxFile.SetInputConnection( vtxFile.GetOutputPort() )
           pvtxFile.Write()
           
           orig = open("./DISPLAY/TEMP.tmp","r")
           dest = open("./DISPLAY/"+fic+"_"+str(k+1)+extension.replace(".",".p"),'w')
           
           text = orig.read()
           for j in range(nb_SDM):
              piece_orig = "TEMP_"+str(j)+extension
              domain = int(piece_orig.split("_")[1].split(".")[0]) +1
              piece_new = "../DDM_WD_"+"%(#)05d" % {"#": domain} + "/DISPLAY/"+fic+"_" + str(k+1) + extension
              text = text.replace(piece_orig,piece_new)
           dest.write(text)
           orig.close()
           dest.close()
           os.remove( "./DISPLAY/TEMP_0"+extension) 
           os.remove( "./DISPLAY/TEMP.tmp" )       
        my_output.write("</Collection>")
        my_output.write("</VTKFile>")
        my_output.close()

