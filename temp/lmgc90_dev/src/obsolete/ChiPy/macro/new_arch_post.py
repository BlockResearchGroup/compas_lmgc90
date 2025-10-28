from chipy import *
from vtk   import *
from numpy import *

############################
# model_handles

import numpy

from .new_arch_post_utils import *

def pushMdlHdl(Append):

   #print 'managing: model_handles'
   nbm = model_handler_getNb() 
   #print nbm
 
   for i_mdl_hdl in range(1,nbm+1,1):
     
     mdl_type = model_handler_getModelType(i_mdl_hdl)

     # getting coordinates
     if mdl_type == 'therMAILx':
       coor = model_handler_getCoordinates(i_mdl_hdl,'ref')
     else:
       coor = model_handler_getCoordinates(i_mdl_hdl,'now')

     dim    = numpy.size(coor,1)
     ele    = model_handler_getConnectivities(i_mdl_hdl)
     ug     = vtk.vtkUnstructuredGrid()
     points = vtk.vtkPoints()

     if dim==2 :
       for x, y in coor:
         points.InsertNextPoint(x, y, 0.0)
         # so dirty...
         if mdl_type=='RBDY2' or mdl_type=='RBDY3':
           break
     elif dim==3:
       for x, y, z in coor:
         points.InsertNextPoint(x, y, z)
         # so dirty...
         if mdl_type=='RBDY2' or mdl_type=='RBDY3':
           break
     else :
       print('wrong dimension')
       sys.exit()

     ug.SetPoints(points)


     field_list = eval("getFieldsToWrite_"+mdl_type+"(i_mdl_hdl, dim)")

     for f in field_list:
       ug.GetPointData().AddArray(f)

     #print ele[0]
     idx=1
     for j in range(ele[0]):

       #print j,idx,ele[idx]
       if dim==2 :
         if ele[idx] == 4 :
           xx = vtkQuad()
         elif ele[idx] == 8 :
           xx = vtkQuadraticQuad()
         elif ele[idx] == 3 :
           xx = vtkTriangle()
         elif ele[idx] == 6 :
           xx = vtkQuadraticTriangle()
         elif ele[idx] == 2 :
           xx = vtkLine()
         elif ele[idx] == 1 :
           xx = vtkVertex()
         else:
           print('unable to draw element with size ',ele[idx],' in 2D')
           sys.exit()
       elif dim ==3 :
         if ele[idx] == 4 :
           xx = vtkTetra()
         elif ele[idx] == 10 :
           xx = vtkQuadraticTetra()
         elif ele[idx] == 6 :
           xx = vtkWedge()
         elif ele[idx] == 12 :
           xx = vtkQuadraticWedge()
         elif ele[idx] == 8 :
           xx = vtkHexahedron()
         elif ele[idx] == 20 :
           xx = vtkQuadraticHexahedron()
         elif ele[idx] == 2 :
           xx = vtkLine()
         elif ele[idx] == 1 :
           xx = vtkVertex()
         else:
           print('unable to draw element with size ',ele[idx],' in 3D')
           sys.exit()

       #print ele[idx+1:idx+1+ele[idx]]
       for k in range(ele[idx]):  
         xx.GetPointIds().SetId(k, ele[idx+1+k]-1)
       ug.InsertNextCell(xx.GetCellType(),xx.GetPointIds())

       idx += 1 + ele[idx]

     Append.AddInput(ug)

def pushContactors(Append,nb_points_outlines, outlines):

   # on recupere le pointeur sur outlines
   for itact in arange(1,size(nb_points_outlines)):

     #
     # ca collecte
     points = vtkPoints()
     cells  = vtkCellArray()

     Ids    = vtkFloatArray()
     Ids.SetNumberOfComponents(1)
     Ids.SetName("Ids")

     #print 'MAJ'
     vertex = outlines[nb_points_outlines[itact-1]:nb_points_outlines[itact],:]

     num_point = 0
     cell_ids=[]
     for x,y in vertex:
       points.InsertPoint(num_point,x,y,0)
       cell_ids.append(num_point)
       num_point+=1
        
     cell = vtkPolygon()		
     cell.GetPointIds().SetNumberOfIds(len(cell_ids))
        
     for i,j in enumerate(cell_ids):
        cell.GetPointIds().SetId(i,j)

     cells.InsertNextCell(cell)
     #ibdyty = tact[1][itact-1][0]
     ibdyty = itact
     Ids.InsertNextTuple1(float(ibdyty))

     # ca pousse dans un polydata
     polydata = vtkPolyData()
     polydata.SetPoints(points)
     polydata.SetPolys(cells)
     polydata.GetCellData().AddArray(Ids)

     Append.AddInput(polydata)           

def writeMdlHdlToVTK(fichier,fid):

   vtuFile = vtkXMLUnstructuredGridWriter()
   vtuFile.SetFileName(fichier)

   AppendAll=vtkAppendFilter()

   pushMdlHdl(AppendAll)

   vtuFile.SetInput(AppendAll.GetOutput())

   vtuFile.Write()

   time=TimeEvolution_GetTime()
   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)

def writeContactorsToVTK(fichier,fid,nb_points_outlines,outlines):

   vtuFile = vtkXMLUnstructuredGridWriter()
   vtuFile.SetFileName(fichier)

   AppendAll=vtkAppendFilter()

   pushContactors(AppendAll,nb_points_outlines,outlines)

   vtuFile.SetInput(AppendAll.GetOutput())

   vtuFile.Write()

   time=TimeEvolution_GetTime()
   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)


#######################################
# ecriture collections

def startCollection(fichier):
  header= '<?xml version="1.0"?>\n<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">\n<Collection>'
  fid = open(fichier,'w')
  fid.write(header) 
  return fid

def stopCollection(fid):
  closing = '</Collection>\n</VTKFile>'
  fid.write(closing)
  fid.close()

