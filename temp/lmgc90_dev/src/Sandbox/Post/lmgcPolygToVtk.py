from chipy import *
from vtk   import *

def writePolygToVTK(fichier):
       vtpFile = vtkXMLPolyDataWriter()
       vtpFile.SetFileName(fichier)
       points     = vtkPoints()
       cells      = vtkCellArray()

       Ids = vtkFloatArray()
       Ids.SetNumberOfComponents(1)
       Ids.SetName("Ids")

       num_points = 0

       for ipolyg in arange(1,POLYG_GetNbPOLYG()):
           nverts = POLYG_GetNbVertex(int(ipolyg))
           vertex = POLYG_GetVertex(int(ipolyg),int(nverts*2))
           vertex.shape = (nverts,2)
           cell_ids=[]
           for x,y in vertex:
                   points.InsertPoint(num_points,x,y,0)
                   cell_ids.append(num_points)
                   num_points+=1
           
           cell = vtkPolygon()		
           cell.GetPointIds().SetNumberOfIds(len(cell_ids))
           
           for i,j in enumerate(cell_ids):
                   cell.GetPointIds().SetId(i,j)

           cells.InsertNextCell(cell)
           ibdyty = int(POLYG_GetBodyId(int(ipolyg)))
           Ids.InsertNextTuple1(float(ibdyty))
          

       polydata = vtkPolyData()
       polydata.SetPoints(points)
       polydata.SetPolys(cells)
       polydata.GetCellData().AddArray(Ids)

       vtpFile.SetInput(polydata)
       vtpFile.Write()

def writePolyrToVTK(fichier):
       vtpFile = vtkXMLPolyDataWriter()
       vtpFile.SetFileName(fichier)
       points     = vtkPoints()
       cells      = vtkCellArray()

       Ids = vtkFloatArray()
       Ids.SetNumberOfComponents(1)
       Ids.SetName("Ids")

       num_points = 0

       for ipolyr in arange(1,POLYR_GetNb()):
           vertex = POLYR_GetVertexPtr(int(ipolyr))
           cell_ids=[]
           for v in vertex:
                   points.InsertPoint(num_points,v[0],v[1],v[2])
                   cell_ids.append(num_points)
                   num_points+=1
           
           cell = vtkPolygon()		
           cell.GetPointIds().SetNumberOfIds(len(cell_ids))
           
           for i,j in enumerate(cell_ids):
                   cell.GetPointIds().SetId(i,j)

           cells.InsertNextCell(cell)
           ibdyty = int(POLYR_GetBodyId(int(ipolyr)))
           Ids.InsertNextTuple1(float(ibdyty))
          

       polydata = vtkPolyData()
       polydata.SetPoints(points)
       polydata.SetPolys(cells)
       polydata.GetCellData().AddArray(Ids)

       vtpFile.SetInput(polydata)
       vtpFile.Write()

