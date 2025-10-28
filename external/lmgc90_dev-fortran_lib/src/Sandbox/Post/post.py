from chipy import *
from vtk   import *
from numpy import *

def rotateContour(vertex,angle):
  """ tourne un objet autour de 0"""
  c = math.cos(angle)
  s = math.sin(angle)
  i=0
  for x,y in vertex:
    vertex[i,0] = (x*c) - (y*s)
    vertex[i,1] = (x*s) + (y*c)
    i+=1
# pas besoin de return car vertex est un tableau ...
#  return vertex
  
def translateContour(vertex,dx,dy):

  i=0
  for x,y in vertex:
    vertex[i,0] = x + dx
    vertex[i,1] = y + dy
    i+=1
# pas besoin de return car vertex est un tableau ...
#  return vertex

def buildContourJONCx(a1,a2):

   # nb de segments sur le demi arc
   nb = 9
   dtt = math.pi/nb
   vertex=zeros((2*(nb+1),2),dtype=float)
   tt=0.
   for i in xrange(0,nb+1):   
      vertex[i,0]= (a1 + a2*math.sin(tt)) 
      vertex[i,1]=-a2*math.cos(tt)

      vertex[nb+1+i,0]=-1.*(a1 + a2*math.sin(tt)) 
      vertex[nb+1+i,1]= a2*math.cos(tt)

      tt+=dtt
   return vertex

def buildContourDISKx(r):

   # nb de segments sur le demi arc
   nb = 9
   dtt = math.pi/nb
   vertex=zeros((2*nb,2),dtype=float)
   tt=0.
   for i in xrange(0,nb):   
      vertex[i,0]= r*math.sin(tt)
      vertex[i,1]=-r*math.cos(tt)

      vertex[nb+i,0]=-r*math.sin(tt)
      vertex[nb+i,1]= r*math.cos(tt)

      tt+=dtt
   return vertex

def buildFrite(coorx,coory,nx,ny,rf,lr):

   tx= ny
   ty=-nx
   vertex=zeros((4,2),dtype=float)
   vertex[0,0]= coorx - (nx*lr) - (tx*rf*lr)
   vertex[0,1]= coory - (ny*lr) - (ty*rf*lr)
   vertex[1,0]= coorx + (nx*lr) - (tx*rf*lr)
   vertex[1,1]= coory + (ny*lr) - (ty*rf*lr)
   vertex[2,0]= coorx + (nx*lr) + (tx*rf*lr)
   vertex[2,1]= coory + (ny*lr) + (ty*rf*lr)
   vertex[3,0]= coorx - (nx*lr) + (tx*rf*lr)
   vertex[3,1]= coory - (ny*lr) + (ty*rf*lr)

   return vertex


def collectPOLYG(Append):

  for ipolyg in arange(1,POLYG_GetNbPOLYG()+1):

     # ca collecte
     points = vtkPoints()
     cells  = vtkCellArray()
     Ids    = vtkFloatArray()
     Ids.SetNumberOfComponents(1)
     Ids.SetName("Ids")

     num_points = 0

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
           

        # ca pousse dans un polydata
        polydata = vtkPolyData()
        polydata.SetPoints(points)
        polydata.SetPolys(cells)
        polydata.GetCellData().AddArray(Ids)
        polydata.Update()

        Append.AddInput(polydata)           

def collectJONCx(Append):

   for ijoncx in arange(1,JONCx_GetNbJONCx()+1):
        # ca collecte
        points = vtkPoints()
        cells  = vtkCellArray()
        Ids    = vtkFloatArray()
        Ids.SetNumberOfComponents(1)
        Ids.SetName("Ids")

        axes = JONCx_GetShape(int(ijoncx),2)
        vertex = buildContourJONCx(axes[0],axes[1])
        coor = JONCx_GetCoor(int(ijoncx),3)
        rotateContour(vertex,coor[2])
        translateContour(vertex,coor[0],coor[1])

        num_points = 0
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
        ibdyty = int(JONCx_GetBodyId(int(ijoncx)))
        Ids.InsertNextTuple1(float(ibdyty))
           

        # ca pousse dans un polydata
        polydata = vtkPolyData()
        polydata.SetPoints(points)
        polydata.SetPolys(cells)
        polydata.GetCellData().AddArray(Ids)
        polydata.Update()

        Append.AddInput(polydata)           

def collectDISKx(Append):

   nb_diskx=DISKx_GetNbDISKx()
   diskx2rbdy2 = DISKx_GetDISKx2RBDY2(nb_diskx)

   for idiskx in arange(1,nb_diskx+1):
        # ca collecte
        points = vtkPoints()
        cells  = vtkCellArray()
        Ids    = vtkFloatArray()
        Ids.SetNumberOfComponents(1)
        Ids.SetName("Ids")

        rd = DISKx_GetContactorRadius(int(idiskx))
        vertex = buildContourDISKx(rd)
        coor = DISKx_GetContactorCoor(int(idiskx), 3)
        rotateContour(vertex,coor[2])
        translateContour(vertex,coor[0],coor[1])

        num_points = 0
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
        ibdyty = diskx2rbdy2[int(idiskx)-1]
        Ids.InsertNextTuple1(float(ibdyty))

        # ca pousse dans un polydata
        polydata = vtkPolyData()
        polydata.SetPoints(points)
        polydata.SetPolys(cells)
        polydata.GetCellData().AddArray(Ids)
        polydata.Update()

        Append.AddInput(polydata)           

def collectPOLYR(Append):

  for ipolyr in arange(1,POLYR_GetNb()+1):

     # ca collecte
     points = vtkPoints()
     cells  = vtkCellArray()
     Ids    = vtkFloatArray()
     Ids.SetNumberOfComponents(1)
     Ids.SetName("Ids")

     num_points = 0

     vertex = POLYR_GetVertexPtr(int(ipolyg))
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
        ibdyty = int(POLYR_GetBodyId(int(ipolyg)))
        Ids.InsertNextTuple1(float(ibdyty))
           

        # ca pousse dans un polydata
        polydata = vtkPolyData()
        polydata.SetPoints(points)
        polydata.SetPolys(cells)
        polydata.GetCellData().AddArray(Ids)
        polydata.Update()

        Append.AddInput(polydata)           


#######################################
def collectDKDKx(Append,lr):

   nb_DKDKx = DKDKx_GetNbvDKDKx()

   if (nb_DKDKx == 0):
      return

   All = DKDKx_GetAll(6*nb_DKDKx)
   All.shape=(nb_DKDKx,6)

   mean_fn=mean(All[:,4])
   max_fn =max(All[:,4])
   min_fn =min(All[:,4]) 

   # pour eviter de diviser par la suite apres
   if max_fn == 0.: max_fn=1.

   idkdkx=0
   for coorx,coory,nx,ny,fn,ft in All:
        idkdkx+=1

        # on collecte
        points = vtkPoints()
        cells  = vtkCellArray()
        Ids    = vtkFloatArray()
        Ids.SetNumberOfComponents(1)
        Ids.SetName("Ids")
        Rn = vtkFloatArray()
        Rn.SetNumberOfComponents(1)
        Rn.SetName("Rn")
        Rn.InsertNextTuple1(fn);

        Rt = vtkFloatArray()
        Rt.SetNumberOfComponents(1)
        Rt.SetName("Rt")
        Rt.InsertNextTuple1(ft);

        Strong = vtkUnsignedIntArray()
        Strong.SetNumberOfComponents(1)
        Strong.SetName("Strong")
        if fn > mean_fn:
          s = 1
        else:
          s = 0
        Strong.InsertNextTuple1(s);

        vertex = buildFrite(coorx,coory,nx,ny,fn/max_fn,lr)

        num_points = 0
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
        Ids.InsertNextTuple1(float(idkdkx))

        # ca pousse dans un polydata
        polydata = vtkPolyData()
        polydata.SetPoints(points)
        polydata.SetPolys(cells)
        polydata.GetCellData().AddArray(Ids)
	#polydata.GetCellData().SetScalars(Colors);
        polydata.GetCellData().AddArray(Rn)
        polydata.GetCellData().AddArray(Rt)
        polydata.GetCellData().AddArray(Strong)

        polydata.Update()

        Append.AddInput(polydata)           

def collectAll2D(Append,lr):

   All = nlgs_GetAllThis()

   mean_fn=mean(All[:,7])
   max_fn =max(All[:,7])
   min_fn =min(All[:,7]) 

   # pour eviter de diviser par la suite apres
   if max_fn == 0.: max_fn=1.

   i_cdan = 0
   for coorx,coory,tx,ty,nx,ny,ft,fn,vt,vn in All:
        i_cdan += 1

        # on collecte
        points = vtkPoints()
        cells  = vtkCellArray()
        Ids    = vtkFloatArray()
        Ids.SetNumberOfComponents(1)
        Ids.SetName("Ids")
        Rn = vtkFloatArray()
        Rn.SetNumberOfComponents(1)
        Rn.SetName("Rn")
        Rn.InsertNextTuple1(fn);

        Rt = vtkFloatArray()
        Rt.SetNumberOfComponents(1)
        Rt.SetName("Rt")
        Rt.InsertNextTuple1(ft);

        Strong = vtkUnsignedIntArray()
        Strong.SetNumberOfComponents(1)
        Strong.SetName("Strong")
        if fn > mean_fn:
          s = 1
        else:
          s = 0
        Strong.InsertNextTuple1(s);

        vertex = buildFrite(coorx,coory,nx,ny,fn/max_fn,lr)

        num_points = 0
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
        Ids.InsertNextTuple1(float(i_cdan))

        # ca pousse dans un polydata
        polydata = vtkPolyData()
        polydata.SetPoints(points)
        polydata.SetPolys(cells)
        polydata.GetCellData().AddArray(Ids)
	#polydata.GetCellData().SetScalars(Colors);
        polydata.GetCellData().AddArray(Rn)
        polydata.GetCellData().AddArray(Rt)
        polydata.GetCellData().AddArray(Strong)

        polydata.Update()

        Append.AddInput(polydata)           

#######################################

def writeTactsToVTK(fichier,fid):

   vtpFile = vtkXMLPolyDataWriter()
   vtpFile.SetFileName(fichier)

   AppendAll=vtkAppendPolyData()

   collectJONCx(AppendAll)
   collectDISKx(AppendAll)
   collectPOLYG(AppendAll)

   vtpFile.SetInput(AppendAll.GetOutput())

   vtpFile.Write()

   time=TimeEvolution_GetTime()
   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)


def writeInterToVTK(fichier,fid,lr):

   vtpFile = vtkXMLPolyDataWriter()
   vtpFile.SetFileName(fichier)

   AppendAll=vtkAppendPolyData()

   collectDKDKx(AppendAll,lr)

   vtpFile.SetInput(AppendAll.GetOutput())

   vtpFile.Write()

   time=TimeEvolution_GetTime()
   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)

def writeThisToVTK(fichier,fid,lr,time):

   vtpFile = vtkXMLPolyDataWriter()
   vtpFile.SetFileName(fichier)

   AppendAll=vtkAppendPolyData()

   collectAll2D(AppendAll,lr)

   vtpFile.SetInput(AppendAll.GetOutput())

   vtpFile.Write()

   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)




def startCollection(fichier):
  header= '<?xml version="1.0"?>\n<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">\n<Collection>'
  fid = open(fichier,'w')
  fid.write(header) 
  return fid

def stopCollection(fid):
  closing = '</Collection>\n</VTKFile>'
  fid.write(closing)
  fid.close()

