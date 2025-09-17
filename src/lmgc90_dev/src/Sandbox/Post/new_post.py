import sys
from chipy import *
from vtk   import *
from numpy import *

########################################
# utilitaires

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

##########################################################
# tactors

class TactorGenerator():
  def __init__(self,name):

    if name == 'CLxxx' or name == 'ALpxx':
      print name,' contactor not supported yet'
      sys.exit(1)

    self.name=name
    self.GetNb=eval(name+'_GetNb'+name)  
    self.GetTactor2Body=eval(name+'_Get'+name+'2RBDY2Ptr')

    #old self.GetTactor2RBDY2=eval(name+'_Get'+name+'2RBDY2')
    self.GetNbPointOutlines=eval(name+'_GetNbPointOutlines')
    self.GetNbScalarfields=eval(name+'_GetNbScalarfields')
    self.InitOutlines=eval(name+'_InitOutlines')
    self.InitScalarfields=eval(name+'_InitScalarfields')
    self.UpdatePostdata=eval(name+'_UpdatePostdata') 
    
def InitTactor(dict,name):

   tact=TactorGenerator(name)

   vals=[]
   # 0 le nombre de tactors
   vals.append(tact.GetNb())
   # 1 la map vers rbdy2
   vals.append(tact.GetTactor2Body())
   # old vals.append(tact.GetTactor2RBDY2(vals[0]))
   # 2 pointeur sur le tableau de contour
   vals.append(tact.InitOutlines())
   # 3 le nombre de pt sur le contour des diskx
   vals.append(tact.GetNbPointOutlines())
   # 4 pointeur sur le tableau de scalar fields
   vals.append(tact.InitScalarfields())   
   # 5 le nombre de fields
   vals.append(tact.GetNbScalarfields())
   # 6 update postdata
   vals.append(tact.UpdatePostdata)
               
   dict[name]=vals 

def pushTactors(tacts_dict,Append):

   for key in tacts_dict:
      print 'managing: ',key

      tact=tacts_dict[key]

      # update postdata
      tact[6]()
      
      for itact in arange(1,tact[0]+1):
         # ca collecte
         points = vtkPoints()
         cells  = vtkCellArray()

         Ids    = vtkFloatArray()
         Ids.SetNumberOfComponents(1)
         Ids.SetName("Ids")

         field  = tact[4][itact-1,:]


         Dx     = vtkFloatArray()
         Dx.SetNumberOfComponents(1)
         Dx.SetName("disp X")
         Dx.InsertNextTuple1(field[0])
         
         Dy     = vtkFloatArray()
         Dy.SetNumberOfComponents(1)
         Dy.SetName("disp Y")
         Dy.InsertNextTuple1(field[1])
         
         Rz     = vtkFloatArray()
         Rz.SetNumberOfComponents(1)
         Rz.SetName("rot Z")
         Rz.InsertNextTuple1(field[2])
         
         Vx     = vtkFloatArray()
         Vx.SetNumberOfComponents(1)
         Vx.SetName("velocy X")
         Vx.InsertNextTuple1(field[3])

         Vy     = vtkFloatArray()
         Vy.SetNumberOfComponents(1)
         Vy.SetName("velocy Y")
         Vy.InsertNextTuple1(field[4])
         
         Wz     = vtkFloatArray()
         Wz.SetNumberOfComponents(1)
         Wz.SetName("spin Z")
         Wz.InsertNextTuple1(field[5])

         Fx     = vtkFloatArray()
         Fx.SetNumberOfComponents(1)
         Fx.SetName("Reac X")
         Fx.InsertNextTuple1(field[6])

         Fy     = vtkFloatArray()
         Fy.SetNumberOfComponents(1)
         Fy.SetName("Reac Y")
         Fy.InsertNextTuple1(field[7])

         Mz     = vtkFloatArray()
         Mz.SetNumberOfComponents(1)
         Mz.SetName("Torque Z")
         Mz.InsertNextTuple1(field[8])

         vertex = tact[2][tact[3][itact-1]:tact[3][itact],:]

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
         ibdyty = tact[1][itact-1][0]
         Ids.InsertNextTuple1(float(ibdyty))

         # ca pousse dans un polydata
         polydata = vtkPolyData()
         polydata.SetPoints(points)
         polydata.SetPolys(cells)
         polydata.GetCellData().AddArray(Ids)
         polydata.GetCellData().AddArray(Dx)
         polydata.GetCellData().AddArray(Dy)
         polydata.GetCellData().AddArray(Rz)
         polydata.GetCellData().AddArray(Vx)
         polydata.GetCellData().AddArray(Vy)
         polydata.GetCellData().AddArray(Wz)
         polydata.GetCellData().AddArray(Fx)
         polydata.GetCellData().AddArray(Fy)
         polydata.GetCellData().AddArray(Mz)
         polydata.Update()

         Append.AddInput(polydata)           

def InitTactorsToVTK(tacts_list,tacts_dict):
   for tact in tacts_list:
      InitTactor(tacts_dict,tact)
       

def writeTactorsToVTK(fichier,fid,tacts_dict):

   vtpFile = vtkXMLPolyDataWriter()
   vtpFile.SetFileName(fichier)

   AppendAll=vtkAppendPolyData()

   pushTactors(tacts_dict,AppendAll)

   vtpFile.SetInput(AppendAll.GetOutput())

   vtpFile.Write()

   time=TimeEvolution_GetTime()
   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)


#######################################
# inter

class InterGenerator():
  def __init__(self,name):
    self.name=name
    self.GetNb=eval(name+'_GetNbv'+name)  
    self.GetAll=eval(name+'_GetAll')

def InitInter(dict,name):

   tact=InterGenerator(name)

   vals=[]
   vals.append(tact.GetNb)
   vals.append(tact.GetAll)

   dict[name]=vals 

def pushInters(inters_dict,Append,lr):

   for key in inters_dict:
     print 'managing: ',key
 
     vals=inters_dict[key]
     nb = vals[0]()

     print 'nb inter ',nb 

     if (nb == 0):
        return

     All = vals[1]()

     mean_fn=mean(All[:,4])
     max_fn =max(All[:,4])
     min_fn =min(All[:,4]) 

     # pour eviter de diviser par la suite apres
     if max_fn == 0.: max_fn=1.

     iinter=0
     for coorx,coory,nx,ny,fn,ft in All:
          iinter+=1

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
          Ids.InsertNextTuple1(float(iinter))

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

def InitIntersToVTK(inters_list,inters_dict):
   for inter in inters_list:
      InitInter(inters_dict,inter)

def writeIntersToVTK(fichier,fid,inters_dict,lr):

   vtpFile = vtkXMLPolyDataWriter()
   vtpFile.SetFileName(fichier)

   AppendAll=vtkAppendPolyData()

   pushInters(inters_dict,AppendAll,lr)

   vtpFile.SetInput(AppendAll.GetOutput())

   vtpFile.Write()

   time=TimeEvolution_GetTime()
   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)

############################
# mecamailx

def pushMecafe(Append,dim):

   print 'managing: mecafe'

   nbm = mecaMAILx_GetNbMecaMAILx() 

   print nbm
 
   for i in xrange(1,nbm+1,1):
     
     coor = mecaMAILx_GetCooref(i)
     ele = mecaMAILx_GetConnectivity(i)
     All = mecaMAILx_GetAll(i)
     
     ug = vtk.vtkUnstructuredGrid()

     points = vtk.vtkPoints()
     if dim==2 :
       for x,y in coor:
         #print x,y
         points.InsertNextPoint(x, y, 0.0)
     elif dim==3:
       for x,y,z in coor:
         points.InsertNextPoint(x, y, z)

     ug.SetPoints(points)



     if dim==2:
       Dx= vtkDoubleArray()
       Dx.SetName("disp X")
       Dy= vtkDoubleArray()
       Dy.SetName("disp Y")
       Vx= vtkDoubleArray()
       Vx.SetName("velocy X")
       Vy= vtkDoubleArray()
       Vy.SetName("velocy Y")
       Exx= vtkDoubleArray()
       Exx.SetName("Exx")
       Eyy= vtkDoubleArray()
       Eyy.SetName("Exy")
       Exy= vtkDoubleArray()
       Exy.SetName("Eyy")
       Ezz= vtkDoubleArray()
       Ezz.SetName("Ezz")
       Sxx= vtkDoubleArray()
       Sxx.SetName("Sxx")
       Sxy= vtkDoubleArray()
       Sxy.SetName("Sxy")
       Syy= vtkDoubleArray()
       Syy.SetName("Syy")
       Szz= vtkDoubleArray()
       Szz.SetName("Szz")
       Svm= vtkDoubleArray()
       Svm.SetName("Svm")

       i=0  
       for dx,dy,vx,vy,exx,exy,eyy,ezz,J,sxx,sxy,syy,szz,svm in All:
          Dx.InsertTuple1(i, dx)
          Dy.InsertTuple1(i, dy)
          Vx.InsertTuple1(i, vx)
          Vy.InsertTuple1(i, vy)          
          Exx.InsertTuple1(i, exx)
          Exy.InsertTuple1(i, exy)
          Eyy.InsertTuple1(i, eyy)
          Ezz.InsertTuple1(i, ezz)
          Sxx.InsertTuple1(i, sxx)
          Sxy.InsertTuple1(i, sxy)
          Syy.InsertTuple1(i, syy)
          Szz.InsertTuple1(i, szz)
          Svm.InsertTuple1(i, svm)

          i+=1

       ug.GetPointData().AddArray(Dx)
       ug.GetPointData().AddArray(Dy)
       ug.GetPointData().AddArray(Vx)
       ug.GetPointData().AddArray(Vy)
       ug.GetPointData().AddArray(Exx)
       ug.GetPointData().AddArray(Exy)
       ug.GetPointData().AddArray(Eyy)
       ug.GetPointData().AddArray(Ezz)
       ug.GetPointData().AddArray(Sxx)
       ug.GetPointData().AddArray(Sxy)
       ug.GetPointData().AddArray(Syy)
       ug.GetPointData().AddArray(Szz)
       ug.GetPointData().AddArray(Svm)

     #print ele[0]
     idx=1
     for j in xrange(ele[0]):

        #print j,idx,ele[idx]

        if ele[idx] == 4:
          xx = vtkQuad()
        elif ele[idx] == 3:
          xx = vtkTriangle()
        else:
          sys.exit(0)

        #print ele[idx+1:idx+1+ele[idx]]
        for k in xrange(ele[idx]):  
          xx.GetPointIds().SetId(k, ele[idx+1+k]-1)
        ug.InsertNextCell(xx.GetCellType(),xx.GetPointIds())

        idx += 1 + ele[idx]
        
     ug.Update()
     Append.AddInput(ug)

def writeMecafeToVTK(fichier,fid,dim):

   vtuFile = vtkXMLUnstructuredGridWriter()
   vtuFile.SetFileName(fichier)

   AppendAll=vtkAppendFilter()

   pushMecafe(AppendAll,dim)

   vtuFile.SetInput(AppendAll.GetOutput())

   vtuFile.Write()

   time=TimeEvolution_GetTime()
   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)

#######################################################
# info solver

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

def writeThisToVTK(fichier,fid,lr,time):

   vtpFile = vtkXMLPolyDataWriter()
   vtpFile.SetFileName(fichier)

   AppendAll=vtkAppendPolyData()

   collectAll2D(AppendAll,lr)

   vtpFile.SetInput(AppendAll.GetOutput())

   vtpFile.Write()

   impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
   fid.write(impr)

###################
#   
def writeMAILxToVTK(fichier,fid,mailx_dict):

   vtuFile = vtkXMLUnstructuredGridWriter()
   vtuFile.SetFileName(fichier)

   AppendAll=vtkAppendPolyData()

   pushInters(inters_dict,AppendAll,lr)

   vtpFile.SetInput(AppendAll.GetOutput())

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

