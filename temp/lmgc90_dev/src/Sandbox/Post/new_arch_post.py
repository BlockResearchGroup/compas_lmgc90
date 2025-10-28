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

############################
# model_handles

import numpy

# rotten dictionnaries to generate the name of a scalar field at runtime
rotten_dict_nf = { 'displacement' : {'RBDY2'    : ['disp X','disp Y','rot Z'],
                                     'RBDY3'    : ['disp X','disp Y','disp Z',
                                                   'alpha1','alpha2','alpha3',
                                                   'beta1' ,'beta2' ,'beta3' ,
                                                   'gamma1','gamma2','gamma3'],
                                     'mecaMAILx': ['disp X', 'disp Y', 'disp Z']},
                   'velocity'     : {'RBDY2'    : ['velocy X', 'velocy Y', 'spin Z'],
                                     'RBDY3'    : ['velocy X', 'velocy Y', 'velocy Z',
                                                   'velocy alpha', 'velocy beta', 'velocy gamma'],
                                     'mecaMAILx': ['velocy X', 'velocy Y', 'velocy Z']},
                   'temperature'  : {'therMAILx': ['temperature']}, 
                  }

rotten_dict_ef = { 'mecaMAILx' : {'strain'    : [ ['xx', 'yy', 'xy', 'zz'],
                                                  ['11','12','22', '13', '23', '33']
                                                ],
                                  'stress'    : [ ['xx', 'yy', 'xy', 'zz', 'vm'],
                                                  ['11','12','22', '13', '23', '33', 'vm']
                                                ]
                                 }
                 }
rotten_dict_erc = { 'RBDY2'     : { 'Fext' : ['Fext X', 'Fext Y', 'Mext Z'],
                                    'Fint' : ['Fint X', 'Fint Y', 'Mint Z']},
                    'RBDY3'     : { 'Fext' : ['Fext X', 'Fext Y', 'Fext Z', 'Mext X', 'Mext Y', 'Mext Z'],
                                    'Fint' : ['Fint X', 'Fint Y', 'Fint Z', 'Mint X', 'Mint Y', 'Mint Z']},
                    'mecaMAILx' : { 'Fext' : ['Fext X', 'Fext Y', 'Fext Z'],
                                    'Fint' : ['Fint X', 'Fint Y', 'Fint Z']},
                    'therMAILx' : { 'Fext' : ['Fext X', 'Fext Y', 'Fext Z'],
                                    'Fint' : ['Fint X', 'Fint Y', 'Fint Z']},
                  }

def pushMdlHdl(Append):

   #print 'managing: model_handles'
   nbm = model_handler_getNb() 
   #print nbm
 
   for i_mdl_hdl in xrange(1,nbm+1,1):
     
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
       for x,y in coor:
         #print x,y
         points.InsertNextPoint(x, y, 0.0)
     elif dim==3:
       for x,y,z in coor:
         points.InsertNextPoint(x, y, z)
     else :
       print 'wrong dimension'
       sys.exit()

     ug.SetPoints(points)

     # defining all data to write
     field_list = []

     # nodal fields
     field_names = model_handler_getNodalFieldNames(i_mdl_hdl)
     for fn in field_names:
       name = fn.strip()
       field = model_handler_getNodalField(i_mdl_hdl,name,1)
       for i in xrange(numpy.size(field,1)):
         f = vtkDoubleArray()
         f.SetName(rotten_dict_nf[name][mdl_type][i])
         for j in xrange(numpy.size(field,0)):
           f.InsertTuple1(j, field[j,i])
         field_list.append(f)

     # elementary fields
     #field_names = model_handler_getElementaryFieldNames(i_mdl_hdl)
     if mdl_type in rotten_dict_ef.keys():
       field_names = rotten_dict_ef[mdl_type].keys()
       for name in field_names:
         field = model_handler_getElementaryField(i_mdl_hdl,name,1)

         for i in xrange(numpy.size(field,1)):
           f = vtkDoubleArray()
           f.SetName( name+rotten_dict_ef[mdl_type][name][dim-2][i] )
           for j in xrange(numpy.size(field,0)):
             f.InsertTuple1(j, field[j,i])
           field_list.append(f)

     # rhs contribs
     field_names = model_handler_getRhsContributionNames(i_mdl_hdl)
     for fn in field_names:
       name = fn.strip()
       field = model_handler_getRhsContribution(i_mdl_hdl,name,1)
       for i in xrange(numpy.size(field,1)):
         f = vtkDoubleArray()
         f.SetName(rotten_dict_erc[mdl_type][name][i])
         for j in xrange(numpy.size(field,0)):
           f.InsertTuple1(j, field[j,i])
         field_list.append(f)

     for f in field_list:
       ug.GetPointData().AddArray(f)

     #print ele[0]
     idx=1
     for j in xrange(ele[0]):

       #print j,idx,ele[idx]
       if dim==2 :
         if ele[idx] == 4 :
           xx = vtkQuad()
         elif ele[idx] == 3 :
           xx = vtkTriangle()
         elif ele[idx] == 2 :
           xx = vtkLine()
         elif ele[idx] == 1 :
           xx = vtkVertex()
         else:
           print 'unable to draw element with size ',ele[idx],' in 2D'
           sys.exit()
       elif dim ==3 :
         if ele[idx] == 20 :
           xx = vtkQuadraticHexahedron()
         elif ele[idx] == 8 :
           xx = vtkHexahedron()
         elif ele[idx] == 4 :
           xx = vtkTetra()
         elif ele[idx] == 2 :
           xx = vtkLine()
         elif ele[idx] == 1 :
           xx = vtkVertex()
         else:
           print 'unable to draw element with size ',ele[idx],' in 3D'
           sys.exit()

       #print ele[idx+1:idx+1+ele[idx]]
       for k in xrange(ele[idx]):  
         xx.GetPointIds().SetId(k, ele[idx+1+k]-1)
       ug.InsertNextCell(xx.GetCellType(),xx.GetPointIds())

       idx += 1 + ele[idx]

     Append.AddInput(ug)

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

