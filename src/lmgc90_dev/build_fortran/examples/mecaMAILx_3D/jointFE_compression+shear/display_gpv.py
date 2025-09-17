from pylmgc90.chipy import *

def removeLines(fichier, ltd=2):

    import collections, fileinput


    # print(fichier)
    
    # taken from stackoverflow :
    # smart way to remove last ltd lines from a file
    q = collections.deque()
    ltd = 2
    for line in fileinput.input(fichier, inplace=True):
      q.append(line)
      if ltd == 0:
        print(q.popleft(), end='')
      else:
        ltd -= 1
    q.clear()


def open_gp_joint():
 
    import os
    wd = overall_GetWorkingDirectory()    
    fichier= os.path.join(wd,'DISPLAY','jgp.pvd')

    with open(fichier,'w') as fid:
        header= '<?xml version="1.0"?>\n<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">\n<Collection>\n'
        fid.write(header)
        closing = '</Collection>\n</VTKFile>'
        fid.write(closing)
    k=0
    return fichier,k

def close_gp_joint(f):
    pass

    
def write_gp_joint(fid,k,all):
  import vtk

  dim=GetDimension()
  
  fielddef=[ ('n', False, 3, lambda i: all[i,3:6],),
             ('e', False, 3, lambda i: all[i,6:9],),
             ('s', False, 3, lambda i: all[i,9:12],),
             ('d', False, 1, lambda i: all[i,12],),             
           ]


  # print(all) 

  
  wd = overall_GetWorkingDirectory()
  fichier=os.path.join(wd,'DISPLAY','jgp_'+str(k)+'.vtp')
  k+=1
  
  # fichiers ptc.vtu
  File_ptc = vtk.vtkXMLPolyDataWriter()
  File_ptc.SetFileName(fichier)

  Append_ptc = vtk.vtkAppendPolyData()
  
  # Allocate poydata, points, cells and vertices
  pdata = vtk.vtkPolyData()
  pdata.Allocate(1)

  points = vtk.vtkPoints()
  points.SetNumberOfPoints(all.shape[0])

  cells = vtk.vtkCellArray()

  pdata.SetPoints( points )
  pdata.SetVerts( cells )

  cellData  = pdata.GetCellData()

  # defining fields
  field_list = []
  for field in fielddef:
    name, is_int, size, _ = field
    new_field = vtk.vtkIntArray() if is_int else vtk.vtkFloatArray()
    new_field.SetName(name)
    new_field.SetNumberOfComponents(size)
    new_field.SetNumberOfTuples(all.shape[0])
    cellData.AddArray(new_field)

    field_list.append(new_field)


  cx = all[:,0]
  cy = all[:,1]
  cz = all[:,2] if dim==3 else np.zeros([all.shape[0]])

  # #uc = np.transpose( inters['uc'], axes=[0,2,1] )
  # #rl = inters['rl'][:,:,np.newaxis]
  # reac = np.matmul( inters['uc'], inters['rl'][:,:,np.newaxis] )
  # reac = reac[:,:,0]

  for id in range(all.shape[0]):

     # on pose le point de l'interaction (fichiers ptc.vtp)
     points.SetPoint(id, cx[id],cy[id],cz[id])
     vertex = vtk.vtkVertex()
     vertex.GetPointIds().SetId(0, id)
     cells.InsertNextCell(vertex)

     # setting inter type field
     for field, fdef in zip(field_list, fielddef):
        name, _, size, access = fdef
        if size == 1 :
           field.SetTuple1(id, access(id))
        elif dim == 2:
           a, b = access(id)
           field.SetTuple3(id, a, b, 0.)
        else:
           # print(id,access(id)) 
           field.SetTuple(id, access(id))

  Append_ptc.AddInputData(pdata)

  time=TimeEvolution_GetTime()
  impr ='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,'./'+os.path.basename(fichier))
  impr+= '</Collection>\n</VTKFile>'

  # remove closing part of pvd
  removeLines(fid)
  # add new lines of pvd
  with open(fid,'a') as f:
    f.write(impr)

  File_ptc.SetInputConnection(Append_ptc.GetOutputPort())
  File_ptc.Write()

  return k

