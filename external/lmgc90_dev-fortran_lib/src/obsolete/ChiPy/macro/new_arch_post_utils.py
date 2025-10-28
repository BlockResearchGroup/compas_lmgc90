from chipy import *
from vtk   import *
from numpy import *

def getFieldsToWrite_RBDY2(i_mdl_hdl, dim):

     field_list = []

     field = model_handler_getNodalField(i_mdl_hdl,'displacement',2)

     # disp
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('disp')
     f.InsertNextTuple3(field[0,0], field[0,1], 0.)
     field_list.append(f)
     # alpha
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('alpha')
     f.InsertNextTuple3(field[0,2], field[0,3], 0.)
     field_list.append(f)
     # beta
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('beta')
     f.InsertNextTuple3(field[0,4], field[0,5], 0.)
     field_list.append(f)

     field = model_handler_getNodalField(i_mdl_hdl,'velocity',2)
     # vlocy
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('vlocy')
     f.InsertNextTuple3(field[0,0], field[0,1], 0.)
     field_list.append(f)
     # rot z
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('spin Z')
     f.InsertNextTuple3(0., 0., field[0,2])
     field_list.append(f)

     field = model_handler_getRhsContribution(i_mdl_hdl,'Fext',2)
     # Fext
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('Fext')
     f.InsertNextTuple3(field[0,0], field[0,1], 0.)
     field_list.append(f)
     # Mext z
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('Mext Z')
     f.InsertNextTuple3(0., 0., field[0,2])
     field_list.append(f)

     field = model_handler_getRhsContribution(i_mdl_hdl,'Fint',1)
     # Fint
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('Fint')
     f.InsertNextTuple3(field[0,0], field[0,1], 0.)
     field_list.append(f)
     # Mint z
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('Mint Z')
     f.InsertNextTuple3(0., 0., field[0,2])
     field_list.append(f)

     return field_list

def getFieldsToWrite_RBDY3(i_mdl_hdl, dim):

     field_list = []

     field = model_handler_getNodalField(i_mdl_hdl,'displacement',2)
     # disp
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('disp')
     f.InsertNextTuple3(field[0,0],field[0,1],field[0,2])
     field_list.append(f)
     # alpha
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('alpha')
     f.InsertNextTuple3(field[0,3],field[0,4],field[0,5])
     field_list.append(f)
     # beta
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('beta')
     f.InsertNextTuple3(field[0,6],field[0,7],field[0,8])
     field_list.append(f)
     # gamma
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('gamma')
     f.InsertNextTuple3(field[0,9],field[0,10],field[0,11])
     field_list.append(f)

     field = model_handler_getNodalField(i_mdl_hdl,'velocity',2)
     # vlocy
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('vlocy')
     f.InsertNextTuple3(field[0,0],field[0,1],field[0,2])
     field_list.append(f)
     # omega alpha
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('omega alpha')
     f.InsertNextTuple1(field[0,3])
     field_list.append(f)
     # omega beta
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('omega beta')
     f.InsertNextTuple1(field[0,4])
     field_list.append(f)
     # omega gamma
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('omega gamma')
     f.InsertNextTuple1(field[0,5])
     field_list.append(f)

     field = model_handler_getRhsContribution(i_mdl_hdl,'Fext',2)
     # Fext
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('Fext')
     f.InsertNextTuple3(field[0,0],field[0,1],field[0,2])
     field_list.append(f)
     # M alpha
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('Mext alpha')
     f.InsertNextTuple1(field[0,3])
     field_list.append(f)
     # M beta
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('Mext beta')
     f.InsertNextTuple1(field[0,4])
     field_list.append(f)
     # M gamma
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('Mext gamma')
     f.InsertNextTuple1(field[0,5])
     field_list.append(f)

     field = model_handler_getRhsContribution(i_mdl_hdl,'Fint',1)
     # Fint
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('Fint')
     f.InsertNextTuple3(field[0,0],field[0,1],field[0,2])
     field_list.append(f)
     # Mint alpha
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('Mint alpha')
     f.InsertNextTuple1(field[0,3])
     field_list.append(f)
     # Mint beta
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('Mint beta')
     f.InsertNextTuple1(field[0,4])
     field_list.append(f)
     # Mint gamma
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('Mint gamma')
     f.InsertNextTuple1(field[0,5])
     field_list.append(f)

     return field_list

def getFieldsToWrite_mecaMAILx(i_mdl_hdl, dim):

     field_list = []

     field = model_handler_getNodalField(i_mdl_hdl,'displacement',2)
     # disp
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('disp')
     if dim == 2:
       for dx, dy in field:
         f.InsertNextTuple3(dx, dy, 0.)
     else:
       for dx, dy, dz in field:
         f.InsertNextTuple3(dx, dy, dz)
     field_list.append(f)

     field = model_handler_getNodalField(i_mdl_hdl,'velocity',2)
     # vlocy
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('vlocy')
     if dim == 2:
       for vx, vy in field:
         f.InsertNextTuple3(vx, vy, 0.)
     else:
       for vx, vy, vz in field:
         f.InsertNextTuple3(vx, vy, vz)
     field_list.append(f)

     # elementary fields
     field = model_handler_getElementaryField(i_mdl_hdl,'strain',2)
     f = vtkDoubleArray()
     f.SetNumberOfComponents(9)
     f.SetName( 'strain' )
     if dim == 2:
       for xx, yy, xy, zz in field:
         f.InsertNextTuple9(xx, xy, 0., xy, yy, 0., 0., 0., zz)
     else:
       for xx, xy, yy, xz, yz, zz in field:
         f.InsertNextTuple9(xx, xy, xz, xy, yy, yz, xz, yz, zz)
     field_list.append(f)

     field = model_handler_getElementaryField(i_mdl_hdl,'stress',2)
     f = vtkDoubleArray()
     f.SetNumberOfComponents(9)
     f.SetName( 'stress' )
     f2 = vtkDoubleArray()
     f2.SetNumberOfComponents(1)
     f2.SetName( 'svm' )
     if dim == 2:
       for xx, yy, xy, zz, vm in field:
         f.InsertNextTuple9(xx, xy, 0., xy, yy, 0., 0., 0., zz)
         f2.InsertNextTuple1(vm)
     else:
       for xx, xy, yy, xz, yz, zz, vm in field:
         f.InsertNextTuple9(xx, xy, xz, xy, yy, yz, xz, yz, zz)
         f2.InsertNextTuple1(vm)
     field_list.append(f)
     field_list.append(f2)

     field = model_handler_getRhsContribution(i_mdl_hdl,'Fext',2)
     # Fext
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('Fext')
     if dim == 2:
       for fx, fy in field:
         f.InsertNextTuple3(fx, fy, 0.)
     else:
       for fx, fy, fz in field:
         f.InsertNextTuple3(fx, fy, fz)
     field_list.append(f)

     field = model_handler_getRhsContribution(i_mdl_hdl,'Fint',1)
     # Fext
     f = vtkDoubleArray()
     f.SetNumberOfComponents(3)
     f.SetName('Fint')
     if dim == 2:
       for fx, fy in field:
         f.InsertNextTuple3(fx, fy, 0.)
     else:
       for fx, fy, fz in field:
         f.InsertNextTuple3(fx, fy, fz)
     field_list.append(f)

     return field_list

def getFieldsToWrite_therMAILx(i_mdl_hdl, dim):

     field_list = []

     field = model_handler_getNodalField(i_mdl_hdl,'temperature',2)
     # disp
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('T')
     for T in field:
       f.InsertNextTuple1(T)
     field_list.append(f)

     ## elementary fields
     #field = model_handler_getElementaryField(i_mdl_hdl,'sphv',1)
     #f = vtkDoubleArray()
     #f.SetNumberOfComponents(9)
     #f.SetName( 'strain' )
     #if dim == 2:
     #  for xx, yy, xy, zz in field:
     #    f.InsertNextTuple9(xx, xy, 0., xy, yy, 0., 0., 0., zz)
     #else:
     #  for 11, 12, 22, 13, 23, 33 in field:
     #    f.InsertNextTuple9(11, 12, 13, 12, 22, 23, 13, 23, 33)
     #field_list.append(f)

     field = model_handler_getRhsContribution(i_mdl_hdl,'Fext',2)
     # Fext
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('Fext')
     for ft in field:
       f.InsertNextTuple1(ft)
     field_list.append(f)

     field = model_handler_getRhsContribution(i_mdl_hdl,'Fint',1)
     # Fext
     f = vtkDoubleArray()
     f.SetNumberOfComponents(1)
     f.SetName('Fint')
     for ft in field:
       f.InsertNextTuple1(ft)
     ### Fext
     ##f = vtkDoubleArray()
     ##f.SetNumberOfComponents(3)
     ##f.SetName('Fint')
     ##if dim == 2:
     ##  for fx, fy in field:
     ##    f.InsertNextTuple3(fx, fy, 0.)
     ##else:
     ##  for fx, fy, fz in field:
     ##    f.InsertNextTuple3(fx, fy, fz)
     ##field_list.append(f)

     return field_list

