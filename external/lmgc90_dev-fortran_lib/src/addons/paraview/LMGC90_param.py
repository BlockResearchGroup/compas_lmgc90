# trace generated using paraview version 5.11.0
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 11

import numpy as np

from paraview import simple as pasimple
from paraview.vtk.numpy_interface import dataset_adapter as dsa

# so ugly !!!
def stringArray2List( sa ):
  l = []
  for k in range(sa.GetNumberOfValues()):
    v = sa.GetValue(k)
    if v:
      l.append(v)
  return l


# the list of mappable fields 
mappable = ['inter', 'status', 'Shape', 'cdbdy', 'anbdy']
m2d = {'inter' :'inter' ,
       'status':'status',
       'Shape' :'Shape' ,
       'cdbdy' :'bdyty' ,
       'anbdy' :'bdyty' ,
      }

# check if the source is valid
source = pasimple.GetActiveSource()
params = pasimple.FindSource('parameters.csv')
if source and params:

  # and as mappable field
  display = pasimple.GetDisplayProperties(source)
  dname = display.ColorArrayName[1]
  if dname in mappable:

    # fetch parameters data
    pval = pasimple.servermanager.Fetch(params)
    ptab = dsa.WrapDataObject(pval)

    # get the map value
    sa = ptab.RowData[m2d[dname]]
    li = []
    for k in range(sa.GetNumberOfValues()):
      v = sa.GetValue(k)
      if v.strip():
        li.append(v)

    # get the LUT (LookUp Table)
    lut = pasimple.GetColorTransferFunction(dname)

    # set as categories an reset annotations
    lut.InterpretValuesAsCategories = 1

    # get the current values to reset the annotations
    data = pasimple.servermanager.Fetch(source)
    data = dsa.WrapDataObject( data )

    if dname in data.PointData.keys():
      uval = np.unique( data.PointData[dname].GetArrays()[0] )
    elif dname in data.CellData.keys():
      uval = np.unique( data.CellData[dname].GetArrays()[0] )
    else:
      uval = []

    # map annotations
    annotations = []
    for v in uval:
      annotations.append( str(v) )
      annotations.append( li[v] )
    lut.Annotations = annotations

