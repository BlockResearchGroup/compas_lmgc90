# needed to read again :
# 
# https://docs.paraview.org/en/latest/UsersGuide/understandingData.html
# https://kitware.github.io/vtk-examples/site/Python/GeometricObjects/Polygon/
# https://kitware.github.io/vtk-examples/site/VTKFileFormats/

from pathlib import Path

import numpy as np

import vtk
from vtk.util import numpy_support as ns

from ...chipy.vtk_display import removeLines, addUserField
from ...chipy.lmgc90 import PRPRx_GetF2fAllIdata, POLYR_GetPOLYR2BDYTY


def push_ck(ck, ck_uf):

   pdata = vtk.vtkPolyData()
   pts   = vtk.vtkPoints()
   faces = vtk.vtkCellArray()

   nb_ck   = len(ck)

   # counting to size points
   nb_points = np.zeros( nb_ck+1, dtype=int )
   for i_ck, data in enumerate(ck):
     nb_points[i_ck+1] = nb_points[i_ck] + data[0].shape[0]
   
   nb_ck_p = nb_points[-1]

   pts.SetNumberOfPoints(nb_ck_p)
   pts_coor = ns.vtk_to_numpy( pts.GetData() )

   # finally set the cells in polydata
   if int(vtk.vtkVersion().GetVTKVersion().split('.')[0]) < 9 :
     vconnecs = vtk.vtkIdTypeArray()
     vconnecs.SetNumberOfValues(nb_ck+nb_ck_p)
     connecs  = ns.vtk_to_numpy(vconnecs)
     c_idx = 0
     for p_idx, nb_p in enumerate(nb_points[1:]):
       c_size = nb_p-nb_points[p_idx]
       connecs[c_idx] = c_size
       connecs[c_idx+1:c_idx+c_size+1] = range(nb_points[p_idx],nb_p)
       c_idx += c_size+1
     faces.SetCells(nb_ck, vconnecs)
   else:
     connecs = np.arange( nb_ck_p )
     connecs = ns.numpy_to_vtk( connecs, deep=True, array_type=vtk.VTK_INT )
     offsets = ns.numpy_to_vtk( nb_points, deep=True, array_type=vtk.VTK_INT )
     faces.SetData(offsets, connecs)

   # link container of points/faces to polydata
   pdata.SetPoints(pts)
   pdata.SetPolys(faces)

   # adding field to cells/points of polydata
   cellData  = pdata.GetCellData()
   pointData = pdata.GetPointData()

   # integer scalar field for f2f id
   for field in ["ids", "icdbdy", "ianbdy", "icdtac", "iantac"]:
     i_cf = vtk.vtkIntArray()
     i_cf.SetNumberOfComponents(1)
     i_cf.SetNumberOfTuples(nb_ck)
     i_cf.SetName(field)
     cellData.AddArray(i_cf)

   ids  = ns.vtk_to_numpy( cellData.GetArray('ids')    )
   icdb = ns.vtk_to_numpy( cellData.GetArray('icdbdy') )
   ianb = ns.vtk_to_numpy( cellData.GetArray('ianbdy') )
   icdt = ns.vtk_to_numpy( cellData.GetArray('icdtac') )
   iant = ns.vtk_to_numpy( cellData.GetArray('iantac') )

   sigma_field = vtk.vtkFloatArray()
   sigma_field.SetNumberOfComponents(1)
   sigma_field.SetNumberOfTuples(nb_ck)
   sigma_field.SetName("Sigma_n")
   cellData.AddArray(sigma_field)
   sigma_field = ns.vtk_to_numpy( sigma_field )

   # integer scalar field for status of xc (in or out of ck)
   cell_status = vtk.vtkIntArray()
   cell_status.SetNumberOfComponents(1)
   cell_status.SetNumberOfTuples(nb_ck)
   cell_status.SetName("Status")
   cellData.AddArray(cell_status)
   cell_status = ns.vtk_to_numpy( cell_status )

   p2b = POLYR_GetPOLYR2BDYTY()
   aid = PRPRx_GetF2fAllIdata()

   for i_ck, data in enumerate(ck):

     ck_coor, sigma, is_in, i_f2f = data

     # set coordinates of ck
     idx_b = nb_points[i_ck]
     idx_e = nb_points[i_ck+1]
     pts_coor[idx_b:idx_e,:] = ck_coor[:,:]

     ids[i_ck]  = i_f2f
     icdb[i_ck] = p2b[ aid[i_f2f-1,0]-1, 0 ]
     ianb[i_ck] = p2b[ aid[i_f2f-1,1]-1, 0 ]
     icdt[i_ck] = p2b[ aid[i_f2f-1,0]-1, 1 ]
     iant[i_ck] = p2b[ aid[i_f2f-1,1]-1, 1 ]

     sigma_field[i_ck] = sigma

     #r = np.sum(reac, axis=0)
     #reac_field.SetTuple3(nb_v+nb_v_ck, r[0], r[1], r[2])

     cell_status[i_ck] = is_in

   # user field management:
   for name, val in ck_uf.items():
     addUserField(name, val, cellData, dim=3)

   ## to manage non convex polygons...
   #tri_filter = vtk.vtkTriangleFilter()
   #tri_filter.SetInputData(pdata)
   #tri_filter.Update()

   return pdata


def push_stress(f2f_stress, ck_list):

    p2b = POLYR_GetPOLYR2BDYTY()
    aid = PRPRx_GetF2fAllIdata()

    for i_f2f, data in enumerate( f2f_stress ):

        #_c for compression, _d for decompression
        coor_c, connec_c, coor_d, connec_d, sigmas, decomp, if_f2f = data

        nb_poly = connec_c.size + connec_d.size
        nb_vert = np.sum(connec_c) + np.sum(connec_d)

        pdata = vtk.vtkPolyData()
        pdata.Allocate(nb_poly)

        # allocate all the points
        pts   = vtk.vtkPoints()
        pts.SetNumberOfPoints(nb_vert)

        faces = vtk.vtkCellArray()

        # map each compressed/decompressed surface to a polygon
        p_offset = 0
        for connec, coor in zip([connec_c, connec_d], [coor_c,coor_d]):
          f_offset = 0
          for i_shape, nb_v in enumerate(connec):
              polyg = vtk.vtkPolygon()
              polyg.GetPointIds().SetNumberOfIds(nb_v)
              for i_coor in range(nb_v):
                  i_point = p_offset + i_coor
                  c = coor[f_offset+i_coor]
                  pts.SetPoint(i_point, c[0], c[1], c[2])
                  polyg.GetPointIds().SetId(i_coor, i_point)
              f_offset += nb_v
              p_offset += nb_v
              if nb_v > 0:
                faces.InsertNextCell(polyg)

        pdata.SetPoints(pts)
        pdata.SetPolys(faces)

        # adding field to cells/points of polydata
        cellData  = pdata.GetCellData()
        pointData = pdata.GetPointData()

        # integer scalar field for f2f id
        for ifield, dtype in [('Ids', int), ('IsCompressed',int), ('Decompression',float),
                              ('icdbdy', int), ('ianbdy', int), ('icdtac', int), ('iantac', int), ]:
          field = vtk.vtkIntArray() if dtype==int else vtk.vtkFloatArray()
          field.SetNumberOfComponents(1)
          field.SetNumberOfTuples(nb_poly)
          field.SetName(ifield)
          cellData.AddArray(field)

        ids = ns.vtk_to_numpy( cellData.GetArray( 'Ids' ) )
        ids[:] = if_f2f

        icdb = ns.vtk_to_numpy( cellData.GetArray( 'icdbdy' ) )
        ianb = ns.vtk_to_numpy( cellData.GetArray( 'ianbdy' ) )
        icdt = ns.vtk_to_numpy( cellData.GetArray( 'icdtac' ) )
        iant = ns.vtk_to_numpy( cellData.GetArray( 'iantac' ) )

        icdb[:] = p2b[ aid[if_f2f-1,0]-1, 0 ]
        ianb[:] = p2b[ aid[if_f2f-1,1]-1, 0 ]
        icdt[:] = p2b[ aid[if_f2f-1,0]-1, 1 ]
        iant[:] = p2b[ aid[if_f2f-1,1]-1, 1 ]

        isc = ns.vtk_to_numpy( cellData.GetArray( 'IsCompressed' ) )
        isc[:connec_c.size] = 1
        isc[connec_c.size:] = 0

        dec = ns.vtk_to_numpy( cellData.GetArray( 'Decompression' ) )
        dec[:] = decomp

        sigma_field = vtk.vtkFloatArray()
        sigma_field.SetNumberOfComponents(1)
        sigma_field.SetNumberOfTuples(nb_vert)
        sigma_field.SetName("Sigma")
        pointData.AddArray(sigma_field)

        sigma_field = ns.vtk_to_numpy( sigma_field )

        nb_vert_c = np.sum(connec_c)
        sigma_field[:nb_vert_c] = -sigmas[:]
        sigma_field[nb_vert_c:] = 0.

        # to manage non convex polygons...
        #tri_filter = vtk.vtkTriangleFilter()
        #tri_filter.SetInputData(pdata)
        #tri_filter.Update()

        #ck_list.AddInputData( tri_filter.GetOutput() )
        ck_list.AddInputData( pdata )


def addCkToVtkBlocks(blocks, i_block, ck, f2f_stress=None, ck_uf=None):

    # creating a polydata for central kernel
    ck_list = push_ck( ck, ck_uf)
    blocks.SetBlock(i_block, ck_list)

    if f2f_stress is not None and len(f2f_stress)>0:

      # a container of polydata for Compression/Decompression polygon
      cd_list = vtk.vtkAppendPolyData()
      push_stress(f2f_stress, cd_list)
      cd_list.Update() # not to forget !!!

      blocks.SetBlock(i_block+1, cd_list.GetOutput())

def write_vtk(time, filename, fid, ck, f2f_stress=None, ck_uf=None):

    # create file generator
    vtk_file = vtk.vtkXMLMultiBlockDataWriter()
    vtk_file.SetFileName(str(filename))
    vtk_file.SetDataMode(vtk.vtkXMLWriter.Binary)

    # a multiblock
    root = vtk.vtkMultiBlockDataSet()
    nb_blocks = 1 if f2f_stress is None else 2
    #root.SetNumberOfBlocks(nb_blocks)

    addCkToVtkBlocks(root, 0, ck, f2f_stress, ck_uf)

    root.GetMetaData( 0 ).Set( vtk.vtkCompositeDataSet.NAME(), 'CentralKernel' )

    if f2f_stress is not None and len(f2f_stress)>0:
      root.GetMetaData( 1 ).Set( vtk.vtkCompositeDataSet.NAME(), 'CompressionStress')

    vtk_file.SetInputData(root)
    vtk_file.Write()

    impr = '<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,filename.name)
    impr+= '</Collection>\n</VTKFile>'

    removeLines(str(fid))
    with open(fid,'a') as f:
      f.write(impr)

