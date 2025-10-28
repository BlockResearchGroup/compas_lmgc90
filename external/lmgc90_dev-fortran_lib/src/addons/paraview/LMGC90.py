# trace generated using paraview version 5.11.0
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 11

#### import the simple module from the paraview
from paraview import simple as pasimple

def valid_source(pxm, cs):
  """
  pxm : pasimple.servermanager.ProxyManager
  cs  : pasimple.GetActiveSource()
  """

  # is valid for paraview
  if not cs:
    raise ValueError("no source")

  # is valid for this macro
  valid_source = ['PVDReader', 'XMLMultiBlockDataReader']
  if cs.SMProxy.xml_name not in valid_source:
    raise ValueError("not a valid source")

  # get the radical of displayed name
  cname = pxm.GetProxyName('sources', cs.SMProxy)

  return cname.split('.')[0]


def blocks_to_extract(bl, cs):
  """
  bl : block list (as a set of strings)
  cs : the current source
  return the intersection of bl and the set of blocks of cs
  """

  # among the block of the input list,
  # select those available in the current source
  di = cs.GetDataInformation().DataInformation
  nb = di.GetNumberOfDataSets()

  b2e = { di.GetBlockName(idx) for idx in range(1, nb+1) }

  return b2e & bl


def extract_block(source, bname, pname):
  """
  source: the multiblock source
  bname: the name of the block to extract
  pname: the new name in the pipeline
  return: the block object
  """
  block = pasimple.ExtractBlock(registrationName=pname, Input=source)
  block.Selectors = ['/Root/'+bname]

  return block


def find_in_pipeline(all_s, source, pname):
  """
  all_s: pasimple.GetSources()
  source: a parent source
  pname: the name in the pipeline
  return: the block if pname found with parent source
  """

  # list of source with pname:
  blocks = [ v for k, v in all_s.items() if k[0]==pname ]

  if not blocks:
    return None

  # among the source with pname
  # look for one with matching Input
  for block in blocks:
    if hasattr(block, 'Input') and block.Input is source:
      return block

  return None


#### disable automatic camera reset on 'Show'
pasimple._DisableFirstRenderCameraReset()

# get active view
rv = pasimple.GetActiveViewOrCreate('RenderView')

# get the manager
pxm = pasimple.servermanager.ProxyManager()

# use current source
cs = pasimple.GetActiveSource()

radical = valid_source(pxm, cs)

# removing leading lmgc90 or _
radical = radical.removeprefix('lmgc90')
radical = radical.removeprefix('_')
radical = radical.removesuffix('_')
radical = radical+'_' if radical else radical


# the list of extractable blocks (as a set) by the macro:
block_list = {'rigids', 'tacts', 'ptc', 'meca_R',
              'mecafe', 'therfe', 'porofe', 'multife'}


# the list of extractable blocks (as a set) from the source:
b2e = blocks_to_extract(block_list, cs)

# the dict of all sources in the pipeline
pipeline = pasimple.GetSources()

# for each block name
for bname in b2e:

    # define pipeline name
    pname = radical+bname

    # find block if already extracted
    block = find_in_pipeline(pipeline, cs, pname)
    if not block:
      block = extract_block(cs, bname, pname)

    # visibility management... if possible:
    if block and ('Visible' in block.CellData or 'Visible' in block.PointData):
      pname = radical+'visible_'+bname
      vblock = find_in_pipeline(pipeline, block, pname)
      if not vblock:
        vblock = pasimple.Threshold(registrationName=pname, Input=block)
        ftype = 'CELLS' if 'Visible' in block.CellData else 'Points'
        vblock.Scalars = [ftype, 'Visible']
        vblock.LowerThreshold = 1
        vblock.UpperThreshold = 1
        b_display  = pasimple.Show(block,  rv, 'UnstructuredGridRepresentation')
        pasimple.Hide(block, rv)
        vb_display = pasimple.Show(vblock, rv, 'UnstructuredGridRepresentation')
      if bname != 'tacts':
        pasimple.Hide(vblock, rv)

      if bname in ['mecafe', 'porofe']:
        pname = radical+'visible_'+bname+'_warped'
        wblock = find_in_pipeline(pipeline, vblock, pname)
        if not wblock:
          wblock = pasimple.WarpByVector(registrationName=pname, Input=vblock)
          wblock.Vectors = ['POINTS', 'Disp']
          wblock.ScaleFactor = 1.0
          wb_display = pasimple.Show(wblock, rv, 'UnstructuredGridRepresentation')
          wb_display.Representation = 'Surface'
          pasimple.ColorBy(wb_display, ('POINTS', 'Disp', 'Magnitude'))
          wb_display.RescaleTransferFunctionToDataRange(True, False)

# hide orignal data in view
pasimple.Hide(cs, rv)

###--------------------------------------------
### uncomment the following to render all views
### RenderAllViews()
### alternatively, if you want to write images, you can use SaveScreenshot(...).
