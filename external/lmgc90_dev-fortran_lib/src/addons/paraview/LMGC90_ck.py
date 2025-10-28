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
block_list = {'Face2Face', 'PressureCenter', 'ContactPointContour',
              'CentralKernel', 'CompressionStress'
             }


# the list of extractable blocks (as a set) from the source:
b2e = blocks_to_extract(block_list, cs)

# the dict of all sources in the pipeline
pipeline = pasimple.GetSources()

pasimple.Hide(cs, rv)

# for each block name
for bname in b2e:

    # define pipeline name
    pname = radical+bname

    # find block if already extracted
    block = find_in_pipeline(pipeline, cs, pname)
    if not block:
      block = extract_block(cs, bname, pname)

    match bname:
      case 'Face2Face':
        pname = radical+bname+'_tri'
        sblock = find_in_pipeline(pipeline, block, pname)
        if not sblock:
          sblock = pasimple.Triangulate(registrationName=pname, Input=block)
          dblock = pasimple.Show(sblock, rv, 'UnstructuredGridRepresentation')
          dblock.Representation = 'Surface'
          pasimple.ColorBy(dblock, ('CELLS', 'SN'))
          pasimple.Hide(block, rv)
          pasimple.Hide(sblock, rv)
      case 'PressureCenter':
        pname = radical+bname+'_line'
        sblock = find_in_pipeline(pipeline, block, pname)
        if not sblock:
          sblock = pasimple.Glyph(registrationName=pname, Input=block, GlyphType='Line')
          sblock.OrientationArray = ['CELLS', 'N']
          sblock.GlyphMode = 'All Points'
          sblock.ScaleFactor = 0.1
          sblock.GlyphTransform = 'Transform2'
          dblock = pasimple.Show(sblock, rv, 'GeometryRepresentation')
      case 'CentralKernel':
        dblock = pasimple.Show(block, rv, 'UnstructuredGridRepresentation')
        dblock.Representation = 'Surface'
        pasimple.ColorBy(dblock, ('CELLS', 'Status'))
      case 'CompressionStress':
        pname = radical+bname+'_tri'
        sblock = find_in_pipeline(pipeline, block, pname)
        if not sblock:
          sblock = pasimple.Triangulate(registrationName=pname, Input=block)
          dblock = pasimple.Show(sblock, rv, 'UnstructuredGridRepresentation')
          dblock.Representation = 'Feature Edges'
          pasimple.ColorBy(dblock, ('POINTS', 'Sigma'))
          pasimple.Hide(block, rv)

rv.Update()

#--------------------------------------------
# uncomment the following to render all views
# RenderAllViews()
# alternatively, if you want to write images, you can use SaveScreenshot(...).
