# trace generated using paraview version 5.11.0
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 11

#### import the simple module from the paraview
from paraview import simple as pasimple

def valid_source(pxm, cs, names=None):
  """
  pxm : pasimple.servermanager.ProxyManager
  cs  : pasimple.GetActiveSource()
  names: a list of valid block names
  """

  # is valid for paraview
  if not cs:
    raise ValueError("no source")

  # is valid for this macro
  valid_source = ['PVDReader', 'XMLMultiBlockDataReader']
  if cs.SMProxy.xml_name == 'ExtractBlock':
    if cs.Selectors[0].split('/')[-1] not in names:
      raise ValueError("not a valid block source")
  elif cs.SMProxy.xml_name not in valid_source:
    raise ValueError("not a valid reader source")

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

# the list of extractable blocks (as a set) by the macro:
bname = 'ptc'
block_list = {bname}

radical = valid_source(pxm, cs, block_list)

# the dict of all sources in the pipeline
pipeline = pasimple.GetSources()

if bname not in radical:
  # must extract block first

  # removing leading lmgc90 or _
  radical = radical.removeprefix('lmgc90')
  radical = radical.removeprefix('_')
  radical = radical.removesuffix('_')
  radical = radical+'_' if radical else radical


  # the list of extractable blocks (as a set) from the source:
  b2e = blocks_to_extract(block_list, cs)

  # for each block name
  assert bname in b2e, f"no {bname} block"

  # define pipeline name
  pname = radical+bname

  # find block if already extracted
  ptc = find_in_pipeline(pipeline, cs, pname)
  if not ptc:
    ptc = extract_block(cs, bname, pname)
    pasimple.Hide(cs, rv)

else:
  # block already extracted

  ptc = cs
  pasimple.Hide(ptc, rv)
  radical = radical.split(bname)[0]


pname = radical+bname+'_interLine'
il = find_in_pipeline(pipeline, ptc, pname)
if not il:
    il = pasimple.Glyph(registrationName=pname, Input=ptc, GlyphType='Line')
    il.OrientationArray = ['CELLS', 'N']
    il.GlyphMode = 'All Points'
    # must not add line to get the correct 'no scale array' behaviour
    #il.ScaleArray = ['CELLS', 'No scale array']
    il.ScaleFactor = 0.1
    il.GlyphTransform = 'Transform2'
    pasimple.HideInteractiveWidgets(proxy=il.GlyphType)
    # show data in view
    dil = pasimple.Show(il, rv, 'GeometryRepresentation')

pname = radical+bname+'_interTube'
it = find_in_pipeline(pipeline, il, pname)
if not it:
    # create a new 'Tube'
    it = pasimple.Tube(registrationName=pname, Input=il)
    it.Scalars = ['POINTS', 'rln']
    it.Vectors = ['POINTS', 'N']

    # Properties modified on tube_inter
    it.VaryRadius = 'By Scalar'
    it.Radius = 0.01
    it.RadiusFactor = 5.0
    it.NumberofSides = 12

    # show data in view
    dit = pasimple.Show(it, rv, 'GeometryRepresentation')
    # hide data in view
    pasimple.Hide(il, rv)

# create a new 'Ctc' threshold to exclude all 'noctc'
pname = radical+bname+'_withCtc'
wc = find_in_pipeline(pipeline, it, pname)
if not wc:

    wc = pasimple.Threshold(registrationName=pname, Input=it)
    wc.Scalars = ['POINTS', 'status']
    wc.LowerThreshold = 3.0
    wc.UpperThreshold = 3.0
    wc.Invert = 1

    # show data in view
    dwc = pasimple.Show(wc, rv, 'UnstructuredGridRepresentation')
    # hide data in view
    pasimple.Hide(it, rv)

    pasimple.ColorBy(dwc, ('POINTS', 'rln'))

# update the view to ensure updated data information
rv.Update()

