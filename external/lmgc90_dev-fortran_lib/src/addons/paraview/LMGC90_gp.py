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

bname = 'mecagp'
# the list of extractable blocks (as a set) by the macro:
block_list = {bname}

# a reader or a an extracted block with name in block_list
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

  assert bname in b2e, f"no {bname} block"

  # define pipeline name
  pname = radical+bname

  # find block if already extracted
  mecagp = find_in_pipeline(pipeline, cs, pname)
  if not mecagp:
      mecagp = extract_block(cs, bname, pname)
      pasimple.Hide(cs, rv)

else:
  # block already extracted

  mecagp = cs
  pasimple.Hide(mecagp, rv)
  radical = radical.split(bname)[0]


# list of field to try to work on
field_list = [ f+"_ev"+str(i) for f in ['S','E'] for i in [1,2,3] ]
field_list = [ f for f in field_list if f in mecagp.PointData.keys() ]

for field in field_list:

    pname  = radical+bname+'_'+field
    line   = find_in_pipeline(pipeline, mecagp, pname)
    sfield = field[0]+field[-1]

    if not line:
        line = pasimple.Glyph(registrationName=pname, Input=mecagp, GlyphType='Line')
        line.OrientationArray = ['POINTS', field]
        line.ScaleArray       = ['POINTS', 'No scale array']
        line.GlyphMode = 'All Points'
        line.GlyphTransform = 'Transform2'
        pasimple.HideInteractiveWidgets(proxy=line.GlyphType)

        dline = pasimple.Show(line, rv, 'GeometryRepresentation')
        pasimple.ColorBy(dline, ('POINTS', sfield))
        pasimple.Hide(line, rv)


    pname = pname+'_t'
    tube  = find_in_pipeline(pipeline, line, pname)
    if not tube:

        # create a new 'Tube'
        tube = pasimple.Tube(registrationName=pname, Input=line)
        tube.Scalars = ['POINTS', sfield ]
        tube.Vectors = ['POINTS',  field ]

        # Properties modified on tube_inter
        tube.VaryRadius = 'By Scalar'
        tube.Radius = 0.01
        tube.RadiusFactor = 5.0
        tube.NumberofSides = 12

        # show data in view
        dtube = pasimple.Show(tube, rv, 'GeometryRepresentation')
        pasimple.ColorBy(dtube, ('POINTS', sfield))
        dtube.RescaleTransferFunctionToDataRange(True, False)
        # show color bar/color legend
        dtube.SetScalarBarVisibility(rv, True)

rv.Update()
