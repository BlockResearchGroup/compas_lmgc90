import collections
try:
    import gmsh
except ImportError as e:
    print( "ERROR: 'gmsh' python module not found. Before importing gmshutils submodule," )
    print( "       please install gmsh API python module  with 'pip install gmsh'" )

is_init = False

def initGmsh():
    """
    A function to initialize gmsh instance with global options...

    TODO : improve to set options instead of defaulting ?
    """
    global is_init

    gmsh.initialize()
    gmsh.option.setNumber("General.Terminal", 1)
    # 1: MeshAdapt, 2: Automatic, 5: Delaunay, 6: Frontal-Delaunay, 7: BAMG,
    # 8: Frontal-Delaunay for Quads, 9: Packing of Parallelograms
    gmsh.option.setNumber("Mesh.Algorithm", 6)
    # 1: Delaunay, 4: Frontal, 7: MMG3D, 9: R-tree, 10: HXT
    gmsh.option.setNumber("Mesh.Algorithm3D", 10)

    #gmsh.option.setNumber("Mesh.Smoothing", 100)
    #gmsh.option.setNumber("Mesh.CharacteristicLengthMin", lc_);
    #gmsh.option.setNumber("Mesh.CharacteristicLengthMax", lc_);

    gmsh.option.setNumber('Mesh.Optimize',1)
    
    is_init = True

def getMeshAsGModel(mesh, groups=None, **params):
    """
    Create a gmsh.model object from nodes and bulks stored in the mesh object

    Only element of type S2xxx, T3xxx and Q4xxx are read
    to generate a surfacic model of gmsh.

    Parameters:

    - mesh: the pre.mesh object to read to fill gmsh.model.geo
    - groups: the groups of element to use, use everything if None

    Optionnal parameters:

    - lc: the charactheristic length to use when adding points
    - name: the name to set when adding the geometry to gmsh model
    """
    global is_init

    if not is_init:
      initGmsh()


    # parameters and options settings
    lc = params['lc'] if 'lc' in params.keys() else 1.e0
    geo_name = params['name'] if 'name' in params.keys() else "pylmgc90_mesh"

    # creating the geometric model
    gmsh.model.add(geo_name)

    # feeding geo model with the nodes
    for kno, no in mesh.nodes.items():
        gmsh.model.geo.addPoint(no.coor[0], no.coor[1], no.coor[2], lc, kno) 
        #print( f"adding node {kno} at {no.coor}")

    # feeding geo model with the elements:
    pG = collections.defaultdict(list)
    line_count = 1
    surf_count = 1
    for e, el in enumerate(mesh.bulks):

        if groups and el.physicalGroup not in groups:
            continue

        if el.etype == 'S2xxx':
            # add 1 line
            S1, S2 = el.connectivity
            gmsh.model.geo.addLine(S1, S2, line_count)
            #print( f"adding line {line_count} between nodes {S1} and {S2}")
            pG[ (1, el.physicalEntity) ].append( line_count )
            line_count += 1
        elif el.etype == 'T3xxx':
            # add 3 lines and 1 surface
            S1, S2, S3 = el.connectivity
            surf = list( range(line_count, line_count+3) )
            gmsh.model.geo.addLine(S1, S2, surf[0])
            gmsh.model.geo.addLine(S2, S3, surf[1])
            gmsh.model.geo.addLine(S3, S1, surf[2])
            gmsh.model.geo.addCurveLoop(surf, surf_count)
            gmsh.model.geo.addPlaneSurface([surf_count], surf_count)
            pG[ (2, el.physicalEntity) ].append( surf_count )
            line_count += 3
            surf_count += 1
        elif el.etype == 'Q4xxx':
            # add 4 lines and 1 surface
            S1, S2, S3, S4 = el.connectivity
            surf = list( range(line_count, line_count+4) )
            gmsh.model.geo.addLine(S1, S2, surf[0])
            gmsh.model.geo.addLine(S2, S3, surf[1])
            gmsh.model.geo.addLine(S3, S4, surf[2])
            gmsh.model.geo.addLine(S4, S1, surf[3])
            gmsh.model.geo.addCurveLoop(surf, surf_count)
            gmsh.model.geo.addPlaneSurface([surf_count], surf_count)
            pG[ (2, el.physicalEntity) ].append( surf_count )
            line_count += 4
            surf_count += 1

    for key, gr in pG.items() :
        pdim, pname = key
        ps = gmsh.model.addPhysicalGroup(pdim, gr) 
        gmsh.model.setPhysicalName(pdim, ps, pname)

    gmsh.model.geo.removeAllDuplicates()
    
    # important to synchronize since the segment of each
    # element is added when only one would be needed !
    gmsh.model.geo.synchronize()

    # is this ok ?
    return gmsh.model
 
def addVolumesToGeo(vol):
    """
    Add volumes to the geometry of the active gmsh.model

    Parameters:

    vol: a dictionnary where keys will be the physical volume name
         and the associated value must be a list of physical surfaces
         defining the named volume. If there is a closed surface in the
         list of surfaces, it substract it.
    """

    # generate a dict associating physical group with their entities
    pname2pent = { gmsh.model.getPhysicalName(*group) : gmsh.model.getEntitiesForPhysicalGroup(*group)
                   for group in gmsh.model.getPhysicalGroups()
                 }

    # first making surface loop for each groups of surface to use:
    surf_list = { s_name for surf in vol.values() for s_name in surf }
    sname2id  = {}
    surf_count = 1
    for surf in surf_list:
        gmsh.model.geo.addSurfaceLoop( pname2pent[surf], surf_count )
        sname2id[surf] = surf_count
        surf_count += 1   
    
    vol_count  = 1
    # now creating volume and physical entity associated to it
    for vol_name, surf_list in vol.items():
        surf_list_id = [ sname2id[s_name] for s_name in surf_list ]
        gmsh.model.geo.addVolume( surf_list_id, vol_count )
        gmsh.model.addPhysicalGroup( 3, [vol_count], vol_count)
        gmsh.model.setPhysicalName(  3, vol_count, vol_name )
        vol_count += 1


def meshAndSave(name, dim):
    """
    Mesh the model in the gmsh module and save it.

    Parameters:
    -----------

    name: the name of file in which to save
    dim: the dimension of the mesh
    """

    gmsh.model.geo.synchronize()
    gmsh.model.mesh.generate(dim)
    gmsh.write(name)

