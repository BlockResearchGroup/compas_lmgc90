from ..mesh   import mesh
from ...avatar.node.node    import node
from ...avatar.bulk.element import element

msg = 'ERROR : med file reading module not available...\n'
msg+= '      : please build LMGC90 with -DWITH_MEDPRE=ON to use this feature'

try:
   from . import medpre
   error = None
except ImportError as e:
   error = e
except:
   raise


def lecturemed(filename,verbose=False):

    if error:
        print(msg)
        raise error

    read_mesh = None
    try:
        # Load mesh
        if verbose: print('LOADING MESH FROM FILE ',filename)
        h = medpre_LoadMeshFile(filename)
        if h is None:
            raise RuntimeError
        # Mesh info
        dim    = medpre_GetSpaceDimension(h)
        nnodes = medpre_GetNumberOfNodes(h)
        nelts  = medpre_GetNumberOfElements(h)
        coor   = medpre_GetAllCoordinates(h)
        if dim==2:
            typeno="NO2xx"
        else:
            typeno="NO3xx"
        # The pre mesh object
        if verbose: print('CREATING PRE_LMGC.MESH, DIMENSION ',dim)
        read_mesh = mesh(dim)
        # Add nodes
        if verbose: print('ADDING NODES')
        for num in range(nnodes):
            start=num*dim
            end=start+dim
            read_mesh.addNode(node(coor   = coor[start:end],
                                   number = num))
        # Add elements
        if verbose: print('ADDING ELEMENTS')
        for i in range(medpre_GetNumberOfElements(h)):
            geomtype= medpre_GetElementGeomType(h,i)
            # for the record nnodes  = medpre_GetElementNbNodes(h,i)
            conn    = medpre_GetElementNodes(h,i)
            read_mesh.addBulk(element(type=geomtype,connectivity = conn))
        if verbose: print('ADDING GROUPS')
        # Add groups of elements
        nbgroups=medpre_GetNbEltGroups(h)
        # need this mapping to retrieve bulks
        mapIdx2Bulk=[]
        ide=0
        for b in read_mesh.bulks:
            mapIdx2Bulk.append(b)
            ide=ide+1
        # add group names in all bulks
        for gr in range(nbgroups):
            grName=medpre_GetEltGroupName(h,gr)
            ids=medpre_GetEltGroupListOfIds(h,gr)
            for ide in ids:
                b = mapIdx2Bulk[ide]
                b.physicalEntity = grName
        # Add groups of nodes
        nbgroups=medpre_GetNbNodeGroups(h)
        for gr in range(nbgroups):
            grName = medpre_GetNodeGroupName(h,gr)
            grIds  = medpre_GetNodeGroupListOfIds(h,gr)
            read_mesh.addBulk( element(type='Point', connectivity=grIds, physicalEntity=grName) )
        # The end
        if verbose: print('LECTUREMED DONE.')
        medpre_Close(h)
    except:
        print('COULD NOT LOAD MED FILE: ',filename)
    return read_mesh

