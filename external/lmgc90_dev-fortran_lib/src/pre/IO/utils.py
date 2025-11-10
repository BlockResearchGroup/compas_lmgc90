import re

try:
  import igraph
  WITH_IGRAPH = True
  IGRAPH_ERR  = None
except ImportError as err:
  IGRAPH_ERR  = err
  WITH_IGRAPH = False
except:
  raise 


def read_line(fid):
    """
    Read a line from .DAT/.INI/.OUT file. Skip comments and empty lines

    :param fid: file object from which to read
 
    :return: the line read as a string or None if end of file
    """

    line = fid.readline()

    # checking end of file
    if not line:
        return None

    # ignoring comments or empty lines
    while not line.strip() or line.lstrip().startswith("!") or line.lstrip().startswith("#") :
        line = fid.readline()
        if not line:
            return None

    return line.split("!")[0]

#ffr = re.compile( r"^(?P<radical>[ -]\d\.\d{7})(([deDE](?P<exp>[+-]\d{2}))|(?P<bug>-\d{3}))" )
bug309 = re.compile( r"(?P<radical>[ -]\d\.\d{7})((?P<sign>[+-])\d{3})" )
def str2float(str_value):
    """
    Convert a string to float
    Uses a regex to fix wrong value in +-309 instead of D+99
    and replace 'D' into 'E' to manage difference between Fortran and Python

    :param str_value: the 14 characters string to convert

    :return: the float value
    """
    
    # fortran writes 0.6xxe-12 but python writes 6.xxxe-13
    # thus when reading from fortran to writes with Python
    # the -309 is changed in e-98 to write a e-99

    m = bug309.match( str_value )
    if m:
        return float( m.group('radical')+'e'+m.group('sign')+'98' )
    else:
        return float( str_value.replace('D','E') )



def generate_graph(bodies, inters):
    """
    Generate a directed igraph.Graph object with the avatar as the nodes
    and the interactions as the edges.
    """

    if not WITH_IGRAPH:
       msg = '[ERROR:IO.utils.generate_graph] asked for a graph '
       msg+= 'but igraph module not available.'
       print( msg)
       raise IGRAPH_ERR
       # return None

    #remap (avatar_type,avatar_number) to index in bodies container
    avatar_index = { (av.atype,av.number,):idx for idx,av in enumerate(bodies) }
    ginters = igraph.Graph(directed=True)
    ginters.add_vertices( len(bodies) )
    ginters.vs['avatar'] = bodies

    edges = []
    for inter in inters:
        cd = ( inter['cdbdy'].decode(), inter['icdbdy'], )
        an = ( inter['anbdy'].decode(), inter['ianbdy'], )
        edges.append( (avatar_index[cd], avatar_index[an],) )

    ginters.add_edges( edges )
    ginters.es['inter'] = inters

    return ginters


def update_graph(ginters):
    """
    Update the interaction of a graph with the values of the avatar.number so that
    the VlocRloc.INI file may be consistent with the BODIES.DAT .
    """

    if not WITH_IGRAPH:
       msg = '[ERROR:IO.utils.update_graph] could not access '
       msg+= 'to igraph module.'
       print( msg)
       raise IGRAPH_ERR

    msg = '[ERROR:IO.utils.update_graph] ginters is not an igraph.Graph object but '

    assert isinstance( ginters, igraph.Graph), msg+str(type(ginters))

    # blind renumbering of inters with avatar number
    for inter in ginters.es:
        #print( f"changing icdbdy : {inter['inter']['icdbdy']} -> {ginters.vs[inter.source]['avatar'].number}")
        inter['inter']['icdbdy'] = ginters.vs[inter.source]['avatar'].number
        #print( f"changing ianbdy : {inter['inter']['ianbdy']} -> {ginters.vs[inter.target]['avatar'].number}")
        inter['inter']['ianbdy'] = ginters.vs[inter.target]['avatar'].number


