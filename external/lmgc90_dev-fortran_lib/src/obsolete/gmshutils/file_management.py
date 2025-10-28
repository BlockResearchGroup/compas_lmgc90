try:
  import gmsh
except ImportError:
  print('ERROR : Python binding of GMSH not available !')
  print('ERROR : You cannot use gmshutils module until gmsh sdk is installed')
  raise

def readInpFile(fname):
  """
  Read Abaqus file (only mesh part though).

  Read the *NODES and *ELEMENTS sections of an Abaqus file. Only
  triangles and segments elements have beend tried.

  Parameters:
  - fname: string holding the name of the Abaqus file to read

  Returns:
  - nodes   : dictionnary with node index as the key and coordinates as a tuple of floats
  - elements: dictionnary with element index as the key and connectivity as a list of integers
  - groups  : dictionnary with a string as the key and a the associated eleents as a list of integers
  """

  nodes    = {}
  elements = {}
  groups   = {}

  f = open(fname)

  line = f.readline()
  while line:

    while line and line.strip() != "*NODE" and line.split(',')[0] != "*ELEMENT" :
      line = f.readline()

    if line.strip() == "*NODE":
      line = f.readline()
      while line[0] != '*':
        line = line.split(',')
        nodes[int(line[0])] = ( float(line[1]), float(line[2]), float(line[3]) )
        line = f.readline()

    if line.split(',')[0] == "*ELEMENT":
      line=line.split(',')
      key=line[-1].split('=')[-1].strip()
      groups[key] = (line[1].split('=')[-1],[])
      line = f.readline()
      while line[0] != '*':
        line = line.split(',')
        elements[int(line[0])] = list(map(int,line[1:]))
        groups[key][1].append(int(line[0]))
        line = f.readline()

  f.close()

  return nodes, elements, groups


def writeGeoFile(nodes,elements,groups,fname,create_volume=None,lc=1.0):
  """
  Write a gmsh geometry file.

  Written to go with readInpFile function. Only "STRI3" and "T3D2" element are managed.

  Parameters:
  - nodes        : dictionnary with node index as the key and coordinates as a tuple of floats
  - elements     : dictionnary with element index as the key and connectivity as a list of integers
  - groups       : dictionnary with a string as the key and a the associated eleents as a list of integers
  - fname        : name of file in which to save the geometry
  - create_volume: dictionnary allowing to create geometrical/physical volumes. The keys are the name
    of the physical to create, the associated value is a list of physical surfaces bounding the volume.
  - lc           : default length element size attached to nodes
  """

  f = open(fname,'w')

  line = 'lc = '+str(lc)+' ;\n\n'
  f.write( line )

  for n in nodes:
    line = 'Point(%5d) = ' % n
    line+= '{ %14.7E, %14.7E, %14.7E, lc };\n' % nodes[n]
    f.write( line )

  # this a little disgusting : for each triangle the 3 corresponding lines are created...
  # the "Coherence" keyword is added at the end to remove the duplicated lines.
  tri_shift = len(elements)
  for e in elements:
    if len(elements[e]) == 2:
      #adding line
      line = 'Line(%5d) = ' % e
      line+= '{ %5d, %5d };\n' % tuple(elements[e])
      f.write( line )
    elif len(elements[e]) == 3:
      ts = (tri_shift,tri_shift+1,tri_shift+2)
      tri_shift += 3
      #adding triangle
      line = 'Line(%5d) = ' % ts[0]
      line+= '{ %5d, %5d };\n' % tuple(elements[e][:2])
      line+= 'Line(%5d) = ' % ts[1]
      line+= '{ %5d, %5d };\n' % tuple(elements[e][1:3])
      line+= 'Line(%5d) = ' % ts[2]
      line+= '{ %5d, %5d };\n' % tuple(elements[e][-1::-2])
      line+= 'Line Loop(%5d) = ' % e
      line+= '{ %5d, %5d, %5d };\n' % ts
      line+= 'Plane Surface(%5d) = %5d;\n' % (e,e)
      f.write( line )

  surf_count = 0
  vol = {}
  for g in groups:
    if groups[g][0] == "T3D2":
      # group of lines
      line = 'Physical Line("%s") = ' % g
      line+= '{'+str(groups[g][1])[1:-1]+'};\n'
      f.write( line )
    elif groups[g][0] == "STRI3":
      # group of triangles
      surf_count += 1
      line = 'Physical Surface("%s") = ' % g
      line+= '{'+str(groups[g][1])[1:-1]+'};\n'
      f.write( line )
      line = 'Surface Loop(%5d) = ' % surf_count
      line+= '{'+str(groups[g][1])[1:-1]+'};\n'
      f.write( line )
      vol[g] = surf_count

  vol_count = 0
  for p in create_volume:
    vol_count += 1
    line = 'Volume(%5d) = ' % vol_count
    line+= '{'+str([vol[i] for i in create_volume[p]])[1:-1]+'};\n'
    f.write( line )
    line = 'Physical Volume("%s") = {%5d};\n' % (p, vol_count)
    f.write( line )

  f.write('Coherence;\n')

  f.close()

def geo2mshFile(fin,fout,dim):
  """
  Create a .msh file from a .geo file with gmsh.

  Use gmsh python module to 
    * load the .geo file,
    * mesh it and save the corresponding .msh file.

  Parameters:
  - fin : input file name
  - fout: outpout file name
  - dim : dimension of the mesh
  """

  import sys

  gmsh.initialize(sys.argv)

  gmsh.option.setNumber("General.Terminal", 1)
  # 1: MeshAdapt, 2: Automatic, 5: Delaunay, 6: Frontal-Delaunay, 7: BAMG, 8: Frontal-Delaunay for Quads, 9: Packing of Parallelograms
  gmsh.option.setNumber("Mesh.Algorithm", 6) # frontal
  # 1: Delaunay, 4: Frontal, 7: MMG3D, 9: R-tree, 10: HXT
  gmsh.option.setNumber("Mesh.Algorithm3D", 10) # hxt

  #gmsh.option.setNumber("Mesh.Smoothing", 100)
  #gmsh.option.setNumber("Mesh.CharacteristicLengthMin", lc_);
  #gmsh.option.setNumber("Mesh.CharacteristicLengthMax", lc_);

  gmsh.option.setNumber('Mesh.Optimize',1)


  gmsh.open(fin)  
  gmsh.model.mesh.generate(dim)
  gmsh.write(fout)
  gmsh.finalize()


def generateMshFileFromInp(fin,fout,dim,create_volume=None,lc=1.):
  """
  Create a .msh file from a .inp file with gmsh.

  Use readInpFile and writeGeoFile to generate a .geo file.
  Then load it in a gmsh GModel and mesh it.

  Parameters:
  - fin : input file name
  - fout: outpout file name
  - dim : dimension of the mesh
  - create_volume: dictionnary allowing to create geometrical/physical volumes. The keys are the name
  - lc           : default length element size attached to nodes
  """

  #intermediate geo file
  ftmp = fout.split('.')
  ftmp[-1] = 'geo'
  for i in ftmp[1:]:
    ftmp[0] = ftmp[0]+'.'+i
  ftmp = ftmp[0]

  n, e, g = readInpFile(fin)
  writeGeoFile(n,e,g,ftmp,create_volume,lc)
  geo2mshFile(ftmp,fout,dim)

