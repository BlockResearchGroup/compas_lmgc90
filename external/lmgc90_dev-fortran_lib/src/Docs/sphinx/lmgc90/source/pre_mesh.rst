
.. py:currentmodule:: pylmgc90.pre

Mesh
====

This section presents functions dedicated to creation
or manipulation of meshes to produce avatars (rigid or deformable). 
The :py:class:`mesh` class allows to define a mesh object in the lightest way: nodes, connectivity
and possibly the groups elements belongs to. 
In 3D only a volumic mesh can become a deformable or rigid avatar whereas
a surfacic mesh can only be used to generate a rigid avatar. 
In 2D a mesh may define a deformable or a rigid avatar.

Hand made mesh
--------------

Again it is possible to define a mesh by hand using the class constructor
and the basic methods as explained in :ref:`avatar-definition`
section. But this solution is not usable to generate big meshes.

Basic example ::

 m = pre.mesh(dimension=2) 
 m.addNode( pre.node(numpy.array([0.,0.]), number=1) ) 
 m.addNode( pre.node(numpy.array([1.,0.]), number=2) )
 m.addNode( pre.node(numpy.array([0.,1.]), number=3) )
 m.addNode( pre.node(numpy.array([1.,1.]), number=4) )
 m.addBulk( pre.element(2, [1,2,4,3], physicalEntity='1quad') )

Available geometrical elements are: 

  * 1D : Point, S2xxx, S3xxx,
  * 2D : Point, S2xxx, S3xxx, T3xxx, Q4xxx, T6xxx, Q8xxx, Q9xxx,
  * 3D : Point, S2xxx, S3xxx, T3xxx, Q4xxx, T6xxx, Q8xxx, Q9xxx,
    H8xxx, H20xx, TE4xx, TE10x, PRI6x, PRI15

    
 .. c'est des elements physiques 
 .. * 'Rxx2D','Rxx3D',
 .. * 'SPRG2','SPRG3','Beam','Cable','S2xth',
 .. * 'Q4xxx','Q4P0x','Q44xx',
 .. * 'T3xxx','DKTxx','T33xx',
 .. * 'Q8xxx','Q8Rxx','Q84xx',
 .. * 'Q9xxx',
 .. * 'T6xxx','T63xx',
 .. * 'TE4xx','TE4lx','TE44x',
 .. * 'TE10x','TE104',
 .. * 'H8xxx','H88xx','SHB8x',
 .. * 'H20xx','H20Rx','H208x','SHB20',
 .. * 'PRI6x','SHB6x',
 .. * 'PRI15','SHB15'
  
Remember that rigid2d() and rigid3d() allow to define *rigid*
element. It is equivalent to ::

 pre.element( 0, connectivity=[1], physicalEntity='1')


Built-in Generation
-------------------

For the specific case of 2D rectangular mesh, the :py:func:`buildMesh2D` function can
be used and for the case of 3D paralleloid mesh :py:func:`buildMeshH8`
also.

**Example:**

Generating a simple rectangular mesh::

  my_mesh = pre.buildMesh2D('Q4', x0=0.025, y0=0.05, lx=0.10, ly=0.05, nb_elem_x=10, nb_elem_y=5)


Importing a mesh
----------------

The most efficient way to generate a mesh is to use a meshing software like
`gmsh <http://www.geuz.org/gmsh/>`_ . To this end the :py:func:`readMesh`
function allows to read a file with gmsh format. In this way any kind of mesh
may be put in a mesh object. 

**Example:** ::

   dim=2
   mesh = pre.readMesh('block.msh',dim) 

Mesh to avatar
--------------

Creating an avatar from a mesh is possible using:

* deformable 2D/3D

  * :py:func:`buildMeshedAvatar`

  **Example:** ::

     mesh_cube = pre.readMesh('gmsh/cube_t4.msh', dim)
     cube = pre.buildMeshedAvatar(mesh=mesh_cube, model=m3Dl, material=stone)
     cube.addContactors(group='102', shape='ASpxx', color='BLUEx')
     cube.imposeDrivenDof(group='105' , component=[1, 2, 3], dofty='vlocy')

  **Remarks:**

    It exists various strategies for contactors:
  
    * 2D 
	
      ::
	 
        # candidates at nodes      
        addContactors(group='xx', shape='CLxxx', color='BLUEx')
        # candidates on edges
        addContactors(group='xx', shape='CLxxx', color='BLUEx', weights=[0.25,0.75])
        # antagonist
        addContactors(group='yy', shape='ALpxx', color='REDxx')

	
    * 3D

      ::
	 
        # candidates at nodes
        addContactors(group='xx', shape='CSpxx',color='REDxx')	
        # candidates on faces ( quadrature=0 - constant, quadrature=1  - linear, quadrature=2 - quadratic pressure)  
        addContactors(group='xx', shape='CSpxx',color='REDxx',quadrature=1)
        # antagoniste
        addContactors(group='yy', shape='ASpxx', color='BLUEx')
	
* rigid 2D

  * :py:func:`rigidFromMesh2D`

* rigid 3D

  * :py:func:`volumicMeshToRigid3D`
  * :py:func:`surfacicMeshToRigid3D`

  **Example:** ::

     body_donut = pre.volumicMeshToRigid3D(volumic_mesh=mesh_donut, model=mod, material=tdur, color='BLUEx')

For the rigid avatar, the mesh is used to define the boundary of the corresponding polygons/polyhedra. 

The deformable avatars inherit the group of the original mesh,
allowing to define the desired boundary conditions and to add the contactors on them.


Sometimes, it is wanted to explode the continuous mesh in a collection
of avatars 
(if one wants to use cohezive zone model for example). The
function allowing to obtain a container of avatars are:

* :py:func:`explodeMeshedAvatar2D`
* :py:func:`explodeMeshedAvatar3D`
* :py:func:`rigidsFromMesh2D`
* :py:func:`rigidsFromMesh3D`
* :py:func:`surfacicMeshesToRigid3D`


Extracting a mesh from meshes
-----------------------------

Eventually a mesh loaded from a file may contain several parts which
correspond to different avatars. Using  :py:func:`mesh.separateMeshes` it
is possible to separate the meshes.

**Example:** ::

   dim=3
   complete_mesh = pre.readMesh(name='gmsh/3_briques.msh', dim=dim)
   entity2mesh   = complete_mesh.separateMeshes(dim=dim, entity_type="geometricalEntity", keep_all_elements=False)
   for volumic_mesh in entity2mesh.values():
      body = pre.volumicMeshToRigid3D(volumic_mesh=volumic_mesh, model=mod, material=pdur, color='BLUEx')
      bodies.addAvatar(body)

The output when separting meshes is a dictionary. 

By the way it is possible to directly separate a surfacic mesh in
avatars using  :py:func:`surfacicMeshesToRigid3D`

Managing groups
---------------
Groups are necessary to define model, material, contactors, boundary
condition, etc. 

When defining material and models with :py:func:`buildMeshedAvatar` one
implicitely assumes that all the elements have the same group and
usually uses
default group *all*.  However it is possible to define several
material and model: ::

  my_mesh = pre.readMesh('Mesh.msh', dim)
  my_mesh.rankRenumbering()
  body = pre.avatar(number=1, dimension=dim)
  body.addBulks(my_mesh.bulks)
  body.addNodes(my_mesh.nodes)
  body.defineGroups()
  body.defineModel(model=modPorous, group = '11')
  body.defineMaterial(material=matBiot, group = '11')
  body.defineModel(model=modFluid, group = '12')
  body.defineMaterial(material=matStokes, group = '12')


Applying boundary condition without group
-----------------------------------------

predicates and so on ::

  def p(x):
     return abs(x[0] - 0.0016) < 5.e-4

  body.addGroupUsingPredicate(name='relax', predicate=p, super_group='12')
  body.imposeDrivenDof(group='relax', component=1, dofty='vlocy')

Miscellaneous
-------------

other use of :py:func:`avatar.addContactors` ::

  outil.addContactors(group='11', shape='ALpxx', color='BLUEx', reverse='yes')


Using GMSH python API
---------------------

Provided that the Python API of gmsh software is available. It can
be used to handle meshes. The list of features is available here: :ref:`gmsh_pre`.

