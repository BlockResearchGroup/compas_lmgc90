from collections import defaultdict
from pathlib import Path

import numpy as np

from compas.datastructures import Mesh
from compas.geometry import Line
from compas.geometry import Polygon
from compas.geometry import Transformation
from compas_lmgc90 import _lmgc90


class Solver:
    """LMGC90 DEM solver for granular assemblies.

    The solver is created empty: geometry is fed in afterwards with one of the
    ``geometry_from_*`` methods, which can be called several times and mixed
    freely. Blocks are numbered in the order they are added, and that index is
    what :meth:`apply_velocity`, :meth:`apply_force` and :attr:`trimeshes` use.

    Parameters
    ----------
    dt : float, optional
        Time step for the simulation.
    theta : float, optional
        Theta parameter for LMGC90 time integration scheme.
    density : float, optional
        Default material density in kg/m³, used for every block added without
        an explicit density. Default is 2750.0 (stone).
        Note: LMGC90 currently uses a single material type, so only the first
        density value is applied to all blocks. Per-block density support is
        planned for future LMGC90 versions.
    debug : bool, optional
        Write LMGC90 diagnostic dumps to an ``OUTBOX`` directory in the
        current working directory.

    Attributes
    ----------
    trimeshes : list of :class:`compas.datastructures.Mesh`
        Local mesh copies centered at origin.
    centroids : list of list of float
        Original global centroids of each block.
    supports : list of bool
        Support flags for each block.
    densities : list of float
        Material densities in kg/m³, one per block.
    model : :class:`compas.datastructures.Assembly` or None
        The last model passed to :meth:`geometry_from_model`, if any.
    lmgc90 : :class:`_lmgc90.LMGC90Solver`
        The LMGC90 solver instance.

    Examples
    --------
    >>> solver = Solver(dt=1e-2)  # doctest: +SKIP
    >>> solver.geometry_from_model(model)  # doctest: +SKIP
    >>> solver.set_supports(z_threshold=0.4)  # doctest: +SKIP
    >>> solver.contact_law("IQS_CLB_g0", 0.35)  # doctest: +SKIP
    >>> solver.preprocess()  # doctest: +SKIP
    >>> solver.run(nb_steps=100)  # doctest: +SKIP

    """

    def __init__(self, dt=1e-2, theta=0.5, density=2750.0, debug=False):
        # `Solver(model)` used to be the way to build a solver. Catch it
        # explicitly: without this the model would silently land in `dt`.
        if not isinstance(dt, (int, float)) or isinstance(dt, bool):
            raise TypeError("Solver() no longer takes a model. Create the solver first, then feed it geometry: Solver(...).geometry_from_model(model)")
        if isinstance(density, (list, tuple, np.ndarray)):
            raise TypeError("Per-block densities are set when adding geometry, e.g. solver.geometry_from_model(model, density=[...])")

        # Data for LMGC90
        self.trimeshes = []  # Local mesh copies
        self.centroids = []  # Original global centroids
        self.densities = []  # Material density, one per block
        self.supports = []  # Support flags (set via set_supports)
        self.init_coor = []  # Initial coordinates from LMGC90
        self.init_frame = []  # Initial frames from LMGC90

        # Support flags carried by the model elements, one per block, so that
        # set_supports_from_model() stays index-aligned even when geometry
        # comes from several sources. False for blocks added without a model.
        self._element_supports = []

        # Set by geometry_from_model, purely for user introspection.
        self.model = None

        # Default material density for blocks added without an explicit one.
        self.density = float(density)

        # Max distance where contact can be detected, overridden by contact_law.
        self.alert = 1e-3

        # Geometry is frozen once handed over to LMGC90 in preprocess().
        self._preprocessed = False

        # Drvdof management
        self.v_drvdof = defaultdict(dict)
        self.f_drvdof = defaultdict(dict)

        # OUTBOX is the working directory LMGC90 writes its diagnostic
        # dumps to (out_bodies, dof, vloc_rloc, etc.). The Fortran
        # wrapper guards every one of those writes behind ``if(debug)``,
        # so the directory is only ever consulted when debug=True.
        # Don't create it otherwise — under Rhino's ScriptEditor the
        # process cwd is unpredictable (often somewhere users can't
        # write), and silently spawning an empty OUTBOX/ wherever
        # someone runs the script is a footgun.
        if debug:
            Path("./OUTBOX").mkdir(exist_ok=True)
        # Create LMGC90 solver instance.
        # The wrapped Fortran initialize() defensively resets every LMGC90
        # module's state first (PRPRx/POLYR/RBDY3/tact_behav/bulk_behav/
        # models/nlgs_3D/postpro/overall), so re-instantiating Solver in a
        # long-lived process (Rhino's ScriptEditor, Jupyter, a service)
        # works without an explicit finalize() from the previous instance.
        self.lmgc90 = _lmgc90.LMGC90Solver()
        self.lmgc90.initialize(dt, theta, debug)

    # ==========================================================================
    # Geometry
    # ==========================================================================

    def _add_block(self, mesh, density, is_support=False):
        """Register one block from a mesh, in global coordinates.

        LMGC90 wants each rigid body as a shape expressed in its own local
        frame plus the position of that frame, so the mesh is copied and
        recentered on its centroid here.
        """
        if self._preprocessed:
            raise RuntimeError("Geometry cannot be added after preprocess() has run.")

        centroid = list(mesh.centroid())

        # Create local mesh (centered at origin)
        mesh_local = mesh.copy()
        mesh_local.translate([-centroid[0], -centroid[1], -centroid[2]])

        self.trimeshes.append(mesh_local)
        self.centroids.append(centroid)
        self.densities.append(density)
        self._element_supports.append(is_support)

    def _densities_for(self, density, count):
        """Expand the ``density`` argument of a geometry method to one value per block."""
        if density is None:
            return [self.density] * count

        if isinstance(density, (list, tuple, np.ndarray)):
            if len(density) != count:
                raise ValueError(f"Number of densities ({len(density)}) must match number of blocks ({count})")
            return [float(d) for d in density]

        return [float(density)] * count

    def geometry_from_model(self, model, density=None):
        """Add the blocks of a COMPAS assembly model to the solver.

        The ``modelgeometry`` mesh of every element is used, in the order the
        model yields them. An element's ``is_support`` flag, if present, is
        remembered for :meth:`set_supports_from_model`.

        Parameters
        ----------
        model : :class:`compas.datastructures.Assembly`
            The assembly model containing blocks, e.g. a ``compas_dem``
            ``BlockModel``.
        density : float or list of float, optional
            Material density in kg/m³, either one value for all the blocks of
            this model or one value per block. Defaults to the solver density.

        Returns
        -------
        :class:`Solver`
            The solver instance for method chaining.

        """
        elements = list(model.elements())
        densities = self._densities_for(density, len(elements))

        for element, d in zip(elements, densities):
            self._add_block(element.modelgeometry, d, getattr(element, "is_support", False))

        self.model = model
        return self

    def geometry_from_mesh(self, meshes, density=None):
        """Add blocks from COMPAS meshes.

        Parameters
        ----------
        meshes : :class:`compas.datastructures.Mesh` or list of :class:`compas.datastructures.Mesh`
            One mesh, or a list of meshes, positioned in global coordinates.
            Each mesh becomes one rigid body; faces are triangulated on the
            way to LMGC90, so n-gons are fine, but every mesh must be a
            closed convex polyhedron.
        density : float or list of float, optional
            Material density in kg/m³, either one value for all these meshes
            or one value per mesh. Defaults to the solver density.

        Returns
        -------
        :class:`Solver`
            The solver instance for method chaining.

        """
        if not isinstance(meshes, (list, tuple)):
            meshes = [meshes]

        densities = self._densities_for(density, len(meshes))

        for mesh, d in zip(meshes, densities):
            self._add_block(mesh, d)

        return self

    def geometry_from_v_f(self, blocks, density=None):
        """Add blocks from raw vertex/face data, without any COMPAS object.

        This is the dependency-free entry point: anything that can produce
        vertex coordinates and face indices (a mesh library, an OBJ reader,
        a Grasshopper component, a JSON file) can drive the solver through it.

        Parameters
        ----------
        blocks : dict or list of dict
            One block, or a list of blocks. Each block is a dict with:

            - ``"vertices"`` : list of list of float
                Vertex coordinates ``[[x, y, z], ...]`` in global coordinates,
                in meters.
            - ``"faces"`` : list of list of int
                Faces as **zero-based** indices into ``"vertices"``, e.g.
                ``[[0, 1, 2], [0, 2, 3], ...]``. Faces may have any number of
                vertices (they are triangulated on the way to LMGC90) and must
                be wound consistently outwards. Each block must be a closed
                convex polyhedron.

            Extra keys are ignored, so a dict carrying additional metadata can
            be passed through unchanged.
        density : float or list of float, optional
            Material density in kg/m³, either one value for all these blocks
            or one value per block. Defaults to the solver density.

        Returns
        -------
        :class:`Solver`
            The solver instance for method chaining.

        Examples
        --------
        A unit cube sitting on the origin:

        >>> cube = {
        ...     "vertices": [
        ...         [0, 0, 0],
        ...         [1, 0, 0],
        ...         [1, 1, 0],
        ...         [0, 1, 0],
        ...         [0, 0, 1],
        ...         [1, 0, 1],
        ...         [1, 1, 1],
        ...         [0, 1, 1],
        ...     ],
        ...     "faces": [
        ...         [3, 2, 1, 0],
        ...         [4, 5, 6, 7],
        ...         [0, 1, 5, 4],
        ...         [1, 2, 6, 5],
        ...         [2, 3, 7, 6],
        ...         [3, 0, 4, 7],
        ...     ],
        ... }
        >>> solver = Solver()  # doctest: +SKIP
        >>> solver.geometry_from_v_f([cube], density=2750.0)  # doctest: +SKIP

        """
        if isinstance(blocks, dict):
            blocks = [blocks]

        densities = self._densities_for(density, len(blocks))

        for i, (block, d) in enumerate(zip(blocks, densities)):
            try:
                vertices = block["vertices"]
                faces = block["faces"]
            except (TypeError, KeyError) as e:
                raise ValueError(f"Block {i} must be a dict with 'vertices' and 'faces' keys, got: {block!r}") from e

            self._add_block(Mesh.from_vertices_and_faces(vertices, faces), d)

        return self

    def _set_geometry(self):
        """Transfer geometry data to LMGC90 solver."""
        for i, mesh in enumerate(self.trimeshes):
            v, f = mesh.to_vertices_and_faces(True)
            v_flat = [item for sublist in v for item in sublist]
            f_flat = [item + 1 for sublist in f for item in sublist]  # 1-indexed
            mat = self.d2n[self.densities[i]]
            # driven dof managment

            nb_f = len(self.f_drvdof[i]) if i in self.f_drvdof.keys() else 0
            nb_v = len(self.v_drvdof[i]) if i in self.v_drvdof.keys() else 0

            self.lmgc90.set_one_polyr(mat, self.centroids[i], f_flat, v_flat, nb_v, nb_f)

            for i_dof, drv_vals in self.v_drvdof[i].items():
                evol = drv_vals.shape[0] == 2
                self.lmgc90.set_drvdof(i + 1, i_dof, drv_vals.ravel(), True, evol)
            for i_dof, drv_vals in self.f_drvdof[i].items():
                evol = drv_vals.shape[0] == 2
                self.lmgc90.set_drvdof(i + 1, i_dof, drv_vals.ravel(), False, evol)

    def _get_initial_state(self):
        """Retrieve and store initial state from LMGC90."""
        result_init = self.lmgc90.get_initial_state()
        for i in range(len(self.trimeshes)):
            self.init_coor.append(np.array(result_init.init_bodies[i]))
            self.init_frame.append(np.array(result_init.init_body_frames[i]).reshape(3, 3))
            # Transform to global position
            self.trimeshes[i].translate(self.centroids[i])

    def _update_meshes(self, current_state):
        """Update mesh transformations from LMGC90 state.

        Parameters
        ----------
        current_state : :class:`_lmgc90.SimResult`
            Current simulation state from LMGC90.

        """
        trans = np.zeros([4, 4])
        trans[3, 3] = 1.0

        for i, mesh in enumerate(self.trimeshes):
            # Get new coordinates and frame from current state
            new_coor = np.array(current_state.bodies[i])
            new_frame = np.array(current_state.body_frames[i]).reshape(3, 3)

            # Compute incremental transformation
            df = np.matmul(new_frame.T, self.init_frame[i])
            dc = new_coor - np.matmul(df, self.init_coor[i])

            trans[:3, :3] = df[:, :]
            trans[:3, 3] = dc[:]

            # Apply transformation
            T = Transformation.from_matrix(trans.tolist())
            mesh.transform(T)

            # Update for next iteration
            self.init_frame[i][:, :] = new_frame[:, :]
            self.init_coor[i][:] = new_coor[:]

    def set_supports(self, z_threshold=0.4):
        """Set support flags based on z-coordinate threshold.

        Parameters
        ----------
        z_threshold : float, optional
            Z-coordinate below which blocks are considered supports.

        Returns
        -------
        :class:`Solver`
            The solver instance for method chaining.

        """
        self.supports = []
        for centroid in self.centroids:
            self.supports.append(centroid[2] < z_threshold)

        for i, s in enumerate(self.supports):
            if not s:
                continue
            value = np.zeros([6])
            self.v_drvdof[i] = {i_dof: value for i_dof in range(1, 7)}

        return self

    def set_supports_from_model(self):
        """Set support flags from the ``is_support`` attribute of the model elements.

        Only blocks added with :meth:`geometry_from_model` can carry a support
        flag; blocks added from meshes or raw vertices/faces are never supports.

        Returns
        -------
        :class:`Solver`
            The solver instance for method chaining.

        """
        if self.model is None:
            raise RuntimeError("No model was added. Use geometry_from_model(model), or set_supports() for a z-threshold.")

        self.supports = list(self._element_supports)

        for i, s in enumerate(self.supports):
            if not s:
                continue
            value = np.zeros([6])
            self.v_drvdof[i] = {i_dof: value for i_dof in range(1, 7)}
        return self

    def _drvdof_check(self, value):
        # First, attempt to make a numpy array
        if not isinstance(value, np.ndarray):
            if isinstance(value, float):
                v = np.zeros([1, 6])
                v[0, 0] = value
                v[0, 4] = 1.0e0
            else:
                v = np.array(value)
            value = v

        # Second, check that it is usable
        assert value.ndim == 2, "Value array as wrong dimensions"
        if value.shape[0] == 1:
            assert value.size == 6, "Value array must be of size 6"
        else:
            assert value.shape[0] == 2 and value.shape[1] > 1, "Value array must be of shape [2xn], n>1"

        return value

    def apply_velocity(self, block_index, component, value=0.0):
        """Set an imposed velocity on a block

        Parameters
        ----------
        Block_Index: integer
            The index of block on which to apply velocity
        Global_Component: string (of size 2)
            The component on which to apply velocity must be among (Vx, Vy, Vz, Rx, Ry, Rz)
        Value: float or array of floats
            If a single float, imposed value over time
            If 1D array, must be of size 6 and implements the time function
              V(t) = [v[0] + v[1] * cos(v[2]*t+v[3]) ] * min(1, v[4]+v[5]*t)
            If a 2D array, must be of size [2,nb] with nb >=2 to provide velocity
              at different times.
        """

        cmp_s2i = {"Vx": 1, "Vy": 2, "Vz": 3, "Rx": 4, "Ry": 5, "Rz": 6}

        self.v_drvdof[block_index][cmp_s2i[component]] = self._drvdof_check(value)

    def apply_force(self, block_index, component, value=0.0):
        """Add an external force on a block

        Parameters
        ----------
        Block_Index: integer
            The index of block on which to apply velocity
        Global_Component: string (of size 2)
            The component on which to apply force must be among (Fx, Fy, Fz, Mx, My, Mz)
        Value: float or array of floats
            If a single float, imposed value over time
            If 1D array, must be of size 6 and implements the time function
              F(t) = [f[0] + f[1] * cos(f[2]*t+f[3]) ] * min(1, f[4]+f[5]*t)
            If a 2D array, must be of size [2,nb] with nb >=2 to provide force
              at different times.
        """

        cmp_s2i = {"Fx": 1, "Fy": 2, "Fz": 3, "Mx": 4, "My": 5, "Mz": 6}

        self.f_drvdof[block_index][cmp_s2i[component]] = self._drvdof_check(value)

    def contact_law(self, law, coeffs, alert=1e-3):
        """Set contact law parameters.

        Parameters
        ----------
        name_of_contact_law : str
            Name of the contact law.
        coeff : float
            Coefficient for the contact law.
        alert : float
            Max distance where contact can be detected between 2 blocks

        """
        name = "iqsc0"
        coeffs = [coeffs] if isinstance(coeffs, float) else coeffs
        self.lmgc90.add_one_tact_behav(name, law, coeffs)
        self.alert = alert

    def preprocess(self):
        """Initialize LMGC90 simulation.

        This method sets up materials, contact behaviors, and geometry,
        then retrieves the initial state from LMGC90. No geometry can be
        added afterwards.

        """
        if not self.trimeshes:
            raise RuntimeError("No geometry was added. Use geometry_from_model(), geometry_from_mesh() or geometry_from_v_f().")

        # materials are identified by a 5 characters string and a densisty:
        # density to name dic generation
        d2n = np.unique(self.densities)
        if len(d2n) > 9999:
            raise ValueError("Too many materials for LMGC90")
        self.d2n = {d: f"s{i + 1:0>4}" for i, d in enumerate(d2n)}

        self.lmgc90.set_materials(np.fromiter(self.d2n.keys(), dtype=float))
        self.lmgc90.set_see_tables(self.alert)
        self.lmgc90.set_nb_bodies(len(self.trimeshes))
        self._set_geometry()
        self.lmgc90.close_before_computing()
        self._get_initial_state()
        self._preprocessed = True

    def run(self, nb_steps=100):
        """Run the simulation for a specified number of steps.

        Parameters
        ----------
        nb_steps : int, optional
            Number of time steps to compute.

        """
        for k in range(1, nb_steps + 1):
            result = self.lmgc90.compute_one_step()
            self._update_meshes(result)
            self.last_result = result  # Store for contact visualization

    def get_contacts(self, scale_normal=0.1, scale_force=0.001, polygon_size=0.05):
        """Get contact visualization data.

        Parameters
        ----------
        scale_normal : float, optional
            Scale factor for normal vectors.
        scale_force : float, optional
            Scale factor for force vectors.
        polygon_size : float, optional
            Size of contact plane polygons (not currently used).

        Returns
        -------
        dict
            Contact visualization data with keys:

            - contact_points : list of list of float
                Contact point coordinates [x, y, z].
            - contact_polygons : list of :class:`compas.geometry.Polygon`
                Contact area polygons between body pairs.
            - normal_lines : list of :class:`compas.geometry.Line`
                Contact normal direction lines.
            - force_lines : list of :class:`compas.geometry.Line`
                Total force vectors in global coordinates.
            - force_compression_lines : list of :class:`compas.geometry.Line`
                Compression force lines (Fn > 0).
            - force_tension_lines : list of :class:`compas.geometry.Line`
                Tension force lines (Fn < 0).
            - force_tangent1_lines : list of :class:`compas.geometry.Line`
                Tangential force component lines.
            - force_tangent2_lines : list of :class:`compas.geometry.Line`
                Shear force component lines.
            - force_resultants : list of :class:`compas.geometry.Line`
                Resultant forces per contact polygon, including normal and tangential components.
            - force_resultants_total : list of :class:`compas.geometry.Line`
                Same application point and ordering as ``force_resultants``.
            - force_magnitudes : list of float
                Total force magnitudes.
            - force_normal : list of float
                Normal force components (Fn).
            - force_tangent1 : list of float
                First tangential force components (Ft).
            - force_tangent2 : list of float
                Second tangential force components (Fs).
            - gaps : list of float
                Contact gap distances.
            - status : list of str
                Contact status strings.

        """

        result = self.last_result
        # self._update_meshes(result)

        contact_data = {
            "contact_points": [],
            "contact_polygons": [],
            "normal_lines": [],
            "force_lines": [],
            "force_compression_lines": [],  # Fn > 0 (compression)
            "force_tension_lines": [],  # Fn < 0 (tension)
            "force_tangent1_lines": [],  # Ft in T direction
            "force_tangent2_lines": [],  # Fs in S direction
            "force_resultants": [],  # Resultant force lines per contact polygon
            "force_resultants_total": [],  # Backward-compatible duplicate of force_resultants
            "force_magnitudes": [],
            "force_normal": [],  # Fn values
            "force_tangent1": [],  # Ft values
            "force_tangent2": [],  # Fs values
            "gaps": [],
            "status": [],
        }

        # Group contact points by body pairs
        contact_groups = {}
        for i in range(len(result.interaction_coords)):
            body_pair = tuple(sorted(result.interaction_bodies[i]))
            if body_pair not in contact_groups:
                contact_groups[body_pair] = []
            contact_groups[body_pair].append(i)

        for i in range(len(result.interaction_coords)):
            contact_pt = result.interaction_coords[i]
            normal = result.interaction_normals[i]
            tangent1 = result.interaction_tangent1[i]
            tangent2 = result.interaction_tangent2[i]

            # Local forces
            rloc = result.interaction_rloc[i]
            Ft = rloc[0]  # Tangent 1
            Fn = rloc[1]  # Normal
            Fs = rloc[2]  # Tangent 2 (shear)

            # Global force
            force = result.interaction_force_global[i]
            force_mag = result.interaction_force_magnitude[i]
            gap = result.interaction_gap[i]
            status = result.interaction_status[i]

            contact_data["contact_points"].append(contact_pt)
            contact_data["force_magnitudes"].append(force_mag)
            contact_data["force_normal"].append(Fn)
            contact_data["force_tangent1"].append(Ft)
            contact_data["force_tangent2"].append(Fs)
            contact_data["gaps"].append(gap)
            contact_data["status"].append(status)

            # Create line for normal vector visualization
            normal_end = [
                contact_pt[0] + normal[0] * scale_normal,
                contact_pt[1] + normal[1] * scale_normal,
                contact_pt[2] + normal[2] * scale_normal,
            ]
            contact_data["normal_lines"].append(Line(contact_pt, normal_end))

            # Create line for total force visualization
            if force_mag > 1e-6:
                force_end = [
                    contact_pt[0] + force[0] * scale_force,
                    contact_pt[1] + force[1] * scale_force,
                    contact_pt[2] + force[2] * scale_force,
                ]
                contact_data["force_lines"].append(Line(contact_pt, force_end))

            # Create lines for force components (centered at contact point)
            # Normal force - separate compression (Fn > 0) and tension (Fn < 0)
            if abs(Fn) > 1e-6:
                offset = Fn * scale_force / 2.0
                fn_start = [
                    contact_pt[0] - offset * normal[0],
                    contact_pt[1] - offset * normal[1],
                    contact_pt[2] - offset * normal[2],
                ]
                fn_end = [
                    contact_pt[0] + offset * normal[0],
                    contact_pt[1] + offset * normal[1],
                    contact_pt[2] + offset * normal[2],
                ]
                if Fn > 0:  # Compression
                    contact_data["force_compression_lines"].append(Line(fn_start, fn_end))
                else:  # Tension
                    contact_data["force_tension_lines"].append(Line(fn_start, fn_end))

            # Tangent force 1 (green)
            if abs(Ft) > 1e-6:
                offset = Ft * scale_force / 2.0
                ft_start = [
                    contact_pt[0] - offset * tangent1[0],
                    contact_pt[1] - offset * tangent1[1],
                    contact_pt[2] - offset * tangent1[2],
                ]
                ft_end = [
                    contact_pt[0] + offset * tangent1[0],
                    contact_pt[1] + offset * tangent1[1],
                    contact_pt[2] + offset * tangent1[2],
                ]
                contact_data["force_tangent1_lines"].append(Line(ft_start, ft_end))

            # Tangent force 2 / shear (cyan)
            if abs(Fs) > 1e-6:
                offset = Fs * scale_force / 2.0
                fs_start = [
                    contact_pt[0] - offset * tangent2[0],
                    contact_pt[1] - offset * tangent2[1],
                    contact_pt[2] - offset * tangent2[2],
                ]
                fs_end = [
                    contact_pt[0] + offset * tangent2[0],
                    contact_pt[1] + offset * tangent2[1],
                    contact_pt[2] + offset * tangent2[2],
                ]
                contact_data["force_tangent2_lines"].append(Line(fs_start, fs_end))

        # Compute resultant forces for each contact polygon
        for body_pair, indices in contact_groups.items():
            if len(indices) > 0:
                # Sum normal forces only
                sum_fn = sum(result.interaction_rloc[idx][1] for idx in indices)

                if abs(sum_fn) > 1e-6:  # Only create resultant if significant
                    # Compute weighted centroid based on normal force magnitudes (absolute values)
                    weights = [abs(result.interaction_rloc[idx][1]) for idx in indices]
                    total_weight = sum(weights)

                    if total_weight > 1e-9:
                        # Weighted average position
                        resultant_pos = [0, 0, 0]
                        for idx, w in zip(indices, weights):
                            pt = result.interaction_coords[idx]
                            resultant_pos[0] += pt[0] * w / total_weight
                            resultant_pos[1] += pt[1] * w / total_weight
                            resultant_pos[2] += pt[2] * w / total_weight

                        res_total = [sum(result.interaction_force_global[idx][k] for idx in indices) for k in range(3)]
                        r_len = (res_total[0] ** 2 + res_total[1] ** 2 + res_total[2] ** 2) ** 0.5

                        if r_len > 1e-9:
                            half = [res_total[k] * scale_force / 2.0 for k in range(3)]
                            contact_data["force_resultants"].append(
                                Line(
                                    [resultant_pos[k] - half[k] for k in range(3)],
                                    [resultant_pos[k] + half[k] for k in range(3)],
                                )
                            )
                            contact_data["force_resultants_total"].append(
                                Line(
                                    [resultant_pos[k] - half[k] for k in range(3)],
                                    [resultant_pos[k] + half[k] for k in range(3)],
                                )
                            )

        # Create polygons from grouped contact points (same body pair)
        for body_pair, indices in contact_groups.items():
            if len(indices) >= 3:  # Need at least 3 points for a polygon
                points = [result.interaction_coords[idx] for idx in indices]
                contact_data["contact_polygons"].append(Polygon(points))

        return contact_data

    def finalize(self):
        """Release LMGC90's process-global Fortran state held by this solver.

        Calling this explicitly is optional. The C++ ``LMGC90Solver``
        destructor calls ``lmgc90_finalize`` automatically when the
        underlying object is collected (which happens when this Solver
        is collected, since ``self.lmgc90`` is the only strong reference
        the Python side holds), and is idempotent — finalizing an
        already-finalized solver is a no-op.

        It is safe to call multiple times.
        """
        if getattr(self, "lmgc90", None) is not None:
            self.lmgc90.finalize()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        # `with Solver(model) as solver: ...` finalizes deterministically
        # at scope exit, regardless of whether the body raised.
        self.finalize()
        return False

    def __del__(self):
        # Backstop for GC-driven cleanup. The C++ class already finalizes
        # in its destructor when the bound LMGC90Solver goes away, so this
        # is here purely so an early Python-level finalize lets Solver
        # consumers be re-instantiated immediately, without waiting on
        # the GC. Anything raising from a finalizer is suppressed —
        # interpreter shutdown is a hostile environment and the C++
        # destructor will still run.
        try:
            if getattr(self, "lmgc90", None) is not None:
                self.lmgc90.finalize()
        except Exception:
            pass
