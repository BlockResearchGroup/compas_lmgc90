import numpy as np

from compas.geometry import Line
from compas.geometry import Polygon
from compas.geometry import Transformation
from compas_lmgc90 import _lmgc90


class Solver:
    """LMGC90 DEM solver for granular assemblies.

    Parameters
    ----------
    model : :class:`compas.datastructures.Assembly`
        The assembly model containing blocks.
    dt : float, optional
        Time step for the simulation.
    theta : float, optional
        Theta parameter for LMGC90 time integration scheme.

    Attributes
    ----------
    trimeshes : list of :class:`compas.datastructures.Mesh`
        Local mesh copies centered at origin.
    centroids : list of list of float
        Original global centroids of each block.
    supports : list of bool
        Support flags for each block.
    model : :class:`compas.datastructures.Assembly`
        The input assembly model.
    lmgc90 : :class:`_lmgc90.LMGC90Solver`
        The LMGC90 solver instance.

    """

    def __init__(self, model, dt=1e-2, theta=0.5):
        # Data for LMGC90
        self.trimeshes = []  # Local mesh copies
        self.centroids = []  # Original global centroids
        self.supports = []  # Support flags (set via set_supports)
        self.init_coor = []  # Initial coordinates from LMGC90
        self.init_frame = []  # Initial frames from LMGC90

        # Model
        self.model = model
        self._model_to_lmgc90()

        # Create LMGC90 solver instance
        self.lmgc90 = _lmgc90.LMGC90Solver()
        self.lmgc90.initialize(dt, theta)

    def _model_to_lmgc90(self):
        """Extract meshes and centroids from model."""
        for element in self.model.elements():
            mesh = element.modelgeometry
            centroid = list(mesh.centroid())

            # Create local mesh (centered at origin)
            mesh_local = mesh.copy()
            mesh_local.translate([-centroid[0], -centroid[1], -centroid[2]])

            self.trimeshes.append(mesh_local)
            self.centroids.append(centroid)

    def _set_geometry(self):
        """Transfer geometry data to LMGC90 solver."""
        for i, mesh in enumerate(self.trimeshes):
            v, f = mesh.to_vertices_and_faces(True)
            v_flat = [item for sublist in v for item in sublist]
            f_flat = [item + 1 for sublist in f for item in sublist]  # 1-indexed
            self.lmgc90.set_one_polyr(self.centroids[i], f_flat, v_flat, self.supports[i])

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
        return self

    def set_supports_from_model(self):
        """Set support flags from model elements.

        Returns
        -------
        :class:`Solver`
            The solver instance for method chaining.

        """
        self.supports = []
        for element in self.model.elements():
            self.supports.append(getattr(element, "is_support", False))
        return self

    def contact_law(self, name_of_contact_law, coeff):
        """Set contact law parameters.

        Parameters
        ----------
        name_of_contact_law : str
            Name of the contact law.
        coeff : float
            Coefficient for the contact law.

        Notes
        -----
        This method is not yet implemented.

        """
        # _lmgc90.contact_law("name_of_contact_law", coeff)

    def preprocess(self):
        """Initialize LMGC90 simulation.

        This method sets up materials, contact behaviors, and geometry,
        then retrieves the initial state from LMGC90.

        """
        self.lmgc90.set_materials(1)
        self.lmgc90.set_tact_behavs(1)
        self.lmgc90.set_see_tables()
        self.lmgc90.set_nb_bodies(len(self.trimeshes))
        self._set_geometry()
        self.lmgc90.close_before_computing()
        self._get_initial_state()

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
                Resultant normal forces per contact polygon.
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

        result = self.lmgc90.compute_one_step()
        self._update_meshes(result)

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
            normal_end = [contact_pt[0] + normal[0] * scale_normal, contact_pt[1] + normal[1] * scale_normal, contact_pt[2] + normal[2] * scale_normal]
            contact_data["normal_lines"].append(Line(contact_pt, normal_end))

            # Create line for total force visualization
            if force_mag > 1e-6:
                force_end = [contact_pt[0] + force[0] * scale_force, contact_pt[1] + force[1] * scale_force, contact_pt[2] + force[2] * scale_force]
                contact_data["force_lines"].append(Line(contact_pt, force_end))

            # Create lines for force components (centered at contact point)
            # Normal force - separate compression (Fn > 0) and tension (Fn < 0)
            if abs(Fn) > 1e-6:
                offset = Fn * scale_force / 2.0
                fn_start = [contact_pt[0] - offset * normal[0], contact_pt[1] - offset * normal[1], contact_pt[2] - offset * normal[2]]
                fn_end = [contact_pt[0] + offset * normal[0], contact_pt[1] + offset * normal[1], contact_pt[2] + offset * normal[2]]
                if Fn > 0:  # Compression
                    contact_data["force_compression_lines"].append(Line(fn_start, fn_end))
                else:  # Tension
                    contact_data["force_tension_lines"].append(Line(fn_start, fn_end))

            # Tangent force 1 (green)
            if abs(Ft) > 1e-6:
                offset = Ft * scale_force / 2.0
                ft_start = [contact_pt[0] - offset * tangent1[0], contact_pt[1] - offset * tangent1[1], contact_pt[2] - offset * tangent1[2]]
                ft_end = [contact_pt[0] + offset * tangent1[0], contact_pt[1] + offset * tangent1[1], contact_pt[2] + offset * tangent1[2]]
                contact_data["force_tangent1_lines"].append(Line(ft_start, ft_end))

            # Tangent force 2 / shear (cyan)
            if abs(Fs) > 1e-6:
                offset = Fs * scale_force / 2.0
                fs_start = [contact_pt[0] - offset * tangent2[0], contact_pt[1] - offset * tangent2[1], contact_pt[2] - offset * tangent2[2]]
                fs_end = [contact_pt[0] + offset * tangent2[0], contact_pt[1] + offset * tangent2[1], contact_pt[2] + offset * tangent2[2]]
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

                        # Weighted average normal direction
                        avg_normal = [0, 0, 0]
                        for idx, w in zip(indices, weights):
                            n = result.interaction_normals[idx]
                            avg_normal[0] += n[0] * w / total_weight
                            avg_normal[1] += n[1] * w / total_weight
                            avg_normal[2] += n[2] * w / total_weight

                        # Normalize average normal
                        n_len = (avg_normal[0] ** 2 + avg_normal[1] ** 2 + avg_normal[2] ** 2) ** 0.5
                        if n_len > 1e-9:
                            avg_normal = [avg_normal[0] / n_len, avg_normal[1] / n_len, avg_normal[2] / n_len]

                        # Create centered line for normal resultant
                        offset = sum_fn * scale_force / 2.0
                        res_start = [resultant_pos[0] - avg_normal[0] * offset, resultant_pos[1] - avg_normal[1] * offset, resultant_pos[2] - avg_normal[2] * offset]
                        res_end = [resultant_pos[0] + avg_normal[0] * offset, resultant_pos[1] + avg_normal[1] * offset, resultant_pos[2] + avg_normal[2] * offset]
                        contact_data["force_resultants"].append(Line(res_start, res_end))

        # Create polygons from grouped contact points (same body pair)
        for body_pair, indices in contact_groups.items():
            if len(indices) >= 3:  # Need at least 3 points for a polygon
                points = [result.interaction_coords[idx] for idx in indices]
                contact_data["contact_polygons"].append(Polygon(points))

        return contact_data

    def finalize(self):
        """Finalize and cleanup the LMGC90 solver."""
        self.lmgc90.finalize()
