from compas.geometry import Transformation
from compas_lmgc90 import _lmgc90 
import numpy as np

# =============================================================================
# LMGC90 SOLVER API
# =============================================================================

class Solver:

    def __init__(self, model, dt=1e-2, theta=0.5):
        """Process model once: extract meshes and centroids."""
        
        # Data for LMGC90
        self.trimeshes = []  # Local mesh copies
        self.centroids = []  # Original global centroids
        self.supports = []   # Support flags (set via set_supports)
        self.init_coor = []  # Initial coordinates from LMGC90
        self.init_frame = [] # Initial frames from LMGC90

        # Model
        self.model = model
        self._model_to_lmgc90()

        # Initialize LMGC90
        _lmgc90.initialize_simulation(dt=dt, theta=theta)
    
    def _model_to_lmgc90(self):
        """Process model: extract meshes and centroids."""
        for element in self.model.elements():
            mesh = element.modelgeometry
            centroid = list(mesh.centroid())
            
            # Create local mesh (centered at origin)
            mesh_local = mesh.copy()
            mesh_local.translate([-centroid[0], -centroid[1], -centroid[2]])
            
            self.trimeshes.append(mesh_local)
            self.centroids.append(centroid)
    
    def _set_geometry(self):
        """Send geometry to LMGC90."""
        for i, mesh in enumerate(self.trimeshes):
            v, f = mesh.to_vertices_and_faces(True)
            v_flat = [item for sublist in v for item in sublist]
            f_flat = [item + 1 for sublist in f for item in sublist]  # 1-indexed
            _lmgc90.set_one_polyr(self.centroids[i], f_flat, v_flat, self.supports[i])

    def _get_initial_state(self):
        """Get initial state from LMGC90."""
        result_init = _lmgc90.get_initial_state()
        for i in range(len(self.trimeshes)):
            self.init_coor.append(np.array(result_init.init_bodies[i]))
            self.init_frame.append(np.array(result_init.init_body_frames[i]).reshape(3, 3))
            # Transform to global position
            self.trimeshes[i].translate(self.centroids[i])

    def _update_meshes(self, current_state):
        """Update mesh transformations from LMGC90 state."""
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
        """Set support flags based on z-coordinate threshold."""
        self.supports = []
        for centroid in self.centroids:
            self.supports.append(centroid[2] < z_threshold)
        return self
    
    def set_supports_from_model(self):
        """Set support flags from model elements' is_support attribute."""
        self.supports = []
        for element in self.model.elements():
            self.supports.append(getattr(element, 'is_support', False))
        return self

    def contact_law(self, name_of_contact_law, coeff):
        """TODO: .... One of the many contact laws for example: ..."""
        # _lmgc90.contact_law("name_of_contact_law", coeff)

    def preprocess(self):
        """Setup LMGC90 simulation using local data."""
        _lmgc90.set_materials(1)
        _lmgc90.set_tact_behavs(1)
        _lmgc90.set_see_tables()
        _lmgc90.set_nb_bodies(len(self.trimeshes))
        self._set_geometry()
        _lmgc90.close_before_computing()
        self._get_initial_state()
    
    def run(self, nb_steps=100):
        """Run simulation loop."""
        for k in range(1, nb_steps + 1):
            result = _lmgc90.compute_one_step()
            self._update_meshes(result)
    
    def finalize(self):
        _lmgc90.finalize_simulation()