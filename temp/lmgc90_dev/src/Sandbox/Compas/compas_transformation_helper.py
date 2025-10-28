"""
COMPAS-LMGC90 Transformation Helper

This module helps handle transformations between COMPAS and LMGC90 coordinate systems.
LMGC90 recomputes inertia frames internally, so we need to track these transformations.
"""

import numpy as np
from compas.geometry import Transformation, Frame, Point, Vector

class LMGC90TransformationManager:
    """Manages transformations between COMPAS and LMGC90 coordinate systems."""
    
    def __init__(self):
        self.initial_frames = {}  # Store initial LMGC90 frames
        self.original_geometries = {}  # Store original COMPAS geometries
        
    def store_initial_lmgc90_frame(self, body_id, center, frame_matrix):
        """
        Store the initial frame that LMGC90 computed after setup.
        
        Parameters:
        -----------
        body_id : int
            Body identifier
        center : list[3]
            Inertia center coordinates [x, y, z]
        frame_matrix : list[9] 
            3x3 rotation matrix in row-major order [R11,R12,R13,R21,R22,R23,R31,R32,R33]
        """
        # Convert frame_matrix from row-major to 3x3 numpy array
        R = np.array(frame_matrix).reshape(3, 3)
        
        # Create COMPAS Frame
        origin = Point(center[0], center[1], center[2])
        xaxis = Vector(R[0, 0], R[0, 1], R[0, 2])
        yaxis = Vector(R[1, 0], R[1, 1], R[1, 2])
        
        self.initial_frames[body_id] = Frame(origin, xaxis, yaxis)
        
    def create_4x4_transformation_matrix(self, center, frame_matrix):
        """
        Create 4x4 transformation matrix from LMGC90 center and frame.
        
        Parameters:
        -----------
        center : list[3]
            Center coordinates [x, y, z]  
        frame_matrix : list[9]
            3x3 rotation matrix in row-major order
            
        Returns:
        --------
        np.ndarray
            4x4 transformation matrix
        """
        T = np.eye(4)
        
        # Set rotation part (3x3)
        R = np.array(frame_matrix).reshape(3, 3)
        T[0:3, 0:3] = R
        
        # Set translation part
        T[0:3, 3] = center
        
        return T
        
    def create_compas_transformation(self, center, frame_matrix):
        """
        Create COMPAS Transformation from LMGC90 center and frame.
        
        Parameters:
        -----------
        center : list[3]
            Center coordinates [x, y, z]
        frame_matrix : list[9] 
            3x3 rotation matrix in row-major order
            
        Returns:
        --------
        compas.geometry.Transformation
            COMPAS transformation object
        """
        T_matrix = self.create_4x4_transformation_matrix(center, frame_matrix)
        return Transformation.from_matrix(T_matrix.tolist())
        
    def compute_relative_transformation(self, initial_center, initial_frame, 
                                      current_center, current_frame):
        """
        Compute relative transformation from initial to current state.
        
        Parameters:
        -----------
        initial_center, current_center : list[3]
            Center coordinates
        initial_frame, current_frame : list[9]
            3x3 rotation matrices in row-major order
            
        Returns:
        --------
        compas.geometry.Transformation
            Relative transformation from initial to current state
        """
        T_initial = self.create_4x4_transformation_matrix(initial_center, initial_frame)
        T_current = self.create_4x4_transformation_matrix(current_center, current_frame)
        
        # Relative transformation: T_rel = T_current * T_initial^(-1)
        T_rel = T_current @ np.linalg.inv(T_initial)
        
        return Transformation.from_matrix(T_rel.tolist())
        
    def print_transformation_info(self, body_id, center, frame_matrix, step=None):
        """Print transformation information in a readable format."""
        step_info = f"Step {step} - " if step is not None else ""
        print(f"\n=== {step_info}Body {body_id} Transformation ===")
        print(f"Center: [{center[0]:.6f}, {center[1]:.6f}, {center[2]:.6f}]")
        
        R = np.array(frame_matrix).reshape(3, 3)
        print("Rotation Matrix:")
        for i in range(3):
            print(f"  [{R[i,0]:.6f}, {R[i,1]:.6f}, {R[i,2]:.6f}]")
            
        T = self.create_4x4_transformation_matrix(center, frame_matrix)
        print("4x4 Transformation Matrix:")
        for i in range(4):
            print(f"  [{T[i,0]:.6f}, {T[i,1]:.6f}, {T[i,2]:.6f}, {T[i,3]:.6f}]")

# Example usage
if __name__ == "__main__":
    # Example data from your LMGC90 output
    manager = LMGC90TransformationManager()
    
    # Example: Body 0 initial state (replace with actual LMGC90 output)
    initial_center = [0.325, 0.325, 0.25]  # Example values
    initial_frame = [1.0, 0.0, 0.0,  # R11, R12, R13
                    0.0, 1.0, 0.0,   # R21, R22, R23  
                    0.0, 0.0, 1.0]   # R31, R32, R33
    
    # Store initial frame
    manager.store_initial_lmgc90_frame(0, initial_center, initial_frame)
    
    # Print transformation info
    manager.print_transformation_info(0, initial_center, initial_frame, step="Initial")
    
    # Example: Body 0 after 100 steps (replace with actual LMGC90 output)
    current_center = [0.325, 0.325, 0.15]  # Example values after simulation
    current_frame = [0.98, 0.02, 0.0,      # Example rotated frame
                    -0.02, 0.98, 0.0,
                     0.0, 0.0, 1.0]
    
    # Compute relative transformation
    rel_transform = manager.compute_relative_transformation(
        initial_center, initial_frame, current_center, current_frame)
    
    print(f"\nRelative Transformation Matrix:")
    rel_matrix = rel_transform.matrix
    for i in range(4):
        print(f"  [{rel_matrix[i][0]:.6f}, {rel_matrix[i][1]:.6f}, {rel_matrix[i][2]:.6f}, {rel_matrix[i][3]:.6f}]")
