"""Parser for LMGC90 BODIES.DAT file to reconstruct COMPAS meshes."""
import re
from compas.datastructures import Mesh
from compas.geometry import Frame, Transformation


def parse_bodies_dat(filepath):
    """Parse BODIES.DAT file and extract mesh data for each body.
    
    Parameters
    ----------
    filepath : str
        Path to the BODIES.DAT file
        
    Returns
    -------
    list[dict]
        List of dictionaries with keys:
        - 'id': body ID
        - 'position': global position [x, y, z]
        - 'vertices': list of local vertices [[x, y, z], ...]
        - 'faces': list of triangular faces [[v1, v2, v3], ...]
        - 'color': color name (optional)
    """
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Split by body delimiter
    body_blocks = content.split('$$$$$$')
    
    bodies = []
    for block in body_blocks:
        if not block.strip():
            continue
            
        body_data = {}
        
        # Extract body ID
        id_match = re.search(r'\$bdyty\s+\w+\s+(\d+)', block)
        if id_match:
            body_data['id'] = int(id_match.group(1))
        
        # Extract global position and rotation from $nodty section
        pos_match = re.search(
            r'\$nodty.*?coo1=\s*([-+]?\d+\.\d+E[-+]?\d+)\s+coo2=\s*([-+]?\d+\.\d+E[-+]?\d+)\s+coo3=\s*([-+]?\d+\.\d+E[-+]?\d+)\s+coo4=\s*([-+]?\d+\.\d+E[-+]?\d+)\s+coo5=\s*([-+]?\d+\.\d+E[-+]?\d+)\s+coo6=\s*([-+]?\d+\.\d+E[-+]?\d+)',
            block, re.DOTALL
        )
        if pos_match:
            body_data['position'] = [
                float(pos_match.group(1)),
                float(pos_match.group(2)),
                float(pos_match.group(3))
            ]
            body_data['rotation_angles'] = [
                float(pos_match.group(4)),
                float(pos_match.group(5)),
                float(pos_match.group(6))
            ]
        
        # Extract POLYR section
        polyr_match = re.search(
            r'POLYR\s+\d+\s+color\s+(\w+)\s+nb_vertex=\s*(\d+)\s+nb_faces=\s*(\d+)',
            block
        )
        
        if polyr_match:
            body_data['color'] = polyr_match.group(1)
            nb_vertices = int(polyr_match.group(2))
            nb_faces = int(polyr_match.group(3))
            
            # Extract vertices (local coordinates)
            vertex_pattern = r'coo1=\s*([-+]?\d+\.\d+E[-+]?\d+)\s+coo2=\s*([-+]?\d+\.\d+E[-+]?\d+)\s+coo3=\s*([-+]?\d+\.\d+E[-+]?\d+)'
            vertices = []
            
            # Find all coordinate lines in the POLYR section
            polyr_section = block[polyr_match.start():]
            for v_match in re.finditer(vertex_pattern, polyr_section):
                vertices.append([
                    float(v_match.group(1)),
                    float(v_match.group(2)),
                    float(v_match.group(3))
                ])
                if len(vertices) == nb_vertices:
                    break
            
            body_data['vertices'] = vertices
            
            # Extract faces
            face_pattern = r'ver1=\s*(\d+)\s+ver2=\s*(\d+)\s+ver3=\s*(\d+)'
            faces = []
            
            for f_match in re.finditer(face_pattern, polyr_section):
                # Convert from 1-indexed to 0-indexed
                faces.append([
                    int(f_match.group(1)) - 1,
                    int(f_match.group(2)) - 1,
                    int(f_match.group(3)) - 1
                ])
                if len(faces) == nb_faces:
                    break
            
            body_data['faces'] = faces
        
        if 'vertices' in body_data and 'faces' in body_data:
            bodies.append(body_data)
    
    return bodies


def create_mesh_from_body(body_data, transform_to_global=False, rotation_matrix=None, position=None):
    """Create a COMPAS Mesh from parsed body data.
    
    Parameters
    ----------
    body_data : dict
        Dictionary with 'vertices' and 'faces' keys
    transform_to_global : bool, optional
        If True, transform vertices from local to global coordinates
    rotation_matrix : list, optional
        9-element list representing 3x3 rotation matrix (column-major order from Fortran)
    position : list, optional
        [x, y, z] global position (overrides body_data['position'])
        
    Returns
    -------
    compas.geometry.Mesh
        Reconstructed mesh
    """
    import numpy as np
    
    vertices = body_data['vertices']
    faces = body_data['faces']
    
    # Transform to global coordinates if requested
    if transform_to_global:
        pos = position if position is not None else body_data.get('position', [0, 0, 0])
        
        if rotation_matrix is not None:
            # Apply rotation matrix (column-major from Fortran)
            R = np.array(rotation_matrix).reshape(3, 3, order='F')
            transformed_verts = []
            for v in vertices:
                v_rotated = R @ np.array(v)
                v_global = v_rotated + np.array(pos)
                transformed_verts.append(v_global.tolist())
            vertices = transformed_verts
        else:
            # Just translate
            vertices = [[v[0] + pos[0], v[1] + pos[1], v[2] + pos[2]] for v in vertices]
    
    mesh = Mesh.from_vertices_and_faces(vertices, faces)
    return mesh


def main():
    """Example usage - just parse BODIES.DAT without running simulation."""
    import os
    
    # Path to BODIES.DAT
    datbox_path = '/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/Compas/DATBOX'
    bodies_file = os.path.join(datbox_path, 'BODIES.DAT')
    
    # Parse the file
    bodies = parse_bodies_dat(bodies_file)
    print(f"✓ Parsed {len(bodies)} bodies from BODIES.DAT")
    
    # Show first 3
    for i, body in enumerate(bodies[:3]):
        print(f"\nBody {body['id']}:")
        print(f"  - Position: {body['position']}")
        print(f"  - Rotation angles: {body['rotation_angles']}")
        print(f"  - Color: {body['color']}")
        print(f"  - Vertices: {len(body['vertices'])}")
        print(f"  - Faces: {len(body['faces'])}")
    
    # Create meshes (orientation is already baked into local vertices)
    from compas_viewer import Viewer
    
    viewer = Viewer()
    for body in bodies:
        mesh = create_mesh_from_body(body, transform_to_global=True)
        viewer.scene.add(mesh, name=f"Body_{body['id']}")
    
    print(f"\n✓ Opening viewer with {len(bodies)} bodies...")
    viewer.show()


if __name__ == '__main__':
    main()
