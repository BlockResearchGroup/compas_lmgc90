from compas_lmgc90._class_primitives import Data

sphere = Data("sphere", 42)
print(f"Sphere: {sphere.name}, {sphere.value}")
sphere.name = "cone"
sphere.value = 77
print(f"\nAfter modification: {sphere.to_string()}")
