from compas_lmgc90._class_unique_pointer import create, consume

data = create()
data.name = "sphere"
data.value = 42
print(data.to_string())

consume(data)
# print(data.to_string())
