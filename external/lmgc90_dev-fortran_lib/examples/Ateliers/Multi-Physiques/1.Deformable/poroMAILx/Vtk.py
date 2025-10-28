"""VTK data file interface."""


import struct


########################################################################
# Constants

# File formats
ASCII = 'ASCII'
BINARY = 'BINARY'

# Data types
BIT = 'bit'
UNSIGNED_CHAR = 'unsigned_char'
CHAR = 'char'
UNSIGNED_SHORT = 'unsigned_short'
SHORT = 'short'
UNSIGNED_INT = 'unsigned_int'
INT = 'int'
UNSIGNED_LONG = 'unsigned_long'
LONG = 'long'
FLOAT = 'float'
DOUBLE = 'double'

# Cell types
VERTEX = 1
LINE = 3
POLY_VERTEX = 2
POLY_LINE = 4
TRIANGLE_STRIP = 6
TRIANGLE = 5
QUAD = 9
POLYGON = 7
PIXEL = 8
TETRA = 10
VOXEL = 11
HEXAHEDRON = 12
WEDGE = 13
PYRAMID = 14


#######################################################################
# Auxiliary tables


_data_type_fmt = {
	UNSIGNED_CHAR: 'B',
	CHAR: 'b',
	UNSIGNED_SHORT: 'H',
	SHORT: 'h',
	UNSIGNED_INT: 'I',
	INT: 'i',
	UNSIGNED_LONG: 'L',
	LONG: 'l',
	FLOAT: 'f',
	DOUBLE: 'd',
}


########################################################################
# Formatter

class Formatter:
	"""Create a VTK data file."""

	version = (2, 0)
	
	def __init__(self, fp, format = ASCII):
		if format not in (ASCII, BINARY):
			raise ValueError('invalid format')

		self.fp = fp
		self.format = format

	def header(self, title = ''):
		if len(title) >= 256:
			raise ValueError('title must not exceed 256 characters')
		
		self.fp.write('# vtk DataFile Version %i.%i\n' % self.version)
		self.fp.write(title + '\n')
		self.fp.write(self.format + '\n')
	
	def item(self, data_type, item):
		if self.format is ASCII:
			self.fp.write(str(item))
		else:
			self.fp.write(struct.pack('!' + _data_type_fmt[data_type], item))

	def sequence(self, data_type, sequence, dimension = 1, seperator = None):
		dimension = dimension - 1

		if self.format is ASCII:
			if seperator is None:
				seperator = (' ', '\n', '\n\n')[dimension]
		else:
			seperator = ''

		first = 1
		for item in sequence:
				if first:
					first = 0
				else:
					self.fp.write(seperator)
				if dimension:
					self.sequence(data_type, item, dimension)
				else:
					self.item(data_type, item)

	# FIXME: implement other dataset formats
	
	def unstructured_grid(self, data_type, points, cells, cell_types):
		self.fp.write('DATASET UNSTRUCTURED_GRID\n')
		self.fp.write('POINTS %i %s\n' % (len(points), data_type))
		self.sequence(data_type, points, 2)
		self.fp.write('\n')
		self.fp.write('\n')
		self.fp.write('CELLS %i %i\n' % (len(cells), len(cells) + sum(map(len, cells))))
		self.sequence(INT, [(len(cell),) + tuple(cell) for cell in cells], 2)
		self.fp.write('\n')
		self.fp.write('\n')
		self.fp.write('CELL_TYPES %i\n' % (len(cell_types),))
		self.sequence(INT, cell_types, 1, '\n')
		self.fp.write('\n')
		self.fp.write('\n')
	
	def point_data(self, npoints):
		self.fp.write('POINT_DATA %i\n' % npoints)
	
	def cell_data(self, ncells):
		self.fp.write('CELL_DATA %i\n' % ncells)
		
	def field_data(self,name = 'Not_defined', number = 1):
		self.fp.write('FIELD %s %i\n' % (name,number))
		
	# FIXME: implement other dataset attribute formats

	def vectors(self, data_type, vectors, npoints, name = None):
		self.fp.write('%s %i %i %s\n' % (name ,3 ,npoints ,data_type))
		self.sequence(data_type, vectors, 2)
		self.fp.write('\n')
		
	def scalar(self, data_type, scalars, npoints, name = None):
		self.fp.write('%s %i %i %s\n' % (name ,1 ,npoints ,data_type))
		self.sequence(data_type, scalars, 1)
		self.fp.write('\n')

		
