


header= '<?xml version="1.0"?>\n<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">\n<Collection>'

closing = '</Collection>\n</VTKFile>'

class pvDataFile:
    def __init__(self,fichier):
        self._fichier = fichier
        self._fid     = open(fichier,'w')
        self._fid.write(header)
    def line(self,time,fichier):
        impr='<DataSet timestep="%s" group="" part="0" file="%s"/>\n' % (time,fichier)
        self._fid.write(impr)
    def close(self):
        self._fid.write(closing)
        self._fid.close()
