from vtk   import *
from pylab import *

class vtpFile(vtkXMLPolyDataWriter):
    def __init__(self,name):
        self.SetFileName(name)
    def setGrains(self,grains):
        points     = vtkPoints()
        cells      = vtkCellArray()

	Colors = vtkUnsignedCharArray();
	Colors.SetNumberOfComponents(3);
	Colors.SetName("Colors");
        Orientations = vtkFloatArray()
        Orientations.SetNumberOfComponents(1)
        Orientations.SetName("Orientations")
        Ids = vtkFloatArray()
        Ids.SetNumberOfComponents(1)
        Ids.SetName("Ids")
        num_points = 0
        
        #ids = []
        for gr in grains:
            cell_ids=[]
            #ids.append(gr.Id())
            for x,y in gr.shape():
                points.InsertPoint(num_points,x,y,0)
                cell_ids.append(num_points)
                num_points+=1
	    
            cell = vtkPolygon()		
            cell.GetPointIds().SetNumberOfIds(len(cell_ids))
            
            for i,j in enumerate(cell_ids):
                cell.GetPointIds().SetId(i,j)

            col = gr.color()
            Orientations.InsertNextTuple1(gr.orientation())
	    Colors.InsertNextTuple3(col[0],col[1],col[2]);
            cells.InsertNextCell(cell)
            Ids.InsertNextTuple1(gr.id())

        polydata = vtkPolyData()
        polydata.SetPoints(points)
        polydata.SetPolys(cells)
	polydata.GetCellData().SetScalars(Colors);
        polydata.GetCellData().AddArray(Orientations)
        polydata.GetCellData().AddArray(Ids)
        self._polydata = polydata
        #self.SetInput(polydata)

    def setFluidNetwork(self,fn):
        points     = vtkPoints()
        cells      = vtkCellArray()

        pts = fn.points()
        for ii,pt in pts.items():
            points.InsertPoint(ii,pt.x(),pt.y(),0.)
 
        segs = fn.segments()

        for seg in segs:
            cells.InsertNextCell(2)
            cells.InsertCellPoint(seg.source())
            cells.InsertCellPoint(seg.target())
           
          
 
        polydata = vtkPolyData()
        polydata.SetPoints(points)
        polydata.SetLines(cells)
       
        self._polydata = polydata
    def setCellScalars(self,name,Ts):
        Temperatures = vtkFloatArray()
        Temperatures.SetNumberOfComponents(1)
        Temperatures.SetName(name)
        
        for t in Ts:
            Temperatures.InsertNextTuple1(t)
        
        self._polydata.GetCellData().AddArray(Temperatures)
    def setPointScalars(self,name,Ts):
        Temperatures = vtkFloatArray()
        Temperatures.SetNumberOfComponents(1)
        Temperatures.SetName(name)
        
        for t in Ts:
            Temperatures.InsertNextTuple1(t)
        
        self._polydata.GetPointData().AddArray(Temperatures)
    def write(self):
        self._polydata.Update()
        self.SetInput(self._polydata)
        self.Write()
class structFile(vtkXMLStructuredGridWriter):
    def __init__(self,name):
        self.SetFileName(name)
    def addPoints(self,xs,ys):
        points     = vtkPoints()

        num_points = 0 
        for y in ys:
            for x in xs:
                points.InsertPoint(num_points,x,y,0.)
                num_points+=1

        self._sg = vtkStructuredGrid()
        self._sg.SetDimensions(len(xs),len(ys),1)
        self._sg.SetPoints(points)      
    def setCellColors(self,colors):
	Colors = vtkUnsignedCharArray();
	Colors.SetNumberOfComponents(3);
	Colors.SetName("Colors");
        for col in colors:
            Colors.InsertNextTuple3(col[0],col[1],col[2])
        self._sg.GetCellData().SetScalars(Colors)
    def setCellScalars(self,name,Ts,gradient=True):
        Temperatures = vtkFloatArray()
        Temperatures.SetNumberOfComponents(1)
        Temperatures.SetName(name)
        
        for t in Ts:
            Temperatures.InsertNextTuple1(t)
        
        self._sg.GetCellData().AddArray(Temperatures)
        if gradient:
            pointGradients = vtkGradientFilter()

            pointGradients.SetInput(self._sg)
            pointGradients.SetInputScalars(1,name);
            pointGradients.SetResultArrayName('Gradient '+name);
            pointGradients.Update()
            pdatas = pointGradients.GetOutput().GetCellData().GetArray('Gradient '+name)
            self._sg.GetCellData().AddArray(pdatas)
        
    def write(self):
        self._sg.Update()
        self.SetInput(self._sg)
        self.Write()
