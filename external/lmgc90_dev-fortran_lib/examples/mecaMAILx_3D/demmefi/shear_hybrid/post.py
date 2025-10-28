# used modules
import pathlib
import pandas as pd

def read2df(file,names,widths):
  df=pd.read_fwf(file,header=None,index_col=None,names=names,widths=widths)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df

## 2D postprocessing

def get_bodytracking(_path,_id):
    
  _names=['time','coorx','coory','coorz','depx','depy','theta','vx','vy','w']
  _w    =[ 15   , 15    , 15    , 15    , 15   , 15   , 15    , 15 , 15 , 15]
  
  file=_path/('BODY_'+'{:07d}'.format(_id)+'.DAT')
  df = read2df(file,_names,_w)
  
  return df.set_index('time')

def get_torque(_path,_id):

  _names=['time','reacx','reacy','mreacz','fextx','fexty','mextz']
  _w    =[ 15   , 15    , 15    , 15     , 15    , 15    , 15    ]
  
  file=_path/('TORQUE_'+'{:07d}'.format(_id)+'.DAT')
  
  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_old_coordination(_path):
    
  _names=['time','all','comp','tract','active']
  _w    =[ 15   , 15  , 15   , 15    , 15     ]
  
  file=_path/'COORDINATION_NUMBER.DAT'

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_coordination(_path):
    
  _names=['time','nbg','nbp0','nbc0','nbc0c','nbc0t','nbp1','nbc1','nbc1c','nbc1t']
  _w    =[ 15   , 8   ,  8   , 8    , 8     , 8     , 8    , 8    , 8     , 8     ]

  file=_path/'COORDINATION_NUMBER.DAT'

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_compacity(_path):
    
  _names=['time','compacity','Vg','V']
  _w    =[ 15   , 15        , 15 , 15]

  file=_path/'COMPACITY_EVOLUTION.DAT'

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_display(_path):
    
  _names=['time','sxx','sxy','syx','syy','fxx','fxy','fyx','fyy','s1','s2','f1','f2']
  _w    =[ 15   , 15  , 15  , 15  , 15  , 15  , 15  , 15  , 15  , 15 , 15 , 15 , 15 ]

  file=_path/'DISPLAY_TENSORS.DAT'

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_normalcontactdistribution(_path,_id):
    
  _names=['tracx','tracy','wtracx','wtracy','stracx','stracy','compx','compy','wcompx','wcompy','scompx','scompy']
  _w    =[ 15    , 15    , 15     , 15     , 15     , 15     , 15    , 15    , 15     , 15     , 15     , 15     ]

  file=_path/('NORMAL_CONTACT_DIST.'+'{:04d}'.format(_id)+'.DAT')

  df = read2df(file,_names,_w)
  return df

## 3D postprocessing

def get_3D_bodytracking(_path,_id):
    
  _names=['time','coorx','coory','coorz','depx','depy','depz','vx','vy','vz','w1','w2','w3']
  _w    =[ 15   , 15    , 15    , 15    , 15   , 15   , 15   , 15 , 15 , 15 , 15 , 15 , 15 ]
  
  file=_path/('BODY_'+'{:07d}'.format(_id)+'.DAT')

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_3D_fext(_path,_id):

  _names=['time','fextx','fexty','fextz','mext1','mext2','mext3']
  _w    =[ 15   , 15    , 15    , 15    , 15    , 15    , 15    ]
  
  file=_path/('Fext_'+'{:07d}'.format(_id)+'.DAT')

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_3D_reac(_path,_id):

  _names=['time','reacx','reacy','reacz','meac1','meac2','meac3']
  _w    =[ 15   , 15    , 15    , 15    , 15    , 15    , 15    ]
  
  file=_path/('REAC_'+'{:07d}'.format(_id)+'.DAT')

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_3D_fine(_path,_id):

  _names=['time','finex','finey','finez','mine1','mine2','mine3']
  _w    =[ 15   , 15    , 15    , 15    , 15    , 15    , 15    ]
  
  file=_path/('Fine_'+'{:07d}'.format(_id)+'.DAT')

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_3D_dep(_path,_id):

  _names=['time','depx','depy','depz','vx','vy','vz']
  _widths=[15   , 15   , 15   , 15   , 15 , 15 , 15 ]
  
  file=_path/('Dep_'+'{:07d}'.format(_id)+'.DAT')
  dep = read2df(file,_names,_widths)
  return dep.set_index('time')

def get_3D_fint(_path,_id):

  _names=['time','reacx','reacy','reacz','fintx','finty','fintz','finerx','finery','finerz','fextx','fexty','fextz','resx','resy','resz','momx','momy','momz']
  _widths=[15   , 15    , 15    , 15   , 15     , 15    , 15    , 15     , 15     , 15     , 15    , 15    , 15    , 15   , 15   , 15   , 15   , 15   , 15]
  
  file=_path/('Fint_'+'{:07d}'.format(_id)+'.DAT')
  fint = read2df(file,_names,_widths)
  return fint.set_index('time')

## 2D|3D postprocessing

def get_solverinformations(_path):
    
  _names=['time','iter','err1','err2','err3','nb contacts']
  _w    =[ 15   , 9    , 15   , 15   , 15   , 9           ]

  file=_path/'SOLVER_INFORMATIONS.DAT'

  df = read2df(file,_names,_w)
  return df.set_index('time')

def get_violationevolution(_path):
    
  _names=['time','mean g0','mean g','max gO','max g']
  _w    =[ 15   , 15      , 15     , 15     , 15    ]

  file=_path/'VIOLATION_EVOLUTION.DAT'

  df = read2df(file,_names,_w)
  return df.set_index('time')
