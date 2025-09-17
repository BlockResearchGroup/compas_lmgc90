# used modules
import math
import os
import numpy as np
from matplotlib import pyplot as plt
import pickle
import os
import shutil
import sys
import subprocess
import pandas as pd
import fnmatch

## 2D postprocessing

def get_bodytracking(_path,_id):
    
  _names=['time','coorx','coory','coorz','depx','depy','theta','vx','vy','w']
  _w    =[ 15   , 15    , 15    , 15    , 15   , 15   , 15    , 15 , 15 , 15]
  
  file=os.path.join(_path,'BODY_'+'{:07d}'.format(_id)+'.DAT')
  
  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_torque(_path,_id):

  _names=['time','reacx','reacy','mreacz','fextx','fexty','mextz']
  _w    =[ 15   , 15    , 15    , 15     , 15    , 15    , 15    ]
  
  file=os.path.join(_path,'TORQUE_'+'{:07d}'.format(_id)+'.DAT')
  
  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_old_coordination(_path):
    
  _names=['time','all','comp','tract','active']
  _w    =[ 15   , 15  , 15   , 15    , 15     ]
  
  file=os.path.join(_path,'COORDINATION_NUMBER.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_coordination(_path):
    
  _names=['time','nbg','nbp0','nbc0','nbc0c','nbc0t','nbp1','nbc1','nbc1c','nbc1t']
  _w    =[ 15   , 8   ,  8   , 8    , 8     , 8     , 8    , 8    , 8     , 8     ]

  file=os.path.join(_path,'COORDINATION_NUMBER.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_compacity(_path):
    
  _names=['time','compacity','Vg','V']
  _w    =[ 15   , 15        , 15 , 15]

  file=os.path.join(_path,'COMPACITY_EVOLUTION.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_display(_path):
    
  _names=['time','sxx','sxy','syx','syy','fxx','fxy','fyx','fyy','s1','s2','f1','f2']
  _w    =[ 15   , 15  , 15  , 15  , 15  , 15  , 15  , 15  , 15  , 15 , 15 , 15 , 15 ]

  file=os.path.join(_path,'DISPLAY_TENSORS.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_normalcontactdistribution(_path,_id):
    
  _names=['tracx','tracy','wtracx','wtracy','stracx','stracy','compx','compy','wcompx','wcompy','scompx','scompy']
  _w    =[ 15    , 15    , 15     , 15     , 15     , 15     , 15    , 15    , 15     , 15     , 15     , 15     ]

  file=os.path.join(_path,'NORMAL_CONTACT_DIST.'+'{:04d}'.format(_id)+'.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df

## 3D postprocessing

def get_3D_bodytracking(_path,_id):
    
  _names=['time','coorx','coory','coorz','depx','depy','depz','vx','vy','vz','w1','w2','w3']
  _w    =[ 15   , 15    , 15    , 15    , 15   , 15   , 15   , 15 , 15 , 15 , 15 , 15 , 15 ]
  
  file=os.path.join(_path,'BODY_'+'{:07d}'.format(_id)+'.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_3D_fext(_path,_id):

  _names=['time','fextx','fexty','fextz','mext1','mext2','mext3']
  _w    =[ 15   , 15    , 15    , 15    , 15    , 15    , 15    ]
  
  file=os.path.join(_path,'Fext_'+'{:07d}'.format(_id)+'.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_3D_reac(_path,_id):

  _names=['time','reacx','reacy','reacz','meac1','meac2','meac3']
  _w    =[ 15   , 15    , 15    , 15    , 15    , 15    , 15    ]
  
  file=os.path.join(_path,'REAC_'+'{:07d}'.format(_id)+'.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_3D_fine(_path,_id):

  _names=['time','finex','finey','finez','mine1','mine2','mine3']
  _w    =[ 15   , 15    , 15    , 15    , 15    , 15    , 15    ]
  
  file=os.path.join(_path,'Fine_'+'{:07d}'.format(_id)+'.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')


## 2D|3D postprocessing

def get_solverinformations(_path):
    
  _names=['time','iter','err1','err2','err3','nb contacts']
  _w    =[ 15   , 9    , 15   , 15   , 15   , 9           ]

  file=os.path.join(_path,'SOLVER_INFORMATIONS.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')

def get_violationevolution(_path):
    
  _names=['time','mean g0','mean g','max gO','max g']
  _w    =[ 15   , 15      , 15     , 15     , 15    ]

  file=os.path.join(_path,'VIOLATION_EVOLUTION.DAT')

  df=pd.read_fwf(file,header=None,index_col=None,names=_names,widths=_w)
  df=df.replace('D','e',regex=True)
  df=df.apply(pd.to_numeric)
  return df.set_index('time')
