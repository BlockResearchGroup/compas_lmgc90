import matplotlib.pyplot as plt
import matplotlib.ticker as ptick
from matplotlib.ticker import ScalarFormatter, FormatStrFormatter

import numpy as np

def do_plot(row,col,nb,title,xlabel,ylabel,x,y,xref=None,yref=None):

  plt.subplot(row,col,nb)
  plt.title(title)
  plt.xlabel(xlabel)
  plt.ylabel(ylabel)

  plt.plot(x,y,'x',label='LMGC90')
  if isinstance(xref,np.ndarray) and isinstance(yref,np.ndarray) :
    plt.plot(xref,yref,'red',label='ref')
  
  plt.legend()
  plt.gca().get_yaxis().get_major_formatter().set_powerlimits((0, 0))
  plt.gca().get_xaxis().get_major_formatter().set_powerlimits((0, 0))


if __name__ == "__main__":

  import pickle

  from all_cohesive_laws import *

  with open("RnGapBeta.p", "rb" ) as f:
      law2inter = pickle.load( f )
  
  nn = 1000

  for k, v in list(law2inter.items()):

    params = v[1]
    val    = v[2]

    if k in authorized_laws:

      maxdep = eval(k+'_maxdep')(params)

      b = np.zeros(nn)
      r = np.zeros([nn,3])
      u = np.zeros([nn,3])

      u[:,1] = np.linspace(0.,maxdep,nn)

      b[-1] = 1.
      for i in range(nn):
        # specious: surf is considered constant
        b[i], r[i,:] = eval(k)(b[i-1],u[i,:],params,val[3,0])

    maxn = val.shape[1]
    if k in authorized_laws:
      if val[0,-1] > maxdep:
        maxn = np.argmax( val[0,:] > maxdep )

      do_plot(1,2,1,'Normal Adhesion','gap','Rn',val[0,:maxn],val[1,:maxn],u[:,1],r[:,1])
      do_plot(1,2,2,'Damage','gap','beta',val[0,:maxn],val[2,:maxn],u[:,1],b)

    plt.suptitle(k)
    plt.show()

