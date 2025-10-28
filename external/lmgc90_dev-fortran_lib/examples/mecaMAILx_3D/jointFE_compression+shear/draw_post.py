from math import *
import numpy as np
import matplotlib.pyplot as plt
import sys


if __name__ == '__main__':    

    # traction

    # dep_p,s,dep_tot
    du=np.genfromtxt("./POSTPRO/Dep_0000001.DAT")
    dl=np.genfromtxt("./POSTPRO/Dep_0000002.DAT")
    dr=np.genfromtxt("./POSTPRO/Dep_0000003.DAT")    
    f=np.genfromtxt("./POSTPRO/Fint_0000001.DAT")


    plt.subplot(1,2,1)
    plt.title('vertical displacement - time')
    plt.xlabel(r'$t (s)$')
    plt.ylabel(r'$u_z (mm)$')

    plt.plot(du[:,0],1e3*du[:,3],'mo-')

    plt.subplot(1,2,2)
    plt.title('displacement horizontal - time')
    plt.xlabel(r'$t (s)$')
    plt.ylabel(r'$u_x (mm)$')

    plt.plot(dr[:,0],1e3*dr[:,1],'ro-')
    plt.plot(dl[:,0],1e3*dl[:,1],'bo-')

    plt.show()

    
    plt.subplot(1,2,1)
    plt.title('force-displacement vertical')
    plt.xlabel(r'$u_z (mm)$')
    plt.ylabel(r'$f_z (kN)$')

    plt.plot(1e3*du[:,3],1e-3*f[:,6],'mo-')

    plt.subplot(1,2,2)
    plt.title('force-displacement horizontal')
    plt.xlabel(r'$u_x (mm)$')
    plt.ylabel(r'$f_x (kN)$')

    plt.plot(1e3*dl[:,1],1e-3*f[:,4],'mo-')

    plt.show()


