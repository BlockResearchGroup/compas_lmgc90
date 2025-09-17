from math import *
import numpy as np
import matplotlib.pyplot as plt
import sys

def TRACE_CRITERE(RTRAC,C,PHI,ZMU):
    CRICIS1 = []
    CRICIS2 = []
    CRICIP1 = []
    CRICIP2 = []
    ABSTAU1 = []
    ABSTAU2 = []
    CRINOR = []

    ZERO = []

    TAU = []
    t = -5*C

    SIGMA=[]
    s = -5*RTRAC

    if (ZMU == 0.):
        COTMU = 1. / tan(PHI)
    else:
        COTMU = 1. / tan(ZMU)

    Cprime = C - RTRAC * tan(PHI)
    Cseconde = C - RTRAC * (tan(PHI) + (COTMU))

    ds = 10.*RTRAC/100.
    dt = 10.*C/100.
    for i in range (0,100):
        s = s + ds
        t = t + dt

        cri1 = s * tan(PHI) - C
        if (cri1 <= -Cprime):
            CRICIS1 = CRICIS1 + [cri1]
        else:
            CRICIS1 = CRICIS1 + [-Cprime]

        cri2 = - s * tan(PHI) + C            
        if (cri2 >= Cprime):
            CRICIS2 = CRICIS2 + [cri2]
        else:
            CRICIS2 = CRICIS2 + [Cprime]

        cip1 = s * (COTMU) + Cseconde
        if (cip1 >= Cprime):
            CRICIP1 = CRICIP1 + [cip1]
        else:
            CRICIP1 = CRICIP1 + [Cprime]
            
        cip2 = - s * (COTMU) - Cseconde
        if (cip2 <= -Cprime):
            CRICIP2 = CRICIP2 + [cip2]
        else:
            CRICIP2 = CRICIP2 + [-Cprime]

        ABSTAU1 = ABSTAU1 + [Cprime]
        
        ABSTAU2 = ABSTAU2 + [-Cprime]

        CRINOR = CRINOR + [RTRAC]

        ZERO = ZERO + [0.]

        SIGMA = SIGMA + [s]
        TAU = TAU + [t]

    plt.axis("equal")
    # plt.xlim(-10.,10.)
    # plt.ylim(-10.,10.)

    plt.plot(SIGMA,ZERO,"k", linewidth=1)
    plt.plot(ZERO,TAU, "k", linewidth=1)
    plt.plot(SIGMA,CRICIS1,"r", linewidth=1 )
    plt.plot(SIGMA,CRICIS2,"r", linewidth=1 )
    plt.plot(SIGMA,CRICIP1,"b", linewidth=1 )
    plt.plot(SIGMA,CRICIP2,"b", linewidth=1 )
    plt.plot(SIGMA,ABSTAU1,"m", linewidth=1 )
    plt.plot(SIGMA,ABSTAU2,"m", linewidth=1 )
    plt.plot(CRINOR,TAU,"g",linewidth=1)

    plt.xlabel("sigma")
    plt.ylabel("tau")

    return(plt)

if __name__ == '__main__':    


    import pickle
    f = open('data.pkl', 'rb')
    (RTRAC,C,PHI,ZMU) = pickle.load(f)
    f.close()

    # traction

    # dep_p,s,dep_tot
    a=np.genfromtxt("gpi.txt")

    plt.subplot(1,2,1)
    plt.title('contrainte-deformation')
    plt.xlabel(r'$u_n$')
    plt.ylabel(r'$\sigma_n$')
    # dep_tot_n,s_n
    plt.plot(a[:,3],a[:,6],'m-')

    plt.subplot(1,2,2)
    plt.title('defo plastique-deformation')
    plt.xlabel(r'$u_n$')
    plt.ylabel(r'$u^p_n$')
    # dep_tot_n,dep_p_n
    plt.plot(a[:,3],a[:,9],'m-')

    plt.show()

    # tracé état de contraintes
    plt = TRACE_CRITERE(RTRAC,C,PHI,ZMU)

    for b in a:
      # s_n  
      Xnor = b[6]
      # s_t1,s_t2
      Ycis = sqrt( (b[4] ** 2) + (b[5] ** 2) )
      plt.plot(Xnor,Ycis,"b^",linewidth=1)
    
    plt.show()

