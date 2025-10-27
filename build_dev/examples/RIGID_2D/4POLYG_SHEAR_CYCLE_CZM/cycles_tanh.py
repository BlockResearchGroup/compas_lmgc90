# -*- coding: utf-8 -*-
import os
import numpy as np
from scipy.integrate import quad 

def cycles_tanh(pdt,t0,list_depl,dt0,vitesse, tr=0.99999, figure = True, ofile = True, filename='evolution.dat', dir_ofile="") :
    """ Configuring of a cycling try depending of input displacements.

        * Parameters:
            - pdt = time step
            - t0 = first load time
            - list_depl = displacements list
            - dt0 = time of regime change
            - vitesse1 = end velocity (which will stay constant)
            - tr = troncature of the hyperbolic tangent function
            - figure = if display is activated
            - ofile = output file creation activated (2 columns time and velocity)
            - filename = name of ofile
            - dir_ofile = directory where ofile is put

        * Return 3 lists:
            - time steps list
            - results of velocity function list
            - associated displacements list
    """

    print( '---------------------------------------------')
    print( "Input data of cycles_tanh function:")
    print( '---------------------------------------------')
    print( "* List of displacements= \n{}".format(list_depl))
    print( "* Imposed velocity = {:.2e} m/s".format(vitesse))
    # print( "* Temps de changement de regime = {} s".format(dt0))
    print( "* First loading time = {} s".format(t0))
    print( "* Time step = {} s".format(pdt))
    
    #troncature hyperbolic tangent:
    tr1=np.arctanh(tr)
    
    #Computing displacement increment when slope change
    v0=vitesse
    v1=-vitesse
    def ffin(t):
        return np.tanh((t)*2*tr1/(dt0))*(v1-v0)/2+(v1+v0)/2.  
    
    xfin=np.linspace(-dt0*0.5,0,1000001)
    

    depres,err= quad(ffin, -dt0*0.5, 0) 

    print( '* norm : {:.2e}'.format(depres))
    
    #########boucle de chargement#############
    #initialisation
    i=0
    ltps=[0,t0]
    lVit1=[0,0]
    lDep1=[0,0]
    Dep1=0.
    tps=t0
    Vit1=0.
    list_depl = np.array(list_depl)
    while i<len(list_depl)-1:
    
        t1=t0+dt0
        tmoy=(t0+t1)/2.
        if i == 0 :
            v0=0
            v1=vitesse
        else:
            if i%2 == 0:
                v0=-vitesse
                v1=vitesse
            else:
                v0=+vitesse
                v1=-vitesse
            
        def Vimp(t):
            if t<=t0:
                return v0
            elif t>t0 and t<=t1:
                return np.tanh((t-tmoy)*2*tr1/(t1-t0))*(v1-v0)/2+(v1+v0)/2.
            else:
                return v1
        condition=True  
        while condition: 
            tps=tps+pdt
            Vit1=Vimp(tps)
            Dep1=Dep1+Vit1*pdt
            ltps.append(tps)
            lVit1.append(Vit1)
            lDep1.append(Dep1)
            if i%2 == 0:
                condition=Dep1<list_depl[i+1]-depres
            else:
                condition=Dep1>list_depl[i+1]+depres
        t0=tps
        i=i+1
    print( "* Final time = {:.2} s".format(tps))
    if figure == True :
        import matplotlib.pyplot as plt
        plt.figure('Courbe 1 : Time - Velocity')
        xlabel=('Time (s)')
        plt.ylabel('Velocity (m/s)')
        plt.ylim(-0.0011,0.0011)
        plt.plot(ltps, lVit1)
        plt.grid()
        # plt.savefig('Cycles_de_la_fontion_vitesse.pdf')
        
        plt.figure(u'Courbe 2 : Time - Displacement')
        xlabel=('Time (s)')
        plt.ylabel=('Displacement (m)')
        plt.plot(ltps, lDep1)
        plt.grid()
        plt.show()
    else :
        pass
    if ofile == True :
        # convert 2 lists in one array of 2 columns
        mat = np.column_stack((ltps,lVit1))
        # create the txt file for (lmgc90) gen-sample
        np.savetxt(fname = '{}{}'.format(dir_ofile,filename),fmt ='%14.7E ',X = mat)
    else :
        pass
    return [ltps,lVit1,lDep1]

if __name__ == '__main__' :
    
    ldeplacement=np.array([0.,0.020,0.000], float)
    a,b,c = cycles_tanh(pdt=1e-4,t0=0.1,list_depl=ldeplacement,dt0=0.1,vitesse=1e-3)
    
