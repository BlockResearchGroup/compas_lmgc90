
def write_thermal_model(T0,alert,ldiff,gdiff,lconv,lkine,bound,Gcond=0.,GTemp=0.,PATH='./DATBOX'):
    """
                               
    T0    : initial temperature

    alert : distance between boundary contactors (necessary for diffusion in a boundary made as a cluster)

    ldiff : managing thermal exchange between grains ; can be: Cylnd | Hertz

    gdiff : managing thermal exchange between grains ; can be: continue | discrete

    lconv : convection on free surface ; can be: no | yes
            if yes must provide conductivity (Gcond) and temperature (GTemp)

    lkine : part of local dissipation contributing to heating ; can be : no | all | dvn | dvt

    bound : managing thermal excahnge on bounds ; can be: adia | line | 1D | 2D
            if bound is 1D or 2D a 'bound model' is mandatory

    """

    #    12345678901234
    s = '              '
    
    with open(PATH+'/MP_DEM.DAT','w') as ofile:
        ofile.write('$model  therm\n')
        ofile.write(s+'T0___=%14.7e\n' % T0)
        ofile.write(s+'alert:%14.7e\n' % alert)
        ofile.write(s+'ldiff: %5s %s\n' % (ldiff,gdiff))
        ofile.write(s+'lconv: %s\n' % lconv)
        if lconv == 'yes':
          ofile.write(s+'       Gcond:%14.7e  GTemp:%14.7e\n' % (Gcond,GTemp))  
        ofile.write(s+'lkine: %s\n' % lkine)
        ofile.write(s+'bound: %s\n' % bound)

    return

def write_thermal_bounds(first,last,T0,thickness,length,alpha,locus=None,PATH='./DATBOX'):
    """
          
    first     : first body

    last      : last body

    T0        : applied temperature

    thickness : thickness of the bound

    length    : length of the bound

    alpha     : diffusivity in the bound (H/rho*C*V)

    locus     : locus of the bound ; may be 'U','D','L','R' or none

    """

    #    1234567890123456789012345678901
    s = '                               '

    with open(PATH+'/MP_DEM.DAT','a+') as ofile:
        if locus is None:
          ofile.write('$bounds  therm\n')
        else :
          ofile.write('$bounds  therm %1s\n' % locus[0])
          
        ofile.write('         %07d  to  %07d  T0__=%14.7e  Thkn=%14.7e\n' % (first,last,T0,thickness))
        ofile.write(s+                       'leng=%14.7e  alph=%14.7e\n' % (length,alpha))
    
    return

def write_thermal_source(first,last,T0,PATH='./DATBOX'):
    """
          
    first : first body

    last  : last body

    T0    : applied temperature


    """
    with open(PATH+'/MP_DEM.DAT','a+') as ofile:
        ofile.write('$source  therm\n')
        ofile.write('         %07d  to  %07d  T0__=%14.7e\n' % (first,last,T0))
    return

def write_electrical_model(PATH='.DATBOX'):
    """

    """

    s = '              '
    
    with open(PATH+'/MP_DEM.DAT','w') as ofile:
        ofile.write('$model  elec_\n')
    return
