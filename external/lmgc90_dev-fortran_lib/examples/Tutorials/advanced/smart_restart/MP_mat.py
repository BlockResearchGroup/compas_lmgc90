import sys

def write_thermal_material(name,materialType,density,
                           anisotropy,thermal_conductivity,
                           specific_heat,
                           thermal_young,thermal_nu,
                           WSlaw=None,WS=0.,WSvar=0.,Tmelt=0.,Tvar=0.,                         
                           PATH='./DATBOX'):
    """
    name         : the name of the law (corresponds to the one defined for avatar)   

    materialType : 'THERMO_RIGID' or 'THERMO_CHEMICAL_RIGID'

    anisotropy   : defines anisotropy of conduction; must be 'iso' or 'ani'

    thermal_conductivity : if 'iso' a scalar ; if 'ani' is a liste(4)

    specific_heat :

    thermal_young :

    thermal_nu    :

    WSlaw         : management of surface energy (if THERMO_CHEMICAL_RIGID) ; should be 'LMM05' | ???

    WS            : initial surface energy

    WSvar         :

    Tmelt         : 

    Tvar          : 

    """
    #    1234567890123456789012345678901234567890
    s = '                                        '
    with open(PATH+'/BULK_BEHAV.DAT','a+') as ofile:
        ofile.write('$behav  lawty\n')
        ofile.write(' %-5s  %-30s  Umas=%14.7e\n' % (name,materialType,density))
        ofile.write(s+'%3s:\n' % anisotropy[0:3])
        if anisotropy[0:3] == 'iso': 
          if isinstance(thermal_conductivity, list) :
             print('error if isotropy thermal_conductivity is a scalar')
             sys.exit()
         
          ofile.write(s+'TCnd=%14.7e\n' % thermal_conductivity)
        elif anisotropy[0:3] == 'ani':
          if len(thermal_conductivity) != 4 :
             print('error if anisotropy thermal_conductivity is a vector of 4 values')
             sys.exit()
            
          ofile.write(s+'PrTC=%14.7e  ScTC=%14.7e\n' % (thermal_conductivity[0], thermal_conductivity[1]))
          ofile.write(s+'PDnx=%14.7e  PDny=%14.7e\n' % (thermal_conductivity[2], thermal_conductivity[3]))
        else :
          print('error anisotropy unknown', anisotropy[0:3])
          sys.exit()

        ofile.write(s+'Hspe=%14.7e\n' % specific_heat)
        ofile.write(s+'Eeq_=%14.7e  Nueq=%14.7e\n' % (thermal_young,thermal_nu))

        if materialType == 'THERMO_CHEMICAL_RIGID' :
          ofile.write(s+'WSlw=%s\n' % WSlaw)
          ofile.write(s+'WS__=%14.7e  WSvr=%14.7e\n' % (WS, WSvar))      
          ofile.write(s+'TMlt=%14.7e  Tvar=%14.7e\n' % (TMelt, Tvar))

    return

