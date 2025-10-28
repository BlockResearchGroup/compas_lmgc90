#from common.variables import *

import os
import re
import itertools
import numpy as np
#
#
#
#--Fichier   : GPV_INI
#
#
#
# RAJOUTER LES COORDONEES DANS LE DOF.INI SOUS FORMAT  1 2 3

#pfff
x2m = re.compile( r"THERx" )

def initGPV_ini(chemin=''):
    """
    Write in a GPV.INI file the header.

    :param chemin: the directory in which to write the file.
    """

    print()
    print('Start writing file\t:\tGPV.INI')
    fid = open(os.path.join(chemin,'GPV.INI'),'w')
    fid.write('\n! Gauss Point Values\n\n')
    fid.write('$steps      0                time= 0.0000000D+00\n')
    fid.write('\n!-----------------------------------------------------------------------\n')
    fid.close()
    
def closeGPV_ini(chemin=''):
    """
    Write in a GPV.INI file the tailer.

    :param chemin: the directory in which to find the file.
    """
    fid=open(os.path.join(chemin,'GPV.INI'),'a')
    fid.write('      \n')
    fid.close()
    print('End of writing file\t:\tGPV.INI')
    
def writeGPVIni(parts,chemin=''):
    """
    Write a GPV.INI file from an avatar container.

    :param part: an avatar container
    :param chemin: the directory in which to write the file.
    """

    initGPV_ini(chemin)

    with open( os.path.join(chemin,'GPV.INI'), 'a' ) as fid:

        for p in parts:

            if p.atype != 'MAILx' or not p.iniGpv:
                continue

            line = '$bdyty'
            fid.write( line+'\n' )

            line = ' {}{: 7d}'.format( p.atype, p.number )
            fid.write( line+'\n' )
            for b in p.bulks:
                if b.model is None:
                    continue
                line = '$blmty'
                fid.write( line+'\n' )

                line = ' {}{: 7d}'.format( b.etype, b.number+1 )
                fid.write( line+'\n' )
                line = '$model'
                fid.write( line+'\n' )
                line = ' {}'.format( x2m.sub( "THERM", b.model.physics) )
                fid.write( line+'\n' )

                list_mfields = itertools.zip_longest(b.mgrad, b.mflux, b.minte, fillvalue = None)
                for fields in  list_mfields:
                    for f in fields:
                        if f is not None:
                            # damned formatting of Fortran
                            f = np.where( np.abs(f)<1.e-100, 0., f )
                            f = np.where( f<-1.e+100, -9.999999e99, f )
                            f = np.where( f> 1.e+100, +9.999999e99, f )
                            # ouch... this one is strange !
                            f = np.where( np.isnan(f), 0., f )
                            line = "".join( ["{: .7e}".format(v) for v in f ] )
                            fid.write( line+'\n' )

                list_tfields = itertools.zip_longest(b.temp , b.tgrad, b.tflux, b.tinte, fillvalue = None)
                # writing tfields after meca for correct poro manangment
                for fields in  list_tfields:
                    for f in fields:
                        if f is not None:
                            # damned formatting of Fortran
                            f = np.where( np.abs(f)<1.e-100, 0., f )
                            f = np.where( f<-1.e+100, -9.999999e99, f )
                            f = np.where( f> 1.e+100, +9.999999e99, f )
                            # ouch... this one is strange !
                            f = np.where( np.isnan(f), 0., f )
                            line = "".join( ["{: .7e}".format(v) for v in f ] )
                            fid.write( line+'\n' )

            line = '$$$$$$'
            fid.write( line+'\n' )

    closeGPV_ini(chemin)
