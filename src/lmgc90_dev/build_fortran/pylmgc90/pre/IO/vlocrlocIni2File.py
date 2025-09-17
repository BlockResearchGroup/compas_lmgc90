#
#
#
#--Fichier   : Vloc_Rloc_INI
#
#
#
import os

import numpy as np

from ..utilities import check_compiled_modules

if check_compiled_modules.import_lmgc90():
    try:
        from ...chipy import lmgc90
    except:
        raise

from .formatVlocRloc import INTERS2FORMAT, INTERS2HEADER, HEADERS, FORMATS, XL, \
                            COO_FIRST, STN_INTERS, TNS_INTERS, STN_ORDER, TNS_ORDER


from . import utils

def initVlocRloc_ini(chemin=''):
    """
    Write the header of a VlocRloc.INI file

    :param chemin: the directory in which to write the file
    """

    print()
    print('Start writing file\t:\tVloc_Rloc.INI')
    fid = open(os.path.join(chemin,'Vloc_Rloc.INI'),'w')
    fid.write('\n! Vloc_Rloc\n\n')
    fid.write('$steps      0                time= 0.0000000D+00\n\n')
    fid.write('!-----------------------------------------------------------------------\n')
    fid.close()


def closeVlocRloc_ini(chemin=''):
    """
    Write the tailer of a VlocRloc.INI file

    :param chemin: the directory in which to write the file
    """

    fid=open(os.path.join(chemin,'Vloc_Rloc.INI'),'a')
    fid.write('      \n')
    fid.close()
    print('End of writing file\t:\tVloc_Rloc.INI')


def writeVlocRlocIni(chemin='', inters=None, tacts=None, with_xl=False):
    """
    Write a VlocRloc.INI file

    :param chemin: the directory in which to write the file
    :param inters: the interactions numpy array
    :param tacts: the contact law container associated with the inters array
    :param with_xl: (optional) if using XL format
    """

    if inters is not None and not isinstance(inters, (np.ndarray,list,)):
        utils.update_graph(inters)
        inters = inters.es['inter']

    initVlocRloc_ini(chemin)

    with open(os.path.join(chemin,'Vloc_Rloc.INI'),'a') as fid:
        for inter in inters if inters is not None else []:

           # beurk ?
           dim = inter['coor'].size
           inter_type = inter['inter'].decode()

           tns = 'tns'[:dim]

           #line = f"$icdan  {inter_type}{inter['icdan']:9d}"
           line = "$icdan  {}{:9d}".format( inter_type, inter['icdan'] )
           fid.write(line+'\n')

           # writing format:
           format_id = INTERS2FORMAT[inter_type]
           if inter_type in XL and with_xl:
             format_id = XL[inter_type]
           line = HEADERS[ INTERS2HEADER[inter_type] ]
           fid.write(line+'\n')

           # building integer formatted line:
           line = ""
           for field, convert, prefix, length in FORMATS[format_id] :
               if convert is int :
                   v = "{:{length}d}".format( inter[field], length=length )
               else:
                   v = inter[field].decode()
               line = line + prefix+v

           #line = "".join([ prefix+f"{inter[field]:{lenght}d}" if convert is int else prefix+inter[field].decode() for field, convert prefix, length in FORMATS[format_id] ])
           fid.write(line+'\n')

           if dim==2 or inter_type in ('PRPRx', 'PTPT3',):
               cmp_order = TNS_ORDER[:dim]
           else:
               cmp_order = STN_ORDER
           if inter_type in ('CDCDx', 'CDPLx', 'SPCDx', 'SPDCx', 'SPPLx', 'SPSPx'):
               gapTT = 'gTT ='
           else:
               gapTT = 'gapTT'
 
           # writing rl, vl and gapTT
           rl = [ "  rl{}/H{: .7e}".format(c, inter['rl'][i]) for i, c in cmp_order ]
           line = " "*27 + "".join( rl )
           fid.write(line+'\n')
           vl = [ "  vl{} ={: .7e}".format(c, inter['vl'][i]) for i, c in cmp_order ]
           line = " "*27 + "".join(vl )
           fid.write(line+'\n')
           line = " "*27 + "  {}{: .7e}".format(gapTT, inter['gapTT'])
           fid.write(line+'\n')

           # sometimes writing coor first
           coor = [ "  coo{}={: .7e}".format(i+1, inter['coor'][i]) for i in range(dim) ]
           if inter_type in COO_FIRST:
               line = " "*27 + "".join( coor )
               fid.write(line+'\n')

           # then tns in the right order
           if dim == 2 or inter_type == 'PTPT3':
               ucc  = [ "  n({})={: .7e}".format(i+1, inter['uc'][1,i]) for i in range(dim) ]
               line = " "*27 + "".join( ucc )
               fid.write(line+'\n')
           else:
               cmp_list = STN_ORDER if inter_type in STN_INTERS else TNS_ORDER
               for i, c in cmp_list:
                 ucc  = [ "  {}({})={: .7e}".format(c, j+1, inter['uc'][i,j]) for j in range(dim) ]
                 line = " "*27 + "".join( ucc )
                 fid.write(line+'\n')

           # and most times writing coor last
           if inter_type not in COO_FIRST:
               line = " "*27 + "".join( coor )
               fid.write(line+'\n')

           # write internals here...
           nb_int = inter['nb_int']
           if nb_int :
               lmgc90.overall_DIME(dim,1)
               law_type = tacts[inter['behav'].decode()].law.ljust(30)
               line = lmgc90.tact_behav_GetLawInternalComment( law_type )
               fid.write(line+'\n')
               internals = [ "{: .7e}".format(value) for value in inter['internals'][:nb_int] ]
               line = " " + " ".join( internals )
               fid.write(line+'\n')

           # and go to next !
           fid.write('\n')
           
    closeVlocRloc_ini(chemin)
