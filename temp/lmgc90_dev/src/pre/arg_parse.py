
import sys
import argparse

from . import config

def parse( argv = sys.argv ):
    """ Parse option given on command line.

    optional arguments:
      -h, --help   show this help message and exit
      --novisu     disable the display of visuAvatars() window
    """

    # Remove the name of script from the list of arguments.
    argv = argv[ 1 : ]

    parser = argparse.ArgumentParser( description = 'Parse option given on command line.' )

    parser.add_argument( "--novisu", dest = "novisu", action = "store_true", default = False, \
                         help="disable the display of visuAvatars() window" )

    parser.add_argument( "--nowarning", dest = "nowarning", action = "store_true", default = False, \
                         help="disable the warning messages" )

    arguments, unknown = parser.parse_known_args( )

    dict_arguments = vars( arguments )

    # Accordingly to the list of arguments, set variable of the module 'config'.
    for k, v in list(dict_arguments.items( )):
        if k in ( "novisu" ):
            if ( v ):
                config.novisu = True
        if k in ( "nowarning" ):
            if ( v ):
                config.nowarning = True

