import sys

# variable indiquant la facon d'arreter l'execution en cas d'erreur :
#   * 'standard' : on sort avec un sys.exit(0)
#   * 'exception' : on sort en lancant une exception, qui arretera le programme si elle n'est pas rattrapee
# N.B.: l'option exception est notamment utile dans le cas de Salome, en mode GUI, qui plante si on utilise sys.exit
#   * 'pass' : on ne fait rien et le programme continue
# N.B.: l'option pass et utile pour l'auto-verification du module lmgc90dicts
stop_mode='exception'

def setStopMode(stop):
    """setStopMode(stop):
       ithis function changes the way the the program is stopped if an error occurs.
       parameter:
          - stop: possible values are:
             * 'standard' : the program is stopped using a call of sys.exit (dafault value)
             * 'exception' : an exception is raised, and this will stop the program if it's not catched
             * 'pass' : the program is not stopped
    """

    global stop_mode

    if stop in ('standard', 'exception', 'pass'):
       stop_mode = stop
    else:
       showWarning("stop mode is unchanged, since \"stop_mode\"=" + str(stop) + " is unhandled")

def showError(msg):
    """showError(msg):
       this function prints an error message and stops the execution the program.
       parameter:
          - msg: the message to be shown
    """

    # on affiche le message d'erreur
    print('ERROR\n'+msg+'\n')

    # on interrompt l'execution
    if stop_mode == 'standard':
       # * cas standard : sys.exit
       sys.exit(-1)
    elif stop_mode == 'exception':
       # * cas de l'exception
       raise Exception
    elif stop_mode == 'pass':
       # * cas ou on ne fait rien
       pass

def showWarning(msg):
    """showError(msg):
       this function prints a warning message. 
       parameter:
          - msg: the message to be shown
       N.B.: Unlike showError, this function do not stop the program.
    """

    from .. import config

    if ( config.nowarning ):
        return

    print('WARNING\n'+msg+'\n')
