#!/usr/bin/env python

from rev_2012 import *

# ... probably a bit too much
acceptance = ['y', 'yes', 'Y', 'o', 'O', 'oui']
refusal    = ['n', 'no' , 'N', 'non'          ]

def update_commands(file, com_dic):
  """ replace in input 'file' according to 'up_commands' dictionnary"""
  import fileinput
  for k in com_dic:
    for l in fileinput.input(file,inplace=1):
      print l.replace(k,com_dic[k])[:-1]

def remove_commands(file, com_lis):
  """ delete in input 'file' all lines containing strings in 'ob_commands' list"""
  import fileinput
  for e in com_lis:
    for l in fileinput.input(file,inplace=1):
      if not e in l:
        print l[:-1]

def remove_last_arguments(file, arg_dic):
  """ remove last argument in input 'file' according to 'arg_dic' dictionnary"""
  import fileinput
  for k in arg_dic:
    for l in fileinput.input(file,inplace=1):
      if k in l:
        debut   = l.find('(')
        fin     = l.rfind(')')
        args    = l[debut+1:fin].split(',')
        nb_args = len(args)
        if nb_args == 1:
          if len(l[debut+1:fin].strip()) == 0:
            nb_args = 0

        if nb_args > arg_dic[k]:
          if nb_args > 1:
            print l.replace(','+args[-1],'')[:-1]
          else:
            print l.replace(args[0],'')[:-1]
        else:
          print l[:-1]
      else:
        print l[:-1]

def check_input():
  """Verify input of the script and returns a list of files
     on which the updates will be done"""
  import sys
  import os
  
  if len(sys.argv) == 1:
    print 'no input file/directory given'

  file_list = []
  for arg in sys.argv[1:]:
    if os.path.isfile(arg):
      file_list.append(arg)
    elif os.path.isdir(arg):
      for root, dirs, files in os.walk(arg):
        # should exit all directories starting with dot and a alphanumeric character after ?
        if '.svn' in dirs: dirs.remove('.svn')
        for f in files:
          if f.endswith('.py') : file_list.append(os.path.join(root,f))
    else:
      print arg, ' is neither a file or directory ; ignored argument'

  # just in case...
  if sys.argv[0] in file_list:
    file_list.remove(sys.argv[0])
  if os.path.join(os.curdir,sys.argv[0]) in file_list:
    file_list.remove(os.path.join(os.curdir,sys.argv[0]))

  return file_list


if __name__ == '__main__':
  import sys

  fl = check_input()

  print 'The list of files to process is :'
  print fl
  todo = raw_input('Are you ok ? (y/n)\n')

  while todo not in acceptance and todo not in refusal:
    # magic trick just in case
    if todo.startswith('delete '):
      for f in todo.split(' ')[1:]:
        fl.remove(f)

    print 'The list of files to process is :'
    print fl
    todo = raw_input('Are you ok ? (y/n)\n')
  
  if todo in refusal:
    print 'exiting...'
    sys.exit()

  for f in fl:
    print 'removing obsolete commands in file : ', f
    remove_commands(f, ob_commands)
    print 'updating arguments in file : ', f
    remove_last_arguments(f,ch_arguments)
    print 'updating commands in file : ', f
    update_commands(f, up_commands)

