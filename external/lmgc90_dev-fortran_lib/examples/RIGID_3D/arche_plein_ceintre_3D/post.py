import pickle

from pylmgc90 import post

with open('f2f_inters.pkl', 'rb') as f:
  f2f, inters = pickle.load(f)

polyg, ckernel = post.central_kernel.get(f2f, inters)

