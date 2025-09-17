import math, numpy
try:
  import scipy
  import scipy.special
except ImportError:
  pass

def granulo_Random(nb, r_min, r_max, seed=None):
  """Generates a list of radii bewteen r_min and r_max following a uniform
     distribution in number"""
  
  assert r_min <= r_max, "r_min must be inferior to r_max"
  assert r_min > 0., "r_min must be greater than 0."

  if seed is not None: numpy.random.seed(seed)

  return numpy.random.uniform(r_min,r_max, nb)

def granulo_Uniform(nb, r_min, r_max, seed=None):
  """Generates a list of radii between r_min and r_max following a uniform
      distribution in surface"""

  assert r_min <= r_max, "r_min must be inferior to r_max"
  assert r_min > 0., "r_min must be greater than 0."

  if seed is not None: numpy.random.seed(seed)

  return r_max*r_min/( r_min + r_max - numpy.random.uniform(r_min,r_max,nb) )

def granulo_TwoSizesNumber(nb, r_min, r_max, p_min, seed=None):
  """Generates a granulometry composed of two radii, r_min and r_max.
     The distribution is binomial in number"""
  
  assert r_min <= r_max, "r_min must be inferior to r_max"
  assert r_min > 0., "r_min must be greater than 0."

  if seed is not None: numpy.random.seed(seed)

  #a = numpy.random.binomial(1,p_min,[nb])
  #g = numpy.vectorize(lambda x: r_min if x==1 else r_max, [numpy.float_])
  #return g(a)
  g = numpy.random.random_sample(nb)
  g = numpy.where(g < p_min, r_min, r_max)
  return g
  
def granulo_TwoSizesVolume(nb, r_min, r_max, p_min, seed=None):
  """Generates a granulometry composed of two radii, r_min and r_max.
     The distribution is binomial in surface"""
  
  assert r_min <= r_max, "r_min must be inferior to r_max"
  assert r_min > 0., "r_min must be greater than 0."

  p = 1.0/ ( 1.0 + (1.-p_min)*r_min**2/(p_min*r_max**2) )
  return granulo_TwoSizesNumber(nb, r_min, r_max, p, seed)
  
def granulo_ReadFromFile(name):
  """Read a granulometry from a file"""

  return numpy.loadtxt(name,'d')

def granulo_BetaDistribution(r_min, r_max, nb_t, vol, a, b, seed=None):
  """Generates a granulometry curve between r_min and r_max
     in a volume vol using a Beta function split in nb_t slices.
     a and b parameters are driving the shape of the distribution.
     See Voivret's PhD 2008 for details. 
  """

  assert r_min <= r_max, "r_min must be inferior to r_max"
  assert r_min > 0., "r_min must be greater than 0."

  if seed is not None: numpy.random.seed(seed)

  R3 = 3.*vol/(4.*math.pi)
  dr = (r_max-r_min) / nb_t

  beta_old = 0.
  g = []
  for i in range(1,nb_t+1):

    beta = scipy.special.betainc(a,b,i*1./nb_t)

    rb= r_min + (i-1)*dr
    r = rb + 0.5*dr

    nb = int((beta-beta_old)*R3/r**3)
    beta_old = beta
  
    g.append(numpy.random.uniform(rb,rb+dr,nb))

  gr = numpy.concatenate(g)
  numpy.random.shuffle(gr)
  return  gr

if __name__ == "__main__":
  import time

  nb = 100000
  ra = 2.
  rb = 20.
  pm = 0.25
  fname = 'granulo.txt'
  V  = 1.e6
  x  = 1.
  a  = b = 1.

  t1 = time.time()
  rand = granulo_Random(nb,ra,rb)
  print(time.time()-t1)
    
  t1 = time.time()
  unif = granulo_Uniform(nb,ra,rb)
  print(time.time()-t1)
  
  t1 = time.time()
  twos = granulo_TwoSizesNumber(nb,ra,rb,pm)
  print(time.time()-t1)

  t1 = time.time()
  twov = granulo_TwoSizesVolume(nb,ra,rb,pm)
  print(time.time()-t1)

  numpy.savetxt(fname,twov)
  t1 = time.time()
  read = granulo_ReadFromFile(fname)
  print(time.time()-t1)
  assert numpy.all(twov==read)

  t1 = time.time()
  ra=4.e-3; rb=1.e-2; V=1.e-3; a=b=1;nb_t=10
  beta = granulo_BetaDistribution(ra,rb,nb_t,V,a,b)
  print(time.time()-t1)
  
  try:
    import matplotlib.pyplot as plt
  
    plt.figure(1)
  
    dist = [rand, unif, twos, twov, read]
    if beta is not None :
      dist.append(beta)

    for i, d in enumerate(dist):
      plt.subplot(231+i)
      count, bins, ignored = plt.hist(d, 20)
  
    plt.show()
  except:
    raise
