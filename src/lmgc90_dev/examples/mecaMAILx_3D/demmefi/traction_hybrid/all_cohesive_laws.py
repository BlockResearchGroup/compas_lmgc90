import math
import numpy as np
import matplotlib.pyplot as plt

# the list of cohesive laws implemented in here
authorized_laws = ['MAC_CZM', 'MAL_CZM', 
                   'TH_CZM' , 'ABP_CZM',
                   'MP3_CZM', 'EXPO_CZM']


# MAC_CZM law computation
def MAC_CZM(beta, u, params, surf):
  """Compute beta (damage) and adhesive stress
     from an initial beta value and a gap
  """

  cn   = params[2]
  ct   = params[3]
  w    = params[5]

  nucut = u[0]*u[0] + u[2]*u[2]
  nucun = ( ( abs(u[1])+u[1] ) * 0.5 ) **2

  if (w - beta*(ct*nucut + cn*nucun)) < 0.:
     new_beta = w/(ct*nucut + cn*nucun)
  else:
     new_beta = beta

  radh = u*surf*new_beta*new_beta
  radh *= np.array([ct,cn,ct])

  return new_beta,radh

def MAC_CZM_maxdep(params):
  """Compute an value of gap to completely ruin the interface
  """

  return 10. * math.sqrt( params[5] / params[2] )

# compute MAC_CZM law
def MAL_CZM(beta, u, params, surf):
  """Compute beta (damage) and adhesive stress
     from an initial beta value and a gap
  """

  cn = params[2]
  ct = params[3]
  s1 = params[4]
  s2 = params[5]  
  G1 = params[6]
  G2 = params[7]  
  
  nucut = u[0]*u[0] + u[2]*u[2]
  nucun = ( ( abs(u[1])+u[1] ) * 0.5 ) **2
  nd = math.sqrt(nucut+nucun)

  if nucun == 0. : 
    dir_ = 1e+20
  else :
    dir_ = nucut / nucun

  di1 = s1/cn
  di2 = s2/ct

  # la rigidite initiale au carre
  C2 = (cn/(1.+dir_**2)) + (ct*dir_**2/(1.+dir_**2))
  GG = (G2*cn/(1.+dir_**2)) + (G1*ct*dir_**2/(1.+dir_**2))

  #dc = 0.5 * smax * ((1.d0/cn) + (1.d0/ct)) 

  dc = di1*di2*math.sqrt((1.+(dir_**2))/ ((di2**2) + (dir_**2)*(di1**2)))

  #dt = 1.5 * ((w/smax) -(0.5 * dc))

  dt = 1.5*(G1*G2 - (0.5 * dc**2 * GG))/(dc * GG)            

  if (nd < dc):
    new_beta = min(beta, 1.)
  elif ( dc <= nd and nd < (dc+dt) ):
    new_beta = min(beta, (dc/nd)*(1. - (((nd - dc)/dt)**2)))
  else:
    new_beta = 0.

  radh = u*surf*new_beta
  radh *= np.array([ct,cn,ct])

  return new_beta,radh      


def MAL_CZM_maxdep(params):
  """Compute an value of gap to completely ruin the interface
  """
  cn = params[2]
  ct = params[3]
  s1 = params[4]
  s2 = params[5]  
  G1 = params[6]
  G2 = params[7]  

  dir_ = 0.
  
  di1 = s1/cn
  di2 = s2/ct

  # la rigidite initiale au carre
  C2 = (cn/(1.+dir_**2)) + (ct*dir_**2/(1.+dir_**2))
  GG = (G2*cn/(1.+dir_**2)) + (G1*ct*dir_**2/(1.+dir_**2))

  #dc = 0.5 * smax * ((1.d0/cn) + (1.d0/ct)) 

  dc = di1*di2*math.sqrt((1.+(dir_**2))/ ((di2**2) + (dir_**2)*(di1**2)))

  #dt = 1.5 * ((w/smax) -(0.5 * dc))

  dt = 1.5*(G1*G2 - (0.5 * dc**2 * GG))/(dc * GG)            

  return 1.5*(dc+dt)


def TH_CZM(beta, u, params, surf):
  """Compute beta (damage) and adhesive stress
     from an initial beta value and a gap
  """

  cn  = params[2]
  ct  = params[3]
  s1  = params[4]
  s2  = params[5]
  G1  = params[6]
  G2  = params[7]
  dp1 = params[8]
  dp2 = params[9]
  
  nucut = u[0]*u[0] + u[2]*u[2]
  nucun = ( ( abs(u[1])+u[1] ) * 0.5 ) **2
  nd = math.sqrt(nucut+nucun)

  if nucun == 0. :
    dir_ = 1e+20
  else :
    dir_ = nucut / nucun

  di1 = s1/cn
  di2 = s2/ct

  #di = ( smax * 0.5 ) * ( 1.d0/cn + 1.d0/ct )

  di = di1*di2*math.sqrt((1+(dir_**2))/ ((di2**2) + (dir_**2)*(di1**2)))

  dp = dp1*dp2*math.sqrt((1+(dir_**2))/ ((dp2**2) + (dir_**2)*(dp1**2)))

  tmp =  (cn*G2) + (dir_*dir_*ct*G1) 

  du = ( (2.*G1*G2*(1+dir_**2)) - ( di*(dp-di)*tmp))/(di*tmp)
  
  if ( nd < di ):
     new_beta = min(1.,beta)
  elif ( di <= nd  and nd < dp ):
     new_beta = min(di/nd,beta)
  elif ( dp <= nd and nd < du):
     new_beta = min((di/nd) * ( (du-nd) / (du-dp) ), beta)
  else:
     new_beta = 0.

  radh = u*surf*new_beta
  radh *= np.array([ct,cn,ct])

  return new_beta, radh      

def TH_CZM_maxdep(params):
  """Compute an value of gap to completely ruin the interface
  """
  cn  = params[2]
  ct  = params[3]
  s1  = params[4]
  s2  = params[5]
  G1  = params[6]
  G2  = params[7]
  dp1 = params[8]
  dp2 = params[9]

  dir_= 0.
  
  di1 = s1/cn
  di2 = s2/ct
  
  di = di1*di2*math.sqrt((1+(dir_**2))/ ((di2**2) + (dir_**2)*(di1**2)))

  dp = dp1*dp2*math.sqrt((1+(dir_**2))/ ((dp2**2) + (dir_**2)*(dp1**2)))

  tmp =  (cn*G2) + (dir_*dir_*ct*G1) 

  du = ( (2.*G1*G2*(1+dir_**2)) - ( di*(dp-di)*tmp))/(di*tmp)
  
  return 1.5*du

def ABP_CZM(beta, u, params, surf):
  """Compute beta (damage) and adhesive stress
     from an initial beta value and a gap
  """

  cn  = params[2]
  ct  = params[3]
  s1  = params[4]
  s2  = params[5]
  w1  = params[6]
  w2  = params[7]
  d1  = params[8]
  d2  = params[9]
  phi = params[10]

  nucut = u[0]*u[0] + u[2]*u[2]
  nucun = ( ( abs(u[1])+u[1] ) * 0.5 ) **2
  nd = math.sqrt(nucut+nucun)

  if nucun == 0.:
    ratio = 1e+20
  else:
    ratio = nucut / nucun

  r2 = ratio*ratio

  # smax/c
  di1 = s1/cn
  di2 = s2/ct

  # 2*b/du ; b=w*(1-phi)
  sp1 = 2. * w1 * (1.-phi) / d1
  sp2 = 2. * w2 * (1.-phi) / d2

  # (2*mu + sp*di)/smax ; mu = w*p
  dp1 = ( (2.*w1*phi) + (sp1*di1) ) /s1
  dp2 = ( (2.*w2*phi) + (sp2*di2) ) /s2

  # mixing
  di = di1 * di2 * math.sqrt( (1.+r2) / ( di2**2 + (r2)*(di1**2) ) )
  dp = dp1 * dp2 * math.sqrt(1.+r2) * math.sqrt( di2*di2 + r2*di1*di1 ) / ( di2*dp2 + r2*di1*dp1 )

  du = ( (1.+r2)/dp ) * dp1 * d1 * dp2 * d2 / ( dp2*d2 + r2*dp1*d1 )
  k0 = math.sqrt( ( cn*cn + ct*ct*r2 ) / (1.+r2) )

  si = k0 * di
  sp = si * dp / ( di + du*phi/(1.-phi) )

  if nd > du:
    new_beta = 0.
  elif nd > dp and nd <= du:
    new_beta = min(( sp + sp*(nd-dp)/(dp-du) ) / (nd*k0),beta)
  elif nd > di and nd <= dp:
    new_beta = min(( si +  (sp-si)*(nd-di)/(dp-di) ) / (nd*k0), beta)
  else:
    new_beta = min(1.,beta)

  radh = u * surf * new_beta
  radh *= np.array([ct,cn,ct])

  return new_beta, radh      

def ABP_CZM_maxdep(params):
  """Compute an value of gap to completely ruin the interface
  """

  d1  = params[8]
  d2  = params[9]

  return 1.5 * max(d1,d2)

def EXPO_CZM(beta, u, params, surf):
  """Compute beta (damage) and adhesive stress
     from an initial beta value and a gap
  """

  cn  = params[2]
  ct  = params[3]
  s1  = params[4]
  s2  = params[5]
  G1  = params[6]
  G2  = params[7]
  eta = params[8]
  
  nucut = u[0]*u[0] + u[2]*u[2]
  nucun = ( ( abs(u[1])+u[1] ) * 0.5 ) **2
  nd = math.sqrt(nucut+nucun)
  
  if nucun == 0.:
    dir_ = 1e+20
  else:
    dir_ = nucut / nucun

  # smax/c
  di1 = s1/cn
  di2 = s2/ct

  # di : dep elastique
  di = di1 * di2 * math.sqrt((1.+(dir_**2)) / ((di2**2) + (dir_**2)*(di1**2)))

  # gi : energie totale
  gi = ( (di2**2 * G1) + (dir_**2 * di1**2 * G2) ) / ((di2**2) + (dir_**2)*(di1**2)) 

  k0 = math.sqrt((cn*cn + ct*ct*dir_*dir_)/(1. + dir_*dir_))
     
  # phi : parametre expo 
  phi = (k0 * di)/(gi - (0.5 * k0 * di * di)) 
     
  # du
  du = di - (math.log(eta)/phi) 

  #
  si = k0 * di

  new_beta = min(1.,beta)
  
  if nd > du :

    new_beta = 0.

  elif nd > di and nd <= du :

    new_beta = min((si/(k0*nd)) * math.exp(phi*(di-nd)), beta)

  radh = u * surf * new_beta
  radh *= np.array([ct,cn,ct])

  return new_beta,radh
    
  
def EXPO_CZM_maxdep(params):
  """Compute an value of gap to completely ruin the interface
  """

  cn  = params[2]
  ct  = params[3]
  s1  = params[4]
  s2  = params[5]
  G1  = params[6]
  G2  = params[7]
  eta = params[8]

  dir_=0.
  
  # smax/c
  di1 = s1/cn
  di2 = s2/ct

  # di : dep elastique
  di = di1 * di2 * math.sqrt((1.+(dir_**2)) / ((di2**2) + (dir_**2)*(di1**2)))

  # gi : energie totale
  gi = ( (di2**2 * G1) + (dir_**2 * di1**2 * G2) ) / ((di2**2) + (dir_**2)*(di1**2)) 

  k0 = math.sqrt((cn*cn + ct*ct*dir_*dir_)/(1. + dir_*dir_))
     
  # phi : parametre expo 
  phi = (k0 * di)/(gi - (0.5 * k0 * di * di)) 
     
  # du
  du = di - (math.log(eta)/phi) 

  return 1.5 * du
  
def MP3_CZM(beta, u, params, surf):
  """Compute beta (damage) and adhesive stress
     from an initial beta value and a gap
  """

  cn = params[2]
  ct = params[3]
  sm = params[4]
  w  = params[5]
  
  nucut = u[0]*u[0] + u[2]*u[2]
  nucun = ( ( abs(u[1])+u[1] ) * 0.5 ) **2
  nd = math.sqrt(nucut+nucun)

  dc = sm  * 0.5 * ((1./cn) + (1./ct))
  dt = 1.5 * ( (w/sm) - (0.5*dc) )

  if (nd < dc):
    new_beta = min(beta, 1.)
  elif ( dc <= nd and nd < (dc+dt) ):
    new_beta = min(beta, (dc/nd)*(1. - (((nd - dc)/dt)**2)))
  else:
    new_beta = 0.

  radh = u*surf*new_beta
  radh *= np.array([ct,cn,ct])

  return new_beta,radh      

def MP3_CZM_maxdep(params):
  """Compute an value of gap to completely ruin the interface
  """
  dc =  params[4] * 0.5 * ( 1./params[2] + 1./params[3] )
  dt =  1.5 * ( params[5]/params[4] - 0.5*dc )
  return dc+dt

