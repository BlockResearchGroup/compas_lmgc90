from pathlib import Path

from pylmgc90 import pre

dim = 2

bodies = pre.avatars()
mat = pre.material(name='PLEXx',materialType='RIGID',density=2000.)
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# bricks, half-brick, linteau and opening definition
brick      = pre.brick2D('brick',      1.e-1, 5.e-2)
half_brick = pre.brick2D('half-brick', 5.e-2, 5.e-2)
linteau    = pre.brick2D('linteau',    3.e-1, 5.e-2) 
ghost      = pre.brick2D('ghost',      2.e-1, 5.e-2)


# joint thickness
horizontal_joint_thickness = 0.001
vertical_joint_thickness   = 0.001

# rows definitions
even_row        = [half_brick, brick, brick, brick, brick, half_brick]
odd_row         = [brick, brick, brick, brick, brick]
even_window_row = [half_brick, brick, ghost, brick, half_brick]
odd_window_row  = [brick, half_brick, ghost, half_brick, brick]
linteau_row     = [brick, linteau, brick]

# wall as a list of rows
wall = [even_row,
        odd_row ,
        even_row, 
        odd_row ,
        even_window_row, 
        odd_window_row , 
        even_window_row, 
        linteau_row    , 
        even_row       ]


# wall building :
nb_bricks=0
x=0.
y=0.
for j in range(0, len(wall), 1):
   row = wall[j]
   if j % 2 == 0:
      color='BLUEx'
   else:
      color='REDxx'

   x=0.
   for i in range(0, len(row), 1):
      nb_bricks += 1
      brick = row[i]
      if i == 0:
         y += 0.5*brick.ly

      x += 0.5*brick.lx
      if brick.name != 'ghost':
         bodies += brick.rigidBrick(center=[x, y], model=mod, material=mat, color=color)

      x += 0.5*brick.lx + vertical_joint_thickness

   y += 0.5*brick.ly + horizontal_joint_thickness

## wall done... everything else (floor and other container)

mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

mut = pre.material(name='TDURx',materialType='RIGID',density=2500.)
mats.addMaterial(mat,mut)

floor = pre.rigidJonc(axe1=3.e-1, axe2=2.5e-2, center=[2.5e-1, -2.5e-2], 
                      model=mod, material=mut, color='WALLx')
floor.imposeDrivenDof(component=[1, 2, 3],dofty='vlocy')

bodies += floor

try:
  pre.visuAvatars(bodies)
except:
  pass

# interactions management :
lplpl=pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+=lplpl
lpljc=pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+=lpljc
svbbbb = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG',colorAntagoniste='BLUEx',
                       behav=lplpl,alert=5.e-3)
svs+=svbbbb
svbrbr = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG',colorAntagoniste='REDxx',
                       behav=lplpl,alert=5.e-3)
svs+=svbrbr
svbbbr = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG',colorAntagoniste='REDxx',
                       behav=lplpl, alert=5.e-3)
svs+=svbbbr
svpljc = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                       behav=lpljc,alert=5.e-3)
svs+=svpljc

post = pre.postpro_commands()

# file writing 
datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, datbox_path=datbox)
