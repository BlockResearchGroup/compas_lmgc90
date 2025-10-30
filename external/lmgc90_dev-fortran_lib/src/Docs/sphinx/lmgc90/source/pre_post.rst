.. _pre_post-label:

Managing POSTPRO
================

Postpro commands available in pre:

 * NEW MECAx SETS : mecax_sets
 * NEW RIGID SETS : rigid_sets
 * BODY TRACKING  : rigid_set
 * TORQUE EVOLUTION : rigid_set
 * Fint EVOLUTION : 
 * Dep EVOLUTION : 
 * SOLVER INFORMATIONS :
 * VIOLATION EVOLUTION :
 * KINETIC ENERGY :
 * DISSIPATED ENERGY :
 * COORDINATION NUMBER :
 * CLxxx ANALYSIS : CLxxx_sets
 * DOUBLETS TORQUE EVOLUTION : doublets
 * DOUBLET INTERACTIONS : inter_type, rigid_set
 * CONTACT FORCE DISTRIBUTION : val
 * NORMAL CONTACT DISTRIBUTION : val
 * DENSE SAMPLE COMPACITY : skip_body
 * DISPLAY TENSORS :
 * AVERAGE VELOCITY EVOLUTION : color
 * DRY CONTACT NATURE :
 * WET CONTACT NATURE :
 * PLPLx ANALYSIS :
 * COMPACITY EVOLUTION : keep_behav, shape, rigid_set
 * TRIAXIAL COMPACITY : rigid_set
 * PRxxx DETECTION :
 * INTER ANALYSIS :
 * VISIBILITY STATE :


**Example:** ::

 post = pre.postpro_commands()
 #
 # creating a deformable set
 #
 mecax_set = [(body_deformable_brick, "coin")]
 post.addCommand(pre.postpro_command(name='NEW MECAx SETS', mecax_sets=[mecax_set]))
 #
 # following mean displacement and force of deformable sets
 #
 post.addCommand(pre.postpro_command(name='Dep EVOLUTION', step=1))
 post.addCommand(pre.postpro_command(name='Fint EVOLUTION', step=1))
 # 
 # following displacement and force on a rigid object :
 #
 post.addCommand(pre.postpro_command(name='BODY TRACKING', step=1, rigid_set=[body_rigid_brick]))
 post.addCommand(pre.postpro_command(name='TORQUE EVOLUTION', step=1, rigid_set=[body_rigid_brick]))
 #
 post.addCommand(pre.postpro_command(name='DOUBLET TORQUE EVOLUTION', step=1,doublets=[(up,down)]) )
 #
 post.addCommand(pre.postpro_command(name='SOLVER INFORMATIONS', step=1))
 #
 pre.writePostpro(commands=post, parts=bodies, path='DATBOX/')
