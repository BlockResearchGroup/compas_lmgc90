.. py:currentmodule:: pylmgc90

Interaction model and parameters definition
===========================================

One needs to define a tact_behav object with :py:func:`pre.tact_behav` which corresponds to an
interaction model with its parameters.

**Example :** ::
  
  lcsas=pre.tact_behav(name='gapc0', law='MAC_CZM', dyfr=0., stfr=0., cn=4.e+14, ct=3.e+14, b=0., w=60.)

Various models are available. The contact detection computes some
quantities as: local frame, initale gap :math:`g_{\star}`, 
etc. Unkowns are relative velocity :math:`\Vln`, :math:`\Vlt` and
contact reactions (impulse) :math:`\Rln`, :math:`\Rlt`.

Meaning of some parameters:

* friction: either constant *fric* or *stfr* static and *dyfr*
  dynamic friction
* restitution (newton): *rstn* normal, *rstt* tangential
* cohesion: *cohn* normal, *coht* tangential (unused with wet model)
* CZM parameters:

  * *cn* normal stiffness (pure mode I)
  * *ct* tangential stiffness (pure mode II)
  * *s1* and *s2* critical stress (pure mode I and pure mode II)
  * *G1* and *G2* fracture energy (pure mode I and pure mode II)
  * *dp1* and *dp2* plastic displacement for TH (dp1=s1/cn and dp2=s2/ct means triangles)
  * *du1* and *du2* rupture displacement (pure mode I and pure mode II) for ABP
  * *phi* ratio between micro and macro fracture energy for ABP

Furthermore some interaction have some internal variables (not managed
by the user) which needs a careful use of :py:func:`chipy.macro.StockRloc`,
:py:func:`chipy.macro.UpdateTactBehav`, :py:func:`chipy.macro.RecupRloc`.   

Here is a list of existing models with the expected parameters.
Each model is described by a normal and a tangential part.
Most models are rewritten, through change of variables, as a guenuine Signorini-Coulomb problem.

RIGID/RIGID :
-------------

* **IQS_CLB** : fric  (:math:`\mu`).

  Normal : **Signorini** formulation with :math:`\bar{g} =
  \max(0,g_{\star}) + h \Vln` and :math:`\bRln = \Rln`.
  
  Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and  :math:`\bRlt = \Rlt`

* **IQS_CLB_g0** : fric  (:math:`\mu`).
  
 | Normal : **Signorini** formulation with :math:`\bar{g} =
  \max(0,g_{\star}-g_0) + h \Vln` and :math:`\bRln =
  \Rln`.
 | :math:`g_0` is the gap computed at the first step and stored as an internal variable.
  
 | Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and  :math:`\bRlt = \Rlt`

 | Internal variables : g_ref
 
* **IQS_DS_CLB** : dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`).

  Normal : **Signorini** formulation with :math:`\bar{g} =
  \max(0,g_{\star}) + h \Vln` and :math:`\bRln = \Rln`.
  
  Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and
  :math:`\bRlt = \Rlt`.
	
  The friction coefficient is depending on the contact status (be carefull :math:`\mu_d \leq \mu_s`) :
  
     If status_begin == stick then :math:`\mu=\mu_s`
  
     else  :math:`\mu=\mu_d` 
  
* **IQS_WET_DS_CLB** : cohn (:math:`\Rln^{coh}`), coht (not used), Wthk (:math:`g^{coh}`), dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`).

  Normal : **Signorini** formulation with :math:`\bar{g} =
  \max(0,g_{\star}) + h \Vln` and

    if :math:`g_{\star} \le g^{coh}` then :math:`\bRln = \Rln + \Rln^{coh}`

    else :math:`\bRln = \Rln`.
  
  Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and
  :math:`\bRlt = \Rlt`.
	
  The friction coefficient is depending on the contact status:

	if status_begin == stick then :math:`\mu=\mu_s`

        else  :math:`\mu=\mu_d` 
     
* **IQS_MOHR_DS_CLB** : cohn (:math:`\sigma_n^{coh}`), coht (:math:`\sigma_t^{coh}`), dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`).

  :math:`\Rln^{coh}=S \sigma_n^{coh}`, :math:`\Rlt^{coh}=S \sigma_t^{coh}`, with :math:`S` the contact surface computed during contact detection.  
  
  Normal : **Signorini** formulation with :math:`\bar{g} = \max(0,g_{\star}) + h \Vln` and

     if status_begin == cohesive then :math:`\bRln = \Rln + \Rln^{coh}`

     else :math:`\bRln = \Rln`.

  Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and :math:`\bRlt = \Rlt`.
	
  The friction coefficient is depending on the contact status:

	if status_begin == cohesive then :math:`\mu = \sigma_t^{coh}/ \sigma_n^{coh}`

	else

           if status_begin == stick then :math:`\mu=\mu_s`

           else  :math:`\mu=\mu_d` 

* **IQS_STICK** :
  
  Normal : **Signorini** formulation with :math:`\bar{g} =
  \max(0,g_{\star}) + h \Vln` and :math:`\bRln = \Rln`.
  
  Tangential :

     if  :math:`\Rln \geq 0` then :math:`\Rlt` such that :math:`\Vlt = 0`
  
     else  :math:`\Rlt = 0` and :math:`\Vlt` free

* **RST_CLB** : rstn (:math:`e_n`), rstt (:math:`e_t`), fric (:math:`\mu`).

  if :math:`g_{\star} \leq 0` then

      Normal : **Signorini** formulation with :math:`\bar{g} = \Vln+e_n\Vln{}_{,begin}` and :math:`\bRln = \Rln`.
  
      Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt+e_t\Vlt{}_{,begin}` and :math:`\bRlt = \Rlt`
	
  else

      :math:`\Rln = 0` and  :math:`\Rlt = 0`
  
* **IQS_MAL_CZM** : dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`), cn, ct, s1, s2, G1, G2

* **IQS_TH_CZM** : dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`), cn, ct, s1, s2, G1, G2, dp1, dp2
  
* **IQS_ABP_CZM** : dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`), cn, ct, s1, s2, G1, G2, du1, du2, phi  

* **IQS_EXPO_CZM** : dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`), cn, ct, s1, s2, G1, G2, eta    

* **IQS_EXPO_CZM_SPRING** : dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`), cn, ct, s1, s2, G1, G2, eta, k1, k2    

* **IQS_MAC_CZM**: dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`), cn, ct , b, w.

  :math:`\Rln^{coh}=S \sigma_n^{coh}`, :math:`\Rlt^{coh}=S \sigma_t^{coh}`, with :math:`S` the contact surface computed during contact detection.
  
  :math:`\sigma_n^{coh} = \beta^2 cn \lbrack\mid u \mid \rbrack \cdot
  n` and  :math:`\sigma_t^{coh} = \beta^2 ct \lbrack\mid u \mid
  \rbrack \cdot (I - n \times n)`

  :math:`f(\beta)=0`

  Normal : **Signorini** formulation with :math:`\bar{g} =
  \max(0,g_{\star}) + h \Vln` and :math:`\bRln = \Rln +\Rln^{coh}`.
  
  Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and
  :math:`\bRlt = \Rlt + \Rlt^{coh}`

  :math:`\mu` can depend on the damage parameter 

  internal variables : taille_ele,saut_de_ut,saut_de_un,[3D saut_de_us],beta,TPSini    
   
any/DEFORMABLE :
----------------

* **GAP_SGR_CLB** : fric
  
  Normal : **Signorini** formulation with :math:`\bar{g}=g_{\star} + h \Vln` and :math:`\bRln = \Rln`.
  
  Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and  :math:`\bRlt = \Rlt`
    
* **GAP_SGR_CLB_g0** : fric
  
 | Normal : **Signorini** formulation with :math:`\bar{g} = g_{\star}-g_0 + h \Vln` and :math:`\bRln =
  \Rln`.
 | :math:`g_0` (g_ref) is the gap computed at the first step and stored as an internal variable.
  
 | Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and  :math:`\bRlt = \Rlt`

 | Internal variables : g_ref
  
* **preGAP_SGR_CLB**: pgap, fric


  
 | Internal variables : taille_ele,  
  
* **GAP_SGR_STICK**
  
  Normal : **Signorini** formulation with :math:`\bar{g} = g_{\star} + h \Vln` and :math:`\bRln = \Rln`.
  
  Tangential :

     if  :math:`\Rln \geq 0` then :math:`\Rlt` such that :math:`\Vlt = 0`
  
     else  :math:`\Rlt = 0` and :math:`\Vlt` free
   
* **GAP_MOHR_DS_CLB** : cohn (:math:`\sigma_n^{coh}`), coht (:math:`\sigma_t^{coh}`), dyfr (:math:`\mu_d`), stfr (:math:`\mu_s`).

  :math:`\Rln^{coh}=S \sigma_n^{coh}`, :math:`\Rlt^{coh}=S \sigma_t^{coh}`, with :math:`S` the contact surface computed during contact detection.  
  
  Normal : **Signorini** formulation with :math:`\bar{g} = g_{\star} + h \Vln` and

     if status_begin == cohesive then :math:`\bRln = \Rln + \Rln^{coh}`

     else :math:`\bRln = \Rln`.

  Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and :math:`\bRlt = \Rlt`.
	
  The friction coefficient is depending on the contact status:

	if status_begin == cohesive then :math:`\mu = \sigma_t^{coh}/ \sigma_n^{coh}`

	else

           if status_begin == stick then :math:`\mu=\mu_s`

           else  :math:`\mu=\mu_d` 
  
* **VEL_SGR_CLB** : fric

  if :math:`g_{\star} \leq 0` then

      Normal : **Signorini** formulation with :math:`\bar{g} = \Vln` and :math:`\bRln = \Rln`.
  
      Tangential : **Coulomb** formulation with :math:`\bVlt = \Vlt` and :math:`\bRlt = \Rlt`
	
  else

      :math:`\Rln = 0` and  :math:`\Rlt = 0`
  
* **MAL_CZM** : dyfr, stfr, cn, ct, s1, s2, G1, G2

* **TH_CZM** : dyfr, stfr, cn, ct, s1, s2, G1 , G2, dp1, dp2
  
* **ABP_CZM** : dyfr, stfr, cn, ct, s1, s2, G1 , G2, du1, du2, phi

* **EXPO_CZM** : dyfr, stfr, cn, ct, s1, s2, G1 , G2, eta

* **EXPO_CZM_SPRING** : dyfr, stfr, cn, ct, s1, s2, G1 , G2, eta, k1, k2  

* **MAC_CZM** : dyfr, stfr, cn, ct, b, w
  
* **MP_CZM** : dyfr, stfr, cn, ct, p, p0, w
  
* **MP3_CZM**: dyfr, stfr, cn, ct, p, p0, smax, w
  
* **MP3_CZM_THER** : dyfr, stfr, cn, ct, p, p0, smax, w, lambdas, lambdac

   
POINT/POINT:
------------

* **ELASTIC_WIRE**: stiffness, prestrain
  
* **BRITTLE_ELASTIC_WIRE** : stiffness, prestrain, Fmax
  
* **ELASTIC_ROD** : stiffness, prestrain
  
* **VOIGT_ROD** : stiffness, prestrain, viscosity

* **NARD_ROD** : E, nu, s1, s2

any/any:
--------

* **COUPLED_DOF** :
  
* **NORMAL_COUPLED_DOF** :

* **TANGENTIAL_COUPLED_DOF** :

* **ELASTIC_REPELL_CLB** : stiffness, fric
  
* **ELASTIC_REPELL_CLB_g0** : stiffness, fric
  
* **VISCO_ELASTIC_REPELL_CLB** : stiffness, viscosity, fric

Genuine Laws:
--------------

**Signorini** condition (unilateral condition)

 .. math::
    \bar{g} \geq 0; \;\; \bRln \geq 0; \;\; \bar{g} \cdot \bRln = 0
  

**Coulomb** friction

 .. math::
    \parallel \bRlt \parallel \leqslant \mu \bRln
    \left \lbrace
    \begin{array}{l} \parallel \bRlt \parallel <  \mu \bRln \Rightarrow \bVlt = 0 \\
                           \parallel \bRlt \parallel = \mu \bRln \Rightarrow \exists \alpha \geqslant 0, \bVlt = -\alpha \bRlt
    \end{array}
    \right .
  




  
