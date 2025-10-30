Some theory around DEM (Discrete Element Methods)
#################################################

Preamble
========

Many solid materials and structures are made of components in interaction:

* Dry granular material (divided media):

  .. image:: figures/ballast.jpg
     :height: 205 px
     :width: 134 px
     :alt: ballast sample on a railway track

  .. image:: figures/sol.jpg
     :height: 205 px
     :width: 134 px
     :alt: sol samples

  .. image:: figures/dune.jpg
     :height: 205 px
     :width: 300 px

  - component: grain, cluster of grain, piece of grain, coarse grain,etc.
  - interaction: unilateral contact, elasticity or/and cohesion, friction, etc.

* masonry structures (divided structure):

  .. image:: figures/pontdugard.jpg
     :height: 205 px
     :width: 134 px
     :alt: Pont du Gard

  .. image:: figures/arche.jpg
     :height: 205 px
     :width: 134 px
     :alt: Arches

  .. image:: figures/stairs.jpg
     :height: 205 px
     :width: 300 px
     :alt: 

  - component: brick, stone, mortar, mortar grain.
  - interaction: unilateral contact, friction, cohesion, etc.

* Solid grains in a solid matrix (composites, etc) and/or in a fluid matrix (colloids, concrete, etc) 

  .. image:: figures/beton.jpg
     :height: 205 px
     :width: 134 px
     :alt: coupe de beton

  .. image:: figures/ble.jpg
     :height: 205 px
     :width: 134 px
     :alt: ble

* Changing the scale a continuous media may become discontinuous:

   - component: metal grain, crystal in a poly-crystal, molecules, atoms;
   - interaction: grain bond, defects, Wan der Waals effects, Lenhard-Jones potentials, atomic bonds, etc.


Considering divided media, it exists various space scales :

* microscopic
* mesoscopic
* macroscopic

Behavior depends on:

* composition: shape (angularity, elongation, etc), dispersity
* state: dense-loose, confining pressure, dry-wet-composite
* load: static-dynamic

Depending on the solicitation it may behave like: a solid, a liquid or a gas

It exists numerous methods to model these kind of media and structures.

At the macroscopic scale the divided media is considered as continuous. It is possible to use various discretization:

* classical methods (Finite Difference, Finite Element Method, Finite Volume Method, etc)
* dedicated methods (Thin Layer Model, Spreading-based model, etc)

it needs an ad-hoc rheological model to represent the complex behavior of the media (elasticity, plasticity, visco-plasticity with threshold, fracture, etc).

Drawbacks: building a model with the relevant phenomenological parameters and identifying the parameters.

At the microscopic or mesoscopic scale, methods rely on a modeling of:

* the behavior of each component
* the behavior of the interactions of each component with its neighborhood

A lot of methods aim to model divided media (and/or structures) at the component and interaction scale:
Cellular Automata, SPH, Lattice Element methods, Discontinuous Deformation Analysis (DDA), etc and Discrete Element Methods (DEM).

Advantages: simpler model, less parameters, easier to identify.

Drawback: computational cost.

One can also consider DEMs to model fictitious divided media as in: coarse grain approach domain decomposition approach.
These methods may also be used for modeling mechanism or robotics DEMs can be mixed with continuous methods to model,
for example, the transition between a continuous and a divided media: fragmentation of a media, wear, powder compaction to obtain ceramics

Modeling
========

Smooth Dynamic
--------------

Equations of motion
^^^^^^^^^^^^^^^^^^^

Assuming a smooth evolution of the system allows to describe the motion of each mechanical component by a semi-discretized in space system:

..
   , \mathbf{q}uad

.. math::
   \mathbb{M}(\mathbf{q},t) \ddot{\mathbf{q}} = \mathbf{F}(\mathbf{q},\mathbf{\dot{q}},t) + \mathbf{P}(t) + \mathbf{r}
   :label: dyna

where:

 - :math:`\mathbf{q}\in\mathbb{R}^n`: represents the vector of generalized degrees of freedom,
 - :math:`\mathbf{\dot{q}}\in\mathbb{R}^n`: the generalized velocities,
 - :math:`\mathbf{r}\in\mathbb{R}^n`: the contact forces,
 - :math:`\mathbf{P}(t)\in\mathbb{R}^n`: the external forces,
 - :math:`\mathbf{F}(\mathbf{q},\mathbf{\dot{q}},t)\in\mathbb{R}^n`: the internal force (deformable bodies) and the nonlinear inertia terms (centrifugal and gyroscopic),
 - :math:`\mathbb{M}(\mathbf{q},t)\in\mathbb{R}^n`: the inertia matrix.

Initial and boundary conditions must be added to fully describe the evolution of the system.

Considering a rigid component, a more suitable dynamical equation may be used introducing the v and ω, the translation and rotation velocities of the center of mass.

The Eq. (:eq:`dyna`) is replaced by the well-known Newton-Euler system of equations:

.. math::
   \mathbb{M} \mathbf{v} = \mathbf{P}(t) + \mathbf{r} \\
   \mathbb{I} \mathbf{\omega} = \omega \wedge \mathbb{I} \mathbf{\omega} + \mathcal{M}_{\mathbf{P}(t)} + \mathcal{M}_{\mathbf{r}}


where:

 - :math:`\mathbf{P}(t)` and :math:`\mathbf{r}` represent respectively the resultant of external and contact forces,
 - :math:`\mathcal{M}_{\mathbf{P}(t)}` and :math:`\mathcal{M}_{\mathbf{r}}` represent respectively the momentum due to external and contact forces,
 - :math:`\mathbb{M}` and :math:`\mathbb{I}` represents respectively the mass and the inertia matrices.

Note that for 2D or 3D components with geometric isotropy, the vector :math:`\omega \wedge \mathbb{I} \mathbf{\omega}` is equal to zero.

The choice of angle parameters is very important, especially in 3D (inertia frame mapping, Euler angles, quaternion, etc.)

Interaction description
^^^^^^^^^^^^^^^^^^^^^^^

At any time of the evolution of the system one needs to define the interaction locus and an associated local frame in order to describe the interaction behavior. 

.. image:: figures/contactdef.jpg
  :width: 200px
  :align: center

It is assumed that one is able to define for each point (C) of the candidate boundary its (unique) nearest point (A) on the antagonist boundary.
It allows to define for each couple of points a local frame :math:`(\mathbf{s},\mathbf{t},\mathbf{n})` with :math:`\mathbf{n}` the normal vector
of the antagonist boundary and :math:`(\mathbf{s},\mathbf{t})`: two vectors of its tangential plane. 

Only in simplest cases (rigid body with strictly convex boundary) the interaction locus may be considered as punctual.

Less trivial in usual cases: 

.. image:: figures/grains_complexes.jpg
  :width: 200px
  :align: center

* Strictly convex, i.e. cubes, bricks, etc.
* Locally convex, i.e. general polyhedron, triangulated surface
* Not convex at all but ... *It may be decomposed in not strictly convex shapes*.

In these cases many choices:

* punctual contact with extended law (transmission of torque)

 -> how to define the normal ? The interaction law depends on the objects shape ! etc.

* multi-punctual contacts with classical interaction laws

 -> how many contact points ? normal choice ? It mays introduce local indetermination of contact forces, etc.  

* continuous surfaced description as in mortar methods

 -> needs to perform integration on non-conforming triangulation, etc.

* etc.

Implicitly DEMs rely on the following hypotheses :

* the deformation concerns only the contact point neighborhood

 -> components of the system may be considered as rigid;

* the contact area is small behind the size of the component;
* locus of interaction may be supposed as punctual;
* interactions are binary (no effect of connected interaction by particle on their behavior);

 -> interaction law depends only on related component;

At least, for every potential contact `\alpha`, it will be determined:

* Contact point coordinates;
* The local frame :math:`(\mathbf{s}_{\alpha},\mathbf{t}_{\alpha},\mathbf{n}_{\alpha})`;
* The gap :math:`(g_{\alpha})`, i.e. the algebraic distance between two bodies;
* The contact relative velocity between the two bodies :math:`(\mathbf{U}_{\alpha})`.

Contact description is a key point of DEMs. 

Local-Global mapping
^^^^^^^^^^^^^^^^^^^^

Two sets of unknowns:

* **global unknowns** (or kinematic space unknowns) related to the bodies: center of inertia or mesh node displacement and velocity (:math:`\mathbf{q},\dot{\mathbf{q}}`), resulting force and momentum (:math:`\mathbf{r}`), etc.
* **local unknowns** (or contact space unknowns) related to interactions: gap (:math:`g`), relative velocities (:math:`\mathbf{U}`), forces (:math:`\mathbf{R}`), etc.

Related by kinematic relations:

.. math::
   g = D(\mathbf{q}) \\
   \mathbf{U} = \mathbb{H}^{\star} (\mathbf{q}) \dot{\mathbf{q}} = \nabla_\mathbf{q} D(\mathbf{q}) \dot{\mathbf{q}}

Lets consider two rigid bodies.
The mapping between inertia center and contact point velocities will be write as follow:

.. math::
   \mathbf{v}(M) = \mathbf{v}(G) + \omega \times \mathbf{l}

where :math:`\mathbf{l}` represents the vector between the inertia center :math:`(G)` and the contact point :math:`(M)`.

Then it is possible to write the relative velocity between the two points :math:`(M_i)` (i bodies) and :math:`(M_j)` (j bodies):

.. math::
   \mathbf{U}_{X,Y,Z}= \mathbf{v}(M_i) - \mathbf{v}(M_j) = \mathbf{v}(G_i) - \mathbf{v}(G_j) + \omega_i \times \l_i - \omega_j \times \l_j


which can be written in a *matricial* way:

.. math::
   \mathbf{U}_{X,Y,Z}= \left < \begin{array}{cccc} 1 & \l_i & -1 & -\l_j \end{array} \right >
   \left [ 
   \begin{array}{l} \mathbf{v}(G_i) \\ \omega_i \\ \mathbf{v}(G_j) \\ \omega_j \end{array} 
   \right ] = H^{\star}_{i,j}(\mathbf{q}) \dot{\mathbf{q}}

And expressed in a local frame as:

.. math::
   \mathbf{U}_{t,n,s} =  \left < \begin{array}{ccc} \mathbf{U} \cdot \mathbf{t} & \mathbf{U} \cdot \mathbf{n} & \mathbf{U} \cdot \mathbf{s} \end{array} \right > =  
   \left [ \begin{array}{l} \mathbf{t}^T \\ \mathbf{n}^T \\ \mathbf{s}^T \end{array} \right ] H^{\star}_{i,j}(\mathbf{q}) \dot{\mathbf{q}} =\mathbb{H}^{\star}_{i,j}(\mathbf{q}) \dot{\mathbf{q}}

More generally, using kinematic relations, one can write for  a given contact :math:`\alpha`:

.. math::
   \mathbf{U}_{\alpha} = \mathbb{H}^{\star}_{\alpha}(\mathbf{q}) \dot{\mathbf{q}}

Using duality consideration (equality of power expressed in terms of global or local unknowns), the local contact force may be mapped on the global unknowns:

.. math::
   \mathbf{r}_{\alpha} = \mathbb{H}_{\alpha}(\mathbf{q}) \mathbf{R}_{\alpha},

where :math:`\mathbb{H}^{\star}_{\alpha}(\mathbf{q})` is the transpose of :math:`\mathbb{H}_{\alpha}(\mathbf{q})`.

In the following, operators mapping all the local and global unknowns are introduced:

.. math::
   \mathbb{H}(\mathbf{q})  :  \mathbf{R} = \{ \mathbf{R}_{\alpha} \} \rightarrow \mathbf{r}=\sum_{\alpha=1}^{n_c} \mathbb{H}_{\alpha}(\mathbf{q}) \mathbf{R}^{\alpha} \\
   \mathbb{H}^{\star}(\mathbf{q})  :  \dot{\mathbf{q}} \rightarrow \mathbf{U} = \{ \mathbf{U}^{\alpha} \}   =  \{ \mathbb{H}^{\star}_{\alpha}(\mathbf{q}) \dot{\mathbf{q}}  \}

Remark: even if :math:`\mathbb{H}^{\star}_{\alpha}(\mathbf{q})` and :math:`\mathbb{H}_{\alpha}(\mathbf{q})` have good theoretical properties (surjectivity and injectivity),
it is not necessary the case for :math:`\mathbb{H}^{\star}` and :math:`\mathbb{H}`. Loose of these properties is due to the introduction of kinematic relation between contacts.

Interaction law
^^^^^^^^^^^^^^^

In DEMs, a large part of the physics of the problem is described through interaction laws. Two ways of thinking:

1. Interaction behavior is a coarse representation of what happens at the boundary scale: impenetrability, plastic deformation of asperities, capillarity effects, friction, wear, etc.
2. Interaction behavior is a fine representation of what happens both at the bulk and boundary scale: idem as 1 + bulk behavior, etc.

The two ways have different space scales which imply different time scales. When considering rigid bulk behavior, one is able to describe:

1. Rigid body motion
   * *Large time scale*;
   * Recover plastic behavior of the media.

2. Motion due to wave propagation
   * *Fine time scale*
   * Recover elastic and plastic behavior of the media. No indeterminacy.

From a *mathematical* point of view:

1. Set valued function, defined by an implicit law : :math:`h(R,\mathbf{U},g) == true`
2. Function, defined by an explicit law : :math:`R=f(\mathbf{U},g)`

Case of **explicit laws**:

First let's considered the normal part, denoted :math:`\delta = <-g>^+`.Several models are available:

* Hertz law: :math:`R_n = k_n\delta^{\frac{3}{2}} = K_n(\delta)\delta`
* Hook law with viscous damping: :math:`R_n = max(0,K_n\delta +\eta_n \frac{m_{eff}\mathbf{U}_n.\mathbf{d}}{r_{eff} d})`
* JKR cohesion law [Johnson.Kendall.ea1971]_: :math:`R_n = k_n\delta^{\frac{3}{2}}+\eta_n \sqrt{m_{eff}U_n} - \gamma_n\sqrt{\delta r_{eff}}`. 

where :math:`K_n` is the contact stiffness, :math:`\eta_n` viscosity coefficient and :math:`\gamma_n` contact cohesion.
:math:`\mathbf{U}` represents the relative velocity between particles and :math:`\mathbf{d} (=d\mathbf{n})` is defined as
a center-center vector, :math:`m_{eff}` and :math:`r_{eff}` represent respectively the effective mass and radius associated
to the contact.

For Hertz law :math:`K_n = \frac{E \sqrt{2 r_{eff} \delta}}{3(1-\nu^2)}`. Behavior parameter are "structure" dependant (i.e. geometry).
For a given pressure :math:`P`, the stiffness level may be characterized by:

* :math:`\kappa = (\frac{E}{P})^{2/3}` (Hertz) 
* :math:`\kappa = \frac{K_n}{P r_{eff}^{d-2}}` (Hook) 

So the elastic deflection :math:`\delta \propto \frac{r_{eff}}{\kappa}` 

Remarks:

* Rigid grains if :math:`\kappa \rightarrow \infty`, a "Good" value are 10000;
* Quasi-static problem: :math:`\delta \ll <g>^+` ( :math:`<g>^+` interstice between grains);
* Flow problem: :math:`\tau \ll \frac{<g>^+}{r_{eff} \dot{\gamma}}` (:math:`\tau` duration of contact).

Considering the oscillator made by two particles in contact (Hook law):

:math:`m_{eff} \frac{d^2 \delta}{d t^2} + \eta_n \frac{d \delta}{d t} + K_n \delta =0`

* Critical damping :math:`\eta_n^c = 2 \sqrt{m_{eff} K_n}` and :math:`\alpha_n =\frac{\eta_n}{\eta_n^c}`
* Pulsation :math:`\omega = \sqrt{\frac{K_n}{m_{eff}} (1-\alpha_n^2)}` and contact duration :math:`\tau = \pi/\omega`
* Restitution coefficient :math:`e_n = exp[ -\frac{\pi \alpha_n}{\sqrt{1-\alpha_n^2}}]`

Remarks:

* :math:`\eta_n \leq \eta_n^c`
* For Hertz law, same results with a varying :math:`K_n`


Secondly let's considered the tangential part of the interaction law:

* Viscous law with Coulomb threshold: :math:`\mathbf{R}_t^{Tr}= -\eta_t m_{eff} \mathbf{U}_t`
  if :math:`|| \mathbf{R}_t^{Tr}|| \geq  \mu|r_n|` then :math:`\mathbf{R}_t = \mu|r_n| \frac{\mathbf{R}_t^{Tr}}{|| \mathbf{R}_t^{Tr}||}`
  else :math:`\mathbf{R}_t =  \mathbf{R}_t^{Tr}`

* Incremental elastic law with Coulomb threshold:

  - a previous :math:`\mathbf{R}_t` is known, :math:`\Delta \mathbf{R}_t = K_t h \mathbf{U}_t` and :math:`\mathbf{R}_t^{Tr}= \mathbf{R}_t + \Delta \mathbf{R}_t`
  - if :math:`|| \mathbf{R}_t^{Tr}|| \geq  \mu|r_n|` then :math:`\mathbf{R}_t = \mu|r_n| \frac{\mathbf{R}_t^{Tr}}{|| \mathbf{R}_t^{Tr}||}` else  :math:`\mathbf{R}_t =  \mathbf{R}_t^{Tr}`
    with :math:`K_t = \frac{2(1- \nu)}{2-\nu} K_n`.  

Remarks:

* Compute :math:`\eta_t^c` as before by taking :math:`m_{eff}=\frac{m_1m_2}{m_1+m_2}+m_1m_2(\frac{R_1^2}{I_1} + \frac{R_2^2}{I_2})` and replacing :math:`K_n` by :math:`K_t`
* :math:`\eta_t \leq \eta_t^c` 
* Difficult to give a physical meaning to :math:`\eta_n` and :math:`\eta_t`

Explicit laws allow to substitute the displacement (or the velocity) to the force in the dynamical equation.
If considering a simplified problem described by the Newton equation:

.. math::
   \mathbb{M} \dot{\mathbf{v}} = \mathbf{F}(\mathbf{q},\dot{\mathbf{q}},t) + \mathbf{P}(t) + \mathbf{r}

using :math:`\mathbf{U} = \mathbb{H}^{\star} (\mathbf{q}) \mathbf{v}`, :math:`g = D(\mathbf{q})`, :math:`\mathbf{r} = \mathbb{H}(\mathbf{q}) \mathbf{R}`
and :math:`\mathbf{R}=f(\mathbf{U},g)` one can write:

.. math::
   \mathbb{M}\dot{\mathbf{v}} = \mathbf{P}(t)+ \mathbb{H}(\mathbf{q}) f(\mathbb{H}^{\star} (\mathbf{q}) \mathbf{v}, D(\mathbf{q}))

which is a "classical" non linear problem only written in term of kinematic unknowns. The kinematic unknowns of each component are dependent due to the interactions. 

Case of **implicit laws**

Frictional contact: Signorini-Coulomb

.. math::
   \begin{array}{l}
   R_N \geqslant 0 ~ g \geqslant 0 ~ R_N \cdot g = 0  \\
   \parallel R_T \parallel \leqslant \mu R_N, 
   \left \lbrace \begin{array}{l} \parallel R_T \parallel <  \mu R_N \Rightarrow U_T = 0 \\
   \parallel R_T \parallel = \mu R_N \Rightarrow \exists \alpha \geqslant 0, U_T = -\alpha R_T
   \end{array} \right .
   \end{array}

.. image:: figures/Signorini.jpg
   :width: 200px
   :align: center

.. image:: figures/Coulomb.jpg
   :width: 200px
   :align: center


Remark:

For dynamical problems, it is more natural to formulate the unilateral contact in term of velocities

.. math::
   \begin{array}{l}
   \text{Assuming} ~ g(t_0) \geq 0 ~ \text{then} ~ \forall t > t_0 \\
   \text{if} ~ g(t) \leq 0  ~ \text{then} ~ U_N \geq 0, ~ R_N \geq 0, ~ U_N R_N = 0  \\
   \text{else} ~ R_N = 0 
   \end{array}

It's not possible to substitute reaction by kinematic unknowns, the problem remains implicit.
One can consider explicit law (smooth) as a regularization of implicit law (non smooth).
In this case the parameters are not due to physical considerations but numerical ones.

An explicit law may be written as an implicit law.

Example:

Lets consider the basic example of a single ball bouncing on a plan:

.. image:: figures/ball.jpg
   :width: 200px
   :align: center

.. image:: figures/impact.jpg
   :width: 200px
   :align: center

**Question:** Is the usual formalism adapted to describe a dynamical system with unilateral contact?

So let try to solve the (simplified) problem:

.. math::
   \left \{ 
   \begin{array}{l}
   M \ddot{q} = R \\
   0 \leq q  \; \; \bot \; \; R \geq 0 \\ 
   \mbox{initial conditions: } q(0)=0, \dot{q}(0)=-1
   \end{array}
   \right.

Using an implicit Euler scheme, one obtains the following system:

.. math::
   \left \{ 
   \begin{array}{l} 
   \ddot{q}_{i+1} =( \dot{q}_{i+1} - \dot{q}_i )/h \\ 
   q_{i+1} = q_i + h \dot{q}_{i+1}
   \end{array}
   \right.

Thus the solution is:

.. math::
   \left \{
   \begin{array}{l} 
   q^1 = 0, \dot{q}^1 = 0, R^1 = M\dot{q}^0/h \\ 
   q^k = 0, \dot{q}^k = 0, R^k = 0
   \end{array}
   \right.

So if :math:`h \rightarrow 0` then :math:`R^1 = \infty`

Two solutions are possible:

* Using smooth interaction law as before but needs *reasonably small* time steps
* Adapting the formalism to face the problem

Introducing a deformable body will not solve the problem.


Non Smooth Dynamic
------------------

Equations of motion
^^^^^^^^^^^^^^^^^^^

The differential system must be modified to describe collisions and other non smooth phenomena.

The "spirit" of the approach is to consider a weaken form of the dynamical system, e.g. balance of momentum:

.. math::
   \mathbb{M} (\dot{\mathbf{q}}^{+} - \dot{\mathbf{q}}^{-})   =  \int_{t^-}^{t^+} (\mathbf{F}(\mathbf{q},\dot{\mathbf{q}},s)+ \mathbf{P}(s)) ds + \mathbf{p}

where the impulse :math:`\mathbf{p}` will contain both the sum of:

* the contribution of smooth load over the time interval (:math:`\int_{t^-}^{t^+}R dt`),
* the percussion, denoted  :math:`\mathbf{P}`, at shock time (supposed instantaneous).

To describe :math:`\dot{\mathbf{q}}` one uses the local bounded variation framework over all sub-intervals of :math:`I_{Tps}=[0,T]`,
i.e. :math:`lbv(I_{Tps},R_n))` and one introduces differential measures allowing the generalization of the equation of motion
to non smooth phenomena ([Moreau1988]_). 

Remark:
Event driven approaches will consider smooth motion, but will re-initialize velocity (acceleration, etc.) at each non smooth event using shock law. 
Only reasonable to compute a granular gas or a loose flow of granular material. 

More precisely the classical equation of motion is reformulated in terms of a differential measure equation:

.. math::
   \mathbb{M} \dot{\mathbf{q}} = \mathbf{F}(t,\mathbf{q},\dot{\mathbf{q}}^+)dt + \mathbf{P}(t,\mathbf{q},\dot{\mathbf{q}}^+)dt + \mathbf{dp}

.. math::
   \mathbf{q}(t) = \mathbf{q}(t_0) + \int_{t_0}^{t}\dot{\mathbf{q}}^+ dt

In the previous equation, :math:`\dot{\mathbf{q}}` is the differential measure of :math:`\dot{\mathbf{q}}`
(atomistic at the discontinuity and :math:`\ddot{\mathbf{q}}dt` on the continuous part), :math:`dt` is the
Lebesgue measure on :math:`\mathbb{R}` while :math:`\mathbf{dp}` is the differential measure of contact forces.

The measure :math:`\mathbf{dp}` contains: 

* The contribution of smooth contact (diffuse contribution :math:`\mathbf{R}dt`):
* The contribution of local impulsion densities exerted by shocks :math:`\mathbf{P} \delta` (atomic contributions),


Interaction law
^^^^^^^^^^^^^^^

Local - global mapping:

.. math::
   \begin{array}{l}
   \mathbf{U}^+ = \mathbb{H}^{\star}(\mathbf{q}) \dot{\mathbf{q}}^+ \\
   d\mathbf{p}=\mathbb{H}(\mathbf{q}) d\mathbf{P}
   \end{array}

Frictional contact:

.. math::
   \begin{array}{l}
   \text{Assuming} ~ g(t_0) \geq 0 ~ \text{then} ~ \forall t > t_0 \\
   \text{if} ~ g(t) \leq 0  ~ \text{then} ~ U^+_N \geq 0, ~ dP_N \geq 0, ~ U^+_N dP_N = 0  \\
   \text{else} ~ dP_N = 0 
   \end{array}

Coulomb friction (threshold law):

.. math::
   \begin{array}{l} 
   \parallel d\mathbf{P}_T \parallel \leqslant \mu dP_N, 
   \left \lbrace \begin{array}{l} \parallel d\mathbf{P}_T \parallel <  \mu dP_N \Rightarrow \mathbf{U}^+_T = 0 \\
   \parallel d\mathbf{P}_T \parallel = \mu dP_N \Rightarrow \exists \alpha \geqslant 0, \mathbf{U}^+_T = -\alpha d\mathbf{P}_T
   \end{array} \right .
   \end{array}

Shock law:

When shocks occur in a rigid body collection, the equation of motion and the interaction law are not sufficient to describe properly all the physics of the problem.

It must take into account:

* Local phenomena as inelastic behavior of materials at the interface depending on both the contact geometry and the material behavior.
* Global phenomena as the wave propagation in the body bulk, body geometry dependency and boundary conditions.
* More complex effects: long distance effects due to simultaneous impact

In the case of binary shocks, three kind of restitution can be used:

* Newton restitution, which relates the velocity after (:math:`U^+`) to the velocity before (:math:`U^-`) impact  :math:`U^+ = -e_n U^-`
* Poisson restitution, which relates restitution impulsion (:math:`R_r`) to the compression impulsion (:math:`R_c`) according to the decomposition of the shocks in a compression and restitution phase: :math:`R^r = e_p R^c`
* Energy or Stronge restitution, which relates restitution energy to compression energy: :math:`e_s = \frac{\int_{t_c}^{t_r} R U dt }{\int_{t^{\star}}^{t_c} R U dt}`

Several remarks could be done:

* Impact law for the normal component is well understood, it is not the case of the tangential component.
* The choice of the restitution coefficient is a difficult task for complex structures [Stoianovici.Hurmuzlu1996]_.
* In the case of dense granular material, the effect of impacts may be neglected and previous laws can be used.
* Binary impact law are not sufficient to model phenomena such as Newton cradle (multiple impacts). One can refer to [Acary.Brogliato2003]_ for more details about the construction of a restitution law.


Conclusion
----------

Various modeling choices are possible for DEM depending on:

* space scale: interaction -> smooth, component -> non-smooth
* time scale: waves -> smooth, rigid body motion with impact -> non-smooth 
* the shape of the components -> non-smooth easier but introduce indetermination.
* etc.


Numerical Strategies
====================

Introduction
------------

Depending on modeling choices numerical strategies are build to solve the evolution problem. They depend on:

 * time evolution strategy: \alert{time stepping} or event driven
 * time integrator over a time step: explicit or implicit 
 * implicit contact solver if necessary (Lemke, Gauss-Seidel, bi-potential, etc.)
 * technical aspects: contact detection, rotation integration, etc.
 * etc. 

Over a time step [t,t+h[, three important tasks can be underlined:

.. image:: figures/demscheme.jpg
   :width: 400px
   :align: center

* The contact detection
* The computation of contact forces, called contact problem
* The motion of the different element of the media.

Pioneer DEMs such as Cundall et al. [Cundall.Strack1979]_ or Allen et al. [Allen.Tildesley1987]_ consider explicit interaction model (smooth)
and use explicit time integration scheme. This kind of methods refer to smooth-DEM.

The so-called *Granular Element Method*, proposed by Kishino [Kishino1988]_ consider also an explicit model for contact but is based on
an implicit time integrator. The solving method is similar to the penalization techniques used in Finite Element Method.

In a different way, Moreau developed the *Contact Dynamics* method [Moreau1988]_ for implicit non smooth interaction model.
It uses an implicit time integrator. It needs a dedicated contact solver. Further works lead to the extension of the method
to multi-contact simulations of collections of deformable bodies [Jean1999]_ and the method becomes the so-called
*Non Smooth Contact Dynamics* method (NSCD). This kind of methods refer to NSCD.

Carpenter et al. [Carpenter.Taylor.ea1991]_ proposed a method to solve implicit interaction model using explicit time integrator.

Thus the *end-user* has various possibilities to perform numerical modeling.

Smooth-DEM
----------

Generality
^^^^^^^^^^

Using explicit interaction law leads to solve:

.. math::

  \mathbb{M} \cdot \mathbf{v} = \mathbb{F}(\mathbf{q},\dot{\mathbf{q}},t) + \mathbb{P}(t) + \mathbf{r}

with :math:`\mathbf{r} = \mathbb{H}(\mathbf{q}) \mathbf{R}`, :math:`R=f(U,g)` and :math:`U = \mathbb{H}^{\star} (\mathbf{q}) v`, :math:`g = D(\mathbf{q})`.

.. ~~\ding{220} This is a non-linear problem. \\

Using an implicit time integrator, a non-linear solver (Newton-Raphson) will be necessary.
Assuming linear contact laws and fixed contact network it may be solved directly (see GEM of Kishino).

Using an explicit time integrator leads to an uncoupled set of equations:

.. math::

 \mathbb{M} ( \mathbf{v}_{n+1} - \mathbf{v}_{n} ) = h\mathbb{F}(\mathbf{q}_n,\dot{\mathbf{q}}_n,t_n) + h\mathbb{P}(t_n) + h\mathbf{r}_n

Different explicit time integrator can be used to integrate the motion of particle: 

 * Gear integrator for Molecular Dynamics [Allen.Tildesley1987]_
 * Velocity verlet
 * Time-centered scheme for the Distinct Element Method (Cundall) [Cundall1971]_


Time integrator
^^^^^^^^^^^^^^^

.. \frametitle{DEM::Numerical Strategies::smooth-DEM::Time integrator}

**Predictor-corrector Gear integrator**

*Prediction:* Using classical Taylor expansion (:math:`\mathbf{b}` and :math:`\mathbf{c}` are first and second time derivative of acceleration):

.. math::

  \left\{
  \begin{array}{ll}
  \mathbf{q}^p(t+h)        & = \mathbf{q}(t) + h\dot{\mathbf{q}(t)} + \frac{h^2}{2!} \ddot{\mathbf{q}}(t) + \frac{h^3}{3!} \mathbf{b}(t) + \frac{h^4}{4!} \mathbf{c}(t) + \ldots \\
  \dot{\mathbf{q}}^p(t+h)  & = \dot{\mathbf{q}}(t) + h \ddot{\mathbf{q}}(t) + \frac{h^2}{2!} \mathbf{b}(t) + \frac{h^3}{3!} \mathbf{c}(t) + \ldots \\
  \ddot{\mathbf{q}}^p(t+h) & = \ddot{\mathbf{q}}(t) + h \mathbf{b}(t) + \frac{h^2}{2!} \mathbf{c}(t) \ldots \\
  \mathbf{b}^p(t+h)        & = \mathbf{b}(t) + h \mathbf{c}(t) + \ldots \\
  \mathbf{c}^p(t+h)        & = \mathbf{c}(t) + \ldots
  \end{array}
  \right.


Perform detection in the predicted configuration, compute contact forces, compute the acceleration

.. math::

 \ddot{\mathbf{q}}^c(t+h) = \mathbb{M}^{-1}\mathbf{\mathbb{F} + \mathbb{P} + \mathbf{r}}(t+h)

*Correction:* considering :math:`\Delta \ddot{\mathbf{q}} = \ddot{\mathbf{q}}^c(t+h) -\ddot{\mathbf{q}}^p(t+h)` perform the correction

.. math::

 \left\{
 \begin{array}{lll}
 \mathbf{q}^c(t+h)        & = \mathbf{q}^p(t+h)  & + c_0\Delta \ddot{\mathbf{q}}\\
 \dot{\mathbf{q}}^c(t+h)  & = \dot{\mathbf{q}}^p(t+h) & + c_1\Delta \ddot{\mathbf{q}}\\
 \ddot{\mathbf{q}}^c(t+h) & = \ddot{\mathbf{q}}^p(t+h) & + c_2\Delta \ddot{\mathbf{q}}\\
 \mathbf{b}^c(t+h)        & = \mathbf{b}^p(t+h) & + c_3 \Delta \ddot{\mathbf{q}}\\
 \mathbf{c}^c(t+h)        & = \mathbf{c}^p(t+h) & + c_4 \Delta \ddot{\mathbf{q}}
 \end{array}
 \right. .

The correction step may be repeated.

.. \frametitle{DEM::Numerical Strategies::smooth-DEM::Time integrator}

**Velocity Verlet scheme**

Its also predictor-corrector approach.

*Prediction:* 

.. math::

 \left\{
 \begin{array}{ll}
 \mathbf{q}(t+h)  & = \mathbf{q}(t)  + h \dot{\mathbf{q}}(t)+\frac{1}{2}h^2\ddot{\mathbf{q}}(t)\\
 \dot{\mathbf{q}}(t+h/2)    & = \dot{\mathbf{q}}(t) + \frac{1}{2}h\ddot{\mathbf{q}}(t)
 \end{array}
 \right. .

Perform detection in the predicted configuration, compute contact forces, compute the acceleration knowing:
:math:`\mathbf{q}(t+h)` and :math:`\dot{\mathbf{q}}(t+h/2)` (:math:`\ddot{\mathbf{q}}(t+h) = \mathbb{M}^{-1}\mathbf{\mathbb{F} + \mathbb{P} + \mathbf{r}}(t+h)`). 

*Correction:*

.. math::

  \dot{\mathbf{q}}(t+h) = \dot{\mathbf{q}}(t+h/2) + \frac{1}{2}h\ddot{\mathbf{q}}(t+h)

Note that the classical Verlet scheme (*leap-frog*) is not usable for DEM.

.. \frametitle{DEM::Numerical Strategies::smooth-DEM::Time integrator}

*Time-centered scheme*

Knowing :math:`\mathbf{q}(t)` and :math:`\dot{\mathbf{q}}(t-h/2)` one perform contact detection, computes contact forces and update acceleration
:math:`\ddot{\mathbf{q}}(t)`.

Then one updates:

.. math::

 \left \{
 \begin{array}{ll}
 \dot{\mathbf{q}}(t+h/2) & = \dot{\mathbf{q}}(t-h/2) + h\ddot{\mathbf{q}}(t) \\
 \mathbf{q}(t+h)    & = \mathbf{q}(t)  + h \dot{\mathbf{q}}(t+h/2)
 \end{array}
 \right. .

It is not written as a predictor-corrector scheme.
But it's the same as velocity verlet.

Contact laws
^^^^^^^^^^^^

.. % \frametitle{DEM::Numerical Strategies::smooth-DEM::Contact laws}

They are computed in the predicted configuration.

.. % ... to be developped ...

Practical aspects
^^^^^^^^^^^^^^^^^

.. \frametitle{DEM::Numerical Strategies::smooth-DEM::Practical aspects}

Some cares must be taken to obtain numerical results that keep a mechanical sense:

* The time step $h$ depends explicitly on the mechanical parameters of the system (:math:`h=\frac{1}{N}\sqrt{\frac{m_{eff}}{K_n}}`)
  and must be small enough to ensure numerical stability and describe with accuracy the contact.
* Dissipation must be added to stabilize the problem (i.e. over-shoots). 

For example non viscous dissipation can be added to study granular flow:

.. math::

  F_d = - \alpha ~ r ~ sign(\dot{\mathbf{q}})

:math:`\alpha` is chosen depending on the studied problem:

* 0.7 for quasi-static problem
* less for dynamic problem
* and 0.1 for wave propagation

Thus control parameter of the simulation are:

* the time step discretization (:math:`N`),
* the local stiffness (normal :math:`K_n` and tangential :math:`K_t`),
* the viscous (:math:`\eta`) or non viscous (:math:`\alpha`) dissipation.

Evaluation criteria related to the computation quality:

 * Respect of interaction law,
 * Control of numerical damping.

.. This is the price to obtain dynamics tractable by standard integration techniques. The drawback is that, for the sake of precision, very steep repulsion laws have to be used, generating stiff differential equations, which require very short step-length for their integration, possibly resorting to the introduction of artificial damping and artificial inertia to secure numerical stability.
.. 
.. It is important to notice that such approaches are very simple to code but may be complicated to drive \cite{Belytschko.Liu.ea2000}.

Non Smooth Contact Dynamics
---------------------------

Time stepping
^^^^^^^^^^^^^

.. \frametitle{DEM::Numerical Strategies::NSCD::Time stepping}

Time integration of the equation of motion leads to:

.. math::

 \begin{array}{lcl}
 \mathbb{M} (\dot{\mathbf{q}}_{i+1} - \dot{\mathbf{q}}_i)  & = & \int_{t_i}^{t_{i+1}} (\mathbb{F}(\mathbf{q},\dot{\mathbf{q}},s)+ \mathbb{P}(s)) ds + p_{i+1} \\ 
                 & = & p_{free} + p_{i+1}\\
 \mathbf{q}_{i+1} & = & \mathbf{q}_i + \int_{t_i}^{t_{i+1}} \dot{\mathbf{q}} ds
 \end{array}

.. \label{dyna:global}

where:

* :math:`p_{i+1} = \int_{t_i}^{t_{i+1}} dp` represents the value of the total impulsion over the time step \alert{the unknown in the following}
* :math:`p_{free}` the integral of applied forces over the time step; 

.. %% relation impulsion -> effort ? voir Abadie

.. \frametitle{DEM::Numerical Strategies::NSCD::Time stepping}

A :math:`\theta`-method (Crank-Nicholson) is used to evaluate :math:`\int_{t_i}^{t_{i+1}} (\mathbb{F}(\mathbf{q},\dot{\mathbf{q}},s)+ \mathbb{P}(s)) ds`
and :math:`\int_{t_i}^{t_{i+1}} \dot{\mathbf{q}} ds` :

.. \begin{eqnarray*} \label{dyna:rfree}

.. math::

 \begin{array}{lcl}
  p_{free} & = & h (1 -\theta) (\mathbb{F}(\mathbf{q}_i,\dot{\mathbf{q}}_i,t_i)+ \mathbb{P}(t_i)) + \\ 
             &   & h \theta (\mathbb{F}(\mathbf{q}_{i+1},\dot{\mathbf{q}}_{i+1},t_{i+1})+ \mathbb{P}(t_{i+1}))\\
  \mathbf{q}_{i+1} & = & \mathbf{q}_i + h ((1-\theta) \dot{\mathbf{q}}_i + \theta \dot{\mathbf{q}}_{i+1}) = \mathbf{q}_m + h \theta \dot{\mathbf{q}}_{i+1}
 \end{array}

with :math:`\mathbf{q}_m = \mathbf{q}_i + h (1 - \theta) \dot{\mathbf{q}}_i`.

* If :math:`\theta \in [0.5,1]` the scheme is implicit and stable unconditionally.
* If :math:`\theta=0.5` the scheme is conservative for smooth evolution problem,
* If :math:`\theta =0` the scheme is explicit [Carpenter.Taylor.ea1991]_

The order of the time integrator is weak (1 or 2).
The time discretization is imposed arbitrarily. Its mainly driven by the precision of the contact treatment.
If discontinuities occur they are treated simultaneously. No limitation on the number of interactions but the time order is lost.

.. \frametitle{DEM::Numerical Strategies::NSCD::Time stepping}

If now we use this formulation for the previous elementary contact problem, one obtains:

.. math::
   \begin{array}{l}
   M (\dot{q}_{i+1} - \dot{q}_i) = p \\
   \mbox{ If } q_i \leq 0,  0 \leq \dot{q}_{i+1} \; \;  \bot \; \;  hR \geq 0 \\ 
   \mbox{ Initial conditions : } q(0)=0, \dot{q}(0)=-1
   \end{array}

The implicit euler scheme gives:

.. math::
  \left \{ 
   \begin{array}{l} 
     q^1 = 0, \dot{q}^1 = 0, p^1 = M\dot{q}^0 \\ 
     q^k = 0, \dot{q}^k = 0, p^k = 0
   \end{array}
  \right.

The obtained solution is independent of the time step.

If :math:`q(0)=\epsilon` then :math:`\dot{q}^1 = 0` and :math:`p^1 = M\dot{q}^0` and :math:`q^k=\epsilon`.

The error made on the gap decreases with the size of the time step.

.. %% remarques de Vincent Acary si R est une mesure ... voir transparents de Bristol

Managing interactions
^^^^^^^^^^^^^^^^^^^^^

.. \frametitle{DEM::Numerical Strategies::NSCD::Interaction management}

The two mappings :math:`\mathbb{H}^{\star}` and :math:`\mathbb{H}` depend on the solution.
They may be evaluated in a 'mean' configuration: :math:`\tilde{q}_{k+1} = q_{k+1} + h(\gamma_1 \dot{\mathbf{q}}_{k} + \gamma_2 \dot{\mathbf{q}}_{k+1})`

If the time step is small enough (small sliding motions, etc) and the curvature of component shapes is small,
the mapping :math:`\mathbb{H}^{\star}` and :math:`\mathbb{H}` can be considered as constant on a whole time interval.

**Remarks:**

* Usually the contact problem is solved in a pseudo-explicit configuration: :math:`\mathbf{q}_m= \mathbf{q}_i +  h(1-\theta) \dot{\mathbf{q}}_i`.
* The pseudo configuration of the next time step will be determined by the final velocity 
  :math:`\dot{\mathbf{q}}_{i+1}`: :math:`\mathbf{q}_{m+1} = q_{m} + h \dot{\mathbf{q}}_{i+1}`. This is a *Leap-Frog* technique.

Contact problem formulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. \frametitle{DEM::Numerical Strategies::NSCD::Contact problem formulation}

The problem to solve is written in terms of:

* discretized equations of motion for each body expressed with global unknowns :
  :math:`\dot{\mathbf{q}}_{i+1} = \dot{\mathbf{q}}_{free} + \mathbb{M}^{-1} p_{i+1}`
* interaction laws expressed with local unknowns (contact :math:`\alpha`):
  :math:`Contact(g^{\alpha}, U_n^{\alpha}, P_n^{\alpha}) = TRUE \quad Friction(\mathbf{U}_t^{\alpha},\mathbb{P}_t^{\alpha}) = TRUE`
* mappings (:math:`\mathbb{H}` and :math:`\mathbb{H}^{\star}`) to pass from local to global unknowns

.. math::
  \begin{array} {ccc}
     \mathbf{q},\dot{\mathbf{q}}    & \gets \textrm{ Equations of motion } \to & p   \\
                                    &                                          &      \\
     \mathbb{H}^{\star} \downarrow  &                                          & \uparrow \mathbb{H} \\
                                    &                                          & \\
                         \mathbf{U} & \gets \textrm{   Interaction laws  } \to & \mathbb{P}
  \end{array}

Using basic algebraic transformations the equations of motion may be expressed in terms of local unknowns 

.. math::
 \mathbf{U} = \mathbf{U}_{free} + \mathbb{W} ~ \mathbb{P}

where :math:`\mathbb{W} = \mathbb{H}^{\star} \mathbb{M}^{-1} \mathbb{H}` and
:math:`\mathbf{U}_{free} = \mathbb{H}^{\star}(\dot{\mathbf{q}}_{i} + \mathbb{M}^{-1} p_{free})= \mathbb{H}^{\star} \dot{q}_{free}`.

.. %%(explicitly or numerically):

.. \frametitle{DEM::Numerical Strategies::NSCD::Resolution Scheme}

.. %%Resolution Scheme:\\

.. \begin{center}
.. \framebox{\parbox{3.5in}{

Iteration matrix computation (:math:`\mathbb{M}`)

Time loop:

.. %\hspace*{0.5cm} Free velocity computation ($\dot{\mathbf{q}}_{free}$) \\
.. %\hspace*{0.5cm} Temporary configuration computation($\mathbf{q}_m$) \\
.. %\hspace*{0.5cm} Contact detection \\
.. %\hspace*{0.5cm} {\bf Contact problem resolution}\\
.. %\hspace*{0.5cm} Velocity correction ($\dot{\mathbf{q}}_{i+1}$)  \\
.. %\hspace*{0.5cm} Update of the kinematics and the configuration \\
.. \hspace*{0.5cm} 

.. math::
 \left[
 \begin{array}{l}
 \mbox{ Free velocity computation} (\dot{\mathbf{q}}_{free}) \\
 \mbox{Temporary configuration computation} (\mathbf{q}_m) \\
 \mbox{Contact detection} \\
 \mbox{{\bf Contact problem resolution}}\\
 \mbox{Velocity correction} (\dot{\mathbf{q}}_{i+1} = \dot{\mathbf{q}}_{free} + \mathbb{M}^{-1} p_{i+1})  \\
 \mbox{Update of the kinematics and the configuration} \\
 \end{array} 
 \right .

Halt criteria

Contact Solvers
^^^^^^^^^^^^^^^

The classical NSCD approach rely on a Non Linear Gauss Seidel (NLGS) algorithm.
Considering one by one the contacts (`\alpha`) of the local systems to solve:

.. math::
  \begin{array}{lll}
  & & \mathbf{U}_{\alpha} = \mathbf{U}_{\alpha,free} + \mathbb{W}_{\alpha \alpha} \mathbb{P}_{\alpha} + \sum_{\beta \neq \alpha} \mathbb{W}_{\alpha \beta} \mathbb{P}_{\beta} \\
  & & Law(g_{\alpha},\mathbf{U}_{\alpha,n}\mathbf{U}_{\alpha,t},P_{\alpha,n},P_{\alpha,t}) =  TRUE
  \end{array}
 
Freezing the contributions of other contact :math:`(\beta \neq \alpha)` to:

* updated values of :math:`\mathbb{P}` if :math:`\beta < \alpha` 
* old values of :math:`\mathbb{P}` if :math:`\beta > \alpha`.

We solve the `\alpha` local problem of 2 unknowns with 2 equations and we repeat the process until convergence
 
Local solvers:

* 2D :

  * explicit uncoupled resolution if :math:`\mathbb{W}^{\alpha \alpha}` is diagonal
  * coupled :math:`(n,t)` graph intersection (:math:`b=\mathbf{U}_{\alpha,free} + \sum_{\beta \neq \alpha} \mathbb{W}_{\alpha \beta} \mathbb{P}_{\beta}` )

.. %\cite{Jean1999} 

.. %%\begin{columns}
.. %%\begin{column}[c]{4cm}
.. %%  \begin{figure}
.. %%  \includegraphics[height=3cm]{./Figs/solving_SC2D_system.jpg}
.. %%  \end{figure} 
.. %%\end{column}
.. %%\begin{column}[c]{3cm}
.. %%  \begin{figure}
.. %%  \includegraphics[width=4cm]{./Figs/solving_SC2D_graphes.jpg}
.. %%  \end{figure} 
.. %%\end{column}
.. %%\begin{column}[c]{7cm}

.. math::

 \begin{array}{lcl}
 If \ b_N \geq 0 & then & \mathbb{P} = 0 \\
               &      &  status: no \ contact \\
 Else          &      &       \\
 \mbox{\hspace{2ex}} compute & \epsilon=-1,1 & Dft_{\epsilon}=W_{TN}+\epsilon \mu W_{TT} \\
                      &               & Dfn_{\epsilon}=W_{NN}+\epsilon \mu W_{NT} \\
 \mbox{\hspace{2ex}} If \epsilon(Dft_{\epsilon}b_N - Dfn_{\epsilon}b_T) \geq 0 & then & P_n = -b_N/Dfn_{\epsilon}, P_t=\epsilon \mu P_N \\
   & & status: sliding (\epsilon) \\
 \mbox{\hspace{2ex}} Else & & \mathbb{P} = \mathbb{W}^{-1}b \\
    & & status: sticking \\ 
 \end{array} 

* pseudo-potential approach (bi-potential)
* LCP solver
* etc. 

.. %\cite{deSaxce.Feng1991}

* 3D:

  * explicit resolution if :math:`\mathbb{W}^{\alpha \alpha}` is diagonal (3D)
  * Generalized Newton algorithm (3D)
  * pseudo-potential approach (2D)
  * LCP solver
  * etc

.. %\cite{Renouf.phd2004}
.. %\cite{Alart.Curnier1991}
.. %\cite{deSaxce.Feng1991} 

Various implementation of NLGS are possible:

Store Delassus Loop strategy (SDL):

(0) Evaluating all the matrices :math:`W_{\alpha \beta}`

.. math::

 \left[
 \begin{array}{l}
 k=k+1 \mbox{ (NLGS iteration)}\\
 \left[
 \begin{array}{l}
 \alpha = \alpha+1 \mbox{ (Contact index)}\\
 \mbox{(a) Evaluating the right-hand side} \\
 \mbox{\hspace{3ex}}\mathbf{U}_{\alpha,loc} = \mathbf{U}_{\alpha,free} + \sum_{\beta < \alpha} \mathbb{W}_{\alpha \beta} \mathbb{P}^{k+1}_{\beta}
   + \sum_{\beta > \alpha} \mathbb{W}_{\alpha \beta} \mathbb{P}^{k}_{\beta}  \\
 \mbox{(b) Solving the local problem}  \\
 \end{array} 
 \right .  \\
 \mbox{Convergence test for } k=0 \ldots k_{max}\\
 \end{array} 
 \right . 

Rigid: efficient in 2D not in 3D 

Deformable: efficient in 2D

Exchange Local Global Strategy (ELG):

(0) Evaluating the :math:`W_{\alpha \alpha}` matrix

.. math::

 \left[
 \begin{array}{l}
 k=k+1 \mbox{ (NLGS iteration)}\\
 \left[
 \begin{array}{l}
 \alpha = \alpha+1 \mbox{ (Contact index)}\\
 \mbox{(i) Identifying the contact bodies} (\alpha = jl) \\
 \mbox{\hspace{3ex}Computing an auxiliary value} \\
 \mbox{\hspace{3ex}}\mathbf{U}_{\alpha,aux} = \mathbb{H}^{\star}_{\alpha} (\mathbb{M}_j^{-1} r^k_{j} - \mathbb{M}_l^{-1} r^k_{l})  \\
 \mbox{(a) Evaluating the right-hand side} \\
 \mbox{\hspace{3ex}}\mathbf{U}_{\alpha,loc} = \mathbf{U}_{\alpha,free} + \mathbf{U}_{\alpha,aux} - \mathbb{W}_{\alpha \alpha} \mathbb{P}^{k}_{\alpha}  \\
 \mbox{(b) Solving the local problem}  \\
 \mbox{(i) Updating the resultant on bodies} (\alpha = jl) \\
 \mbox{\hspace{3ex}}
 \left[ \begin{array}{c} r_j \\ r_l \end{array} \right]^{k+1} = 
 \left[ \begin{array}{c} r_j \\ r_l \end{array} \right]^{k} +
 \mathbb{H}_{\alpha} ( \mathbb{P}^{k+1}_{\alpha} - \mathbb{P}^{k}_{\alpha}) \\
 \end{array} 
 \right .  \\
 \mbox{Convergence test for } k=0 \ldots k_{max}\\
 \end{array} 
 \right .

A parallel treatment of the NLGS is possible

Store Delassus Loop strategy (SDL):

(0) Evaluating all the matrices :math:`W_{\alpha \beta}`

.. math::

  \left[
  \begin{array}{l}
  k=k+1 \mbox{ (NLGS iteration)}\\
  \mbox{\bf{!\$OMP PARALLEL PRIVATE (...) SHARED (...) ...}} \\
  \mbox{\bf{!\$OMP DO ...}} \\
  \left[
  \begin{array}{l}
  \alpha = \alpha+1 \mbox{ (Contact index)}\\
  \mbox{(a) Evaluating the right-hand side} \\
  \mbox{\hspace{3ex}}\mathbf{U}_{\alpha,loc} = \mathbf{U}_{\alpha,free} + \sum_{\beta < \alpha} \mathbb{W}_{\alpha \beta} \mathbb{P}^{k+1}_{\beta}  + \sum_{\beta > \alpha} \mathbb{W}_{\alpha \beta} \mathbb{P}^{k}_{\beta}  \\
  \mbox{(b) Solving the local problem}  \\
  \end{array} 
  \right .  \\
  \mbox{\bf{!\$OMP ENDDO}} \\
  \mbox{\bf{!\$OMP END PARALLEL}} \\
  \mbox{Convergence test for } k=0 \ldots k_{max}\\
  \end{array} 
  \right . 

Needs equivalent parallel treatment of other CPU consuming parts of the code.

*Quasi NLGS* may be derived, rewriting the system:

.. math::
   \begin{array}{lll}
   & & \mathbf{U}_{\alpha} = \mathbf{U}_{\alpha,free} + \mathbb{W}_{\alpha \alpha} (\mathbb{P}_{\alpha} - \mathbb{P}_{\alpha,esti})\\
                 + \sum_{\beta} \mathbb{W}_{\alpha \beta} \mathbb{P}_{\beta}\\
		   & & Law(g_{\alpha},\mathbf{U}_{\alpha,t},P_{\alpha,n},P_{\alpha,t}) =  TRUE
   \end{array}

where :math:`P_{\alpha,esti}` is the impulsion computed at the previous Gauss-Seidel iteration.

Noting that when the algorithm goes close to the solution, :math:`P_{\alpha} - P_{\alpha,esti} \rightarrow 0` one may derive a
*quasi NLGS* replacing the original :math:`\mathbb{W}_{\alpha \alpha}` by an arbitrary one.

Various alternatives are possible for the contact solver: 

* Conjugate Projected Gradient Algorithm [Renouf.Alart2004]_
* Lemke (for small collection of rigid bodies)
* Other possibilities, see Siconos NSSPack


Practical aspects
^^^^^^^^^^^^^^^^^

Numerical model parameters are:

* The :math:`theta` value
* The time step
* The convergence norm of the Gauss-Seidel algorithm
* The direction of reading the contact set

Quality of the computation lays on:

* The respect of interaction laws
* The number of iteration performed
* The free evolution of bodies



.. rubric:: Bibliography

.. [Acary.Brogliato2003]
.. [Carpenter.Taylor.ea1991]
.. [Johnson.Kendall.ea1971] 
.. [Moreau1988] 
.. [Renouf.Alart2004]
.. [Stoianovici.Hurmuzlu1996]


