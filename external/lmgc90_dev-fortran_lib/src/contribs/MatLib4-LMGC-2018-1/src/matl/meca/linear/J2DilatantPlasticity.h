/*
 *  $Id: J2DilatantPlasticity.h 233 2017-03-30 20:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_J2_DILATANT_PLASTICITY_H
#define ZORGLIB_MATL_MECA_LINEAR_J2_DILATANT_PLASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// std C++ library
//#include <fstream>
#include <limits>
// local
#include <matl/meca/linear/ElastoPlasticity.h>
#include <matl/meca/linear/IsotropicElasticPotential.h>
#include <matl/meca/linear/DilatantViscoPlasticity.h>
#include <matl/meca/linear/HardeningModels.h>
#include <matl/meca/linear/RateDependencyModels.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * J2 dilatant plasticity with arbitrary yield surface (associated flow).
 */
template <class ALG>
class J2DilatantPlasticity : virtual public ElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // associated visco-plasticity model
  DilatantViscoPlasticity *viscoPlasticity;

  // empty constructor
  J2DilatantPlasticity(DilatantViscoPlasticity* vp = 0) {
    viscoPlasticity = vp;
  }

 public:

  // constructor
  J2DilatantPlasticity(DilatantViscoPlasticity& vp)
    : Elasticity<ALG>(new IsotropicElasticPotential<ALG>()) {viscoPlasticity = &vp;}
  
  // copy constructor
  J2DilatantPlasticity(const J2DilatantPlasticity& src)
    : Elasticity<ALG>(src), ElastoPlasticity<ALG>(src) {viscoPlasticity = src.viscoPlasticity;}

  // destructor
  virtual ~J2DilatantPlasticity() {
    if (*(this->count) > 1) return;
    if (viscoPlasticity) delete viscoPlasticity;
  }

  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nJ2 dilatant plasticity model (small strains):" << std::endl;
    
    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }
    
    // elastic potential
    this->potential->checkProperties(material,os);
    
    // (elastic) dilatancy model
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);

    // viscoplastic model
    viscoPlasticity->checkProperties(material,os);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    Elasticity<ALG>::updateProperties(mater,extPar);
    viscoPlasticity->updateProperties(mater,extPar);
  }
  
  // number of internal variables
  unsigned int nIntVar() const {
    return SYM_TENSOR::MEMSIZE+2+viscoPlasticity->nIntPar();
  }
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 4;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "PSTN")
      return 0;
    else if (str == "EPLS")
      return 1;
    else if (str == "ENRG")
      return 2;
    else if (str == "PNRG")
      return 3;
    else
      return 4;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SYM_TENSOR;
        break;
      case 1:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 2:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 3:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      default:
        return ConstitutiveModel::TYPE_NONE;
        break;
    }
  }
  unsigned int indexIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return 0;
        break;
      case 1:
        return SYM_TENSOR::MEMSIZE;
        break;
      case 2:
        return SYM_TENSOR::MEMSIZE+1;
        break;
      case 3:
        return SYM_TENSOR::MEMSIZE+2;
        break;
      default:
        return SYM_TENSOR::MEMSIZE+3;
        break;
    }
  }
  std::string labelIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "plastic strain";
        break;
      case 1:
        return "equivalent plastic strain";
        break;
      case 2:
        return "elastically stored energy";
        break;
      case 3:
        return "plastically stored energy";
        break;
      default:
        return "";
        break;
    }
  }

 protected:

  // compute the plastic update
  double plasticUpdate(const MaterialProperties& material,const ParameterSet& extPar,
                       const SYM_TENSOR& eps,SYM_TENSOR& sig,
                       const SYM_TENSOR& epsPl0,SYM_TENSOR& epsPl,
                       const MatLibArray& intV0,MatLibArray& intV,double dTime,
                       SYM_TENSOR4& M,bool update,bool computeTangent) 
   throw (UpdateFailedException) {
    
    static const double ONE_THIRD = 1./3.;
    static const double TWO_THIRD = 2./3.;
    static const SYM_TENSOR I = SYM_TENSOR::identity();
    
    double sigVol;
    SYM_TENSOR epsEl,sigDev,Mp;
    
    // extract equivalent plastic strain
    double ePl0 = intV0[0];
    double ePl  = intV[0];
    
    // extract volumic plastic strain
    double vPl0 = trace(epsPl0);
    double vPl  = trace(epsPl);

    // extract internal parameters
    unsigned int nIntPar = intV.size()-2;
    const MatLibArray intPar0(intV0,nIntPar,2);
    MatLibArray intPar(intV,nIntPar,2);
    
    // get shear modulus
    double G = material.getDoubleProperty("SHEAR_MODULUS");
    double G2=2*G,G3=3*G;
    
    // get bulk modulus
    double K = material.getDoubleProperty("BULK_MODULUS");

    // compute elastic predictor
    double norm0=0.0e0,coef=0.0e0;
    if (update || computeTangent) {
      
      epsEl = eps-epsPl0;
      this->storedEnergy(material,extPar,epsEl,sig,M,true,false);
      
      // compute volumic stress
      sigVol = ONE_THIRD*trace(sig);
      
      // compute stress deviator
      sigDev = sig-sigVol*I;
      
      // compute radial return direction
      norm0 = innerProd2(sigDev,sigDev);
      if (norm0 >= 1.e-16) coef = std::sqrt(1.5/norm0);
      Mp = coef*sigDev;
    }
    
    // update
    viscoPlasticity->initialize = true;
    viscoPlasticity->finalize = false;
    double dEPl=0.0e0,dVPl=0.0e0;
    if (update) {
      // perform update (radial return in proper metric)
      double sigEq = std::sqrt(1.5*norm0);
      radialReturn(material,extPar,*viscoPlasticity,intPar0,intPar,
                   sigEq,sigVol,ePl0,ePl,vPl0,vPl,dTime);
      
      // update internal variables
      intV[0] = ePl;
      
      // update plastic strain
      dEPl = ePl-ePl0;
      dVPl = vPl-vPl0;
      epsPl = epsPl0+dEPl*covariant(Mp)+(ONE_THIRD*dVPl)*I;
      
      viscoPlasticity->finalize = true;
    }
    
    // elastic deformation
    epsEl = eps-epsPl;
    
    // elastic free energy
    double We = this->storedEnergy(material,extPar,epsEl,sig,M,
                                   update || computeTangent,
                                   computeTangent);
    if (update) intV[1] = We;
    
    // plastic free energy increment + dissipated energy
    double dummy,Hdd,Hdv,Hvv;
    double Wp = viscoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,
                                                    ePl0,ePl,vPl0,vPl,dummy,dummy,
                                                    Hdd,Hdv,Hvv,dTime,false,computeTangent);
    
    // tangents
    dEPl = ePl-ePl0;
    dVPl = vPl-vPl0;
     if (computeTangent && (dEPl > 0.0e0 || std::fabs(dVPl) > 0.0e0)) {
      // (visco)plastic correction
      static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
      // first correction
      double coef1 = G2*dEPl*coef;
      M -= (coef1*G2)*(II-KK-TWO_THIRD*outerProd(Mp,Mp));
      // second correction
      ShortSqrMatrix H(2);
      H[0][0] = G3+Hdd;
      H[0][1] = H[1][0] = Hdv;
      H[1][1] = K+Hvv;
      H.invert();
      M -= ( G2*G2*H[0][0]*outerProd(Mp,Mp)
            +G2*K*H[1][0]*(outerProd(Mp,I)+outerProd(I,Mp))
            +K*K*H[1][1]*3*KK );
    }
    
    return We+Wp-intV0[1];
  }
  
 public:

  // radial return algorithm
  static unsigned int radialReturn(const MaterialProperties& material,const ParameterSet& extPar,
                                   DilatantViscoPlasticity& viscoPlasticity,
                                   const MatLibArray& intPar0,MatLibArray& intPar,
                                   double sigEq0,double sigVol0,double ePl0,double& ePl,
                                   double vPl0,double& vPl,double dTime)
   throw (UpdateFailedException) {

    static const unsigned int ITMAX = 30;
    static const double MULT = 0.9;
    static const double PREC = 1.e-16;
    static const double TOLE = 1.e-8;
    static const double THRSHLD = 0.1*std::numeric_limits<double>::max();
     
    // get algorithmic parameter
    unsigned int maxIt;
    if (material.checkProperty("RR_MAX_ITER_PARAMETER"))
      maxIt = material.getIntegerProperty("RR_MAX_ITER_PARAMETER");
    else
      maxIt = ITMAX;

    // compute steepest gradient direction (and value)
    ePl = ePl0;
    vPl = vPl0;
    double dEPlD,dEPlV;
    double grad = viscoPlasticity.steepestGradient(material,extPar,intPar0,intPar,ePl0,ePl,
                                                   vPl0,vPl,sigEq0,sigVol0,dEPlD,dEPlV,dTime);

    // compute gradient of incremental energy
    double test = sigEq0*dEPlD+sigVol0*dEPlV-grad;
    if (test <= 0.0e0) return 0;
    
    // first iteration by steepest descent
    double G = material.getDoubleProperty("SHEAR_MODULUS");
    double K = material.getDoubleProperty("BULK_MODULUS");
    double G3=3*G;
    double sigD,sigV,Hdd,Hdv,Hvv;
    viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,vPl0,vPl,
                                       sigD,sigV,Hdd,Hdv,Hvv,dTime,false,true);
    double Jdd,Jdv,Jvv;
    Jdd = G3+Hdd;
    Jdv = Hdv;
    Jvv = K+Hvv;
    double val = dEPlD*Jdd*dEPlD+2*dEPlD*Jdv*dEPlV+dEPlV*Jvv*dEPlV;
    double coef = 1.0e0/val;
    double dp;
    if (std::fabs(val) < THRSHLD && !std::isnan(coef)) {
      dp = test/val;
    }
    else {
      dp = test/(dEPlD*G3*dEPlD+dEPlV*K*dEPlV);
    }
    ePl += dp*dEPlD;
    vPl += dp*dEPlV;
 
    // plot
    /*unsigned int nx=100,ny=500;
    double xmin=0.0e0,xmax=1.e-4;
    double ymin=0,ymax=5.e-4;
    std::ofstream ofile("fct.plt",std::ofstream::out|std::ofstream::trunc);
    for (unsigned int i=0; i < nx; i++)
      for (unsigned int j=0; j < ny; j++) {
        double x = xmin+i*(xmax-xmin)/nx;
        double y = ymin+j*(ymax-ymin)/ny;
        double v = viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl0+x,
                                                      vPl0,vPl0+y,sigD,sigV,Hdd,Hdv,Hvv,dTime,
                                                      false,false);
        v += G3*x*x+K*y*y-sigEq0*x-sigVol0*y;
        ofile << x << "\t" << y << "\t" << v << std::endl;
      }
    ofile.close();*/

    // apply plastic corrector (Newton loop)
    double dEPl = ePl-ePl0,dVPl = vPl-vPl0;
    double sigEq  = sigEq0-G3*dEPl;
    double sigVol = sigVol0-K*dVPl;
    test = TOLE*(test+TOLE);
    unsigned int iter=0;
    for (; iter < maxIt; iter++) {
      viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,vPl0,vPl,
                                         sigD,sigV,Hdd,Hdv,Hvv,dTime,true,true);
      double gD = sigEq-sigD;
      double gV = sigVol-sigV;
      if (std::sqrt(gD*gD+gV*gV) < test) break;
      double Jdd,Jdv,Jvv;
      Jdd = G3+Hdd;
      Jdv = Hdv;
      Jvv = K+Hvv;
      double detJ = Jdd*Jvv-Jdv*Jdv;
      double coef = 1.0e0/detJ;
      if (std::fabs(detJ) < THRSHLD && !std::isnan(coef)) {
        dEPl = (Jvv*gD-Jdv*gV)*coef;
        dVPl = (Jdd*gV-Jdv*gD)*coef;
      }
      else {
        dEPl = gD/G3;
        dVPl = gV/K;
      }
      if ((ePl+dEPl) < (ePl0+PREC)) dEPl = -MULT*(ePl-ePl0);
      if (std::fabs(dEPl) < PREC && std::fabs(dVPl) < PREC) break;
      //if ((ePl+dEPl) < (ePl00+PREC)) { /* use secant method */
      //  //std::cout << "secant" << std::endl;
      //  double mult = fct/(fct00-fct);
      //  if (mult < -MULT) mult=-MULT;
      //  dEPl = mult*(ePl-ePl00);
      //}
      //if (std::fabs(dEPl) < PREC) break;
      sigEq -= dEPl*G3;
      sigVol -= dVPl*K;
      ePl += dEPl;
      vPl += dVPl;
      //if (fct > 0.0e0 && dEPl < 0.0e0) {
      //  fct00 = fct;
      //  ePl00 = ePl;
      //}
    }
    // check convergence
    if (iter == maxIt) {
      //std::cerr << "no convergence in radial return (after " << iter << " iterations)" << std::endl;
      throw UpdateFailedException("no convergence in radial return");
    }
    
    return iter;
  }
};


/*
 * Implementations of the model.
 */

/**
 * J2 dilatant plasticity with elliptic yield surface and linear isotropic hardening.
 */
class LinearIsotropicEllipticPlasticity3D : public J2DilatantPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearIsotropicEllipticPlasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    J2DilatantPlasticity<TensorAlgebra3D>(
            new EllipticViscoPlasticity(new LinearIsotropicHardeningModel(),
                                        new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicEllipticPlasticity3D(const LinearIsotropicEllipticPlasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src), ElastoPlasticity<TensorAlgebra3D>(src),
    J2DilatantPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicEllipticPlasticity3D() {}
};
class LinearIsotropicEllipticPlasticity2D : public J2DilatantPlasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearIsotropicEllipticPlasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    J2DilatantPlasticity<TensorAlgebra2D>(
            new EllipticViscoPlasticity(new LinearIsotropicHardeningModel(),
                                        new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicEllipticPlasticity2D(const LinearIsotropicEllipticPlasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src), ElastoPlasticity<TensorAlgebra2D>(src),
    J2DilatantPlasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicEllipticPlasticity2D() {}
};
class LinearIsotropicEllipticPlasticity1D : public J2DilatantPlasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  LinearIsotropicEllipticPlasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    J2DilatantPlasticity<TensorAlgebra1D>(
            new EllipticViscoPlasticity(new LinearIsotropicHardeningModel(),
                                        new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicEllipticPlasticity1D(const LinearIsotropicEllipticPlasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src), ElastoPlasticity<TensorAlgebra1D>(src),
    J2DilatantPlasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicEllipticPlasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicEllipticPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearIsotropicEllipticPlasticityBuilder();
  
  // the instance
  static LinearIsotropicEllipticPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearIsotropicEllipticPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
