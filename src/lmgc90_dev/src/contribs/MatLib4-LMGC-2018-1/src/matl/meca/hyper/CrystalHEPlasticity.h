/*
 *  $Id: CrystalHEPlasticity.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_CRYSTAL_PLASTICITY_H
#define ZORGLIB_MATL_MECA_HYPER_CRYSTAL_PLASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
#if defined(_WIN32) || defined(_WIN64)
#define IS_NAN(x) _isnan(x)
#else
#define IS_NAN(x) std::isnan(x)
#endif
// STL
#include <vector>
// local
#include <matl/meca/crystal/CrystalViscoPlasticity.h>
#include <matl/meca/crystal/SingleCrystal.h>
#include <matl/meca/hyper/HyperElastoPlasticity.h>
#include <opti/Unconstrained.h>
#include <opti/UnconstrainedActSet.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Finite-strain variational crystal plasticity.
 */
template <class ALG>
class CrystalHEPlasticity : virtual public HyperElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::Vector           VECTOR;
  
  // useful constants
  static const unsigned int MAX_SLIP_SYSTEMS = 48;
  
  // nested classes
  class IncrementalFunctional;
  
 protected:
    
  // crystallographic structure
  SingleCrystal *crystal;
  
  // viscoplasticity model
  CrystalViscoPlasticity *viscoPlasticity;
  
  // optimization problem and method
  OptiProblem *problem;
  OptiMethod *optimizer;

  // empty constructor
  CrystalHEPlasticity(SingleCrystal *c = 0,CrystalViscoPlasticity *vp = 0) {
    crystal = c;
    viscoPlasticity = vp;
    initialize();
  }
  
  // initialize optimization problem
  void initialize() {
    // objective function
    IncrementalFunctional func(*this);
    // minimization problem
    problem = new BasicOptiProblem(func);
    // constraints
    unsigned int nSys = crystal->nSystems();
    std::vector<unsigned int> idx(nSys);
    std::vector<double> val(nSys);
    for (unsigned int k=0; k < nSys; k++) {
      idx[k] = k; 
      val[k] = 0.0e0;
    }
    problem->setLowerBounds(nSys,idx,val);
    // minimization algorithm
    optimizer = new Unconstrained(*problem);
    //optimizer = new UnconstrainedActSet(*problem);
  }
  
 public:
    
  // constructors
  CrystalHEPlasticity(SingleCrystal& c,CrystalViscoPlasticity& vp,
                      typename HyperElasticity<ALG>::Potential& p)
  : HyperElasticity<ALG>(p) {
    crystal = &c;
    viscoPlasticity = &vp;
    initialize();
  }
  CrystalHEPlasticity(SingleCrystal& c,CrystalViscoPlasticity& vp,
                      typename HyperElasticity<ALG>::Potential& p,EOS& e)
  : HyperElasticity<ALG>(p,e) {
    crystal = &c;
    viscoPlasticity = &vp;
    initialize();
  }

  // copy constructor
  CrystalHEPlasticity(const CrystalHEPlasticity& src)
  : HyperElasticity<ALG>(src),HyperElastoPlasticity<ALG>(src) {
    crystal = src.crystal;
    viscoPlasticity = src.viscoPlasticity;
    initialize();
  }
  
  // destructor
  virtual ~CrystalHEPlasticity() {
    if (*(this->count) > 1) return;
    if (crystal) delete crystal;
    if (viscoPlasticity) delete viscoPlasticity;
    if (problem) delete problem;
    if (optimizer) delete optimizer;
  }
  
  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nCrystal plasticity model (hyperelastic base):" << std::endl;
      
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

    // equation-of-state
    if (this->eos) this->eos->checkProperties(material,os);

    // dilatancy model
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);

    // crystallographic structure
    crystal->checkProperties(material,os);

    // viscoplastic model
    viscoPlasticity->checkProperties(material,os);

    // algorithmic parameter
    const double DEFAULT_MAX_SLIP_INCR = 1.0e0;
    double maxSlipIncr;
    try {
      maxSlipIncr = material.getDoubleProperty("MAX_SLIP_INCREMENT");
    }
    catch (NoSuchPropertyException) {
      maxSlipIncr = DEFAULT_MAX_SLIP_INCR;
      material.setProperty("MAX_SLIP_INCREMENT",maxSlipIncr);
    }
    if (os) {
      (*os) << "\n\t***Algorithmic parameters***\n";
      (*os) << "\tmax. slip increment = " << maxSlipIncr << std::endl;
    }
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& mater,const Rotation& R) {
    HyperElasticity<ALG>::rotateProperties(mater,R);
    crystal->rotateProperties(mater,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    HyperElasticity<ALG>::updateProperties(mater,extPar);
    crystal->updateProperties(mater,extPar);
    viscoPlasticity->updateProperties(mater,extPar);
  }
  
  // number of internal variables
  unsigned int nIntVar() const {
    return TENSOR::MEMSIZE+crystal->nSystems()+1+viscoPlasticity->nIntPar();
  }
  
  // self-documenting utilities
  virtual unsigned int nIntVarBundled() const {return 3+crystal->nSystems();}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "PSTN")
      return 0;
    else if (str == "PLS01")
      return 1;
    else if (str == "PLS02")
      return 2;
    else if (str == "PLS03")
      return 3;
    else if (str == "PLS04")
      return 4;
    else if (str == "PLS05")
      return 5;
    else if (str == "PLS06")
      return 6;
    else if (str == "PLS07")
      return 7;
    else if (str == "PLS08")
      return 8;
    else if (str == "PLS09")
      return 9;
    else if (str == "PLS10")
      return 10;
    else if (str == "PLS11")
      return 11;
    else if (str == "PLS12")
      return 12;
    else if (str == "PLS13")
      return 13;
    else if (str == "PLS14")
      return 14;
    else if (str == "PLS15")
      return 15;
    else if (str == "PLS16")
      return 16;
    else if (str == "PLS17")
      return 17;
    else if (str == "PLS18")
      return 18;
    else if (str == "PLS19")
      return 19;
    else if (str == "PLS20")
      return 20;
    else if (str == "PLS21")
      return 21;
    else if (str == "PLS22")
      return 22;
    else if (str == "PLS23")
      return 23;
    else if (str == "PLS24")
      return 24;
    else if (str == "PLS25")
      return 25;
    else if (str == "PLS26")
      return 26;
    else if (str == "PLS27")
      return 27;
    else if (str == "PLS28")
      return 28;
    else if (str == "PLS29")
      return 29;
    else if (str == "PLS30")
      return 30;
    else if (str == "PLS31")
      return 31;
    else if (str == "PLS32")
      return 32;
    else if (str == "PLS33")
      return 33;
    else if (str == "PLS34")
      return 34;
    else if (str == "PLS35")
      return 35;
    else if (str == "PLS36")
      return 36;
    else if (str == "PLS37")
      return 37;
    else if (str == "PLS38")
      return 38;
    else if (str == "PLS39")
      return 39;
    else if (str == "PLS40")
      return 40;
    else if (str == "PLS41")
      return 41;
    else if (str == "PLS42")
      return 42;
    else if (str == "PLS43")
      return 43;
    else if (str == "PLS44")
      return 44;
    else if (str == "PLS45")
      return 45;
    else if (str == "PLS46")
      return 46;
    else if (str == "PLS47")
      return 47;
    else if (str == "PLS48")
      return 48;
    else if (str == "ENRG")
      return crystal->nSystems()+1;
    else if (str == "PNRG")
      return crystal->nSystems()+2;
    else
      return crystal->nSystems()+3;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    if (i == 0)
      return ConstitutiveModel::TYPE_TENSOR;
    else if (i <= crystal->nSystems())
      return ConstitutiveModel::TYPE_SCALAR;
    else if (i == crystal->nSystems()+1)
      return ConstitutiveModel::TYPE_SCALAR;
    else if (i == crystal->nSystems()+2)
      return ConstitutiveModel::TYPE_SCALAR;
    else
      return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int i) const {
    if (i == 0)
      return 0;
    else if (i <= crystal->nSystems())
      return TENSOR::MEMSIZE+i-1;
    else if (i == crystal->nSystems()+1)
      return TENSOR::MEMSIZE+crystal->nSystems();
    else if (i == crystal->nSystems()+2)
      return TENSOR::MEMSIZE+crystal->nSystems()+1;
    else
      return TENSOR::MEMSIZE+crystal->nSystems()+2;
  }
  std::string labelIntVar(unsigned int i) const {
    if (i == 0)
      return "plastic strain";
    else if (i <= crystal->nSystems())
      return "slip strain (" + crystal->labelSystem(i-1) + ")";
    else if (i == crystal->nSystems()+1)
      return "elastically stored energy";
    else if (i == crystal->nSystems()+2)
      return "plastically stored energy";
    else
      return "";
  }
  
  // compute the plastic update
  double plasticUpdate(const MaterialProperties& material,const ParameterSet& extPar,
                       const SYM_TENSOR& C,SYM_TENSOR& S,const TENSOR& Fp0,TENSOR& Fp,
                       const MatLibArray& intV0,MatLibArray& intV,double dTime,
                       SYM_TENSOR4& M,bool update,bool computeTangent)
   throw (UpdateFailedException) {

    static const double PRECISION = 1.0e-16;

    // extract plastic slips
    unsigned int nSys = crystal->nSystems();
    const MatLibArray gam0(intV0,nSys);
    MatLibArray gam1(intV,nSys);
    
    // extract internal parameters
    unsigned int nIntPar = intV.size()-nSys-1;
    const MatLibArray intPar0(intV0,nIntPar,nSys+1);
    MatLibArray intPar(intV,nIntPar,nSys+1);
      
    // update
    MatLibArray dGam(nSys);
    viscoPlasticity->initialize = true;
    viscoPlasticity->finalize = false;
    if (update) {
      
      // get slip systems
      SlipSystemsProperty<VECTOR>& systems
        = dynamic_cast<SlipSystemsProperty<VECTOR>&>(material.getProperty("SLIP_SYSTEMS"));
      
      // compute elementary slip system contributions
      TENSOR Mp[MAX_SLIP_SYSTEMS];
      for (unsigned int k=0; k < nSys; k++) {
        VECTOR& s = systems.system(k).first;
        VECTOR& m = systems.system(k).second;
        for (unsigned int i=0; i < VECTOR::MEMSIZE; i++)
          for (unsigned int j=0; j < VECTOR::MEMSIZE; j++)
            Mp[k][TENSOR::MAP[i][j]] = s[i]*m[j];
        for (unsigned int i=VECTOR::MEMSIZE; i < 3; i++)
          Mp[k][TENSOR::MAP[i][i]] = s[i]*m[i];
      }
      
      // initialize the functional
      IncrementalFunctional& func 
        = dynamic_cast<IncrementalFunctional&>(problem->objectiveFunction());
      func.init(material,extPar,C,Fp0,Mp,intPar0,intPar,
                gam0,gam1,intV0[nSys],dTime);

      // impose a max. slip increment (for robustness)
      double maxSlipIncr = material.getDoubleProperty("MAX_SLIP_INCREMENT");
      std::vector<unsigned int> idx(nSys);
      for (unsigned int k=0; k < nSys; k++) idx[k] = k;
      std::vector<double> uppB(nSys,maxSlipIncr);
      problem->setUpperBounds(nSys,idx,uppB);
      
      // minimize the function
      dGam = 0.0e0;
#ifdef WITH_OPTI_DEBUG
      optimizer->setDebugLevel(OptiMethod::DBG_TOP);
#endif
      try {
        optimizer->minimum(dGam);
      }
      catch (OptimizationException e) {
        std::string msg("no convergence in ");
        msg.append(e.mesg());
        throw UpdateFailedException(msg);
      }
      
      // update internal variables
      for (unsigned int k=0; k < nSys; k++)
        if (dGam[k] < PRECISION) dGam[k] = 0.0e0;
      gam1 = gam0 + dGam;
      
      // update plastic strain
      TENSOR Fp01;
      Fp01 = 0.0e0;
      for (unsigned int k=0; k < nSys; k++) Fp01 += dGam[k]*Mp[k];
      Fp = exp(Fp01)*Fp0;
      
      viscoPlasticity->finalize = true;
    }
    
    // elastic deformation
    SYM_TENSOR Ce;
    Ce = covariantPush(C,Fp);
    
    // elastic free energy
    SYM_TENSOR Sbar;
    SYM_TENSOR4 Mbar;
    double We = this->storedEnergy(material,extPar,Ce,Sbar,Mbar,
                                   update || computeTangent,computeTangent);
    if (update) intV[nSys] = We;
    
    // plastic free energy + dissipated energy
    MatLibArray dummy;
    MatLibMatrix Hp(nSys);
    double Wp = viscoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,
                                                    gam0,gam1,dummy,Hp,dTime,
                                                    false,computeTangent);
    
    // stresses
    if (update) S = contravariantPull(Sbar,Fp);
    
    // tangents
    if (computeTangent) {
     
      // determine set of active slip systems
      dGam = gam1-gam0;
      std::vector<unsigned int> list;
      list.clear();
      for (unsigned int k=0; k < nSys; k++)
        if (dGam[k] > PRECISION) list.push_back(k);
      
      // plastic part of tangents
      if (list.size() > 0) {
        
        // get slip systems
        SlipSystemsProperty<VECTOR>& systems
          = dynamic_cast<SlipSystemsProperty<VECTOR>&>(material.getProperty("SLIP_SYSTEMS"));
        
        // compute plastic strain increment and its derivatives
        TENSOR Mp[MAX_SLIP_SYSTEMS],Dp;
        Dp = 0.0e0;
        for (unsigned int k=0; k < list.size(); k++) {
          VECTOR& s = systems.system(list[k]).first;
          VECTOR& m = systems.system(list[k]).second;
          for (unsigned int i=0; i < VECTOR::MEMSIZE; i++)
            for (unsigned int j=0; j < VECTOR::MEMSIZE; j++)
              Mp[k][TENSOR::MAP[i][j]] = s[i]*m[j];
          for (unsigned int i=VECTOR::MEMSIZE; i < 3; i++)
            Mp[k][TENSOR::MAP[i][i]] = s[i]*m[i];
          Dp += dGam[list[k]]*Mp[k];
        }
        TENSOR dFp01[TENSOR::MEMSIZE],d2Fp01[TENSOR::MEMSIZE][TENSOR::MEMSIZE];
        TENSOR Fp01 = exp(Dp,dFp01,d2Fp01,true,true);
        TENSOR Fp01inv = invert(Fp01);
        
        // compute intermediate quantities
        TENSOR r[MAX_SLIP_SYSTEMS],s[MAX_SLIP_SYSTEMS],tmp;
        SYM_TENSOR dS[MAX_SLIP_SYSTEMS];
        for (unsigned int k=0; k < list.size(); k++) {
          tmp = 0.0e0;
          for (unsigned int ij=0; ij < TENSOR::MEMSIZE; ij++)
            tmp += Mp[k][ij]*dFp01[ij];
          tmp = tmp*Fp01inv;
          r[k] = Ce*tmp;
          s[k] = tmp*Sbar;
          dS[k] = Mbar*covariantSym(r[k]);
        }
        
        // compute the Hessian of the incremental energy function
        // elastic part (plastic part already in)
        MatLibMatrix HpEff(list.size());
        for (unsigned int k=0; k < list.size(); k++)
          for (unsigned int l=0; l < list.size(); l++) {
            TENSOR *p = *d2Fp01;
            tmp = 0.0e0;
            for (unsigned int ij=0; ij < TENSOR::MEMSIZE; ij++)
              for (unsigned int mn=0; mn < TENSOR::MEMSIZE; mn++, p++)
                tmp += (Mp[k][ij]*Mp[l][mn])*(*p);
            tmp = Ce*tmp*Fp01inv;
            HpEff[k][l] = Hp[list[k]][list[l]]
                         +innerProd(covariantSym(r[k]),dS[l])
                         +innerProd(r[k].transposed(),s[l])
                         +innerProd(s[k],r[l].transposed())
                         +0.5*(innerProd(r[k],s[l])+innerProd(s[k],r[l]))
                         -innerProd(covariantSym(tmp),Sbar);
          }
        // ...and invert it
        try {
          HpEff.invert();
        }
        catch (SingularMatrixException) {
          std::cout << "singular matrix in plasticUpdate()" << std::endl;
          throw UpdateFailedException("singular matrix in plasticUpdate()");
        }

        // compute tangent
        for (unsigned int k=0; k < list.size(); k++) dS[k] += 2*contravariantSym(s[k]);
        for (unsigned int k=0; k < list.size(); k++)
          for (unsigned int l=0; l < list.size(); l++)
            Mbar -= HpEff[k][l]*outerProd(dS[k],dS[l]);
      }
      
      M = contravariantPull(Mbar,Fp);
    }
    
    return We-intV0[nSys]+Wp;
  }
};


/**
 * Incremental functional for crystal plasticity.
 */
template <class ALG>
class CrystalHEPlasticity<ALG>::IncrementalFunctional : public OptiFunction {
  
 protected:
  
  // associated material model
  CrystalHEPlasticity<ALG> *material;
  
  // associated material data
  const MaterialProperties *matProp;
  
  // associated external parameters
  const ParameterSet *extPar;

  // initial conditions
  const SYM_TENSOR *C;
  const TENSOR *Fp0;
  const TENSOR *Mp;
  const MatLibArray *intPar0,*intVar0;
  MatLibArray *intPar,*intVar;
  double W0e,dTime;
  
 public:
    
  // constructor
  IncrementalFunctional(CrystalHEPlasticity<ALG>& matl) {material = &matl;}
  
  // copy constructor
  IncrementalFunctional(const IncrementalFunctional& src) {
    material = src.material;
    matProp = src.matProp;
    extPar = src.extPar;
    C = src.C;
    Fp0 = src.Fp0;
    Mp = src.Mp;
    intPar0 = src.intPar0;
    intPar = src.intPar;
    intVar0 = src.intVar0;
    intVar = src.intVar;
    W0e = src.W0e;
    dTime = src.dTime;
  }
  
  // destructor
  virtual ~IncrementalFunctional() {}
  
  // clone operation
  virtual IncrementalFunctional* clone() const {
    return new IncrementalFunctional(*this);
  }
  
  // function of nSys variables
  unsigned int dimension() const {return material->crystal->nSystems();}
  
  // initialization
  void init(const MaterialProperties& matP,const ParameterSet& extP,
            const SYM_TENSOR& c,const TENSOR& fP,const TENSOR mP[],
            const MatLibArray& iP0,MatLibArray& iP,
            const MatLibArray& iV0,MatLibArray& iV,
            double w0,double dT) {
    matProp = &matP;
    extPar = &extP;
    C = &c;
    Fp0 = &fP;
    Mp = mP;
    intPar0 = &iP0;
    intPar = &iP;
    intVar0 = &iV0;
    intVar = &iV;
    W0e = w0;
    dTime = dT;
  }
  
  // value
  double value(const ShortArray& dGam,
               MatLibArray *tau = 0,bool computeFirst = false,
               MatLibMatrix *H = 0,bool computeSecond = false) const {

    // update slip strains
    (*intVar) = (*intVar0)+dGam;

    // elastic deformation
    unsigned int nSys = material->crystal->nSystems();
    TENSOR Dp;
    Dp = 0.0e0;
    for (unsigned int k=0; k < nSys; k++) Dp += dGam[k]*Mp[k];
    if (IS_NAN(normL1(Dp))) {
      std::cout << "excessive plastic strain increment 1" << std::endl;
      throw OptimizationException("CrystalHEPlasticityIncrementalFunctional::value(): excessive plastic strain increment");
    }
    TENSOR Fp01,Fp;
    TENSOR dFp01[TENSOR::MEMSIZE],d2Fp01[TENSOR::MEMSIZE][TENSOR::MEMSIZE];
    try {
      Fp01 = exp(Dp,dFp01,d2Fp01,
                 computeFirst || computeSecond,computeSecond);
    }
    catch (ZException e) {
      std::string msg("CrystalHEPlasticityIncrementalFunctional::value(): ");
      msg.append(e.mesg());
      std::cout << "exception in exponential mapping" << std::endl;
      throw OptimizationException(msg);
    }
    Fp = Fp01*(*Fp0);
    SYM_TENSOR Ce;
    Ce = covariantPush(*C,Fp);

    // elastic free energy
    SYM_TENSOR S;
    SYM_TENSOR4 M;
    if (IS_NAN(normL1(Ce))) { // in case we didn't catch it above
      std::cout << "excessive plastic strain increment 2" << std::endl;
      std::cout << dGam << std::endl;
      throw OptimizationException("CrystalHEPlasticityIncrementalFunctional::value(): excessive plastic strain increment");
    }
    double We = material->storedEnergy(*matProp,*extPar,Ce,S,M,
                                       computeFirst || computeSecond,
                                       computeSecond);

    // plastic free energy
    double Wp = material->viscoPlasticity->irreversibleEnergy(*matProp,*extPar,*intPar0,*intPar,
                                                              *intVar0,*intVar,*tau,*H,dTime,
                                                              computeFirst,computeSecond);

    // compute intermediate quantities
    TENSOR Fp01inv,r[MAX_SLIP_SYSTEMS],s[MAX_SLIP_SYSTEMS],tmp;
    if (computeFirst || computeSecond) {
      Fp01inv = invert(Fp01);
      for (unsigned int k=0; k < nSys; k++) {
        tmp = 0.0e0;
        for (unsigned int ij=0; ij < TENSOR::MEMSIZE; ij++)
          tmp += Mp[k][ij]*dFp01[ij];
        tmp = tmp*Fp01inv;
        r[k] = Ce*tmp;
        if (computeSecond) s[k] = tmp*S;
      }
    }

    // first derivatives
    if (computeFirst) {
      for (unsigned int k=0; k < nSys; k++)
        (*tau)[k] -= innerProd(S,covariantSym(r[k]));
    }

    // second derivatives
    if (computeSecond) {
      for (unsigned int k=0; k < nSys; k++)
        for (unsigned int l=0; l < nSys; l++) {
          TENSOR *p = *d2Fp01;
          tmp = 0.0e0;
          for (unsigned int ij=0; ij < TENSOR::MEMSIZE; ij++)
            for (unsigned int mn=0; mn < TENSOR::MEMSIZE; mn++)
              tmp += Mp[k][ij]*Mp[l][mn]*(*p);
          tmp = Ce*tmp*Fp01inv;
          (*H)[k][l] += innerProd(covariantSym(r[k]),M*covariantSym(r[l]))
                       +innerProd(r[k].transposed(),s[l])
                       +innerProd(s[k],r[l].transposed())
                       +0.5*(innerProd(r[k],s[l])+innerProd(s[k],r[l]))
                       -innerProd(covariantSym(tmp),S);
        }
    }

    return We-W0e+Wp;
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
