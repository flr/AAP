// ADMB code for simple catch at age model with CAA matrix and three indices
// Plaice assessment

DATA_SECTION
  init_int    nyrs
  init_int    nages
  init_int    qplat_Fmatrix
  init_int    qplat_surveys
  init_int    minFbar
  init_int    maxFbar
 !! TRACE(maxFbar);
  init_int    F_age_knots
  init_int    F_time_knots
  init_int    pGrp
  !! TRACE(pGrp);
  init_number no_surveys
  init_vector time_surv(1,no_surveys)
  !! TRACE(time_surv);
  init_matrix obs_landings_at_age(1,nyrs,1,nages)
 !! TRACE(obs_landings_at_age);
  init_matrix obs_discards_at_age(1,nyrs,1,nages)
  init_matrix stock_weights(1,nyrs,1,nages)
  init_matrix M(1,nyrs,1,nages)
  init_3darray obs_surv(1,no_surveys,1,nyrs,1,nages)
  init_vector obs_landings(1,nyrs)
 !! TRACE(obs_landings);
  init_vector maturity(1,nages)
  init_matrix bs1(1,F_age_knots,1,qplat_surveys) 
  init_matrix X1(1,qplat_Fmatrix*nyrs,1,F_age_knots*F_time_knots)
  vector obs_landings_NA(1,nyrs);
  matrix obs_landings_at_age_NA(1,nyrs,1,nages);
  matrix obs_discards_at_age_NA(1,nyrs,1,nages);
  3darray obs_surv_NA(1,no_surveys,1,nyrs,1,nages);
  
PARAMETER_SECTION
  init_vector logsigmaC(1,3)
  init_matrix logsigmaU(1,no_surveys,1,3)
  init_vector log_sel_coff1(1,F_age_knots*F_time_knots,1)
  init_matrix log_sel_cofU(1,no_surveys,1,F_age_knots,1)
  init_vector log_initpop(1,nyrs+nages-1,1)
  vector sigmaC(1,nages)
  matrix sigmaU(1,no_surveys,1,nages)
  vector vect1(1,qplat_Fmatrix*nyrs)
  vector log_self1(1,nages)
  vector log_seltemp(1,nages)
  matrix log_selU(1,no_surveys,1,nages)
  vector TSB(1,nyrs)
  sdreport_vector SSBo(1,nyrs)
  vector VB(1,nyrs)
  matrix F(1,nyrs,1,nages)
  matrix S(1,nyrs,1,nages)
  matrix N(1,nyrs,1,nages)
  3darray U(1,no_surveys,1,nyrs,1,nages)
  matrix C(1,nyrs,1,nages)
  matrix L(1,nyrs,1,nages)
  matrix D(1,nyrs,1,nages)
  matrix obs_catches_at_age(1,nyrs,1,nages)
  number f_ca
  vector f_s(1,no_surveys)
  sdreport_vector Fbar(1,nyrs)
  objective_function_value f

PRELIMINARY_CALCS_SECTION

 for (int t=1; t<=nyrs; t++){
    for (int a=1; a<=nages; a++){
      obs_catches_at_age(t,a) = obs_discards_at_age(t,a) + obs_landings_at_age(t,a) ;
    }
  }


  //for all landings and discards create a copy, but fill with [0,1] for likelihood function
  for (int t=1; t<=nyrs; t++){
    obs_landings_NA(t) = (obs_landings(t) <0)?0:1;
    obs_landings(t)    = (obs_landings(t) <0)? -obs_landings(t) :obs_landings(t) ;    
    for (int a=1; a<=nages; a++){
      obs_landings_at_age_NA(t,a) = (obs_landings_at_age(t,a) <0)?0:1;
      obs_discards_at_age_NA(t,a) = (obs_discards_at_age(t,a) <0)?0:1;
    }
  }
  for (int s=1; s<=no_surveys; s++){
    for (int t=1; t<=nyrs; t++){
      for (int a=1; a<=nages; a++){
        obs_surv_NA(s,t,a) = (obs_surv(s,t,a) <0)?0:1;
        obs_surv(s,t,a)    = (obs_surv(s,t,a) <0)? -obs_surv(s,t,a) :obs_surv(s,t,a) ;    
      }
    }
  }
 
PROCEDURE_SECTION
  get_sigmas();
  get_mortality_and_survival_rates();
  get_numbers_at_age();
  get_catch_at_age();
  get_surveys_at_age();
  calculate_biomass();
  evaluate_the_objective_function();
  if (mceval_phase())
  {
    write_mcmc();
  }
 
REPORT_SECTION
  report << "Likelihoods f, f_la, f_da, f_s1, f_s2, f_s3" << endl;
  report << f  <<endl << f_ca  << endl << f_s  << endl;
  report << "log_self1"         << endl << log_self1 << endl;
  report << "log_selU"          << endl << log_selU << endl;
  report << "sigmaC"            << endl << sigmaC    << endl;
  report << "sigmaU"            << endl << sigmaU   << endl;
  report << "Estimated l@a"     << endl << L         << endl;
  report << "Estimated d@a"     << endl << D         << endl;
  report << "Estimated surveys" << endl << U         << endl;
  report << "Estimated N"       << endl << N         << endl;
  report << "Estimated F"       << endl << F         << endl;
  report << "Estimated Fbar (" << minFbar << "-" << maxFbar << ")" << endl << Fbar << endl ;
  report << "Estimated SSB from obs wts"     << endl << SSBo       << endl;
  report << "Estimated TSB"     << endl << TSB       << endl;

FUNCTION dvariable dnorm(const dvariable& x, const dvariable& mu, const dvariable& sd)
  return 0.5 * (log(2*M_PI*sd*sd) + square(x-mu)/(sd*sd));

FUNCTION get_sigmas
  for (int a=1; a<=nages; a++){
    // landings and discards sigma 
    sigmaC(a) = mfexp(logsigmaC(1) + logsigmaC(2) * a + logsigmaC(3) * a * a );
    // Survey sigma
    for (int s=1; s<=no_surveys; s++) 
      sigmaU(s,a) = mfexp(logsigmaU(s,1) + logsigmaU(s,2) * a + logsigmaU(s,3) * a * a );
  } 


FUNCTION get_mortality_and_survival_rates
  vect1 = log_sel_coff1 * trans(X1) ;
  
  int ii = 1; 
  for (int t=1; t<=nyrs; t++){
    for (int a=1; a<=qplat_Fmatrix; a++){
      F(t,a)= mfexp(vect1[ii]);
      ii++;
    }
  }
  for (int t=1; t<=nyrs; t++){
    for (int a=qplat_Fmatrix; a<=nages; a++){
      F(t,a)= F(t,qplat_Fmatrix);
    }
  }

  for (int t=1; t<=nyrs; t++)
    Fbar(t) = mean(row(F,t)(minFbar,maxFbar));
  S = mfexp(-(F+M));

FUNCTION get_numbers_at_age
  for (int a=2; a<=nages; a++)
    N(1,a) = mfexp(log_initpop(a-1));
 for (int t=1; t<=nyrs; t++)
    N(t,1) = mfexp(log_initpop(nages-1 +t));
  for (int t=1; t<nyrs; t++)
    for (int a=1; a<(nages-1); a++)
      N(t+1,a+1) = N(t,a) * S(t,a);
// plusgroup
  for (int t=1; t<nyrs; t++)
    N(t+1,nages) =  N(t,nages-1) * S(t,nages-1) + pGrp * (N(t,nages) * S(t,nages)) ;

FUNCTION get_catch_at_age
  C = elem_prod(elem_div(F,(F+M)), elem_prod(1-S,N));
  
  for (int t=1; t<=nyrs; t++){
    for (int a=1; a<=nages; a++){  
      L(t,a) = C(t,a)*( obs_landings_at_age(t,a)/( obs_landings_at_age(t,a) + obs_discards_at_age(t,a))) ; 
      D(t,a) = C(t,a)*( obs_discards_at_age(t,a)/( obs_landings_at_age(t,a) + obs_discards_at_age(t,a))) ;;
    }
  }

FUNCTION get_surveys_at_age
  
    for (int s=1; s<=no_surveys; s++){
      log_seltemp(1,qplat_surveys) =  elem_div(exp(log_sel_cofU(s)*bs1), 1+exp(log_sel_cofU(s)*bs1));
      for (int a=qplat_surveys+1; a<=nages; a++)
        log_seltemp(a) =  log_seltemp(qplat_surveys) ;
      for (int a=1; a<=nages; a++)
         log_selU(s,a) = log_seltemp(a);
      for (int t=1; t<=nyrs; t++){
        for (int a=1; a<=nages; a++){
            U(s,t,a) = log_selU(s,a) * N(t,a) * mfexp(-time_surv(s)*(F(t,a)+M(t,a)));
        }
     }
  }

FUNCTION  calculate_biomass

  SSBo = maturity * trans(elem_prod(N, stock_weights));
  TSB = rowsum(elem_prod(N, stock_weights));

FUNCTION evaluate_the_objective_function
  f_ca  = 0.0;
  for (int s=1; s<=no_surveys; s++)
    f_s(s) = 0.0;

 // Commercial catch at age
  for (int t=1; t<=nyrs; t++){
    for (int a=1; a<=nages; a++){
      f_ca +=  dnorm(log(C(t,a)),  log(obs_catches_at_age(t,a)), sigmaC(a));
      for (int s=1; s<=no_surveys; s++)
        f_s(s) += (obs_surv_NA(s,t,a)      * dnorm(log(U(s,t,a)), log(obs_surv(s,t,a)),          sigmaU(s,a))); // Survey 1
    }
  }
  // Add all components
  
  f = f_ca  + sum(f_s) ;

FUNCTION write_mcmc
  // Fbar
  mcmc_F << F << endl;
  // Recruitment
  mcmc_N << N << endl;

RUNTIME_SECTION
  maximum_function_evaluations 8000, 8000, 4000

GLOBALS_SECTION
  #include "admodel.h" 
  #define TRACE(object) tracefile << #object << "\n" << object << "\n\n" << endl;
  ofstream tracefile("data.log");
  ofstream mcmc_F("F.mcmc");
  ofstream mcmc_N("N.mcmc");

