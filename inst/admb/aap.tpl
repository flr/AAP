// ADMB code for simple catch at age model with CAA matrix and three indices
// Sole assessment

DATA_SECTION
  init_int    nyrs
  init_int    nages
  init_int    qplat_Fmatrix
  init_int    qplat_surveys
  init_int    minFbar
  init_int    maxFbar
  init_int    S_age_knots
  init_int    F_age_knots
  init_int    F_time_knots
  init_int    W_time_knots
  init_int    pGrp
  !! TRACE(pGrp);
  init_number no_surveys
  init_vector time_surv(1,no_surveys)
  !! TRACE(time_surv);
  init_matrix obs_landings_at_age(1,nyrs,1,nages)
 !! TRACE(obs_landings_at_age);
  init_matrix obs_discards_at_age(1,nyrs,1,nages)
  init_matrix landings_weights(1,nyrs,1,nages)
  init_matrix discards_weights(1,nyrs,1,nages)
  init_matrix stock_weights(1,nyrs,1,nages)
 !! TRACE(stock_weights);
  init_3darray obs_surv(1,no_surveys,1,nyrs,1,nages)
  init_vector obs_landings(1,nyrs)
 !! TRACE(obs_landings);
  init_number M
  init_vector maturity(1,nages)
  init_matrix bs1(1,S_age_knots,1,qplat_surveys) 
  !! TRACE(bs1);
  init_matrix X1(1,qplat_Fmatrix*nyrs,1,F_age_knots*F_time_knots)
  init_matrix bs3(1,W_time_knots,1,nyrs)
  !! TRACE(bs3); 
  vector Fvec(1,11806);
  number recTAC;                       // recruitment at start of tac year
  number ssbTACyr;                     // SSB at start of TAC year
  number intC;                         // intermediate year catches
  vector TAC(1,11806);
  vector SSBstf(1,11806);
  vector obs_landings_NA(1,nyrs);
  matrix obs_landings_at_age_NA(1,nyrs,1,nages);
  matrix obs_discards_at_age_NA(1,nyrs,1,nages);
  3darray obs_surv_NA(1,no_surveys,1,nyrs,1,nages);
  matrix landings_weights_NA(1,nyrs,1,nages);
  matrix discards_weights_NA(1,nyrs,1,nages);
  matrix stock_weights_NA(1,nyrs,1,nages);
  
PARAMETER_SECTION
  init_vector logsigmaL(1,3)
  init_vector logsigmaD(1,3)
  init_matrix logsigmaU(1,no_surveys,1,3)
  init_vector logsigmaLWTS(1,3) 
  // init_vector logsigmaDWTS(1,3) 
  init_vector logsigmaSWTS(1,3)
  init_bounded_number loga0(0.1,0.9,1)
  init_number logSWfact(1)
  // init_number logDWfact(1)
  init_vector log_sel_coff1(1,F_age_knots*F_time_knots,1)
  init_matrix log_sel_cofU(1,no_surveys,1,S_age_knots,1)
  init_vector disc_curve(1,2,1)
  init_bounded_number logK(-2,3,1)
  init_vector log_temp_wts_Linf(1,W_time_knots,2)
  init_vector log_initpop(1,nyrs+nages-1,1)
  vector sigmaL(1,nages)
  vector sigmaD(1,nages)
  matrix sigmaU(1,no_surveys,1,nages)
  vector sigmaLWTS(1,nages) 
  // vector sigmaDWTS(1,nages) 
  vector sigmaSWTS(1,nages)
  vector vect1(1,qplat_Fmatrix*nyrs)
  vector Linf(1,nyrs)
  vector log_self1(1,nages)
  vector log_seltemp(1,nages)
  matrix log_selU(1,no_surveys,1,nages)
  vector dfact(1,nages)
  vector TSB(1,nyrs)
  sdreport_vector SSBe(1,nyrs)
  sdreport_vector SSBo(1,nyrs)
  vector VB(1,nyrs)
  matrix F(1,nyrs,1,nages)
  matrix S(1,nyrs,1,nages)
  matrix N(1,nyrs,1,nages)
  3darray U(1,no_surveys,1,nyrs,1,nages)
  matrix C(1,nyrs,1,nages)
  matrix L(1,nyrs,1,nages)
  matrix D(1,nyrs,1,nages)
  matrix LWT(1,nyrs,1,nages)
  //  matrix DWT(1,nyrs,1,nages)
  matrix SWT(1,nyrs,1,nages)
  number f_la
  number f_da
  vector f_s(1,no_surveys)
  number f_lw
  // number f_dw
  number f_sw
  sdreport_vector Fbar(1,nyrs)
  number Fmax
  number YPRmax
  //number TAC
  objective_function_value f

INITIALIZATION_SECTION
  loga0 0.498176
  logK 2.04

PRELIMINARY_CALCS_SECTION
  // Create a sequence of Fbar values to evaluate YPR, in order to calculate Fmax:
  // Fvec <- c(0,10^-(9:5),seq(0.0001,1,0.0001),seq(1.001,2,0.001),seq(2.01,10,0.01))
  Fvec(1) = 0;
  Fvec(2) = 1e-9;
  for (int i=3; i<=7; i++)         Fvec(i) = Fvec(i-1) * 10;
  for (int i=8; i<=10006; i++)     Fvec(i) = Fvec(i-1) + 0.0001;
  for (int i=10007; i<=11006; i++) Fvec(i) = Fvec(i-1) + 0.001;
  for (int i=11007; i<=11806; i++) Fvec(i) = Fvec(i-1) + 0.01;

  //for all landings and discards create a copy, but fill with [0,1] for likelihood function
  for (int t=1; t<=nyrs; t++){
    obs_landings_NA(t) = (obs_landings(t) <0)?0:1;
    obs_landings(t)    = (obs_landings(t) <0)? -obs_landings(t) :obs_landings(t) ;    
    for (int a=1; a<=nages; a++){
      obs_landings_at_age_NA(t,a) = (obs_landings_at_age(t,a) <0)?0:1;
      obs_landings_at_age(t,a)    = (obs_landings_at_age(t,a) <0)? -obs_landings_at_age(t,a) :obs_landings_at_age(t,a) ;    
      obs_discards_at_age_NA(t,a) = (obs_discards_at_age(t,a) <0)?0:1;
      obs_discards_at_age(t,a)    = (obs_discards_at_age(t,a) <0)? -obs_discards_at_age(t,a) :obs_discards_at_age(t,a) ;    
   
      landings_weights_NA(t,a) = (landings_weights(t,a) <0)?0:1;
      landings_weights(t,a)    = (landings_weights(t,a) <0)? -landings_weights(t,a) :landings_weights(t,a) ;    
      discards_weights_NA(t,a) = (discards_weights(t,a) <0)?0:1;
      discards_weights(t,a)    = (discards_weights(t,a) <0)? -discards_weights(t,a) :discards_weights(t,a) ;    
      stock_weights_NA(t,a)    = (stock_weights(t,a) <0)?0:1;
      stock_weights(t,a)       = (stock_weights(t,a) <0)? -stock_weights(t,a) :stock_weights(t,a) ;    
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
  get_est_wts();
  calculate_biomass();
  evaluate_the_objective_function();
    get_fmax();
  if (mceval_phase())
  {
    //get_tac();
    write_mcmc();
  }
 
REPORT_SECTION
  report << "Likelihoods f, f_la, f_da, f_s1, f_s2, f_s3, f_lw, f_sw" << endl;
  report << f  <<endl << f_la << endl <<  f_da << endl << f_s  << endl << f_lw  << endl << f_sw << endl;
  report << "log_self1"         << endl << log_self1 << endl;
  report << "log_selU"          << endl << log_selU << endl;
  report << "sigmaL"            << endl << sigmaL    << endl;
  report << "sigmaD"            << endl << sigmaD    << endl;
  report << "sigmaU"            << endl << sigmaU   << endl;
  report << "Estimated l@a"     << endl << L         << endl;
  report << "Estimated d@a"     << endl << D         << endl;
  report << "Estimated surveys" << endl << U         << endl;
  report << "Estimated N"       << endl << N         << endl;
  report << "Estimated F"       << endl << F         << endl;
  report << "Estimated Fbar (" << minFbar << "-" << maxFbar << ")" << endl << Fbar << endl ;
  report << "Estimated SSB from est wts"     << endl << SSBe       << endl;
  report << "Estimated SSB from obs wts"     << endl << SSBo       << endl;
  report << "Estimated TSB"     << endl << TSB       << endl;
  report << "Estimated LWT"     << endl << LWT       << endl;
  // report << "Estimated DWT"     << endl << DWT       << endl;
  report << "Estimated SWT"     << endl << SWT       << endl;
  report << "loga0"             << endl << loga0     << endl; 
  report << "K"                 << endl << mfexp(-logK)  << endl;
  report << "Linf"              << endl << mfexp(Linf) << endl;
  report << "STF" << endl;
  report << "Int_yr_rec Int_yr_ssb Int_yr_landings" << endl;
  report << recTAC << " " << ssbTACyr << " " << intC << endl;
  report << "Fvec TAC resultant_ssb" << endl;
  report << Fvec << endl;
  report << TAC << endl;
  report << SSBstf << endl;
  report << "value lwt nyrs " << value(row(LWT,nyrs)) << endl;
  report <<  Fmax <<  " " << YPRmax << endl;

FUNCTION dvariable dnorm(const dvariable& x, const dvariable& mu, const dvariable& sd)
  return 0.5 * (log(2*M_PI*sd*sd) + square(x-mu)/(sd*sd));

FUNCTION get_sigmas
  for (int a=1; a<=nages; a++){
    // landings and discards sigma 
    sigmaL(a) = mfexp(logsigmaL(1) + logsigmaL(2) * a + logsigmaL(3) * a * a );
    sigmaD(a) = mfexp(logsigmaD(1) + logsigmaD(2) * a + logsigmaD(3) * a * a );
    // Survey sigma
    for (int s=1; s<=no_surveys; s++) 
      sigmaU(s,a) = mfexp(logsigmaU(s,1) + logsigmaU(s,2) * a + logsigmaU(s,3) * a * a );
    //wts sigmas
    sigmaLWTS(a) = mfexp(logsigmaLWTS(1) + logsigmaLWTS(2) * a + logsigmaLWTS(3) * a * a );
   // sigmaDWTS(a) = mfexp(logsigmaDWTS(1) + logsigmaDWTS(2) * a + logsigmaDWTS(3) * a * a );
    sigmaSWTS(a) = mfexp(logsigmaSWTS(1) + logsigmaSWTS(2) * a + logsigmaSWTS(3) * a * a );
  } 

FUNCTION get_est_wts
   Linf = mfexp(log_temp_wts_Linf) * bs3;
  for (int t=1; t<=nyrs; t++){
    for (int a=1; a<=nages; a++){
      LWT(t,a) = 0.00762 *  pow(mfexp(Linf(t))*(1-exp(-mfexp(-logK)*(a + mfexp(loga0)))), 3.068 ) ; //alpha and beta come from 1986 cefas report on literature   
      SWT(t,a) = LWT(t,a) * mfexp(logSWfact);
      // DWT(t,a) = LWT(t,a) * mfexp(logDWfact);
    }
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
  
  for (int a=1; a<=nages; a++)  
    dfact(a) =  1/ (1 + mfexp( disc_curve(1) * (a + disc_curve(2)))); 
  
  for (int t=1; t<=nyrs; t++){
    for (int a=1; a<=nages; a++){  
      L(t,a) = (1-dfact(a)) * C(t,a); 
      D(t,a) = dfact(a) * C(t,a);
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
            U(s,t,a) = log_selU(s,a) * N(t,a) * mfexp(-time_surv(s)*(F(t,a)+M));
        }
     }
  }

FUNCTION  calculate_biomass
  SSBe = maturity * trans(elem_prod(N, SWT));
  SSBo = maturity * trans(elem_prod(N, stock_weights));
  TSB = rowsum(elem_prod(N, SWT));

FUNCTION evaluate_the_objective_function
  f_la  = 0.0;
  f_da  = 0.0;
  for (int s=1; s<=no_surveys; s++)
    f_s(s) = 0.0;
  f_lw  = 0.0;
  // f_dw  = 0.0;
  f_sw  = 0.0;
 // Commercial catch at age
  for (int t=1; t<=nyrs; t++){
    for (int a=1; a<=nages; a++){
      f_la += (obs_landings_at_age_NA(t,a) * dnorm(log(L(t,a)),  log(obs_landings_at_age(t,a)), sigmaL(a)));
      f_da += (obs_discards_at_age_NA(t,a) * dnorm(log(D(t,a)),  log(obs_discards_at_age(t,a)), sigmaD(a)));
      for (int s=1; s<=no_surveys; s++)
        f_s(s) += (obs_surv_NA(s,t,a)      * dnorm(log(U(s,t,a)), log(obs_surv(s,t,a)),          sigmaU(s,a))); // Survey 1
      f_lw += (landings_weights_NA(t,a)    * dnorm(LWT(t,a),     landings_weights(t,a),         sigmaLWTS(a)));  // landing wts
      // f_dw += (discards_weights_NA(t,a)    * dnorm(DWT(t,a),     discards_weights(t,a),         sigmaDWTS(a)));  // landing wts
      f_sw += (stock_weights_NA(t,a)       * dnorm(SWT(t,a),     stock_weights(t,a),            sigmaSWTS(a)));   // stockwts
    }
  }
  // Add all components
  
  f = f_la + f_da + sum(f_s) + f_lw  + f_sw;


FUNCTION get_fmax
  int i = 0;                            // element in Fvec being evaluated
  bool found = false;                   // whether Fmax is found
  dvector sel = value(log_self1);       // selectivity to use, not necessarily between 0 and 1
  dvector f(1,nages);                   // F at age when Fvec(i) is applied
  dvector z(1,nages);                   // Z = F+M
  dvector n(1,nages);                   // equilibrium population
  dvector c(1,nages);                   // equilibrium catches
  dvector lw = value(row(LWT,nyrs));    // catch weights to use
  double ypr = -1;                      // highest YPR found so far
  double proposal;                      // YPR being evaluated
  n(1) = 1;
  while (!found)
  {
    i++;
    f = Fvec(i) * sel;
    z = f + M;
    for (int a=2; a<=nages; a++)
      n(a) = n(a-1) * exp(-(f(a-1)+M));
    for (int a=1; a<=nages; a++)
      c(a) = f(a)/z(a) * n(a) * (1-exp(-z(a)));
    proposal = sum(elem_prod(c, lw));
    if (proposal > ypr)
    {
      ypr = proposal;
    }
    else
    {
      i--;  // move i back to optimum
      found = true;
    }
  }
  Fmax = mean(f(minFbar,maxFbar));
  YPRmax = ypr;

FUNCTION write_mcmc
  // Fbar
  mcmc_F << F << endl;
  // Recruitment
  mcmc_N << N << endl;
  // Reference points
  mcmc_ref << Fmax << endl;
  mcmc_ypr << YPRmax << endl;
  for (int a=1; a<=nages; a++){
    mcmc_lwtfinalyr << LWT(nyrs,a) << " ";
  }
  mcmc_lwtfinalyr << endl;

  mcmc_swt << SWT << endl ;

RUNTIME_SECTION
  maximum_function_evaluations 2000, 2000, 2000

GLOBALS_SECTION
  #include "admodel.h" 
  #define TRACE(object) tracefile << #object << "\n" << object << "\n\n" << endl;
  ofstream tracefile("data.log");
  ofstream mcmc_F("F.mcmc");
  ofstream mcmc_N("N.mcmc");
  ofstream mcmc_ref("ref.mcmc");
  ofstream mcmc_ypr("ypr.mcmc");
  ofstream mcmc_lwtfinalyr("lwtfinalyr.mcmc"); 
  ofstream mcmc_swt("swt.mcmc");
