---
title: FLPKG NOTES
author: Iago Mosqueira, EC JRC G03
license: Creative Commons Attribution-ShareAlike 4.0 International
---

# stdfile

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


