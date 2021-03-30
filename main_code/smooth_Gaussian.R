smooth_Gaussian <-function(cases){
  
  
  r <- 7
  
  cases_smoothed <- cases
  
  GaussTemp <- c(0.0004,0.0022,0.0088,0.0270,0.0648,0.1210,0.1761,0.1995,0.1761,0.1210,0.0648,0.0270,0.0088,0.0022,0.0004)
  
  for (i in (8:(length(cases)-r))){
    cases_smoothed[i] <- sum(cases[c((i-r):(i+r))]*t(GaussTemp))
  }
  
  return(cases_smoothed)
}