## @knitr samplesize
sample.size <- function(population, samp.size = NULL, c.lev = 95, 
                        c.int = NULL, what = "sample", distribution=50) {
  # Returns a data.frame of sample sizes or confidence intervals for 
  #   different conditions provided by the following arguments.
  #
  # --> populaton     Population size
  # --> samp.size     Sample size
  # --> c.lev         Confidence level
  # --> c.int         Confidence interval (+/-)
  # --> what          Whether sample size or confidence interval
  #                     is being calculated.
  # --> distribution  Response distribution
  # 
  # === EXAMPLES ===
  #
  #   sample.size(300)
  #   sample.size(300, 150, what="confidence")
  #   sample.size(c(300, 400, 500), c.lev=97)
  
  z = qnorm(.5+c.lev/200)
  
  if (identical(what, "sample")) {
    if (is.null(c.int)) {
      c.int = 5
      
      message("NOTE! Confidence interval set to 5.
      To override, set >> c.int << to desired value.\n")
      
    } else if (!is.null(c.int) == 1) {
      c.int = c.int
    }
    
    if (!is.null(samp.size)) {
      message("NOTE! >> samp.size << value provided but ignored.
      See output for actual sample size(s).\n")
    }
    
    ss = (z^2 * (distribution/100) * 
      (1-(distribution/100)))/((c.int/100)^2)
    samp.size = ss/(1 + ((ss-1)/population))    
    
  } else if (identical(what, "confidence")) {
    if (is.null(samp.size)) {
      stop("Missing >> samp.size << with no default value.")
    }
    if (!is.null(c.int)) {
      message("NOTE! >> c.int << value provided but ignored.
      See output for actual confidence interval value(s).\n")
    }
    
    ss = ((population*samp.size-samp.size)/(population-samp.size))
    c.int = round(sqrt((z^2 * (distribution/100) * 
      (1-(distribution/100)))/ss)*100, digits = 2)
    
  } else if (what %in% c("sample", "confidence") == 0) {
    stop(">> what << must be either -sample- or -confidence-")
  }
  
  RES = data.frame(population = population,
                   conf.level = c.lev,
                   conf.int = c.int,
                   distribution = distribution,
                   sample.size = round(samp.size, digits = 0))
  RES
}
