# Functions





#' Plot Output
#'
#' Some plotting function that displays output from an OM/EM run using lasac
#'
#' Further details go here
#'
#' @param output some output
#' @importFrom graphics plot
#' @export
myplot = function(output){
  plot(output)
}






#' Rich's selectivity-at-length equation
#'
#' A function for determining selectivity at length bins given selectivity at age and von Bertalanffy growth parameters. Also uses natural mortality.
#'
#' @param ages vector of ages required to translate selectivity by age to selectivity by length
#' @param lbins the length bins required
#' @param sel_type character of type "double_normal" etc. consistent with earthfish syntax
#' @param sel_params list in earthfish syntax appropriate for sel_type
#' @param growth list of VB growth parameters
#' @param natM natural mortality
rich_sell = function(ages, lbins, sel_type, sel_params, growth, natM){
    nages <- length(ages)
    nbins <- length(lbins)-1
    mulbins <- 0.5*(lbins[-c(1)]+lbins[-c(nbins+1)])


    # VB pars
    Linf <- growth[1] # 165
    k <- growth[2]   # 0.057
    t0 <- growth[3] # -0.19
    cvla <- growth[4] # 0.15
    mula <- Linf*(1-exp(-k*(ages-t0)))


    # selectivity pars
    sela = earthfish::ogive(sel_type, ages, sel_params)


    # set up distribution of length-at-age for given length partition
    pla <- array(NA,dim=c(nbins,nages))
    dimnames(pla)[[2]] <- ages
    dimnames(pla)[[1]] <- lbins[1:nbins]

    sdla <- sqrt(log(1+cvla^2))
    for(a in 1:nages) {

      pl <- dlnorm(mulbins,log(mula[a]),sdla)
      pl <- pl/sum(pl)
      pla[,a] <- pl
    }


    # prior distribution for age (~ exp(-M)*sela))
    M <- natM  # 0.13
    pa <- exp(-M*ages)*sela
    pa <- pa/sum(pa)


    # now get distribution of age-given-length
    pal <- array(dim=c(nages,nbins))
    pl <- rep(NA,nbins)
    dimnames(pal)[[1]] <- ages
    dimnames(pal)[[2]] <- lbins[1:nbins]
    for(l in 1:nbins) {

      for(a in 1:nages) pal[a,l] <- pla[l,a] * pa[a]
      pl[l] <- sum(pal[,l])
      pal[,l] <- pal[,l] / pl[l]

    }


    # expected selectivity-at-length calculated RIGHT way
    sell <- rep(NA,nbins)
    for(l in 1:nbins) sell[l] <- sum(sela*pal[,l])

    return(sell)
}


