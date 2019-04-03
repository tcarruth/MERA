
#' Depletion and F estimation from mean length of catches
#'
#' A highly dubious means of getting very uncertain estimates of current stock
#' biomass and (equilibrium) fishing mortality rate from growth, natural
#' mortality rate, recruitment and fishing selectivity.
#'
#' @param OM An object of class 'OM'
#' @param ML A estimate of current mean length of catches
#' @param nsim Number of simulations
#' @param ploty Produce a plot of depletion and F
#' @param Dlim Limits on the depletion that is returned as a fraction of
#' unfished biomass.
#' @return An object of class 'OM' with 'D' slot populated
#' @author T. Carruthers
#' @export
ML2D_frame <- function(OM, ML, nsim = 100, ploty = T, Dlim = c(0.05, 0.6)) {

  nsim2<-nsim*10
  maxage <- OM@maxage
  M <- runif(nsim2, OM@M[1], OM@M[2])  # Natural mortality rate
  h <- runif(nsim2, OM@h[1], OM@h[2])  # Steepness
  Linf <- runif(nsim2, OM@Linf[1], OM@Linf[2])  # Maximum length
  K <- runif(nsim2, OM@K[1], OM@K[2])  # Maximum growth rate
  t0 <- runif(nsim2, OM@t0[1], OM@t0[2])  # Theorectical length at age zero

  if (OM@isRel == "0" | OM@isRel == "FALSE" | OM@isRel == FALSE) {
    if (max(OM@LFS) > 0) {
      LFS <- runif(nsim2*5, OM@LFS[1], OM@LFS[2])
    } else {
      LFS <- runif(nsim2*5, mean(OM@LFSLower), mean(OM@LFSUpper))
    }
  } else {
    if (max(OM@LFS) > 0) {
      LFS <- runif(nsim2*5, OM@LFS[1], OM@LFS[2]) * mean(OM@L50)
    } else {
      LFS <- runif(nsim2*5, mean(OM@LFSLower), mean(OM@LFSUpper)) *
        mean(OM@L50)
    }
  }
  LFS<-LFS[LFS<Linf][1:nsim2]
  AFS <- L2A_frame(t0, Linf, K, LFS, maxage)
  AFS[AFS<1]<-1

  if (OM@isRel == "0" | OM@isRel == "FALSE" | OM@isRel == FALSE) {
    L5 <- runif(nsim2, OM@L5[1], OM@L5[2])
  }else{
    L5 <- runif(nsim2, OM@L5[1], OM@L5[2])* mean(OM@L50)
  }

  age05 <- L2A_frame(t0, Linf, K, L5, maxage)
  age05[age05<0.5] <- 0.5

  Vmaxage <- runif(nsim2, OM@Vmaxlen[1], OM@Vmaxlen[2])  #runif(BT_fleet@Vmaxage[1],BT_fleet@Vmaxage[2]) # selectivity of oldest age class

  LM <- runif(nsim2*5, OM@L50[1], OM@L50[2])
  LM<-LM[LM<Linf][1:nsim2]
  AM <- L2A_frame(t0, Linf, K, LM, maxage)
  AM[AM<1]<-1

  # age at maturity
  a <- OM@a  # length-weight parameter a
  b <- OM@b  # length-weight parameter b

  mod <- AFS  # the age at modal (or youngest max) selectivity

  # deriv <- getDNvulnS(mod, age05, Vmaxage, maxage, nsim2)  # The vulnerability schedule
  # vuln <- deriv[[1]]

  srs <- (maxage - AFS) / ((-log(Vmaxage,2))^0.5) # selectivity parameters are constant for all years
  sls <- (AFS - age05) /((-log(0.05,2))^0.5)

  vuln <- t(sapply(1:nsim2, DLMtool:::getsel, lens=matrix(1:maxage, nrow=nsim2, ncol=maxage, byrow=TRUE),
                   lfs=AFS, sls=sls, srs=srs))

  Agearray <- array(rep(1:maxage, each = nsim2), c(nsim2, maxage))
  mat <- 1/(1 + exp((AM - (Agearray))/(AM * 0.1)))  # Maturity at age array

  nyears <- OM@nyears
  # bootfun<-function(dat,ind)mean(dat[ind]) MLo<-boot(MLt,bootfun,nsim2)
  # ML<-MLo$t
  out <- CSRA_frame(M, h, Linf, K, t0, AM, a, b, vuln, mat, ML = rep(ML, nsim2),
              NA, NA, maxage, nyears)
  cond <- out[, 1] > Dlim[1] & out[, 1] < Dlim[2] & out[, 2] < 2.5  # Stock levels are unlikely to be above 80% unfished, F is unlikely to be above 2.5

  if (ploty & sum(cond) > 5) {
    par(mfrow = c(1, 2))
    plot(density(out[cond, 1], from = 0, adj = 0.4), main = "Depletion")
    plot(density(out[cond, 2], from = 0, adj = 0.4), main = "Fishing mortality rate")
  }
  if (sum(cond) < 5) {
    message("All estimates of Depletion outside bounds of Dlim")
    message("Operating Model object not updated")
  }

  if(sum(cond)>nsim){
    OM@cpars$D<-out[cond,1][1:nsim]
  }

  if(sum(cond) > 5) OM@D <- quantile(out[cond, 1], c(0.05, 0.95))

  return(OM)
}

# Composition stock reduction analysis


#' Catch at size reduction analysis
#'
#' What depletion level and corresponding equlibrium F arise from data
#' regarding mean length of current catches, natural mortality rate, steepness
#' of the stock recruitment curve, maximum length, maximum growth rate, age at
#' maturity, age based vulnerability, maturity at age, maximum age and number
#' of historical years of fishing.
#'
#'
#' @usage CSRA(M,h,Linf,K,t0,AM,a,b,vuln,mat,ML,CAL,CAA,maxage,nyears)
#' @param M A vector of natural mortality rate estimates
#' @param h A vector of sampled steepness (Beverton-Holt stock recruitment)
#' @param Linf A vector of maximum length (von Bertalanffy growth)
#' @param K A vector of maximum growth rate (von Bertalanffy growth)
#' @param t0 A vector of theoretical age at length zero (von Bertalanffy
#' growth)
#' @param AM A vector of age at maturity
#' @param a Length-weight conversion parameter a (W=aL^b)
#' @param b Length-weight conversion parameter b (W=aL^b)
#' @param vuln A matrix nsim x nage of the vulnerabilty at age (max 1) to
#' fishing.
#' @param mat A matrix nsim x nage of the maturity at age (max 1)
#' @param ML A vector of current mean length estimates
#' @param CAL A catch-at-length matrix nyears x (1 Linf unit) length bins
#' @param CAA A catch-at-age matrix nyears x maximum age
#' @param maxage Maximum age
#' @param nyears Number of historical years of fishing
#' @author T. Carruthers
#' @export CSRA
#' @keywords internal
CSRA_frame <- function(M, h, Linf, K, t0, AM, a, b, vuln, mat, ML, CAL, CAA,
                 maxage, nyears) {
  nsim <- length(M)
  Dep <- rep(NA, nsim)
  Fm <- rep(NA, nsim)
  for (i in 1:nsim) {
    fit <- optimize(CSRAfunc_frame, log(c(1e-04, 5)), Mc = M[i], hc = h[i],
                    maxage, nyears, Linfc = Linf[i], Kc = K[i], t0c = t0[i], AMc = AM[i],
                    ac = a, bc = b, vulnc = vuln[i, ], matc = mat[i, ], MLc = ML[i],
                    CAL = NA, CAA = NA, opt = T)


    out <- CSRAfunc_frame(fit$minimum, Mc = M[i], hc = h[i], maxage, nyears,
                    Linfc = Linf[i], Kc = K[i], t0c = t0[i], AMc = AM[i], ac = a,
                    bc = b, vulnc = vuln[i, ], matc = mat[i, ], MLc = ML[i], CAL = NA,
                    CAA = NA, opt = 3)

    Dep[i] <- out[1]
    Fm[i] <- out[2]


  }
  cbind(Dep, Fm)
}

# The function that CSRA operates on

#' Optimization function for CSRA
#'
#' What depletion level and corresponding equlibrium F arise from data
#' regarding mean length of current catches, natural mortality rate, steepness
#' of the stock recruitment curve, maximum length, maximum growth rate, age at
#' maturity, age based vulnerability, maturity at age, maximum age and number
#' of historical years of fishing.
#'
#'
#' @param lnF A proposed value of current instantaneous fishing mortality rate
#' @param Mc Natural mortality rate estimates
#' @param hc Steepness (Beverton-Holt stock recruitment)
#' @param maxage Maximum age
#' @param nyears Number of historical years of fishing
#' @param AFSc Age at full selection
#' @param AFCc Age at first capture
#' @param Linfc Maximum length (von Bertalanffy growth)
#' @param Kc Maximum growth rate (von Bertalanffy growth)
#' @param t0c Theoretical age at length zero (von Bertalanffy growth)
#' @param AMc Age at maturity
#' @param ac Length-weight conversion parameter a (W=aL^b)
#' @param bc Length-weight conversion parameter b (W=aL^b)
#' @param vulnc A vector (nage long) of the vulnerabilty at age (max 1) to
#' fishing.
#' @param matc A vector (nage long) of the maturity at age (max 1)
#' @param MLc A current mean length estimates
#' @param CAL A catch-at-length matrix nyears x (1 Linf unit) length bins
#' @param CAA A catch-at-age matrix nyears x maximum age
#' @param opt Should the measure of fit be returned?
#' @param meth Are we fitting to mean length or catch composition?
#' @author T. Carruthers
#' @keywords internal
CSRAfunc_frame <- function(lnF, Mc, hc, maxage, nyears, AFSc, AFCc, Linfc, Kc,
                     t0c, AMc, ac, bc, vulnc, matc, MLc, CAL, CAA, opt = T, meth = "ML") {

  Fm <- exp(lnF)
  Fc <- vulnc * Fm
  Lac <- Linfc * (1 - exp(-Kc * ((1:maxage) - t0c)))
  Wac <- ac * Lac^bc
  N <- exp(-Mc * ((1:maxage) - 1))
  SSN <- matc * N  # Calculate initial spawning stock numbers
  Biomass <- N * Wac
  SSB <- SSN * Wac  # Calculate spawning stock biomass

  B0 <- sum(Biomass)
  SSB0 <- sum(SSB)
  SSN0 <- SSN
  SSBpR <- sum(SSB)  # Calculate spawning stock biomass per recruit
  SSNpR <- SSN
  Zc <- Fc + Mc
  CN <- array(NA, dim = c(nyears, maxage))
  HR <- rep(0, maxage)
  pen <- 0
  for (y in 1:nyears) {
    VB <- Biomass * vulnc * exp(-Mc)
    CN[y, ] <- N * (1 - exp(-Zc)) * (Fc/Zc)
    N[2:maxage] <- N[1:(maxage - 1)] * exp(-Zc[1:(maxage - 1)])  # Total mortality
    N[1] <- (0.8 * hc * sum(SSB))/(0.2 * SSBpR * (1 - hc) + (hc - 0.2) *
                                     sum(SSB))  # Recruitment assuming regional R0 and stock wide steepness
    Biomass <- N * Wac
    SSN <- N * matc
    SSB <- SSN * Wac
  }  # end of year

  pred <- sum((CN[nyears, ] * Lac))/sum(CN[nyears, ])
  fobj <- (pred - MLc)^2  # Currently a least squares estimator. Probably not worth splitting hairs WRT likelihood functions!
  if (opt == 1) {
    return(fobj)
  } else {
    c(sum(SSB)/sum(SSB0), Fm)
  }
}

# Stochastic inverse growth curve used to back-calculate age at first
# capture from length at first capture
#' Calculate age at first capture from length at first capture and growth
#'
#' As title.
#'
#' @param t0c A vector of theoretical age at length zero (von Bertalanffy
#' growth)
#' @param Linfc A vector of maximum length (von Bertalanffy growth)
#' @param Kc A vector of maximum growth rate (von Bertalanffy growth)
#' @param LFC A vector of length at first capture
#' @param maxage Maximum age
#' @author T. Carruthers
#' @keywords internal
getAFC_frame <- function(t0c, Linfc, Kc, LFC, maxage) {
  nsim <- length(t0c)
  agev <- c(1e-04, 1:maxage)
  agearray <- matrix(rep(agev, each = nsim), nrow = nsim)
  Larray <- Linfc * (1 - exp(-Kc * (agearray - t0c)))
  matplot(agev, t(Larray), type = "l")
  abline(h = LFC, col = "#ff000030", lwd = 2)
  AFC <- (log(1 - (LFC/Linfc))/-Kc) + t0c
  abline(v = AFC, col = "#0000ff30", lwd = 2)
  AFC
}



#' Length to age conversion
#'
#' Simple deterministic length to age conversion given inverse von Bertalanffy
#' growth.
#'
#' @param t0c Theoretical age at length zero
#' @param Linfc Maximum length
#' @param Kc Maximum growth rate
#' @param Len Length
#' @param maxage Maximum age
#' @param ploty Should a plot be included
#' @return An age (vector of ages, matrix of ages) corresponding with Len
#' @author T. Carruthers
#' @keywords internal
L2A_frame <- function(t0c, Linfc, Kc, Len, maxage, ploty=F) {
  nsim <- length(t0c)
  agev <- c(1e-04, 1:maxage)
  agearray <- matrix(rep(agev, each = nsim), nrow = nsim)
  Larray <- Linfc * (1 - exp(-Kc * (agearray - t0c)))
  temp<-Len/Linfc
  temp[temp<0.95]<-0.95
  age <- (log(1 - (Len/Linfc))/-Kc) + t0c
  if(ploty){
    matplot(agev, t(Larray), type = "l")
    abline(h = Len, col = "#ff000030", lwd = 2)
    abline(v = age, col = "#0000ff30", lwd = 2)
  }
  age
}

