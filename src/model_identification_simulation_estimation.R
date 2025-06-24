#--------------------------------
# MODEL IDENTIFICATION SIMULATION
#--------------------------------
# Purposes: to show that our model is identified and we can recover the true parameters 


# Notes: We used the parameter estimates (or the population-level means of them) of our main model
# and the observed values of the independent variables to simulate dormant and active states
# and donation behavior
# For simplification purposes, we do not simulate endogeneity and customer heterogeneity



# list of external packages needed
packages_list <- c("dplyr", "tidyr", "matrixStats", "stats4")

# check and install packages if missing
for(p in packages_list){
  if(!require(p, character.only = T)){
    install.packages(p, dependencies = TRUE)
  } else{
    library(p, character.only = T)
  }
}



#------------------------
# STEP 2: MODEL ESIMATION
#------------------------

# load the simulated dataset
dat <- readRDS("simulated_data_model_identification.rds")


# replace NAs with 0
library(tidyr)
dat <- dat %>%
  replace_na(list(firstTransactMon = 0, 
                  upgradeY = 0, 
                  downgradeY = 0, 
                  churnY = 0))





IDList <- unique(dat$ID[dat$nonconDecisionDummy == 1]) # list of unique customers
lID <- length(IDList) # number of unique customers


# sort the data based on ID and Month
library(dplyr)
dat <- dat %>% arrange(ID, Month)


# response model & noncontractual donation
par1 <- c("CR", "CRAmount_nonlog",
          "quarter2", "quarter3", "quarter4",
          "churnedDummy",
          "donStock", 
          "hhSize", "hhIncome",
          "relationshipLen", 
          "intercept", 
          "DM") 

lenPar1 <- length(par1) # number of parameters

# amount model & noncontractual donation
par2 <- c("CR", "CRAmount_nonlog",
          "quarter2", "quarter3", "quarter4",
          "churnedDummy",
          "donStock", 
          "hhSize", "hhIncome",
          "relationshipLen", 
          "intercept", 
          "DM")

lenPar2 <- length(par2) # number of parameters



# response model & contractual donation
par3 <- c("quarter2", "quarter3", "quarter4",
          "churnedDummy",
          "donStock", 
          "hhSize", "hhIncome",
          "relationshipLen", 
          "intercept", "DM") 

lenPar3 <- length(par3) # number of parameters

# amount model & contractual donation
par4 <- c("quarter2", "quarter3", "quarter4",
          "churnedDummy",
          "donStock", 
          "hhSize", "hhIncome",
          "relationshipLen", 
          "intercept", "DM") 

lenPar4 <- length(par4) # number of parameters





# revision decision
par6 <- c("CRAmount_nonlog",
          "donStock",
          "quarter2", "quarter3", "quarter4",
          "hhSize", "hhIncome",
          "relationshipLen", 
          "intercept",
          "DM") 


lenPar6 <- length(par6) # number of parameters



# Variables explaining latent attrition (donor i being active at time t)
parTrans <- c("DM_lag",
              "quarter2", "quarter3", "quarter4",
              "donStock",
              "hhSize", "hhIncome",
              "relationshipLen", 
              "intercept")

lenParTrans <- length(parTrans)



# create a list, each element contains all estimation data for each donor
monList <- list()
monList <- split(dat$Month[dat$nonconDecisionDummy == 1], f = dat$ID[dat$nonconDecisionDummy == 1])


# create an index to subset the utility matrix for response equation for each donor
library(dplyr)
totMonVec <- as.data.frame(dat %>% filter(nonconDecisionDummy == 1) %>% group_by(ID) %>% summarize(n()))[, 2]
cumTotMon <- cumsum(totMonVec)
cumTotMonVec <- c(0, cumTotMon[-length(cumTotMon)])
cumTotMonList <- list()
cumTotMonList <- split(cumTotMonVec, f = unique(dat$ID[dat$nonconDecisionDummy == 1]))


# number of observations for estimation
numObs <- nrow(dat[dat$nonconDecisionDummy == 1, ])


# Matricies of IVs for each equation
XNonconI <- as.matrix(dat[dat$nonconDecisionDummy == 1, c(par1)])


library(tidyr)
library(dplyr)
XNonconA <- dat %>% filter(nonconDecisionDummy == 1) %>% dplyr::select(!!par2) %>% replace(is.na(.), 0) %>% as.matrix()
XConI <- dat %>% filter(nonconDecisionDummy == 1) %>% dplyr::select(!!par3) %>% replace(is.na(.), 0) %>% as.matrix()
XConA <- dat %>% filter(nonconDecisionDummy == 1) %>% dplyr::select(!!par4) %>% replace(is.na(.), 0) %>% as.matrix()
XRev <- dat %>% filter(nonconDecisionDummy == 1) %>% dplyr::select(!!par6) %>% replace(is.na(.), 0) %>% as.matrix()



XTrans <- as.matrix(dat[dat$nonconDecisionDummy == 1, c(parTrans)])




# DVs for each equation
YNoncon <- dat$nonConY[dat$nonconDecisionDummy == 1]
ANoncon <- dat$nonConA[dat$nonconDecisionDummy == 1]
YCon <- dat$CR[dat$nonconDecisionDummy == 1]
ACon <- dat$CRAmount[dat$nonconDecisionDummy == 1]
YRev <- dat$revY[dat$nonconDecisionDummy == 1]
ARev <- dat$lnAChanged[dat$nonconDecisionDummy == 1]


ANoncon[is.na(ANoncon)] <- 0 
YCon[is.na(YCon)] <- 0 
ACon[is.na(ACon)] <- 0 



# sample dummy
conSample <- dat$conDecisionDummy[dat$nonconDecisionDummy == 1]
revSample <- dat$revDecisionDummy[dat$nonconDecisionDummy == 1]



# Here we create a dummy indicating when a donor did not engaged in any observable action
# 1: not engaged in an observable action, 0: engaged in an observable action
YInact <- rep(0, nrow(dat[dat$nonconDecisionDummy == 1, ]))

YInact[dat$nonConY[dat$nonconDecisionDummy == 1] == 0 & # not donating adhoc
         (dat$CR[dat$nonconDecisionDummy == 1]*dat$conDecisionDummy[dat$nonconDecisionDummy == 1]) == 0 & # not start any new contract
         (dat$upgradeY[dat$nonconDecisionDummy == 1]*dat$revDecisionDummy[dat$nonconDecisionDummy == 1]) == 0 & # not upgrading
         (dat$downgradeY[dat$nonconDecisionDummy == 1]*dat$revDecisionDummy[dat$nonconDecisionDummy == 1]) == 0 & # not downgrading
         (dat$churnY[dat$nonconDecisionDummy == 1]*dat$churnDecisionDummy[dat$nonconDecisionDummy == 1]) == 0] <- 1 # not terminating


table(YInact) # 1: 717433, 0: 24080




numCatRev <- length(unique(dat$revY[dat$revDecisionDummy == 1])) # 4, number of options for the revision decision (1 = terminate, 2 = downgrade, 3 = keep as is, 4 = upgrade)

# create dummy variables indicating the choice of the revision decision
library(dplyr)
library(tidyr)
temp <- dat %>%
  filter(revDecisionDummy == 1) %>%
  select(ID, Month, revY) %>%
  mutate(rn = row_number(), dummy_value = 1) %>%
  pivot_wider(names_from = c("revY"), 
              values_from = c("dummy_value"), 
              values_fill = 0, 
              names_prefix = "rev_") %>%
  select(ID, Month, rev_1, rev_2, rev_3, rev_4)

dat <- dat %>% select(!(starts_with("rev_"))) %>%
  left_join(temp, by = c("ID", "Month")) %>%
  replace_na(list(rev_1 = 0, 
                  rev_2 = 0, 
                  rev_3 = 0, 
                  rev_4 = 0))


# double check
temp <- rowSums(dat[, c(paste0("rev_", c(1:4)))])
unique(temp[dat$revDecisionDummy == 1]) # only 1
unique(temp[dat$revDecisionDummy == 0]) # only 0


library(dplyr)
revMat <- dat %>%
  dplyr::filter(nonconDecisionDummy == 1) %>%
  select(rev_1, rev_2, rev_3, rev_4) %>%
  as.matrix()



orderIDList <- split(x = 1:lID, f = 1:lID)

library(matrixStats) # to use rowCumsums function





LL_RP <- function(parms){
  # create a vector of regression coefficients associated with selected predictors
  temp <- 0
  beta1 <- data.matrix(parms[(temp+1):(temp+lenPar1)])
  temp <- temp + lenPar1
  
  beta2 <- data.matrix(parms[(temp+1):(temp+lenPar2)])
  temp <- temp + lenPar2
  
  beta3 <- data.matrix(parms[(temp+1):(temp+lenPar3)])
  temp <- temp + lenPar3
  
  beta4 <- data.matrix(parms[(temp+1):(temp+lenPar4)])
  temp <- temp + lenPar4
  
  
  beta6 <- data.matrix(parms[(temp+1):(temp+lenPar6)])
  temp <- temp + lenPar6
  
  
  # this is the standard deviations of the original error terms of the amount equations
  sigma <- exp(parms[(temp+1):(temp+2)]) # one sigma for noncontractual, one for contractual
  temp <- temp + 2
  
  # we assume the error term of the selection equation follows standard normal distribution
  # in order to make the model identified
  
  # in order to estimate the correlation between the two error terms, we use tanh transformation
  # to make sure that the correlation is between -1 and 1
  atanhrho <- parms[(temp+1):(temp+2)] # one for noncontractual, one for contractual
  temp <- temp + 2
  
  rho <- tanh(atanhrho)
  
  
  # thresholds of the ordered probit (revision decision)
  revThresholds <- c(0, cumsum(exp(parms[(temp+1):(temp+numCatRev-2)]))) # numCatRev - 2 thresholds to be estimated
  # cumsum and exp functions make sure that the thresholds are all positive and are increasing
  temp <- temp + numCatRev-2
  
  
  # model the amount changed when a donor revises his/her contract
  sdLogRev <- exp(parms[(temp+1):(temp+1)]) # one sd for both upgrade and downgrade: 
  temp <- temp + 1
  
  meanLogRev <- parms[(temp+1):(temp+2)] # two means: one for upgrade, one for downgrade
  temp <- temp + 2
  
  # Note that we model the absolute change of the amount (upgrade: log(Ynew/Yold) and downgrade: -log(Ynew/Yold) to follow a log normal distribution)
  
  
  
  # TRANSITION MATRIX
  # Note: one transition matrix for both noncontractual donors and contractual donors
  
  # parameters for the latent attrition (state-invariant)
  # only the intercepts that are state specific
  # a) active state
  delta1 <- data.matrix(parms[(temp+1):(temp+lenParTrans)])
  temp <- temp + lenParTrans
  
  
  # b) dormant state
  delta2 <- data.matrix(c(delta1[-lenParTrans], 
                          parms[(temp+1):(temp+1)]))
  temp <- temp + 1
  
  
  # create utility vectors
  # 1) noncontractual
  V1 <- XNonconI %*% beta1 # for incidence decision
  
  
  V2 <- XNonconA %*% beta2 # for amount decision
  
  
  # 2) contractual
  V3 <- XConI %*% beta3 # for incidence decision
  
  V4 <- XConA %*% beta4 # for amount decision
  
  
  # 3) revision
  V6 <- XRev %*% beta6
  
  
  
  
  
  
  # probability of the observations
  # noncontractual decisions
  p1 <- (pnorm(V1, lower.tail = 0, log.p = 0, sd = 1)^(1 - YNoncon))*
    ((pnorm((V1 + (ANoncon - V2)*rho[1]/sigma[1])/sqrt(1 - rho[1]^2), lower.tail = 1, log.p = 0, sd = 1)*
        dnorm(x = ANoncon,
              mean = V2,
              sd = sigma[1],
              log = F))^YNoncon)
  
  
  # contractual decisions
  p2 <- (pnorm(V3, lower.tail = 0, log.p = 0, sd = 1)^((1 - YCon)*conSample))*
    ((pnorm((V3 + (ACon - V4)*rho[2]/sigma[2])/sqrt(1 - rho[2]^2), lower.tail = 1, log.p = 0, sd = 1)*
        dnorm(x = ACon,
              mean = V4,
              sd = sigma[2],
              log = F))^(YCon*conSample))
  
  
  
  
  # revision incidence decision
  lowerBoundRev <- matrix(c(-Inf, revThresholds), nrow = numObs, ncol = numCatRev, byrow = T) # lower threshold levels
  upperBoundRev <- matrix(c(revThresholds, Inf), nrow = numObs, ncol = numCatRev, byrow = T) # upper threshold level
  
  phiRev2 <- pnorm(upperBoundRev - as.numeric(V6), log.p = 0, sd = 1)
  phiRev1 <- pnorm(lowerBoundRev - as.numeric(V6), log.p = 0, sd = 1)
  
  p3 <- rowSums((phiRev2 - phiRev1)*revMat)^(revSample)
  
  
  # revision amount decision
  # we assume that the amount change: log(ANew/AOld) in case of upgrade or -log(ANew/AOld) in case of downgrade follows a log normal distribution
  p4 <- (dlnorm(abs(ARev), meanlog = meanLogRev[1], sdlog = sdLogRev)^revMat[, 4])*
    (dlnorm(abs(ARev), meanlog = meanLogRev[2], sdlog = sdLogRev)^revMat[, 2])
  
  
  # joint likelihood conditional on being active
  p <- as.matrix(p1*p2*p3*p4)
  
   
  
  # utility for the transitions
  # for noncontractual donors
  # a) active state
  VTrans1 <- XTrans %*% delta1
  
  
  # b) dormant state
  VTrans2 <- XTrans %*% delta2
  
  # transition matrix: a 2x2 matrix and the dormant state is assumed to be an unabsorbing state
  # see Schweidel and Knox 2013 (p. 476)
  q <- matrix(0, nrow = numObs, ncol = 2*2) # we store a 2x2 matrix in one row (row by row)
  
  # probability that donor i remains active
  q[, 1] <- pnorm(VTrans1, lower.tail = 1, log.p = 0, sd = 1) # previous state: active
  q[, 3] <- pnorm(VTrans2, lower.tail = 1, log.p = 0, sd = 1) # previous state: dormant
  
  # probability that donor i becomes dormant
  q[, 2] <- pnorm(VTrans1, lower.tail = 0, log.p = 0, sd = 1) # previous state: active
  q[, 4] <- pnorm(VTrans2, lower.tail = 0, log.p = 0, sd = 1) # previous state: dormant
  
  
  
  
  # initial states: we assume that all donors begin in the active state
  omegaVec <- t(data.matrix(c(1, 0))) # a row vector
  
  
  
  
  
  LL <- mapply(FUN = function(monVec, cumTotMon, omegaVec, q, p){
    # Number of computation months for each donor
    numRows <- length(monVec)
    
    alpha <- omegaVec*c(p[cumTotMon + 1], YInact[cumTotMon + 1])
    
    
    # scaling factor to avoid underlow in the likelihood multiplication
    lnscale <- log(sum(alpha)) 
    if(sum(alpha) != 0){ # to avoid NaN
      alpha <- alpha/sum(alpha)
    }
    
    
    
    
    for(t in 2:numRows){
      
      # observation-specific transition matrix
      qMat <- matrix(q[cumTotMon + t, ], nrow = 2, ncol = 2, byrow = T)
      
      alpha <- (alpha %*% qMat)*c(p[cumTotMon + t], YInact[cumTotMon + t])
      
      lnscale <- lnscale + log(sum(alpha))
      if(sum(alpha) != 0){ # to avoid NaN
        alpha <- alpha/sum(alpha)
      }
      
      
      
    }
    
    return(max(lnscale, -999999)) # max function is used to avoid -Inf
    
  }, monList, cumTotMonList, MoreArgs = list(omegaVec = omegaVec, q = q, p = p))
  
  
  
  
  
  
  
  
  return(-sum(LL))
  
}

#--------------------------------------------------------------------------
# MODEL ESTIMATION
# NOTES: Optimizing the Log likelihood function by using the optimx package
#
#
#--------------------------------------------------------------------------
# install.packages("stats4") -- Run this if the package is not installed
library(stats4)                                           # Load the package


# number of parameters to be estimated
totLenPar_RP <- lenPar1 + lenPar2 + lenPar3 + lenPar4 + lenPar6 +
  2+2+
  numCatRev-2+ 
  1+2+
  lenParTrans + 1


# initial values for the parameters
set.seed(123)
iniVals <- runif(totLenPar_RP, min = -0.2, max = 0)


ptm       <- proc.time()                                # Start the clock 
my_model_RP <- optim(par= iniVals,
                     fn = LL_RP, 
                     hessian=TRUE,
                     method = "BFGS",
                     control = list(maxit = 200000, trace=TRUE, reltol = 1e-16))                        # minimize -LL
estimation_time_RP        <- proc.time() - ptm                        # Stop the clock
print(estimation_time_RP)                                              # display estimation time
standDev_RP <- sqrt(diag(solve(my_model_RP$hessian)))
tvalue_RP <- my_model_RP$par/standDev_RP









