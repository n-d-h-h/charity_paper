#--------------------------------
# MODEL IDENTIFICATION SIMULATION
#--------------------------------
# Purposes: to show that our model is identified and we can recover the true parameters 


# Notes: We used the parameter estimates (or the population-level means of them) of our main model
# and the observed values of the independent variables to simulate dormant and active states
# and donation behavior
# For simplification purposes, we do not simulate endogeneity and customer heterogeneity




# list of external packages needed
packages_list <- c("dplyr", "tidyr", "MASS", "truncnorm")

# check and install packages if missing
for(p in packages_list){
  if(!require(p, character.only = T)){
    install.packages(p, dependencies = TRUE)
  } else{
    library(p, character.only = T)
  }
}




#------------------------
# STEP 1: DATA SIMULATION
#------------------------


# Data prepration
# load the dataset
dat <- readRDS("XXXX.rds")

# sort the data based on ID and Month
library(dplyr)
dat <- dat %>%
  ungroup() %>%
  arrange(ID, Month)


# response model & adhoc donation
fixedPar1 <- c("CR", "CRAmount_nonlog",
               "quarter2", "quarter3", "quarter4",
               "churnedDummy",
               "donStock", 
               "hhSize", "hhIncome",
               "relationshipLen")

randPar1 <- c("intercept",
              "DM")


# number of parameters
lenFixedPar1 <- length(fixedPar1) 
lenRandPar1 <- length(randPar1)

par1 <- c(fixedPar1, randPar1)
lenPar1 <- length(par1) # number of parameters


# amount model & adhoc donation
fixedPar2 <- c("CR", "CRAmount_nonlog", 
               "quarter2", "quarter3", "quarter4",
               "churnedDummy",
               "donStock", 
               "hhSize", "hhIncome",
               "relationshipLen")

randPar2 <- c("intercept",
              "DM")


# number of parameters
lenFixedPar2 <- length(fixedPar2) 
lenRandPar2 <- length(randPar2)

par2 <- c(fixedPar2, randPar2)
lenPar2 <- length(par2) # number of parameters



# response model & contractual donation
fixedPar3 <- c("quarter2", "quarter3", "quarter4",
               "churnedDummy",
               #"December",
               "donStock", 
               "hhSize", "hhIncome",
               "relationshipLen")

randPar3 <- c("intercept",
              "DM")


# number of parameters
lenFixedPar3 <- length(fixedPar3) 
lenRandPar3 <- length(randPar3)

par3 <- c(fixedPar3, randPar3)
lenPar3 <- length(par3) # number of parameters

# amount model & contractual donation
fixedPar4 <- c("quarter2", "quarter3", "quarter4",
               "churnedDummy",
               #"December",
               "donStock", 
               "hhSize", "hhIncome",
               "relationshipLen")

randPar4 <- c("intercept",
              "DM")

# number of parameters
lenFixedPar4 <- length(fixedPar4) 
lenRandPar4 <- length(randPar4)

par4 <- c(fixedPar4, randPar4)
lenPar4 <- length(par4) # number of parameters




# revision decision
fixedPar6 <- c("CRAmount_nonlog", 
               "donStock",
               "quarter2", "quarter3", "quarter4",
               "hhSize", "hhIncome",
               "relationshipLen")

randPar6 <- c("intercept",
              "DM")


# number of parameters
lenFixedPar6 <- length(fixedPar6) 
lenRandPar6 <- length(randPar6)

par6 <- c(fixedPar6, randPar6)
lenPar6 <- length(par6) # number of parameters



# Variables explaining latent attrition (donor i being active at time t)
fixedParTrans <- c("DM_lag",
                   "quarter2", "quarter3", "quarter4",
                   "donStock",
                   "hhSize", "hhIncome",
                   "relationshipLen")

randParTrans <- c("intercept") 


# number of parameters
lenFixedParTrans <- length(fixedParTrans)
lenRandParTrans <- length(randParTrans)

parTrans <- c(fixedParTrans, randParTrans)
lenParTrans <- length(parTrans)





# update number of total observations
numObs <- nrow(dat)
# create a list, each element contains all estimation months for each donor
monList <- list()
monList <- split(dat$Month,
                 f = dat$ID)


# create an index to subset the utility matrix for response equation for each donor
library(dplyr)
totMonVec <- as.data.frame(dat %>%
                             group_by(ID) %>% summarize(n()))[, 2]
cumTotMon <- cumsum(totMonVec)
cumTotMonVec <- c(0, cumTotMon[-length(cumTotMon)])
cumTotMonList <- list()
cumTotMonList <- split(cumTotMonVec, f = unique(dat$ID))


# Matricies of IVs for each equation
XNonconI <- dat %>%
  dplyr::select(!!par1) %>% replace(is.na(.), 0) %>% as.matrix()

library(tidyr)
library(dplyr)
XNonconA <- dat %>% 
  dplyr::select(!!par2) %>% replace(is.na(.), 0) %>% as.matrix()

XConI <- dat %>% 
  dplyr::select(!!par3) %>% replace(is.na(.), 0) %>% as.matrix()

XConA <- dat %>% 
  dplyr::select(!!par4) %>% replace(is.na(.), 0) %>% as.matrix()

XRev <- dat %>% 
  dplyr::select(!!par6) %>% replace(is.na(.), 0) %>% as.matrix()

XTrans <- dat %>%
  dplyr::select(!!parTrans) %>% replace(is.na(.), 0) %>% as.matrix()





# Here we simulate the latent states and donation behavior

# Assume that everyone is non-contractual at the beginning of the simulation period
XNonconI[, c("CR")] <- 0
XNonconA[, c("CR")] <- 0
XNonconI[, c("CRAmount_nonlog")] <- XNonconI[, c("CRAmount_nonlog")]*0
XNonconA[, c("CRAmount_nonlog")] <- XNonconA[, c("CRAmount_nonlog")]*0
XRev[, c("CRAmount_nonlog")] <- XRev[, c("CRAmount_nonlog")]*0

# relationship length is 0 at the beginning
XNonconI[, c("relationshipLen")] <- 0
XNonconA[, c("relationshipLen")] <- 0
XConI[, c("relationshipLen")] <- 0
XConA[, c("relationshipLen")] <- 0
XRev[, c("relationshipLen")] <- 0
XTrans[, c("relationshipLen")] <- 0


# dummy indicating the start of a relationship (i.e., marked by a donation)
relationshipStarted <- 0


# vectors of outcome variables
YNoncon <- rep(NA, numObs)
ANoncon <- rep(NA, numObs)
YCon <- rep(NA, numObs)
ACon <- rep(NA, numObs)
YRev <- rep(NA, numObs)
ARev <- rep(NA, numObs)


# the "unconditional" probabilities of different states of each donor in each month
probVec <- matrix(NA, nrow = numObs, ncol = 2) # the first column is probability of being active, the second one is prob of being dormant


# the following variables will change values during the simulation
#-----------------------------------------------------------------
# sample dummy
nonconSample <- rep(1, numObs)
conSample <- rep(1, numObs)
revSample <- rep(1, numObs)

# Here we create a dummy indicating when a donor did not engaged in any observable action
# to be updated during the simulation
YInact <- rep(1, numObs) # 1: not engaged in an observable action, 0: engaged in an observable action

# number of revision options
numCatRev <- 4 # 1 = terminate, 2 = downgrade, 3 = keep as is, 4 = upgrade

# create dummy variables indicating the choice of the revision decision
library(dplyr)
library(tidyr)
dat <- dat %>% 
  mutate(rev_1 = 0, 
         rev_2 = 0, 
         rev_3 = 0, 
         rev_4 = 0)

revMat <- dat %>%
  select(rev_1, rev_2, rev_3, rev_4) %>%
  as.matrix()
#----------------------------------------------------------------------



# Empty vectors to store utilities
# for the behaviors
V1 <- rep(0, numObs)
V2 <- rep(0, numObs)
V3 <- rep(0, numObs)
V4 <- rep(0, numObs)
V6 <- rep(0, numObs)

# for the transitions
# for noncontractual donors
# a) active state
VTrans1 <- rep(0, numObs)
# b) dormant state
VTrans2 <- rep(0, numObs)


# initial states: we assume that all donors begin in the active state
omegaVec <- t(data.matrix(c(1, 0))) # a row vector



fullIDList <- unique(dat$ID) # full ID list in the whole simulation period
lID <- length(fullIDList) # number of unique customers

orderIDList <- split(x = 1:lID, f = 1:lID)





# reset values of IVs
XNonconI[, c("CR", "CRAmount_nonlog",
             "churnedDummy", 
             "relationshipLen")] <- 0
XNonconA[, c("CR", "CRAmount_nonlog",
             "churnedDummy", 
             "relationshipLen")] <- 0
XConI[, c("churnedDummy", 
          "relationshipLen")] <- 0
XConA[, c("churnedDummy", 
          "relationshipLen")] <- 0
XRev[, c("CRAmount_nonlog",
         "relationshipLen")] <- 0
XTrans[, c("relationshipLen")] <- 0




# load the parameter estimates of our main model
model_results <- readRDS("XXX.rds")
parEst <- model_results$par


# create a vector of regression coefficients associated with selected predictors
temp <- 0
fixedBeta1 <- parEst[(temp+1):(temp+lenFixedPar1)]
# add 1 here since we don't use the Mundlak term
temp <- temp + lenFixedPar1 + 1 

randBeta1_mu <- parEst[(temp+1):(temp+lenRandPar1)] # mean of random parameters
temp <- temp + lenRandPar1

randBeta1_sigma <- parEst[(temp+1):(temp+lenRandPar1)] # sigma of random parameters
temp <- temp + lenRandPar1


fixedBeta2 <- parEst[(temp+1):(temp+lenFixedPar2)]
temp <- temp + lenFixedPar2 + 1

randBeta2_mu <- parEst[(temp+1):(temp+lenRandPar2)] # mean of random parameters
temp <- temp + lenRandPar2

randBeta2_sigma <- parEst[(temp+1):(temp+lenRandPar2)] # sigma of random parameters
temp <- temp + lenRandPar2


fixedBeta3 <- parEst[(temp+1):(temp+lenFixedPar3)]
temp <- temp + lenFixedPar3 + 1

randBeta3_mu <- parEst[(temp+1):(temp+lenRandPar3)] # mean of random parameters
temp <- temp + lenRandPar3

randBeta3_sigma <- parEst[(temp+1):(temp+lenRandPar3)] # sigma of random parameters
temp <- temp + lenRandPar3



fixedBeta4 <- parEst[(temp+1):(temp+lenFixedPar4)]
temp <- temp + lenFixedPar4 + 1

randBeta4_mu <- parEst[(temp+1):(temp+lenRandPar4)] # mean of random parameters
temp <- temp + lenRandPar4

randBeta4_sigma <- parEst[(temp+1):(temp+lenRandPar4)] # sigma of random parameters
temp <- temp + lenRandPar4


fixedBeta6 <- parEst[(temp+1):(temp+lenFixedPar6)]
temp <- temp + lenFixedPar6 + 1

randBeta6_mu <- parEst[(temp+1):(temp+lenRandPar6)] # mean of random parameters
temp <- temp + lenRandPar6

randBeta6_sigma <- parEst[(temp+1):(temp+lenRandPar6)] # sigma of random parameters
temp <- temp + lenRandPar6


# correlation between intercepts of 5 equations across all states
# these are not state-specific
rho_itc <- parEst[(temp+1):(temp+sum(1:(5-1)))]
temp <- temp + sum(1:(5-1))



# this is the standard deviations of the original error terms of the amount equations
sigma <- exp(parEst[(temp+1):(temp+2)]) # one sigma for noncontractual, one for contractual
temp <- temp + 2

# we assume the error term of the selection equation follows standard normal distribution
# in order to make the model identified

# in order to estimate the correlation between the two error terms, we use tanh transformation
# to make sure that the correlation is between -1 and 1
atanhrho <- parEst[(temp+1):(temp+2)] # one for noncontractual, one for contractual
temp <- temp + 2

rho <- tanh(atanhrho)


# thresholds of the ordered probit (revision decision)
revThresholds <- c(0, cumsum(exp(parEst[(temp+1):(temp+numCatRev-2)]))) # numCatRev - 2 thresholds to be estimated
# cumsum and exp functions make sure that the thresholds are all positive and are increasing
temp <- temp + numCatRev-2


# model the amount changed when a donor revises his/her contract
sdLogRev <- exp(parEst[(temp+1):(temp+1)]) # one sd for both upgrade and downgrade: 
temp <- temp + 1

meanLogRev <- parEst[(temp+1):(temp+2)] # two means: one for upgrade, one for downgrade
temp <- temp + 2

# Note that we model the absolute change of the amount (upgrade: log(Ynew/Yold) and downgrade: -log(Ynew/Yold)) to follow a log normal distribution




# Copula terms
d <- parEst[(temp+1):(temp+5)] # 1 components used to compute the gaussian copula x 5 equations = 5 pars
temp <- temp + 5

# We use tanh function to make sure that this component is between -1 and 1 
# since it is the correlation between the endogenous variable and the original error term

d41 <- tanh(d)
d44 <- sqrt(1 - d41^2)




# sigma of the new error term - amount equations:
sigma_new <- sigma*d44[c(2, 4)]





# TRANSITION MATRIX
# Note: one transition matrix for both noncontractual donors and contractual donors

# parameters for the latent attrition (state-invariant): same parameters for both states
# except for the means of the intercepts that are state specific
# SD of the intercept is also not state-specific
# a) active state
fixedDelta1 <- parEst[(temp+1):(temp+lenFixedParTrans)]
temp <- temp + lenFixedParTrans + 1

randDelta1_mu <- parEst[(temp+1):(temp+lenRandParTrans)] # mean of random parameters
temp <- temp + lenRandParTrans

randDelta1_sigma <- parEst[(temp+1):(temp+lenRandParTrans)] # sigma of random parameters
temp <- temp + lenRandParTrans


# b) dormant state
fixedDelta2 <- fixedDelta1

randDelta2_mu <- parEst[(temp+1):(temp+lenRandParTrans)] # mean of random parameters
temp <- temp + lenRandParTrans

randDelta2_sigma <- randDelta1_sigma 



dTrans <- parEst[(temp+1):(temp+1)] # 1 component used to compute the gaussian copula for both states
temp <- temp + 1

dTrans41 <- tanh(dTrans)
dTrans44 <- sqrt(1-dTrans^2)



# initial states: we assume that all donors begin in the active state
omegaVec <- t(data.matrix(c(1, 0))) # a row vector




# Use population means for the random parameters
beta1 <- data.matrix(c(fixedBeta1, 
                       randBeta1_mu))
beta2 <- data.matrix(c(fixedBeta2, 
                       randBeta2_mu))
beta3 <- data.matrix(c(fixedBeta3, 
                       randBeta3_mu))
beta4 <- data.matrix(c(fixedBeta4, 
                       randBeta4_mu))
beta6 <- data.matrix(c(fixedBeta6, 
                       randBeta6_mu))

delta1 <- data.matrix(c(fixedDelta1, 
                        randDelta1_mu))
delta2 <- data.matrix(c(fixedDelta2, 
                        randDelta2_mu))


# generate correlated error terms (between incidence and amount models)
covMatNoncon <- matrix(nrow = 2, ncol = 2)
covMatNoncon[!upper.tri(covMatNoncon)] <- c(1, rho[1]*sigma_new[1], sigma_new[1]^2)
covMatNoncon[upper.tri(covMatNoncon)] <- covMatNoncon[lower.tri(covMatNoncon)]

covMatCon <- matrix(nrow = 2, ncol = 2)
covMatCon[!upper.tri(covMatCon)] <- c(1, rho[2]*sigma_new[2], sigma_new[2]^2)
covMatCon[upper.tri(covMatCon)] <- covMatCon[lower.tri(covMatCon)]


# we create a set of four error terms which follow a multivariate normal distribution
# and have means of zero and covariance-variance matrix R
library(MASS)
set.seed(300913)
eMatrixNoncon <- mvrnorm(n = numObs,
                         mu = rep(0, 2),
                         Sigma = covMatNoncon,
                         empirical = T)

eMatrixCon <- mvrnorm(n = numObs,
                      mu = rep(0, 2),
                      Sigma = covMatCon,
                      empirical = T)







# true parameters (used for checking later)
trueParms <- c(beta1, beta2, beta3, beta4, beta6, 
                  log(sigma_new), atanhrho, 
                  log(diff(revThresholds)), 
                  log(sdLogRev), 
                  meanLogRev, 
                  delta1, 
                  delta2[lenParTrans])

# export the results to rds file
# saveRDS(trueParms, "trueParms.rds")

# set the seed for reproducibility
set.seed(3009)
library(truncnorm)
for(id in fullIDList){
  # used for indexing
  cumTotMon <- cumTotMonList[[which(fullIDList == id)]]
  monVec <- monList[[which(fullIDList == id)]]
  numRows <- length(monList[[which(fullIDList == id)]])
  
  
  # revision incidence decision
  lowerBoundRev <- matrix(c(-Inf, revThresholds),
                          nrow = numRows,
                          ncol = numCatRev,
                          byrow = T) # lower threshold levels
  upperBoundRev <- matrix(c(revThresholds, Inf),
                          nrow = numRows,
                          ncol = numCatRev,
                          byrow = T) # upper threshold level
  
  
  # timeline for simulation
  timeHorizon <- sort(unique(dat$Month))
  for(t in timeHorizon){
    # used for indexing
    indexMon <- cumTotMon + which(monVec == t)
    
    # first month of the simulation horizon
    if(t == min(timeHorizon)){
      
      # probabilities of being active/dormant
      probVec[indexMon, ] <- omegaVec
      
      
      
      # update relationship length
      relationshipStarted <- 0
      XNonconI[indexMon, c("relationshipLen")] <- 0
      XNonconA[indexMon, c("relationshipLen")] <- 0
      XConI[indexMon, c("relationshipLen")] <- 0
      XConA[indexMon, c("relationshipLen")] <- 0
      XRev[indexMon, c("relationshipLen")] <- 0
      XTrans[indexMon, c("relationshipLen")] <- 0
      
      # update churned-in-the-past dummy
      XNonconI[indexMon, c("churnedDummy")] <- 0
      XNonconA[indexMon, c("churnedDummy")] <- 0
      XConI[indexMon, c("churnedDummy")] <- 0
      XConA[indexMon, c("churnedDummy")] <- 0
      
      
      
      
      # UTILITY COMPUTATION
      # 2) contractual
      V3[indexMon] <- XConI[indexMon,] %*% beta3
      V4[indexMon] <- XConA[indexMon,] %*% beta4
      
      
      # simulate contractual donation decisions
      YCon[indexMon] <- rbinom(n = 1, size = 1, prob = pnorm(V3[indexMon]+eMatrixCon[indexMon, 1])*probVec[indexMon, 1])
      ACon[indexMon] <- if_else(YCon[indexMon]==0, 0, exp(V4[indexMon]+eMatrixCon[indexMon, 2]))
      
      
      # update engaging-in-any-observable-behavior dummy
      YInact[indexMon] <- (1-YCon[indexMon])*YInact[indexMon]
      
      # update contractual relationship dummy
      XNonconI[indexMon, c("CR")] <- YCon[indexMon]
      XNonconA[indexMon, c("CR")] <- YCon[indexMon]
      
      # update contractual amounts
      XNonconI[indexMon, c("CRAmount_nonlog")] <- ACon[indexMon]
      XNonconA[indexMon, c("CRAmount_nonlog")] <- ACon[indexMon]
      XRev[indexMon, c("CRAmount_nonlog")] <- ACon[indexMon]
      
      
      # update observations for contractual donation/revision decisions
      conSample[indexMon] <- 1
      conSample[indexMon + 1] <- 1 - YCon[indexMon]
      revSample[indexMon] <- 0
      revSample[indexMon + 1] <- YCon[indexMon]
      
      
      # update relationship length
      if(YCon[indexMon] == 1){
        relationshipStarted <- 1
        XNonconI[indexMon, c("relationshipLen")] <- 1
        XNonconA[indexMon, c("relationshipLen")] <- 1
        XConI[indexMon, c("relationshipLen")] <- 1
        XConA[indexMon, c("relationshipLen")] <- 1
        XRev[indexMon, c("relationshipLen")] <- 1
        XTrans[indexMon, c("relationshipLen")] <- 1
      } else{
        relationshipStarted <- 0
        XNonconI[indexMon, c("relationshipLen")] <- 0
        XNonconA[indexMon, c("relationshipLen")] <- 0
        XConI[indexMon, c("relationshipLen")] <- 0
        XConA[indexMon, c("relationshipLen")] <- 0
        XRev[indexMon, c("relationshipLen")] <- 0
        XTrans[indexMon, c("relationshipLen")] <- 0
      }
      
      
      # update revision decision results
      # donors cannot revise their contracts in the first months when they have just started them
      YRev[indexMon] <- NA
      ARev[indexMon] <- NA
      
    } # from the second month of the simulation period
    else{
     
      # update relationship length
      if(relationshipStarted == 1){
        XNonconI[indexMon, c("relationshipLen")] <- XNonconI[indexMon-1, c("relationshipLen")]+1
        XNonconA[indexMon, c("relationshipLen")] <- XNonconA[indexMon-1, c("relationshipLen")]+1
        XConI[indexMon, c("relationshipLen")] <- XConI[indexMon-1, c("relationshipLen")]+1
        XConA[indexMon, c("relationshipLen")] <- XConA[indexMon-1, c("relationshipLen")]+1
        XRev[indexMon, c("relationshipLen")] <- XRev[indexMon-1, c("relationshipLen")]+1
        XTrans[indexMon, c("relationshipLen")] <- XTrans[indexMon-1, c("relationshipLen")]+1
      } else{
        XNonconI[indexMon, c("relationshipLen")] <- 0
        XNonconA[indexMon, c("relationshipLen")] <- 0
        XConI[indexMon, c("relationshipLen")] <- 0
        XConA[indexMon, c("relationshipLen")] <- 0
        XRev[indexMon, c("relationshipLen")] <- 0
        XTrans[indexMon, c("relationshipLen")] <- 0
      }
      
      
      # update churned-in-the-past dummy
      XNonconI[indexMon, c("churnedDummy")] <- XNonconI[indexMon-1, c("churnedDummy")]
      XNonconA[indexMon, c("churnedDummy")] <- XNonconA[indexMon-1, c("churnedDummy")]
      XConI[indexMon, c("churnedDummy")] <- XConI[indexMon-1, c("churnedDummy")]
      XConA[indexMon, c("churnedDummy")] <- XConA[indexMon-1, c("churnedDummy")]
      
      
      # Step 1: calculate the the probabilities of being in different latent states
      
      # UTILITY COMPUTATION - TRANSITION MATRIX
      # a) active state
      VTrans1[indexMon] <- XTrans[indexMon,] %*% delta1
      
      # b) dormant state
      VTrans2[indexMon] <- XTrans[indexMon,] %*% delta2
      
     
      
      # transition matrix: a 2x2 matrix
      # see Schweidel and Knox 2013 (p. 476)
      qMat <- matrix(0, nrow = 2, ncol = 2) # we store a 2x2 matrix in one row (row by row)
      
      # probability that donor i becomes active
      qMat[1, 1] <- pnorm(VTrans1[indexMon], lower.tail = 1, log.p = 0, sd = 1) # previous state: active
      qMat[2, 1] <- pnorm(VTrans2[indexMon], lower.tail = 1, log.p = 0, sd = 1) # previous state: dormant
      
      # probability that donor i becomes dormant
      qMat[1, 2] <- 1 - qMat[1, 1] # previous state: active
      qMat[2, 2] <- 1 - qMat[2, 1] # previous state: dormant
      
      
      
      
      # probabilities of being active/dormant
      # taking into account the states in previous months
      if(YInact[indexMon-1] == 0){
        # Here we observed an action in previous month
        # so we knew for sure that the donor was active
        probVec[indexMon-1, ] <- c(1, 0)
        
        
      } else{
        # if we didn't observe any action
        # then the donor was either active or dormant
        
      }
      
      probVec[indexMon, ] <- probVec[indexMon-1, ] %*% qMat
      
      
      
      
      # Step 2: draw contractual/revision decision
      
      # If no contract has been started
      if(conSample[indexMon] == 1){
        # UTILITY COMPUTATION
        # 2) contractual
        V3[indexMon] <- XConI[indexMon,] %*% beta3
        V4[indexMon] <- XConA[indexMon,] %*% beta4
        
        # simulate contractual donation decisions
        YCon[indexMon] <- rbinom(n = 1, size = 1, prob = pnorm(V3[indexMon]+eMatrixCon[indexMon, 1])*probVec[indexMon, 1])
        ACon[indexMon] <- if_else(YCon[indexMon]==0, 0, exp(V4[indexMon]+eMatrixCon[indexMon, 2]))
        
        
        # update engaging-in-any-observable-behavior dummy
        YInact[indexMon] <- (1-YCon[indexMon])*YInact[indexMon]
        
        # update contractual relationship dummy
        XNonconI[indexMon, c("CR")] <- YCon[indexMon]
        XNonconA[indexMon, c("CR")] <- YCon[indexMon]
        
        # update contractual amounts
        XNonconI[indexMon, c("CRAmount_nonlog")] <- ACon[indexMon]
        XNonconA[indexMon, c("CRAmount_nonlog")] <- ACon[indexMon]
        XRev[indexMon, c("CRAmount_nonlog")] <- ACon[indexMon]
        
        if(t < max(timeHorizon)){
          # update observations for contractual donation/revision decisions
          conSample[indexMon + 1] <- 1 - YCon[indexMon]
          revSample[indexMon + 1] <- YCon[indexMon]
          
          
        }
        
        
        # update observations for revision decision
        revSample[indexMon] <- 0
        
        
        # update relationship length
        if(relationshipStarted == 0 &
           YCon[indexMon] == 1){
          relationshipStarted <- 1
          XNonconI[indexMon, c("relationshipLen")] <- 1
          XNonconA[indexMon, c("relationshipLen")] <- 1
          XConI[indexMon, c("relationshipLen")] <- 1
          XConA[indexMon, c("relationshipLen")] <- 1
          XRev[indexMon, c("relationshipLen")] <- 1
          XTrans[indexMon, c("relationshipLen")] <- 1
        }
        
        
        # update revision decision results
        # donors cannot revise their contracts in the first months when they have just started them
        YRev[indexMon] <- NA
        ARev[indexMon] <- NA
        
        
      }
      # if a donor is having a contract or has "recently" terminated a contract and cannot enter a contract now
      else{
        
        
        
        # if the donor was contractual in the last period (so he/she is in a contract now)
        if(revSample[indexMon] == 1){
          
          # if this is the second month of a contract, then they are not allowed to terminate yet
          # therefore, we update the contractual dummy and amount as last month
          if(conSample[indexMon-1] == 1){
            # update contractual relationship dummy: 
            XNonconI[indexMon, c("CR")] <- XNonconI[indexMon-1, c("CR")]
            XNonconA[indexMon, c("CR")] <- XNonconA[indexMon-1, c("CR")]
            YCon[indexMon] <- YCon[indexMon-1]
            
            # update contractual amounts: 
            XNonconI[indexMon, c("CRAmount_nonlog")] <- XNonconI[indexMon-1, c("CRAmount_nonlog")]
            XNonconA[indexMon, c("CRAmount_nonlog")] <- XNonconA[indexMon-1, c("CRAmount_nonlog")]
            XRev[indexMon, c("CRAmount_nonlog")] <- XRev[indexMon-1, c("CRAmount_nonlog")]
            ACon[indexMon] <- ACon[indexMon-1]
          }
          
          
          
          
          
          
          # UTILITY COMPUTATION
          # 3) revision
          V6[indexMon] <- XRev[indexMon,] %*% beta6
          
          
          # simulate revision decisions
          phiRev2 <- pnorm(upperBoundRev[which(timeHorizon == t),] - V6[indexMon], log.p = 0, sd = 1)
          phiRev1 <- pnorm(lowerBoundRev[which(timeHorizon == t),] - V6[indexMon], log.p = 0, sd = 1)
          
          revProb <- as.numeric(phiRev2 - phiRev1)
          
          
          
          # prob of revision decisions (except for keeping as is) conditional upon being active
          revProb[-3] <- revProb[-3]*probVec[indexMon, 1]
          # re-calculate probability of keeping as is
          revProb[3] <- 1 - sum(revProb[-3])
          
          YRev[indexMon] <- sample(x = 1:numCatRev, 
                                   size = 1, 
                                   prob = revProb)
          
          
          # IMPORTANT:
          # Note that the revision decision only influences the contractual dummy/amount "next" month
          
          # when t is not the last month
          if(t < max(timeHorizon)){
            # update contractual relationship dummy: 
            XNonconI[indexMon+1, c("CR")] <- if_else(YRev[indexMon] == 1, 0, 1)
            XNonconA[indexMon+1, c("CR")] <- if_else(YRev[indexMon] == 1, 0, 1)
            YCon[indexMon+1] <- if_else(YRev[indexMon] == 1, 0, 1)
            
            # revision amount decision 1 = terminate, 2 = downgrade, 3 = keep as is, 4 = upgrade
            if(YRev[indexMon] == 1){ # 1 = terminate
              # update upgrade/downgrade amount change in the same month
              ARev[indexMon] <- 0
              
              # update engaging-in-any-observable-behavior dummy
              YInact[indexMon] <- 0
              
              # update contractual amounts next month
              XNonconI[indexMon+1, c("CRAmount_nonlog")] <- 0
              XNonconA[indexMon+1, c("CRAmount_nonlog")] <- 0
              XRev[indexMon+1, c("CRAmount_nonlog")] <- 0
              ACon[indexMon+1] <- 0
              
              
              
              # For simplicity purposes, we assume that all contracts are monthly
              # Thus, if the contract is terminated, we need to wait 3*1 = 3 extra months before the donor can start another contract
              # also no noncontractual donations can be made in this period
              
              # update observations for contractual donation/revision decisions
              conSample[(indexMon + 1):
                          (ifelse((t+3) < max(timeHorizon), indexMon+3, indexMon+max(timeHorizon)-t))] <- 0
              revSample[(indexMon + 1):
                          (ifelse((t+3) < max(timeHorizon), indexMon+3, indexMon+max(timeHorizon)-t))] <- 0
              
              
              # update observations for noncontractual decision
              nonconSample[(indexMon + 1):
                             (ifelse((t+3) < max(timeHorizon), indexMon+3, indexMon+max(timeHorizon)-t))] <- 0
              
              
              # after that, it is possible to make (non)contractual decisions again
              if((t+4) <= max(timeHorizon)){
                # update observations for contractual decisions
                conSample[(indexMon + 4)] <- 1
                
                
                # update observations for noncontractual decisions
                nonconSample[(indexMon + 4)] <- 1
                
              }
              
              
              # update churned-in-the-past dummy
              XNonconI[indexMon, c("churnedDummy")] <- 1
              XNonconA[indexMon, c("churnedDummy")] <- 1
              XConI[indexMon, c("churnedDummy")] <- 1
              XConA[indexMon, c("churnedDummy")] <- 1
              
              
              
              
            } else if(YRev[indexMon] == 2){ # 2 = downgrade
              # update upgrade/downgrade amount change in the same month
              ARev[indexMon] <- -rlnorm(n=1, 
                                        meanlog = meanLogRev[2], 
                                        sdlog = sdLogRev)
              
              # update engaging-in-any-observable-behavior dummy
              YInact[indexMon] <- 0
              
              # update contractual amounts next month
              XNonconI[indexMon+1, c("CRAmount_nonlog")] <- XNonconI[indexMon, c("CRAmount_nonlog")]*exp(ARev[indexMon])
              XNonconA[indexMon+1, c("CRAmount_nonlog")] <- XNonconA[indexMon, c("CRAmount_nonlog")]*exp(ARev[indexMon])
              XRev[indexMon+1, c("CRAmount_nonlog")] <- XRev[indexMon, c("CRAmount_nonlog")]*exp(ARev[indexMon])
              ACon[indexMon+1] <- ACon[indexMon]*exp(ARev[indexMon])
              
              # update observations for contractual donation/revision decisions
              conSample[(indexMon + 1)] <- 0
              revSample[(indexMon + 1)] <- 1
              
              # update observations for noncontractual decision
              nonconSample[(indexMon + 1)] <- 1
              
              
            } else if(YRev[indexMon] == 3){ # 3 = keep as is
              # update upgrade/downgrade amount change in the same month
              ARev[indexMon] <- 0
              
              # update engaging-in-any-observable-behavior dummy
              YInact[indexMon] <- YInact[indexMon]
              
              # update contractual amounts next month
              XNonconI[indexMon+1, c("CRAmount_nonlog")] <- XNonconI[indexMon, c("CRAmount_nonlog")]
              XNonconA[indexMon+1, c("CRAmount_nonlog")] <- XNonconA[indexMon, c("CRAmount_nonlog")]
              XRev[indexMon+1, c("CRAmount_nonlog")] <- XRev[indexMon, c("CRAmount_nonlog")]
              ACon[indexMon+1] <- ACon[indexMon]
              
              # update observations for contractual donation/revision decisions
              conSample[(indexMon + 1)] <- 0
              revSample[(indexMon + 1)] <- 1
              
              # update observations for noncontractual decision
              nonconSample[(indexMon + 1)] <- 1
              
            } else if(YRev[indexMon] == 4){ # 4 = upgrade
              # update upgrade/downgrade amount change in the same month
              ARev[indexMon] <- rlnorm(n=1, 
                                       meanlog = meanLogRev[1], 
                                       sdlog = sdLogRev)
              
              # update engaging-in-any-observable-behavior dummy
              YInact[indexMon] <- 0
              
              # update contractual amounts next month
              XNonconI[indexMon+1, c("CRAmount_nonlog")] <- XNonconI[indexMon, c("CRAmount_nonlog")]*exp(ARev[indexMon])
              XNonconA[indexMon+1, c("CRAmount_nonlog")] <- XNonconA[indexMon, c("CRAmount_nonlog")]*exp(ARev[indexMon])
              XRev[indexMon+1, c("CRAmount_nonlog")] <- XRev[indexMon, c("CRAmount_nonlog")]*exp(ARev[indexMon])
              ACon[indexMon+1] <- ACon[indexMon]*exp(ARev[indexMon])
              
              # update observations for contractual donation/revision decisions
              conSample[(indexMon + 1)] <- 0
              revSample[(indexMon + 1)] <- 1
              
              # update observations for noncontractual decision
              nonconSample[(indexMon + 1)] <- 1
              
            }
            
            
            
          } 
          # when t is the last month
          else{
            
            # revision amount decision 1 = terminate, 2 = downgrade, 3 = keep as is, 4 = upgrade
            if(YRev[indexMon] == 1){ # 1 = terminate
              # update upgrade/downgrade amount change in the same month
              ARev[indexMon] <- 0
              
              
              # update engaging-in-any-observable-behavior dummy
              YInact[indexMon] <- 0
              
              # update churned-in-the-past dummy
              XNonconI[indexMon, c("churnedDummy")] <- 1
              XNonconA[indexMon, c("churnedDummy")] <- 1
              XConI[indexMon, c("churnedDummy")] <- 1
              XConA[indexMon, c("churnedDummy")] <- 1
              
              
            } else if(YRev[indexMon] == 2){ # 2 = downgrade
              # update upgrade/downgrade amount change in the same month
              ARev[indexMon] <- -rlnorm(n=1, 
                                        meanlog = meanLogRev[2], 
                                        sdlog = sdLogRev)
              
              # update engaging-in-any-observable-behavior dummy
              YInact[indexMon] <- 0
              
            } else if(YRev[indexMon] == 3){ # 3 = keep as is
              # update upgrade/downgrade amount change in the same month
              ARev[indexMon] <- 0
              
              # update engaging-in-any-observable-behavior dummy
              YInact[indexMon] <- YInact[indexMon]
              
            } else if(YRev[indexMon] == 4){ # 4 = upgrade
              # update upgrade/downgrade amount change in the same month
              ARev[indexMon] <- rlnorm(n=1, 
                                       meanlog = meanLogRev[1], 
                                       sdlog = sdLogRev)
              
              # update engaging-in-any-observable-behavior dummy
              YInact[indexMon] <- 0
            }
            
            
            
          }
          
        }
        # if the donor has recently terminated a contract and no donation can be made in this period by definition (i.e., 3 months after terminating)
        else{
          # update contractual relationship dummy: 
          XNonconI[indexMon, c("CR")] <- 0
          XNonconA[indexMon, c("CR")] <- 0
          YCon[indexMon] <- 0
          
          # update contractual amounts: 
          XNonconI[indexMon, c("CRAmount_nonlog")] <- 0
          XNonconA[indexMon, c("CRAmount_nonlog")] <- 0
          XRev[indexMon, c("CRAmount_nonlog")] <- 0
          ACon[indexMon] <- 0
        }
        
        
      }
      
      
      
      
      
    }
    
    # if it is possible to make noncontractual donations in the current period
    if(nonconSample[indexMon] == 1){
      # UTILITY COMPUTATION
      # 1) noncontractual
      V1[indexMon] <- XNonconI[indexMon,] %*% beta1
      V2[indexMon] <- XNonconA[indexMon,] %*% beta2
      
     
      # simulate noncontractual donation decisions
      # perform several runs
      # later we can compute mean value over runs
      YNoncon[indexMon] <- rbinom(n = 1, size = 1, prob = pnorm(V1[indexMon]+eMatrixNoncon[indexMon, 1])*probVec[indexMon,1])
      ANoncon[indexMon] <- if_else(YNoncon[indexMon]==0,0,exp(V2[indexMon]+eMatrixNoncon[indexMon, 2]))
      
      # update engaging-in-any-observable-behavior dummy
      YInact[indexMon] <- (1-YNoncon[indexMon])*YInact[indexMon]
      
      # update relationship length
      if(relationshipStarted == 0 &
         YNoncon[indexMon] == 1){
        relationshipStarted <- 1
        XNonconI[indexMon, c("relationshipLen")] <- 1
        XNonconA[indexMon, c("relationshipLen")] <- 1
        XConI[indexMon, c("relationshipLen")] <- 1
        XConA[indexMon, c("relationshipLen")] <- 1
        XRev[indexMon, c("relationshipLen")] <- 1
        XTrans[indexMon, c("relationshipLen")] <- 1
      }
      
      
    } else{ # if it is not possible to make any noncontractual donations (e.g., during the 3 months after terminating)
      # update noncontractual amounts
      YNoncon[indexMon] <- NA
      ANoncon[indexMon] <- 0
    }
    
    
  }
  
}



library(tidyr)
library(dplyr)

dat[, parTrans] <- XTrans
dat[, par1] <- XNonconI

dat$nonConY <- YNoncon
dat$nonConA <- dplyr::if_else(dat$nonConY == 0, 0, log(ANoncon))
dat$CR <- YCon
dat$CRAmount <- dplyr::if_else(dat$CR == 0, 0, log(ACon))
dat$revY <- YRev
dat$lnAChanged <- ARev

dat$churnY <- as.numeric(dat$revY == 1)
dat$upgradeY <- as.numeric(dat$revY == 4)
dat$downgradeY <- as.numeric(dat$revY == 2)



# sample dummy
dat$nonconDecisionDummy <- as.numeric(nonconSample)
dat$conDecisionDummy <- as.numeric(conSample)
dat$revDecisionDummy <- as.numeric(revSample)

dat$churnDecisionDummy <- as.numeric(revSample)

dat[, c(paste0("rev_", 1:4))] <- revMat


# first transaction's month
if(length(which(colnames(dat) == "firstTransactMon")) > 0){
  dat <- dat[, -which(colnames(dat) == "firstTransactMon")]
}


library(dplyr)
dat <- dat %>% group_by(ID) %>%
  filter(nonConY == 1 |
           CR == 1) %>%
  summarise(minMon = min(Month)) %>%
  rename(firstTransactMon = minMon) %>%
  right_join(dat, by = c("ID")) %>%
  ungroup() %>%
  arrange(ID, Month)



# save the simulated data
saveRDS(dat, "simulated_data_model_identification.rds")



