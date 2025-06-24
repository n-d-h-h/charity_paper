# list of external packages needed
packages_list <- c("dplyr", "tidyr", "sfsmisc", "matrixStats", "stats4")

# check and install packages if missing
for(p in packages_list){
  if(!require(p, character.only = T)){
    install.packages(p, dependencies = TRUE)
  } else{
    library(p, character.only = T)
  }
}




#-----------------
# MODEL ESTIMATION
#-----------------

# load the dataset
dat <- readRDS("XXXX.rds")


IDList <- unique(dat$ID[dat$nonconDecisionDummy == 1]) # list of unique customers
lID <- length(IDList) # number of unique customers



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
               "relationshipLen",
               "MundlakDMNonconI") # fixed

randPar1 <- c("intercept",
              "DM") # random, donor-specific


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
               "relationshipLen",
               "MundlakDMNonconA") 

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
               "donStock", 
               "hhSize", "hhIncome",
               "relationshipLen",
               "MundlakDMConI") # fixed 

randPar3 <- c("intercept",
              "DM") # random, donor-specific

# number of parameters
lenFixedPar3 <- length(fixedPar3) 
lenRandPar3 <- length(randPar3)

par3 <- c(fixedPar3, randPar3)
lenPar3 <- length(par3) # number of parameters

# amount model & contractual donation
fixedPar4 <- c("quarter2", "quarter3", "quarter4",
               "churnedDummy",
               "donStock", 
               "hhSize", "hhIncome",
               "relationshipLen",
               "MundlakDMConA") 

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
               "relationshipLen",
               "MundlakDMRev") 

randPar6 <- c("intercept",
              "DM")


# number of parameters
lenFixedPar6 <- length(fixedPar6) 
lenRandPar6 <- length(randPar6)

par6 <- c(fixedPar6, randPar6)
lenPar6 <- length(par6) # number of parameters



# Variables explaining latent attrition (donor i being active or not at time t)
fixedParTrans <- c("DM_lag",
                   "quarter2", "quarter3", "quarter4",
                   "donStock",
                   "hhSize", "hhIncome",
                   "relationshipLen",
                   "MundlakDM_lag")
randParTrans <- c("intercept") 


# number of parameters
lenFixedParTrans <- length(fixedParTrans)
lenRandParTrans <- length(randParTrans)

parTrans <- c(fixedParTrans, randParTrans)
lenParTrans <- length(parTrans)




# create a list, each element contains all estimation months for each donor
monList <- list()
monList <- split(dat$Month[dat$nonconDecisionDummy == 1], f = dat$ID[dat$nonconDecisionDummy == 1])


# create an index to subset the utility matrix for response equation of each donor
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


# DV for each equation
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




# Added regressors to correct for endogeneity using the Copula approach
library(dplyr)
# Direct Marketing
copDMNonconI <- dat %>% filter(nonconDecisionDummy == 1) %>% replace(is.na(.), 0) %>% dplyr::select(CopulaDMNonconI) %>% as.matrix()
copDMNonconA <- dat %>% filter(nonconDecisionDummy == 1) %>% replace(is.na(.), 0) %>% dplyr::select(CopulaDMNonconA) %>% as.matrix()
copDMConI <- dat %>% filter(nonconDecisionDummy == 1) %>% replace(is.na(.), 0) %>% dplyr::select(CopulaDMConI) %>% as.matrix()
copDMConA <- dat %>% filter(nonconDecisionDummy == 1) %>% replace(is.na(.), 0) %>% dplyr::select(CopulaDMConA) %>% as.matrix()
copDM_lag <- dat %>% filter(nonconDecisionDummy == 1) %>% replace(is.na(.), 0) %>% dplyr::select(CopulaDM_lag) %>% as.matrix()
copDMRev <- dat %>% filter(nonconDecisionDummy == 1) %>% replace(is.na(.), 0) %>% dplyr::select(CopulaDMRev) %>% as.matrix()



# create an index of first observations for noncontractual donations
# that will be used to rescale the total probablity of the first observation of each donor
indexFirstDon <- dat$Month[dat$nonconDecisionDummy == 1] == dat$firstTransactMon[dat$nonconDecisionDummy == 1]



# Here we create a dummy indicating when a donor did not engaged in any observable action
# 1: not engaged in an observable action, 0: engaged in an observable action
YInact <- rep(0, nrow(dat[dat$nonconDecisionDummy == 1, ]))

YInact[dat$nonConY[dat$nonconDecisionDummy == 1] == 0 & # not donating adhoc
         (dat$CR[dat$nonconDecisionDummy == 1]*dat$conDecisionDummy[dat$nonconDecisionDummy == 1]) == 0 & # not start any new contract
         (dat$upgradeY[dat$nonconDecisionDummy == 1]*dat$revDecisionDummy[dat$nonconDecisionDummy == 1]) == 0 & # not upgrading
         (dat$downgradeY[dat$nonconDecisionDummy == 1]*dat$revDecisionDummy[dat$nonconDecisionDummy == 1]) == 0 & # not downgrading
         (dat$churnY[dat$nonconDecisionDummy == 1]*dat$churnDecisionDummy[dat$nonconDecisionDummy == 1]) == 0] <- 1 # not terminating





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




library(dplyr)
revMat <- dat %>%
  dplyr::filter(nonconDecisionDummy == 1) %>%
  select(rev_1, rev_2, rev_3, rev_4) %>%
  as.matrix()




# number of random parameters
nrRandParTrans <- lenRandParTrans*2 # random parameters of the transition matrix equation (x 2 states (dormant vs. active states))

nrRandParRes <- lenRandPar1 + lenRandPar2 + lenRandPar3 + lenRandPar4 +
  lenRandPar6 # random parameters in response models

nrRandPar <- nrRandParTrans + nrRandParRes



# Draw halton sequences with nrRandPar dimensions. 
library(sfsmisc)
reps <- 100
 
haltonSeq <- QUnif(n=lID*reps, min=0, max=1, n.min=400, p=nrRandPar, leap=409) 
# n is equal to the length of the sequence 
# p is the number of sequences we want to generate
# n.min is used to remove the initial numbers in the sequence in order to avoid strong correlation between sequences
# leap is used to avoid strong correlation: only every leap-th entry should be taken
haltonSeq <- matrix(qnorm(haltonSeq), nrow = lID*reps, ncol = nrRandPar)
haltonSeq <- t(haltonSeq)




haltonSeqTrans <- haltonSeq[1:nrRandParTrans,]
temp <- 0
haltonSeq1Trans <- haltonSeqTrans[(temp+1):(temp+lenRandParTrans),]
temp <- temp+lenRandParTrans

haltonSeq2Trans <- haltonSeqTrans[(temp+1):(temp+lenRandParTrans),]
temp <- temp+lenRandParTrans



haltonSeqRes <- haltonSeq[(nrRandParTrans+1):nrRandPar,]
temp <- 0
haltonSeq1Res = haltonSeqRes[(temp+1):(temp+lenRandPar1),]
temp <- temp+lenRandPar1

haltonSeq2Res = haltonSeqRes[(temp+1):(temp+lenRandPar2),]
temp <- temp+lenRandPar2

haltonSeq3Res = haltonSeqRes[(temp+1):(temp+lenRandPar3),]
temp <- temp+lenRandPar3

haltonSeq4Res = haltonSeqRes[(temp+1):(temp+lenRandPar4),]
temp <- temp+lenRandPar4


haltonSeq6Res = haltonSeqRes[(temp+1):(temp+lenRandPar6),]
temp <- temp+lenRandPar6












# make a list in which each element contains a set (reps) of random parameters for each individual
haltonSeq1ListTrans <- list() 
haltonSeq2ListTrans <- list()
haltonSeq1ListRes <- list()
haltonSeq2ListRes <- list()
haltonSeq3ListRes <- list()
haltonSeq4ListRes <- list()
haltonSeq6ListRes <- list()


# run for loop to fill those lists
for(i in 1:lID){
  
  index <- 0
  haltonSeq1ListTrans[[i]] <- haltonSeqTrans[(index+1):(index+lenRandParTrans),
                                             (1 + (i - 1)*reps):(reps*i)]
  index <- index+lenRandParTrans
  
  haltonSeq2ListTrans[[i]] <- haltonSeqTrans[(index+1):(index+lenRandParTrans),
                                             (1 + (i - 1)*reps):(reps*i)]
  index <- index+lenRandParTrans
  
  
  
  index <- 0
  haltonSeq1ListRes[[i]] <- haltonSeqRes[(index+1):(index+lenRandPar1),
                                         (1 + (i - 1)*reps):(reps*i)]
  index <- index+lenRandPar1
  
  haltonSeq2ListRes[[i]] <- haltonSeqRes[(index+1):(index+lenRandPar2),
                                         (1 + (i - 1)*reps):(reps*i)]
  index <- index+lenRandPar2
  
  haltonSeq3ListRes[[i]] <- haltonSeqRes[(index+1):(index+lenRandPar3),
                                         (1 + (i - 1)*reps):(reps*i)]
  index <- index+lenRandPar3
  
  haltonSeq4ListRes[[i]] <- haltonSeqRes[(index+1):(index+lenRandPar4),
                                         (1 + (i - 1)*reps):(reps*i)]
  index <- index+lenRandPar4
  
  haltonSeq6ListRes[[i]] <- haltonSeqRes[(index+1):(index+lenRandPar6),
                                         (1 + (i - 1)*reps):(reps*i)]
  index <- index+lenRandPar6
  
}


orderIDList <- split(x = 1:lID, f = 1:lID)

library(matrixStats) # to use rowCumsums function




# Create a function to calculate the log likelihood of the model
LL <- function(parms){
  # create a vector of regression coefficients associated with selected predictors
  temp <- 0
  fixedBeta1 <- parms[(temp+1):(temp+lenFixedPar1)]
  temp <- temp + lenFixedPar1
  
  randBeta1_mu <- parms[(temp+1):(temp+lenRandPar1)] # mean of random parameters
  temp <- temp + lenRandPar1
  
  randBeta1_sigma <- parms[(temp+1):(temp+lenRandPar1)] # sigma of random parameters
  # Note that sd of a variable should be positive, but we dont need to constrain for it here
  temp <- temp + lenRandPar1
  
  
  
  fixedBeta2 <- parms[(temp+1):(temp+lenFixedPar2)]
  temp <- temp + lenFixedPar2
  
  randBeta2_mu <- parms[(temp+1):(temp+lenRandPar2)] # mean of random parameters
  temp <- temp + lenRandPar2
  
  randBeta2_sigma <- parms[(temp+1):(temp+lenRandPar2)] # sigma of random parameters
  temp <- temp + lenRandPar2
  
  
  fixedBeta3 <- parms[(temp+1):(temp+lenFixedPar3)]
  temp <- temp + lenFixedPar3
  
  randBeta3_mu <- parms[(temp+1):(temp+lenRandPar3)] # mean of random parameters
  temp <- temp + lenRandPar3
  
  randBeta3_sigma <- parms[(temp+1):(temp+lenRandPar3)] # sigma of random parameters
  temp <- temp + lenRandPar3
  
  
  
  fixedBeta4 <- parms[(temp+1):(temp+lenFixedPar4)]
  temp <- temp + lenFixedPar4
  
  randBeta4_mu <- parms[(temp+1):(temp+lenRandPar4)] # mean of random parameters
  temp <- temp + lenRandPar4
  
  randBeta4_sigma <- parms[(temp+1):(temp+lenRandPar4)] # sigma of random parameters
  temp <- temp + lenRandPar4
  
  
  
  fixedBeta6 <- parms[(temp+1):(temp+lenFixedPar6)]
  temp <- temp + lenFixedPar6
  
  randBeta6_mu <- parms[(temp+1):(temp+lenRandPar6)] # mean of random parameters
  temp <- temp + lenRandPar6
  
  randBeta6_sigma <- parms[(temp+1):(temp+lenRandPar6)] # sigma of random parameters
  temp <- temp + lenRandPar6
  
  
  # correlation between intercepts of 5 equations across all states
  # these are not state-specific
  rho_itc <- parms[(temp+1):(temp+sum(1:(5-1)))]
  temp <- temp + sum(1:(5-1))
  
  
  
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
  sdLogRev <- exp(parms[(temp+1):(temp+1)]) # one sd of the logarithm for both upgrade and downgrade: 
  temp <- temp + 1
  
  meanLogRev <- parms[(temp+1):(temp+2)] # two means of logarithm: one for upgrade, one for downgrade
  temp <- temp + 2
  
  # Note that we model the absolute change of the amount (upgrade: log(Ynew/Yold) and downgrade: -log(Ynew/Yold) to follow a log normal distribution)
  
  
  
  
  # Copula terms
  d <- parms[(temp+1):(temp+5)] # 1 components used to compute the gaussian copula x 5 equations = 5 pars
  temp <- temp + 5
  
  # We use tanh function to make sure that this component is between -1 and 1 
  # since it is the correlation between the endogenous variable and the original error term
  
  d41 <- tanh(d)
  d44 <- sqrt(1 - d41^2)
  
  
  
  
  # sigma of the new error term - amount equations:
  sigma_new <- sigma*d44[c(2, 4)]
  
  
  
  
  
  # TRANSITION MATRIX
  # Note: one transition matrix for both noncontractual donors and contractual donors
  
  
  
  # parameters for the latent attrition (state-invariant)
  # only the means of the intercepts that are state specific
  # SD of the intercept is also not state-specific
  # a) active state
  fixedDelta1 <- parms[(temp+1):(temp+lenFixedParTrans)]
  temp <- temp + lenFixedParTrans
  
  randDelta1_mu <- parms[(temp+1):(temp+lenRandParTrans)] # mean of random parameters
  temp <- temp + lenRandParTrans
  
  randDelta1_sigma <- parms[(temp+1):(temp+lenRandParTrans)] # sigma of random parameters
  temp <- temp + lenRandParTrans
  
  
  # b) dormant state
  fixedDelta2 <- fixedDelta1
  
  randDelta2_mu <- parms[(temp+1):(temp+lenRandParTrans)] # mean of random parameters
  temp <- temp + lenRandParTrans
  
  randDelta2_sigma <- randDelta1_sigma 
  
  
  
  dTrans <- parms[(temp+1):(temp+1)] # 1 component used to compute the Gaussian copula for both states
  temp <- temp + 1
  
  dTrans41 <- tanh(dTrans)
  dTrans44 <- sqrt(1-dTrans^2)
  
  
  
  # initial states: we assume that all donors begin in the active state
  omegaVec <- t(data.matrix(c(1, 0))) # a row vector
  
  
  
  
  
  LL <- mapply(FUN = function(monVec, cumTotMon, id,
                              haltonSeq1Trans, haltonSeq2Trans,
                              haltonSeq1Res, haltonSeq2Res, haltonSeq3Res, haltonSeq4Res,
                              haltonSeq6Res,
                              reps, omegaVec,
                              XNonconI, XNonconA, XConI, XConA,
                              XRev,
                              XTrans,
                              fixedBeta1,
                              randBeta1_mu, randBeta1_sigma,
                              fixedBeta2,
                              randBeta2_mu, randBeta2_sigma,
                              fixedBeta3,
                              randBeta3_mu, randBeta3_sigma,
                              fixedBeta4,
                              randBeta4_mu, randBeta4_sigma,
                              fixedBeta6,
                              randBeta6_mu, randBeta6_sigma,
                              fixedDelta1, 
                              randDelta1_mu, 
                              randDelta1_sigma, 
                              fixedDelta2, 
                              randDelta2_mu, 
                              randDelta2_sigma,
                              d41,
                              d44, 
                              copDMNonconI, copDMNonconA, copDMConI, copDMConA,
                              copDMRev,
                              dTrans41,
                              dTrans44,
                              copDM_lag, 
                              YNoncon, YCon, ANoncon, ACon,
                              ARev,
                              YRev,
                              conSample,
                              revSample,
                              rho_itc, rho, sigma_new,
                              lenRandPar1, lenFixedPar1,
                              lenRandPar2, lenFixedPar2,
                              lenRandPar3, lenFixedPar3,
                              lenRandPar4, lenFixedPar4,
                              lenRandPar6, lenFixedPar6,
                              lenRandParTrans, lenFixedParTrans, 
                              indexFirstDon, YInact, 
                              meanLogRev, 
                              sdLogRev, 
                              numCatRev, 
                              revThresholds){
    # Number of computation months for each donor
    numRows <- length(monVec)
    
   
    
    logLik <- rep(0, reps)
    
    
    # response equations
    randBeta1 <- randBeta1_mu + randBeta1_sigma*haltonSeq1Res
    beta1 <- rbind(matrix(fixedBeta1, nrow = lenFixedPar1, ncol = reps), 
                   randBeta1)
    
    randBeta2 <- randBeta2_mu + randBeta2_sigma*haltonSeq2Res
    randBeta2[1, ] <- randBeta2[1, ] + rho_itc[1]*haltonSeq1Res[1, ] # correlated intercepts
    beta2 <- rbind(matrix(fixedBeta2, nrow = lenFixedPar2, ncol = reps), 
                   randBeta2)
    
    randBeta3 <- randBeta3_mu + randBeta3_sigma*haltonSeq3Res
    randBeta3[1, ] <- randBeta3[1, ] + rho_itc[2]*haltonSeq1Res[1, ] + rho_itc[3]*haltonSeq2Res[1, ]
    beta3 <- rbind(matrix(fixedBeta3, nrow = lenFixedPar3, ncol = reps), 
                   randBeta3)
    
    randBeta4 <- randBeta4_mu + randBeta4_sigma*haltonSeq4Res
    randBeta4[1, ] <- randBeta4[1, ] +rho_itc[4]*haltonSeq1Res[1, ]+
      rho_itc[5]*haltonSeq2Res[1, ]+
      rho_itc[6]*haltonSeq3Res[1, ]
    
    beta4 <- rbind(matrix(fixedBeta4, nrow = lenFixedPar4, ncol = reps), 
                   randBeta4)
    
    
    randBeta6 <- randBeta6_mu + randBeta6_sigma*haltonSeq6Res
    randBeta6[1, ] <- randBeta6[1, ] + rho_itc[7]*haltonSeq1Res[1, ]+
      rho_itc[8]*haltonSeq2Res[1, ]+
      rho_itc[9]*haltonSeq3Res[1, ]+
      rho_itc[10]*haltonSeq4Res[1, ]
    
    beta6 <- rbind(matrix(fixedBeta6, nrow = lenFixedPar6, ncol = reps), 
                   randBeta6)
    
    # utility of the response equations
    # 1) noncontractual
    V1 <- ((XNonconI[(cumTotMon + 1):(cumTotMon+numRows),] %*% beta1) +
             copDMNonconI[(cumTotMon + 1):(cumTotMon+numRows)]*d41[1])/d44[1]
    
    V2 <- (XNonconA[(cumTotMon + 1):(cumTotMon+numRows),] %*% beta2) +
      copDMNonconA[(cumTotMon + 1):(cumTotMon+numRows)]*d41[2]
    
    
    # 2) contractual
    V3 <- ((XConI[(cumTotMon + 1):(cumTotMon+numRows),] %*% beta3) +
             copDMConI[(cumTotMon + 1):(cumTotMon+numRows)]*d41[3])/d44[3]
    
    V4 <- (XConA[(cumTotMon + 1):(cumTotMon+numRows),] %*% beta4) +
      copDMConA[(cumTotMon + 1):(cumTotMon+numRows)]*d41[4]
    
    
    # 3) revision
    V6 <- ((XRev[(cumTotMon + 1):(cumTotMon+numRows),] %*% beta6) +
             copDMRev[(cumTotMon + 1):(cumTotMon+numRows)]*d41[5])/d44[5]
    
    
    
    
    
    
    
    # probability of the observations
    # noncontractual decisions
    p1 <- (pnorm(V1, lower.tail = 0, log.p = 0, sd = 1)^(1 - YNoncon[(cumTotMon + 1):(cumTotMon+numRows)]))*
      ((pnorm((V1 + (ANoncon[(cumTotMon + 1):(cumTotMon+numRows)] - V2)*rho[1]/sigma_new[1])/sqrt(1 - rho[1]^2), lower.tail = 1, log.p = 0, sd = 1)*
          dnorm(x = ANoncon[(cumTotMon + 1):(cumTotMon+numRows)],
                mean = V2,
                sd = sigma_new[1],
                log = F))^YNoncon[(cumTotMon + 1):(cumTotMon+numRows)])
    
    
    # contractual decisions
    p2 <- (pnorm(V3, lower.tail = 0, log.p = 0, sd = 1)^((1 - YCon[(cumTotMon + 1):(cumTotMon+numRows)])*conSample[(cumTotMon + 1):(cumTotMon+numRows)]))*
      ((pnorm((V3 + (ACon[(cumTotMon + 1):(cumTotMon+numRows)] - V4)*rho[2]/sigma_new[2])/sqrt(1 - rho[2]^2), lower.tail = 1, log.p = 0, sd = 1)*
          dnorm(x = ACon[(cumTotMon + 1):(cumTotMon+numRows)],
                mean = V4,
                sd = sigma_new[2],
                log = F))^(YCon[(cumTotMon + 1):(cumTotMon+numRows)]*conSample[(cumTotMon + 1):(cumTotMon+numRows)]))
    
    
    
    
    
    p3 <- matrix(0, nrow = numRows, ncol = reps)
    
    # revision incidence decision
    lowerBoundRev <- matrix(c(-Inf, revThresholds),
                            nrow = numRows,
                            ncol = numCatRev,
                            byrow = T) # lower threshold levels
    upperBoundRev <- matrix(c(revThresholds, Inf),
                            nrow = numRows,
                            ncol = numCatRev,
                            byrow = T) # upper threshold level
    
    for(i in 1:numCatRev){
      phiRev2 <- pnorm(upperBoundRev[, i] - V6, log.p = 0, sd = 1)
      phiRev1 <- pnorm(lowerBoundRev[, i] - V6, log.p = 0, sd = 1)
      
      p3 <- p3 + (phiRev2 - phiRev1)*revMat[(cumTotMon + 1):(cumTotMon+numRows), i]
      
      
    }
    
    p3 <- p3^(revSample[(cumTotMon + 1):(cumTotMon+numRows)])
    
    # revision amount decision
    # we assume that the amount change, log(ANew/AOld) (in case of upgrade) or -log(ANew/AOld) (in case of downgrade), follows a log normal distribution
    p4 <- (dlnorm(abs(ARev[(cumTotMon + 1):(cumTotMon+numRows)]),
                  meanlog = meanLogRev[1],
                  sdlog = sdLogRev)^revMat[(cumTotMon + 1):(cumTotMon+numRows), 4])*
      (dlnorm(abs(ARev[(cumTotMon + 1):(cumTotMon+numRows)]),
              meanlog = meanLogRev[2],
              sdlog = sdLogRev)^revMat[(cumTotMon + 1):(cumTotMon+numRows), 2])
    
    
    # joint likelihood conditional on being active
    p <- as.matrix(p1*p2*p3*p4)
    
    
    
    # scaling factor (for first-donation months --- rule out the probability that no donation would be made)
    scale <- 1 - pnorm(V1[indexFirstDon[(cumTotMon + 1):(cumTotMon+numRows)], ],
                       log.p = 0, lower.tail = 0)*
      pnorm(V3[indexFirstDon[(cumTotMon + 1):(cumTotMon+numRows)], ],
            log.p = 0, lower.tail = 0)
    
    p[indexFirstDon[(cumTotMon + 1):(cumTotMon+numRows)], ] <- p[indexFirstDon[(cumTotMon + 1):(cumTotMon+numRows)], ]*scale
    
    
    
    
    
    
    # transition matrix
    randDelta1 <- randDelta1_mu + randDelta1_sigma*haltonSeq1Trans
    delta1 <- rbind(matrix(fixedDelta1, nrow = lenFixedParTrans, ncol = reps), 
                    randDelta1)
    
    randDelta2 <- randDelta2_mu + randDelta2_sigma*haltonSeq2Trans
    delta2 <- rbind(matrix(fixedDelta2, nrow = lenFixedParTrans, ncol = reps), 
                    randDelta2)
    
    
    
    
    # utility for the transitions
    # for noncontractual donors
    # a) active state
    VTrans1 <- ((XTrans[(cumTotMon + 1):(cumTotMon+numRows),] %*% delta1) +
                  copDM_lag[(cumTotMon + 1):(cumTotMon+numRows)]*dTrans41)/dTrans44
    
    
    # b) dormant state
    VTrans2 <- ((XTrans[(cumTotMon + 1):(cumTotMon+numRows),] %*% delta2) +
                  copDM_lag[(cumTotMon + 1):(cumTotMon+numRows)]*dTrans41)/dTrans44
    
    
    
    
    
    
    
    for(r in 1:reps){
      
      # transition matrix: a 2x2 matrix and the dormant state is assumed to be an unabsorbing state
      # see Schweidel and Knox 2013 (p. 476)
      q <- matrix(0, nrow = numRows, ncol = 2*2) # we store a 2x2 matrix in one row (row by row)
      
      # probability that donor i remains active
      q[, 1] <- pnorm(VTrans1[, r], lower.tail = 1, log.p = 0, sd = 1) # previous state: active
      q[, 3] <- pnorm(VTrans2[, r], lower.tail = 1, log.p = 0, sd = 1) # previous state: dormant
      
      # probability that donor i becomes dormant
      q[, 2] <- 1 - q[, 1] # previous state: active
      q[, 4] <- 1 - q[, 3] # previous state: dormant
      
      
      
      
      alpha <- omegaVec*c(p[1, r], YInact[cumTotMon + 1])
      
      
      # scaling factor to avoid underflow in the likelihood multiplication
      lnscale <- log(sum(alpha))
      if(is.na(sum(alpha))) cat("id is ", id, "and r is ", r);
      if(sum(alpha) != 0){ # to avoid NaN
        alpha <- alpha/sum(alpha)
      }
      
      
      
      
      for(t in 2:numRows){
        
        # observation-specific transition matrix
        qMat <- matrix(q[t, ], nrow = 2, ncol = 2, byrow = T)
        
        alpha <- (alpha %*% qMat)*c(p[t, r], YInact[cumTotMon + t])
        
        lnscale <- lnscale + log(sum(alpha))
        if(is.na(sum(alpha))) cat("id is ", id, "and r is ", r);
        if(sum(alpha) != 0){ # to avoid NaN
          alpha <- alpha/sum(alpha)
        }
        
        
        
      }
      
      logLik[r] <- max(lnscale, -999999) # max function is used to avoid -Inf
      
    }
    
    # simulated probabilities = mean of all draws' probabilities
    LL <- logSumExp(logLik) - log(reps)
    
    return(LL)
    
  }, monList, cumTotMonList, orderIDList,
  haltonSeq1ListTrans, haltonSeq2ListTrans,
  haltonSeq1ListRes, haltonSeq2ListRes,
  haltonSeq3ListRes, haltonSeq4ListRes,
  haltonSeq6ListRes,
  MoreArgs = list(reps = reps, omegaVec = omegaVec,
                  XNonconI = XNonconI, XNonconA = XNonconA,
                  XConI = XConI, XConA = XConA,
                  XRev = XRev, 
                  XTrans = XTrans, 
                  fixedBeta1 = fixedBeta1,
                  randBeta1_mu = randBeta1_mu,
                  randBeta1_sigma = randBeta1_sigma,
                  fixedBeta2 = fixedBeta2,
                  randBeta2_mu = randBeta2_mu,
                  randBeta2_sigma = randBeta2_sigma,
                  fixedBeta3 = fixedBeta3,
                  randBeta3_mu = randBeta3_mu,
                  randBeta3_sigma = randBeta3_sigma,
                  fixedBeta4 = fixedBeta4,
                  randBeta4_mu = randBeta4_mu,
                  randBeta4_sigma = randBeta4_sigma,
                  fixedBeta6 = fixedBeta6,
                  randBeta6_mu = randBeta6_mu,
                  randBeta6_sigma = randBeta6_sigma,
                  fixedDelta1 = fixedDelta1, 
                  randDelta1_mu = randDelta1_mu, 
                  randDelta1_sigma = randDelta1_sigma, 
                  fixedDelta2 = fixedDelta2, 
                  randDelta2_mu = randDelta2_mu, 
                  randDelta2_sigma = randDelta2_sigma, 
                  d41 = d41,
                  d44 = d44, 
                  copDMNonconI = copDMNonconI, copDMNonconA = copDMNonconA,
                  copDMConI = copDMConI, copDMConA = copDMConA,
                  copDMRev = copDMRev,
                  dTrans41 = dTrans41,
                  dTrans44 = dTrans44, 
                  copDM_lag = copDM_lag, 
                  YNoncon = YNoncon, YCon = YCon, ANoncon = ANoncon, ACon = ACon,
                  ARev = ARev,
                  YRev = YRev,
                  conSample = conSample,
                  revSample = revSample, 
                  rho_itc = rho_itc, rho = rho,
                  sigma_new = sigma_new,
                  lenRandPar1 = lenRandPar1,
                  lenFixedPar1 = lenFixedPar1,
                  lenRandPar2 = lenRandPar2,
                  lenFixedPar2 = lenFixedPar2,
                  lenRandPar3 = lenRandPar3,
                  lenFixedPar3 = lenFixedPar3,
                  lenRandPar4 = lenRandPar4,
                  lenFixedPar4 = lenFixedPar4,
                  lenRandPar6 = lenRandPar6,
                  lenFixedPar6 = lenFixedPar6,
                  lenRandParTrans = lenRandParTrans,
                  lenFixedParTrans = lenFixedParTrans, 
                  indexFirstDon = indexFirstDon, YInact = YInact, 
                  meanLogRev = meanLogRev, 
                  sdLogRev = sdLogRev, 
                  numCatRev = numCatRev, 
                  revThresholds = revThresholds))
  
  
  
  
  
  
  
  return(-sum(LL))
  
}

#----------------------------------------------
# MODEL ESTIMATION
# NOTES: Optimizing the Log likelihood function 
#
#
#----------------------------------------------
# install.packages("stats4") -- Run this if the package is not installed
library(stats4)                                           # Load the package

totLenPar <- lenFixedPar1+lenRandPar1*2+
  lenFixedPar2+lenRandPar2*2+
  lenFixedPar3+lenRandPar3*2+
  lenFixedPar4+lenRandPar4*2+
  lenFixedPar6+lenRandPar6*2+
  sum(1:(5-1))+
  2+2+
  numCatRev-2+ 
  1+2+
  5+
  lenFixedParTrans+lenRandParTrans*3+
  1



# initial values for the parameters
set.seed(123)
iniVals <- runif(totLenPar, min = -0.2, max = 0)




# Optimization
library(stats4)                                           # Load the package

ptm       <- proc.time()                                # Start the clock 
my_model <- optim(par= iniVals,
                       fn = LL, 
                       hessian=TRUE,
                       method = "BFGS",
                       control = list(maxit = 200000, trace=TRUE, reltol = 1e-16))                        # minimize -LL
estimation_time        <- proc.time() - ptm                        # Stop the clock
print(estimation_time)                                              # display estimation time
standDev <- sqrt(diag(solve(my_model$hessian)))
tvalue <- my_model$par/standDev




