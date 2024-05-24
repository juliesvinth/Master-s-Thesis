

install.packages("R2jags")
library(rjags)
library(R2jags)
library(tidyverse)
library(readr)
library(logspline)

df_2021_new <- read.csv("df_2021_new.csv", fileEncoding =  "latin1")


# Remove apostrophy from column
df_2021_new$how_productive_are_you_each_hour_when_working_remotely <- gsub("Â", "", as.factor(df_2021_new$how_productive_are_you_each_hour_when_working_remotely))

#Convert columns by index to factors
columns_to_convert_2021 <- c(3:37, 48,50:109)  


df_2021_new[, columns_to_convert_2021] <- lapply(df_2021_new[, columns_to_convert_2021], factor)
# order likert levels - productivity outcome
df_2021_new$how_productive_are_you_each_hour_when_working_remotely <- ordered(df_2021_new$how_productive_are_you_each_hour_when_working_remotely, levels = c(
  "Im 50% less productive when working remotely (or worse)", 
  "Im 40% less productive when working remotely", 
  "Im 30% less productive when working remotely", 
  "Im 20% less productive when working remotely", 
  "Im 10% less productive when working remotely", 
  "My productivity is about same when I work remotely", 
  "Im 10% more productive when working remotely", 
  "Im 20% more productive when working remotely", 
  "Im 30% more productive when working remotely", 
  "Im 40% more productive when working remotely", 
  "Im 50% more productive when working remotely (or more)"))


# levels or remote work
df_2021_new$this_year_how_much_time_did_you_spend_remote_working <- ordered(df_2021_new$this_year_how_much_time_did_you_spend_remote_working, levels = c(
  "Rarely or never",
  "Less than 10% of my time",
  "10%",
  "20%",
  "30%",
  "40%",
  "50% - I spent about half of my time remote working",
  "60%",
  "70%",
  "80%",
  "90%",
  "100% - I spent all of my time remote working"))



###################### Autonomy questions #####################

# levels of policy I get to choose how much work I do remotely
df_2021_new$employer_policy_I_get_to_choose_how_much_work_I_do_remotely <- ordered(df_2021_new$employer_policy_I_get_to_choose_how_much_work_I_do_remotely, levels = c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"))


# levels of policy I choose which days I work remotely
df_2021_new$employer_policy_I_choose_which_days_I_work_remotely <- ordered(df_2021_new$employer_policy_I_choose_which_days_I_work_remotely, levels = c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"))


# levels of last 6 months easy to get permission to work remotely
df_2021_new$last_6_months_easy_to_get_permission_to_work_remotely <- ordered(df_2021_new$last_6_months_easy_to_get_permission_to_work_remotely, levels = c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"))



############# Response categories turned into numbers #############


### remote work ###

remote <- df_2021_new$this_year_how_much_time_did_you_spend_remote_working

# Create a new numeric vector using nested ifelse statements:
SurveyX <- ifelse(remote == "Rarely or never", 1,
                   ifelse(remote == "Less than 10% of my time", 1,             # Note that the first two questions are coded as 1 as they are collapsed
                          ifelse(remote == "10%", 2,
                                 ifelse(remote == "20%", 3,
                                        ifelse(remote == "30%", 4,
                                               ifelse(remote == "40%", 5,
                                                      ifelse(remote == "50% - I spent about half of my time remote working", 6,
                                                             ifelse(remote == "60%", 7,
                                                                    ifelse(remote == "70%", 8,
                                                                           ifelse(remote == "80%", 9,
                                                                                  ifelse(remote == "90%", 10,
                                                                                      ifelse(remote == "100% - I spent all of my time remote working", 11, NA))))))))))))







###  autonomy_1 ###

autonomy_1 <- df_2021_new$employer_policy_I_get_to_choose_how_much_work_I_do_remotely

# Create a new numeric vector using nested ifelse statements:
SurveyM1 <- ifelse(autonomy_1 == "Strongly disagree", 1,
                           ifelse(autonomy_1 == "Somewhat disagree", 2,
                                  ifelse(autonomy_1 == "Neither agree nor disagree", 3,
                                         ifelse(autonomy_1 == "Somewhat agree", 4,
                                                ifelse(autonomy_1 == "Strongly agree", 5, NA)))))


###  autonomy_2 ###

autonomy_2 <- df_2021_new$employer_policy_I_choose_which_days_I_work_remotely

# Create a new numeric vector using nested ifelse statements:
SurveyM2 <- ifelse(autonomy_2 == "Strongly disagree", 1,
                   ifelse(autonomy_2 == "Somewhat disagree", 2,
                          ifelse(autonomy_2 == "Neither agree nor disagree", 3,
                                 ifelse(autonomy_2 == "Somewhat agree", 4,
                                        ifelse(autonomy_2 == "Strongly agree", 5, NA)))))



###  autonomy_3 ###

autonomy_3 <- df_2021_new$last_6_months_easy_to_get_permission_to_work_remotely

# Create a new numeric vector using nested ifelse statements:
SurveyM3 <- ifelse(autonomy_3 == "Strongly disagree", 1,
                   ifelse(autonomy_3 == "Somewhat disagree", 2,
                          ifelse(autonomy_3 == "Neither agree nor disagree", 3,
                                 ifelse(autonomy_3 == "Somewhat agree", 4,
                                        ifelse(autonomy_3 == "Strongly agree", 5, NA)))))



### productivity ###

### remote work ###

productivity <- df_2021_new$how_productive_are_you_each_hour_when_working_remotely

# Create a new numeric vector using nested ifelse statements:
SurveyY<- ifelse(productivity == "Im 50% less productive when working remotely (or worse)", 1,
                  ifelse(productivity == "Im 40% less productive when working remotely", 2,
                         ifelse(productivity == "Im 30% less productive when working remotely", 3,
                                ifelse(productivity == "Im 20% less productive when working remotely", 4,
                                       ifelse(productivity == "Im 10% less productive when working remotely", 5,
                                              ifelse(productivity == "My productivity is about same when I work remotely", 6,
                                                     ifelse(productivity == "Im 10% more productive when working remotely", 7,
                                                            ifelse(productivity == "Im 20% more productive when working remotely", 8,
                                                                   ifelse(productivity == "Im 30% more productive when working remotely", 9,
                                                                          ifelse(productivity == "Im 40% more productive when working remotely", 10,
                                                                                ifelse(productivity == "Im 50% more productive when working remotely (or more)", 11, NA)))))))))))





nsubs <- 1512

nq_X <- 1
nr_X <- 11 # If Q1 and Q2 are collapsed, we only have 11 responses, but if we have to count each unique response it would be 12

nq_M1 <- 1
nr_M1 <- 5

nq_M2 <- 1
nr_M2 <- 5

nq_M3 <- 1
nr_M3 <- 5

nq_Y <- 1
nr_Y <- 11

surveyX <- array(SurveyX,c(nsubs,1))

surveyM1 <- array(SurveyM1,c(nsubs,1))

surveyM2 <- array(SurveyM2,c(nsubs,1))

surveyM3 <- array(SurveyM3,c(nsubs,1))

surveyY <- array(SurveyY,c(nsubs,1))


#---------------- Apply jags model ---------------------------
#----- effects of autonomy on productivity ----------------------
data <- list("nsubs","nq_X","nr_X","nq_M1","nr_M1","nq_M2","nr_M2",
             "nq_M3","nr_M3","nq_Y","nr_Y",
             "surveyX","surveyM1", "surveyM2", "surveyM3", "surveyY")
params <- c("gamma_M1","gamma_M2","gamma_M3","gamma_total",
            "alpha_M1","alpha_M2","alpha_M3",
            "beta_M1","beta_M2","beta_M3",
            "tau_prime",
            "psy_M1","psy_M2","psy_M3")


# need to initialise survey thresholds

initList <- function(){
  
  list(
    
    theta_X=array(c(rep(.05,nsubs*nq_X),
                    
                    rep(.15,nsubs*nq_X),
                    
                    rep(.25,nsubs*nq_X),
                    
                    rep(.35,nsubs*nq_X),
                    
                    rep(.45,nsubs*nq_X),
                    
                    rep(.55,nsubs*nq_X),
                    
                    rep(.65,nsubs*nq_X),
                    
                    rep(.75,nsubs*nq_X),
                    
                    rep(.85,nsubs*nq_X),
                    
                    rep(.95,nsubs*nq_X)),
                  
                  c(nsubs,nq_X,nr_X-1)),
    
    
    
    theta_M1=array(c(rep(1.5,nsubs*nq_M1),
                     
                     rep(2.5,nsubs*nq_M1),
                     
                     rep(3.5,nsubs*nq_M1),
                     
                     rep(4.5,nsubs*nq_M1)),
                   
                   c(nsubs,nq_M1,nr_M1-1)),
    
    
    
    theta_M2=array(c(rep(1.5,nsubs*nq_M2),
                     
                     rep(2.5,nsubs*nq_M2),
                     
                     rep(3.5,nsubs*nq_M2),
                     
                     rep(4.5,nsubs*nq_M2)),
                   
                   c(nsubs,nq_M2,nr_M2-1)),
    
    
    
    theta_M3=array(c(rep(1.5,nsubs*nq_M3),
                     
                     rep(2.5,nsubs*nq_M3),
                     
                     rep(3.5,nsubs*nq_M3),
                     
                     rep(4.5,nsubs*nq_M3)),
                   
                   c(nsubs,nq_M3,nr_M3-1)),   
    
    
    
    theta_Y=array(c(rep(-.45,nsubs*nq_Y),
                    
                    rep(-.35,nsubs*nq_Y),
                    
                    rep(-.25,nsubs*nq_Y),
                    
                    rep(-.15,nsubs*nq_Y),
                    
                    rep(-.05,nsubs*nq_Y),
                    
                    rep(.05,nsubs*nq_Y),
                    
                    rep(.15,nsubs*nq_Y),
                    
                    rep(.25,nsubs*nq_Y),
                    
                    rep(.35,nsubs*nq_Y),
                    
                    rep(.45,nsubs*nq_Y)),
                  
                  c(nsubs,nq_Y,nr_Y-1))
    
  )
  
}

initList()

set.seed(1997)

samples_autonomy <- jags(data, inits=initList, params,
                model.file ="autonomy_jags.txt",
                n.chains=3, n.iter=10000, n.burnin=2000, n.thin=1)







plot(density(rnorm(10000,0,1/sqrt(1))))

lines(density(samples$BUGSoutput$sims.list$beta))





#bayesfactor calculations alpha



############# Alpha_m1##################
post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$alpha_M1) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_a1 <- prior_fit_zero/post_fit_zero

############## Alpha_m2  #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$alpha_M2) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_a2 <- prior_fit_zero/post_fit_zero
############## Alpha_m #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$alpha_M3) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_a3 <- prior_fit_zero/post_fit_zero


############## Beta_m1 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$beta_M1) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_b1 <- prior_fit_zero/post_fit_zero

############## Beta_m2 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$beta_M2) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_b2 <- prior_fit_zero/post_fit_zero

############## Beta_m3 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$beta_M3) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_b3 <- prior_fit_zero/post_fit_zero

############## gamma_m1 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$gamma_M1) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_g1 <- prior_fit_zero/post_fit_zero

############## gamma_m2 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$gamma_M2) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_g2 <- prior_fit_zero/post_fit_zero

############## gamma_m3 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$gamma_M3) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_g3 <- prior_fit_zero/post_fit_zero

############## gamma total #############
post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$gamma_total) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_g_total <- prior_fit_zero/post_fit_zero

############## psy_m1 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$psy_M1) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_p1 <- prior_fit_zero/post_fit_zero

############## psy_m2 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$psy_M2) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_p2 <- prior_fit_zero/post_fit_zero

############## psy_m3 #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$psy_M3) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_p3 <- prior_fit_zero/post_fit_zero

############## tau' #############

post_fit_fn = logspline(samples_autonomy$BUGSoutput$sims.list$tau_prime) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_tau <- prior_fit_zero/post_fit_zero

