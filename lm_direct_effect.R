

install.packages("R2jags")
library(rjags)
library(R2jags)
library(tidyverse)
library(readr)

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



data <- list("nsubs","SurveyX","SurveyY")

params <- c("alpha","beta")



samples_lm <- jags(data, inits=NULL, params,
                
                model.file ="bayeslm.txt",
                
                n.chains=3, n.iter=10000, n.burnin=2000, n.thin=1)


## Alpha ##
post_fit_fn = logspline(samples_lm$BUGSoutput$sims.list$alpha) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_lm_alpha <- prior_fit_zero/post_fit_zero


## Beta ##
post_fit_fn = logspline(samples_lm$BUGSoutput$sims.list$beta) # find the function that makes the posterior density plot

post_fit_zero = dlogspline(0, post_fit_fn) #find the height of the function/plot at 0

#prior

prior_fit_zero = dnorm(0,0,(1/sqrt(1)))

BF_lm_beta <- prior_fit_zero/post_fit_zero
