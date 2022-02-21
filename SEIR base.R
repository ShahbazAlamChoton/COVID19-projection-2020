#version
#sessionInfo()
rm(list=ls())

# https://github.com/RamiKrispin/coronavirus
# install.packages("devtools")
devtools::install_github("covid19r/coronavirus")
library(coronavirus)
library(tidyverse)
coronavirus::update_datasets(silence = TRUE)

str(coronavirus)
head(coronavirus)

bd_data<- coronavirus %>% 
  select(country = Country.Region, date, type, cases) %>%
  group_by(date,country, type) %>%
  filter(country == "Bangladesh") %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(country, date) %>%
  group_by(date) %>%
  ungroup() %>%
  mutate(cum_deaths=cumsum(death),
         cum_cases = cumsum(confirmed),
         cum_recovered = cumsum(recovered))



# install.packages("deSolve")
library(deSolve)

######################################
## SIER Modeling -------
######################################
# Parameters
# beta = rate of expusore from susceptible infected contact 
# sigma = rate at which exposed person becomes infected
# gamma = rate at which infected person recovers
# S = Initial susceptible population
# E = Initial exposed population
# I = Initial infected population
# R = Recovered population

SEIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * I * S/N
    dE <- beta * I * S/N - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I
    list(c(dS, dE, dI, dR))
  })
}


# define a function to calculate the residual sum of squares
# (RSS), passing in parameters beta and gamma that are to be
# optimised for the best fit to the incidence data

RSS <- function(parameters) {
  names(parameters) <- c("beta", "sigma", "gamma")
  out <- ode(y = init, times = Day, func = SEIR, parms = parameters)
  fit <- out[, 4]
  sum((infected - fit)^2)
}


fit_seir <- function(country_name='Bangladesh', 
                     N=170000000, af=0.5, npast=2, nfuture=10){
  # country = Country name
  # N = population size of the country
  # af = ascertainment factor, default = 0.5
  # country = "Bangladesh(unoff)"
  # npast = number of days in the past to exclude when fitting the model
  # default is npast = 5
  # nfuture = number of days in the future the algorithm to predict to
  # default is nfuture=10
  
  
  SEIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta * I * S/N
      dE <- beta * I * S/N - sigma * E
      dI <- sigma * E - gamma * I
      dR <- gamma * I
      list(c(dS, dE, dI, dR))
    })
  }
  
  # define a function to calculate the residual sum of squares
  # (RSS), passing in parameters beta, sigma, and gamma that are to be
  # optimised for the best fit to the incidence data
  
  RSS <- function(parameters) {
    names(parameters) <- c("beta", "sigma", "gamma")
    out <- ode(y = init, times = Day, func = SEIR, parms = parameters)
    fit <- out[, 4]
    sum((infected - fit)^2)
  }
  
  country = enquo(country_name)
  df <- bd_data %>% filter(country == !!country, cum_cases>0)
  infected <- df %>% filter(date >= min(date), date <= today() - 1 - npast) %>%
    pull(cum_cases)
  
  R = 0; E=0; I = infected[1]; S = N - E - I - R
  
  seir_start_date <- df %>% pull(date) %>% min()
  
  # Ascertainment factor
  infected = infected * 1/af
  
  # Create an incrementing Day vector the same length as our
  # cases vector
  Day <- 1:(length(infected))
  
  # now specify initial values for S, I and R
  init <- c(S = S, E=E, I=I, R=R)
  
  # now find the values of beta, sigma. and gamma that give the
  # smallest RSS, which represents the best fit to the data.
  # We perform a constrined optimization with initial parameters
  # set as .5. We bound the params within 0.01 and 0.999 (I've made 
  # the choice). Feel free to change it to c(0, 1). But you may face
  # issues with convergence
  
  opt <- optim(c(.5, .5, .5), RSS, method = "L-BFGS-B", 
               lower = c(0.01,0.01,0.01), upper = c(.999, .999, .999), 
               control=list(maxit = 1000))
  
  # check for convergence
  opt_msg = opt$message
  opt_par <- setNames(opt$par, c("beta", "sigma", "gamma"))
  
  beta = opt_par["beta"]
  gamma = opt_par["gamma"]
  sigma = opt_par["sigma"]
  R0 = as.numeric(beta/gamma) # there is a more complex formula 
  
  
  # time in days for predictions
  t <- 1:(as.integer(today() - seir_start_date)  + nfuture)
  
  # get the fitted values from our SEIR model
  
  odefit = ode(y = init, times = t, func = SEIR, parms = opt_par)
  fitted_cases <- data.frame(odefit)
  
  # add a Date column and join the observed incidence data
  fitted_cases <- fitted_cases %>% 
    mutate(date = seir_start_date + days(t - 1)) %>% 
    left_join(df %>% filter(cum_cases>0) %>% ungroup() %>%
                select(date, cum_cases))
  
  
  # Return
  list(country=country_name, infected = infected,
       opt_msg=opt_msg, opt_par = opt_par, R0=R0, opt_msg=opt_msg, 
       fitted_cases=fitted_cases, N=N, af=af)
  
}

