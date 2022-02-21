version
sessionInfo()
rm(list=ls())
setwd("E:/shahbaz/R_files/practiseMap/")
# remotes::install_github("wilkelab/ggtext")
# mydata<-read_csv(url(urlfile)
library(coronavirus)
library(tidyverse)
coronavirus::update_dataset(silence = TRUE)

str(coronavirus)
head(coronavirus)
tail(coronavirus)

bd_data<- coronavirus %>% 
  select(country = country, date, type, cases) %>%
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
library(grid)
library(gridExtra)
library(ggplot2)

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



color_var<- c("Exposed" = "royalblue", "Predicted Incidence"= "red3", "Recovered" = "seagreen", "Actual Cases" = "darkorange")

library(lubridate)
p1<- fit_seir(af = .25, nfuture = 10)

# legend positionhelping material
# https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r/24497113

fig1<-ggplot(p1$fitted_cases, aes(x = date)) + 
        geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
        geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
        geom_line(aes(y = R, colour = "Recovered"), lwd = 1.1) +
        geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +  
        
        labs(title = paste0("R0 = ", round(p1$R0, 2), " and assumed reporting = ", p1$af*100, "%"),
        x = "Date",
        y = "Cumulative incidence", color = "") +
        theme_bw()+
        theme(legend.title = element_blank(), legend.position = c(0.17,.72))+
        scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", Recovered = "seagreen", `Actual Cases` = "darkorange"))


p2<- fit_seir(af = .5, nfuture = 10)
fig2<-ggplot(p2$fitted_cases, aes(x = date)) + 
  geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
  geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
  geom_line(aes(y = R, colour = "Recovered"), lwd = 1.1) +
  geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +  
  
  labs(title = paste0("R0 = ", round(p2$R0, 2), " and assumed reporting = ", p2$af*100, "%"),
       x = "Date",
       y = "Cumulative incidence", color = "") +
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.15,.72))+
  scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", Recovered = "seagreen", `Actual Cases` = "darkorange"))


p3<- fit_seir(af = .75, nfuture = 10)

fig3<-ggplot(p3$fitted_cases, aes(x = date)) + 
  geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
  geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
  geom_line(aes(y = R, colour = "Recovered"), lwd = 1.1) +
  geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +  
  
  labs(title = paste0("R0 = ", round(p3$R0, 2), " and assumed reporting = ", p3$af*100, "%"),
       x = "Date",
       y = "Cumulative incidence", color = "") +
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.15,.72))+
  scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", Recovered = "seagreen", `Actual Cases` = "darkorange"))


p4<- fit_seir(af = .9, nfuture = 10)
fig4<-ggplot(p4$fitted_cases, aes(x = date)) + 
  geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
  geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
  geom_line(aes(y = R, colour = "Recovered"), lwd = 1.1) +
  geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +  
  
  labs(title = paste0("R0 = ", round(p4$R0, 2), " and assumed reporting = ", p4$af*100, "%"),
       x = "Date",
       y = "Cumulative incidence", color = "") +
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.15,.72))+
  scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", Recovered = "seagreen", `Actual Cases` = "darkorange"))

library(ggpubr)
figure<- ggarrange(fig1, fig2, fig3, fig4,
                   nrow = 2, ncol = 2, common.legend = TRUE)

figure
# Headline of Multiple plot
# https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html
final_figure_1<- annotate_figure(figure,
                top = text_grob(paste0("COVID-19 observed and 10-day predicted cumulative incidence for Bangladesh\n",
                                       "The upper right plot suggests the best fit\n",today("GMT")), 
                                color = "black", face = "bold", size = 14)
                )
pdf("./Covid-19_bd_10_day_projection.pdf", width = 14, height = 8.5, onefile = TRUE)
print(final_figure_1)
dev.off()

# annotate_figure(figure,
#                 top = text_grob(paste0("COVID-19 observed and 10-day predicted cumulative incidence for Bangladesh\n", today("GMT")), color = "navyblue", face = "bold", size = 14),
#                 bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue",
#                                    hjust = 1, x = 1, face = "italic", size = 10),
#                 left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
#                 right = "I'm done, thanks :-)!",
#                 fig.lab = "Figure 1", fig.lab.face = "bold"
# )

#################################
##### 100 day prediction section
################################

p101<- fit_seir(af = .25, nfuture = 100)
fig101<-ggplot(p101$fitted_cases, aes(x = date)) + 
  geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
  geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
  # geom_line(aes(y = R, colour = "Recovered"), lwd = 1.1) +
  geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +  
  
  labs(title = paste0("R0 = ", round(p101$R0, 2), " and assumed reporting = ", p101$af*100, "%"),
       x = "Date",
       y = "Cumulative incidence", color = "") +
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.15,.72))+
  scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", `Actual Cases` = "darkorange"))


p102<- fit_seir(af = .50, nfuture = 100)
fig102<-ggplot(p102$fitted_cases, aes(x = date)) + 
  geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
  geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
  # geom_line(aes(y = R, colour = "Recovered"), lwd = 1.1) +
  geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +  
  
  labs(title = paste0("R0 = ", round(p102$R0, 2), " and assumed reporting = ", p102$af*100, "%"),
       x = "Date",
       y = "Cumulative incidence", color = "") +
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.15,.72))+
  scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", `Actual Cases` = "darkorange"))


p103<- fit_seir(af = .75, nfuture = 100)
fig103<-ggplot(p103$fitted_cases, aes(x = date)) + 
  geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
  geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
  # geom_line(aes(y = R, colour = "Recovered"), lwd = 1.1) +
  geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +  
  
  labs(title = paste0("R0 = ", round(p103$R0, 2), " and assumed reporting = ", p103$af*100, "%"),
       x = "Date",
       y = "Cumulative incidence", color = "") +
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.15,.72))+
  scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", `Actual Cases` = "darkorange"))



p104<- fit_seir(af = .90, nfuture = 100)
fig104<-ggplot(p104$fitted_cases, aes(x = date)) + 
  geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
  geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
  # geom_line(aes(y = R, colour = "Recovered"), lwd = 1.1) +
  geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +  
  
  labs(title = paste0("R0 = ", round(p104$R0, 2), " and assumed reporting = ", p104$af*100, "%"),
       x = "Date",
       y = "Cumulative incidence", color = "") +
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.15,.72))+
  scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", `Actual Cases` = "darkorange"))




library(ggpubr)
figure_100<- ggarrange(fig101, fig102, fig103, fig104,
                   nrow = 2, ncol = 2, common.legend = TRUE)

final_figure_100<- annotate_figure(figure_100,
                                 top = text_grob(paste0("COVID-19 observed and 100-day predicted cumulative incidence for Bangladesh\n", today("GMT")), 
                                                 color = "black", face = "bold", size = 14)
)
pdf("./Covid-19_bd_100_day_projection.pdf", width = 14, height = 8.5, onefile = TRUE)
print(final_figure_100)
dev.off()



##########################################
###### Best fit section
#########################################
fig103_adding_vline<-ggplot(p102$fitted_cases, aes(x = date)) + 
  geom_line(aes(y = E, colour = "Exposed"), lwd = 1.1) +
  geom_line(aes(y = I, colour = "Predicted Incidence"), lwd = 1.1) +
  geom_point(aes(y = cum_cases, colour = "Actual Cases") , size = 2) +
  geom_vline(data = subset(p102$fitted_cases, I == max(I)), # filter data source
             aes(xintercept = date),
             size = 1.5, linetype = "dotted",colour = "gray25")+
  
  labs(title = paste0("R0 = ", round(p102$R0, 2), " and assumed reporting = ", p102$af*100, "%"),
       x = "Date",
       y = "Cumulative incidence", color = "") +
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.15,.72))+
  scale_color_manual(values = c(Exposed = "royalblue", `Predicted Incidence`= "red3", `Actual Cases` = "darkorange"))


best_figure<-ggarrange(fig2, fig102_adding_vline,
                       nrow = 1, ncol = 2, common.legend = F)
final_best_figure<- annotate_figure(best_figure,
                                    top = text_grob(paste0("the 100 day projection suggests that the the peak of the epidemic will be around the last week of June.\n",
                                                           "The trajectory also suggests that the epidemic will end by end of July or early August.\n",today("GMT")), 
                                                    color = "black", face = "bold", size = 14)
)

# the 100 day projection suggests that the the peak of the epidemic will be around the middle of June. 
# The trajectory also suggests that the epidemic will end by end of July or early August.

pdf("./Covid-19_bd_best_fit_projection.pdf", width = 14, height = 8.5, onefile = TRUE)
print(final_best_figure)
dev.off()


last_six_days_data<- tail(bd_data)
write.csv(last_six_days_data, "covid_last_six_day.csv",row.names = F, col.names = T)
write.csv(p3$fitted_cases, "covid_10day_projection.csv", row.names = F)
