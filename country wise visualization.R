rm(list = ls())
dir <- "E:\\shahbaz\\R_files\\practiseMap" 
setwd(dir)

# https://github.com/RamiKrispin/coronavirus
# library(devtools)
# devtools::install_github("RamiKrispin/coronavirus", force = TRUE)
# library (readr)
# 
# urlfile="https://raw.githubusercontent.com/lrjoshi/webpage/master/public/post/c159s.csv"
# 
# mydata<-read_csv(url(urlfile)
library(coronavirus)
library(tidyverse)
coronavirus::update_datasets(silence = TRUE)

str(coronavirus)
head(coronavirus)

coronavirus %>% 
  select(country = Country.Region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed) %>%
  head(10)

death_report<- coronavirus %>%
  filter(type=="death") %>%
  group_by(date) %>%
  summarise(Daily_Deaths=sum(cases)) %>%
  ungroup() %>%
  mutate(Agg_Deaths=cumsum(Daily_Deaths))

#------------ Comparison of Aggregated Deaths of Bangladesh with top 5 countries and Its Peers COuntries

death_report_countrywise<- coronavirus %>%
  filter(type == "death") %>%
  group_by(Country.Region, date) %>%
  summarise(Daily_Deaths = sum(cases)) %>%
  mutate(Agg_Deaths = cumsum(Daily_Deaths)) %>%
  filter( Agg_Deaths >0) %>%
  ungroup() %>%
  group_by(Country.Region) %>%
  mutate(days_since_first_death =  1:length(date))

bd_peers<- c("Singapore", "Sri Lanka" , "Pakistan" , "Malaysia", "Indonesia", "India", "Bangladesh")
top_5_countries<- c("Spain", "US" , "Iran", "Italy",  "France", "Bangladesh")

top_5bd_death_reports<- death_report_countrywise %>%
  filter(Country.Region %in% top_5_countries)

bd_peers_death_reports<- death_report_countrywise %>%
  filter(Country.Region  %in% bd_peers)


highlight_5bd_deaths<- death_report_countrywise %>%
  filter(Country.Region == "Bangladesh" & days_since_first_death == max(days_since_first_death))


pdf("./Covid-19_bd_peers_top5_agg_death.pdf", width = 14, height = 8.5, onefile = TRUE)

library(directlabels)
library(lubridate)

print(ggplot(top_5bd_death_reports, aes(x = days_since_first_death, y = Agg_Deaths)) + 
        geom_line(aes(color = Country.Region), lwd = 1.1) + 
        geom_point(data = highlight_5bd_deaths, aes(x = days_since_first_death, y = Agg_Deaths), colour = "darkred" , size = 4) +  
        geom_dl(aes(label= Country.Region,  color = Country.Region), method = list("last.points", vjust = -1)) +
        
        ggtitle(paste0("COVID-19 Cumulative Deaths  Since First Death \n(Top 5 Countries Excluding China) \nTotal Deaths: ", highlight_5bd_deaths$Agg_Deaths,"\n", today("GMT")))+
        xlab("Days Since First Death") +
        ylab("Cumulative Deaths") +
        theme_bw()+
        theme(legend.title = element_blank(), legend.position = "None") +
        scale_color_manual(values = c("darkred", "steelblue","#0C2B3C", "seagreen", "#F3E195", "purple", "#1f77b4"))
  )


print(ggplot(bd_peers_death_reports, aes(x = days_since_first_death, y = Agg_Deaths)) + 
        geom_line(aes(color = Country.Region), lwd = 1.1) + 
        geom_point(data = highlight_5bd_deaths, aes(x = days_since_first_death, y = Agg_Deaths), colour = "darkred" , size = 4) +  
        geom_dl(aes(label= Country.Region,  color = Country.Region), method = list("last.points", hjust = .1, vjust = -1)) +
  
        ggtitle(paste0("COVID-19 Cumulative Deaths  Since First Death \n(Bangladesh's Peers) \nTotal Deaths: ", highlight_5bd_deaths$Agg_Deaths,"\n", today("GMT")))+
        xlab("Days Since First Death") +
        ylab("Cumulative Deaths") +
        theme_bw()+
        theme(legend.title = element_blank(), legend.position = "None") +
        scale_color_manual(values = c("darkred", "steelblue","#0C2B3C", "seagreen", "#F3E195", "purple", "#1f77b4"))
    )


# confirmed cases

confirmed_report_countrywise<- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(Country.Region, date) %>%
  summarise(Daily_confirmed = sum(cases)) %>%
  mutate(Agg_confirmed = cumsum(Daily_confirmed)) %>%
  filter( Agg_confirmed >0) %>%
  ungroup() %>%
  group_by(Country.Region) %>%
  mutate(days_since_first_confirmed =  1:length(date))
# 
# bd_peers<- c("Singapore", "Sri Lanka" , "Pakistan" , "Malaysia", "Indonesia", "India", "Bangladesh")
# top_5_countries<- c("Spain", "US" , "Iran", "Italy",  "France", "Bangladesh")

top_5bd_confirmed_reports<- confirmed_report_countrywise %>%
  filter(Country.Region %in% top_5_countries)

bd_peers_confirmed_reports<- confirmed_report_countrywise %>%
  filter(Country.Region  %in% bd_peers)


highlight_5bd_confirmed<- confirmed_report_countrywise %>%
  filter(Country.Region == "Bangladesh" & days_since_first_confirmed == max(days_since_first_confirmed))


print(ggplot(top_5bd_confirmed_reports, aes(x = days_since_first_confirmed, y = Agg_confirmed)) + 
        geom_line(aes(color = Country.Region), lwd = 1.1) + 
        geom_point(data = highlight_5bd_confirmed, aes(x = days_since_first_confirmed, y = Agg_confirmed), colour = "darkred" , size = 4) +  
        geom_dl(aes(label= Country.Region,  color = Country.Region), method = list("last.points", vjust = -1)) +
        
        ggtitle(paste0("COVID-19 Cumulative Cases Since First Confirmed Case \n(Top 5 Countries Excluding China) \nTotal Confirmed Cases: ", highlight_5bd_confirmed$Agg_confirmed,"\n",today("GMT")))+
        xlab("Days Since First Case") +
        ylab("Cumulative Cases") +
        theme_bw()+
        theme(legend.title = element_blank(), legend.position = "None") +
        scale_color_manual(values = c("darkred", "steelblue","#0C2B3C", "seagreen", "#F3E195", "purple", "#1f77b4"))
)


print(ggplot(bd_peers_confirmed_reports, aes(x = days_since_first_confirmed, y = Agg_confirmed)) + 
        geom_line(aes(color = Country.Region), lwd = 1.1) + 
        geom_point(data = highlight_5bd_confirmed, aes(x = days_since_first_confirmed, y = Agg_confirmed), colour = "darkred" , size = 4) +  
        geom_dl(aes(label= Country.Region,  color = Country.Region), method = list("last.points", hjust = .1, vjust = -1)) +
        
        ggtitle(paste0("COVID-19 Cumulative Cases Since First confirmed Case \n(Bangladesh's Peers) \nTotal Confirmed Cases: ", highlight_5bd_confirmed$Agg_confirmed,"\n", today("GMT")))+
        xlab("Days Since First Case") +
        ylab("Cumulative Cases") +
        theme_bw()+
        theme(legend.title = element_blank(), legend.position = "None") +
        scale_color_manual(values = c("darkred", "steelblue","#0C2B3C", "seagreen", "#F3E195", "purple", "#1f77b4"))
)




dev.off()

#-------------------------------------------------------------------------------

#------------------ Growth rate of daily new deaths all world
for( i in 2: nrow(death_report)){
death_report$Growthrate[i]<- death_report$Daily_Deaths[i]/ death_report$Daily_Deaths[i-1]
  
}

death_growth<- death_report %>%
  filter(date >"2020-01-31")

ggplot(death_growth, aes(x = date, y = Growthrate)) + 
        geom_line(color = "gray60", lwd = 1.1) + 
        geom_point(color = "gray60") 
        

#----------------------------------------------------------------------------------------------------
death_report %>%
  ggplot(aes(x=date, Agg_Deaths))+
  geom_point()+geom_line()+
  ggtitle("Aggregate Deaths of COVID-19")



coronavirus %>%
  filter(Country.Region=="Italy", type=="confirmed") %>%
  group_by(date) %>%
  summarise(daily_cases=sum(cases)) %>%
  ungroup() %>%
  mutate(agg_cases= cumsum(daily_cases))


coronavirus%>%
  filter(Country.Region=="Italy", type=="confirmed") %>%
  group_by(date) %>%
  summarise(daily_cases=sum(cases)) %>%
  ungroup() %>%
  mutate(agg_cases=cumsum(daily_cases)) %>%
  ggplot(aes(x=date, y=daily_cases))+
  geom_line()+geom_point()+ggtitle("Italy: Daily Confirmed Cases of COVID-19")

coronavirus%>%
  filter(Country.Region=="Bangladesh", type=="confirmed") %>%
  group_by(date) %>%
  summarise(daily_cases=sum(cases)) %>%
  ungroup() %>%
  mutate(agg_cases=cumsum(daily_cases)) %>%
  ggplot(aes(x=date, y=daily_cases))+
  geom_line()+geom_point()+ggtitle("Bangladesh: Daily Confirmed Cases of COVID-19")



