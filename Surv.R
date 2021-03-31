library(magrittr)
library(tidyverse)
library(janitor)
library(survival)
library(survminer)

# Import -------------
rm(list = ls())
source("Tidying.R")
source("func.R")

# Plot creations -------------------------
mod_data <- mutate(nestdb,
                   # Arrange all by date.bio, and drop any empty GGGs in advance.
                   data.biop = map(data.biop, ~ .x %>% arrange(Biopsy.Dat) %>% drop_na(Biopsy.GGG)),
                   # Pull the first biopsy date which is not a GGG1. NA returned if all were GGG1
                   enddate = map_dbl(data.biop, ~ .x %>%
                                  filter(Biopsy.GGG != 1) %>% 
                                  pull(Biopsy.Dat) %>% first()) %>% as_date(),
                   #Pull the first biopsy.
                   startdate =  map_dbl(data.biop, ~ .x %>%
                                   pull(Biopsy.Dat) %>% first()) %>% as_date(),
                   #Pull last date of followup 
                   lastfoldate = LastFU) %>% 
  mutate(
    #Check if it was censored, 1 = Event at t, 0 = Censored at t. 
    #Consider censored if last GGG=1. Consider treated if Last GGG > 1.Date of censor is the lastFU date.
    censored = ifelse(is.na(enddate),0,1),
    combined.dates = ifelse(is.na(enddate),lastfoldate,enddate) %>% as_date(),
    time = startdate %--% combined.dates/years(1))


km_surv <- survfit(Surv(time,censored) ~ 1,data = mod_data)

survp <- ggsurvplot(km_surv, 
           risk.table = TRUE,
           xlab = "Time since first biopsy (Years)",
           ylab = "Probability of upgrading free survival",
           ggtheme = theme_light())

print(survp)

# Save the plot -----------------------------------------
new_directory("rdsobj")
ggsave(
  "output/survplot.png",
  plot = print(survp),
  scale = 1,
  width = NA,
  height = NA,
  dpi = 500
)
