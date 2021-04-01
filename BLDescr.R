library(magrittr)
library(tidyverse)

# Import -------------
rm(list = ls())
source("Tidying.R")
source("func.R")

# Analysis -------------------
bl_analysis <- function(inputdb){

## Age(mean,sd)
age <- inputdb %>%
  summarise(
    mean = mean(AgeDx,na.rm = TRUE),
    sd = sd(AgeDx,na.rm = TRUE)
  ) 

## Ethnicity(n,%)
eth <- inputdb %>%
  group_by(Eth) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

## First degree fam-relative(n,%)
fmhx <- inputdb %>% 
  mutate(famcount = data.fhx %>% 
           map_int(~ .x$FamHx %in% c("son","sibling","brother","parent","father") %>% sum()) %>% {ifelse(. > 0,T,F)}) %>% 
  group_by(famcount) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)
  

## First PSA (n, %)
psa <- inputdb %>% 
  mutate(firstpsa = data.biop %>% 
            map_dbl(~ .x %>% slice(which.min(Biopsy.Dat)) %>% .$Biopsy.PSA)) %>% 
  summarise(
    med = median(firstpsa, na.rm=TRUE),
    firstq = quantile(firstpsa,  na.rm = TRUE) %>% .[[2]],
    thirdq = quantile(firstpsa,  na.rm = TRUE) %>% .[[4]] 
  )

##GGG on first biopsy (n,%)
replace_na <- function(df){
  while(is.na(df[1,"Biopsy.GGG"])==TRUE){
    df=df[-1,]
  }
  return(df)
}

gggfb <- inputdb %>% 
  mutate(firstggg = data.biop %>% 
               map_int(~ .x %>% replace_na() %>% slice(which.min(Biopsy.Dat)) %>% .$Biopsy.GGG)) %>% 
  group_by(firstggg) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

## Of positive cores (n,%)
cores <- inputdb %>% 
  mutate(firstc = data.biop %>% 
           map_int(~ .x %>% slice(which.min(Biopsy.Dat)) %>% .$Biopsy.PosCores)) %>% 
  group_by(firstc) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

## PSA Density
dens <- inputdb %>% 
  mutate(firstden = data.biop %>% 
           map_dbl(~ .x %>% mutate(Biopsy.PSADens = Biopsy.PSA/Biopsy.Vol) %>% slice(which.min(Biopsy.Dat)) %>% .$Biopsy.PSADens)) %>% 
  mutate(gate = case_when(
    firstden < 0.15 ~ "low",
    firstden >= 0.15 ~ "high"
  )) %>% 
  group_by(gate) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)


# Create a list of all the subtables ---------------------
subtbl_bl <- list("Age at diagnosis" = age,
                  "Ethnicity" = eth,
                  "Family Hx" = fmhx,
                  "First PSA" = psa,
                  "GGG on first biopsy" = gggfb,
                  "Number of postivie cores on first biopsy" = cores,
                  "PSA density at time of first biopsy" = dens) %>% 
  map(~ .x %>% mutate_if(is.numeric, ~ round(.,2)))

print(subtbl_bl)
return(subtbl_bl)
}

new_directory("output")
new_directory("output/squish")

bl_all <- bl_analysis(nestdb) 
amalgated_df(bl_all,"output/bl_all.csv")
squish(bl_all,"output/squish/bl_all_squish.csv")

bl_g1 <- bl_analysis(nestdb_g1)
amalgated_df(bl_g1,"output/bl_g1.csv")
squish(bl_g1,"output/squish/bl_g1_squish.csv")
