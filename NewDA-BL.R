library(magrittr)
library(tidyverse)

# Import -------------
rm(list = ls())
nestdb <- readRDS("rdsobj/nestdb.RDS")
nestdb_md5 <- tools::md5sum("rdsobj/nestdb.RDS")

coln <- read.csv('col_names.csv', header = FALSE)
# Analysis -------------------

## Age(mean,sd)
age <- nestdb %>%
  summarise(
    mean = mean(AgeDx,na.rm = TRUE),
    sd = sd(AgeDx,na.rm = TRUE)
  ) 

## Ethnicity(n,%)
eth <- nestdb %>%
  group_by(Eth) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

## First degree fam-relative(n,%)
fmhx <- nestdb %>% 
  mutate(famcount = data.fhx %>% 
           map_int(~ .x$FamHx %in% c("son","sibling","brother","parent","father") %>% sum()) %>% {ifelse(. > 0,T,F)}) %>% 
  group_by(famcount) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)
  

## First PSA (n, %)
psa <- nestdb %>% 
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

gggfb <- nestdb %>% 
  mutate(firstggg = data.biop %>% 
               map_int(~ .x %>% replace_na() %>% slice(which.min(Biopsy.Dat)) %>% .$Biopsy.GGG)) %>% 
  group_by(firstggg) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

## Of positive cores (n,%)
cores <- nestdb %>% 
  mutate(firstc = data.biop %>% 
           map_int(~ .x %>% slice(which.min(Biopsy.Dat)) %>% .$Biopsy.PosCores)) %>% 
  group_by(firstc) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

## PSA Density
dens <- nestdb %>% 
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

