library(magrittr)
library(tidyverse)
library(janitor)
library(lubridate)

# Import -------------
rm(list = ls())
nestdb <- readRDS("rdsobj/nestdb.RDS")
nestdb_md5 <- tools::md5sum("rdsobj/nestdb.RDS")

coln <- read.csv('col_names.csv', header = FALSE)

#Analysis -------------------

## Length of follow-up(years), median (IQR)
lenfu <- nestdb %>% 
  summarise(
    n = n(),
    med = median(Dx.to.LastFU, na.rm = TRUE),
    firstq = quantile(Dx.to.LastFU, na.rm = TRUE) %>% .[[2]],
    thridq = quantile(Dx.to.LastFU, na.rm = TRUE) %>% .[[4]]
  )

## Number of follow-up biopsies
numfubiop <- nestdb %>% 
  mutate(num = data.biop %>% 
           map_int(~ .x %>% janitor::remove_empty("rows") %>% nrow())) %>% 
  summarise(
    med = median(num, na.rm = TRUE),
    max = max(num, na.rm = TRUE),
    min = min(num, na.rm = TRUE))
  
##Time between biopsies
timebiop <- map(nestdb$data.biop, ~ .x %>% 
      arrange(Biopsy.Dat) %>% 
      mutate(dax = lag(Biopsy.Dat) %--% Biopsy.Dat/years(1))) %>% 
  bind_rows() %>% 
  summarise(
    med = median(dax, na.rm = TRUE),
    firstq = quantile(dax, na.rm = TRUE)[2],
    thirdq = quantile(dax, na.rm = TRUE)[4]
  )

## Number of upgrades at follow up biopsy to different GGG
upgradebiop <- map(nestdb$data.biop, ~ .x %>% 
      arrange(Biopsy.Dat) %>% 
      mutate(deltaggg = (Biopsy.GGG - lag(Biopsy.GGG)))) %>% 
  bind_rows() %>% 
  group_by(deltaggg) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

##Time to first treatment
timetreat <- nestdb %>% 
  mutate(firsttrtdat = data.trt %>% 
           map_dbl(~ .x %>% filter(rank(Trt.Dat, ties.method = "first")==1) %>% .[[1]]) %>% 
           as_date()) %>% 
  mutate(firsttrtmod = data.trt %>% 
           map_chr(~ .x %>% filter(rank(Trt.Dat, ties.method = "first")==1) %>% .[[2]])) %>% 
  mutate(lastbiopsydate = data.biop %>% 
           map_dbl(~ .x %>% filter(Biopsy.Dat.Orig != "biopsy_posttreatment" & rank(Biopsy.Dat, ties.method = "first")==1) %>% .[[1]]) %>% 
           as_date()) %>% 
  mutate(datdiff =  lastbiopsydate %--% firsttrtdat/years(1)) %>% 
  group_by(firsttrtmod) %>% 
  summarise(
    med = median(datdiff, na.rm = TRUE),
    max = max(datdiff, na.rm = TRUE),
    min = min(datdiff, na.rm = TRUE))
  
## Treatment received percentage
trtrec <- nestdb %>% 
  unnest(data.trt) %>% 
  select(ID,Trt.Mod) %>% 
  distinct() %>% 
  group_by(Trt.Mod) %>% 
  summarise(n = n(),
            pct = n/(nrow(nestdb)) * 100)

# Create a list of all the sub-tables ---------------------
subtbl_trt <- list(lenfu,numfubiop,timebiop,upgradebiop,timetreat,trtrec) %>% 
  set_names(c("lenfu","numfubiop","timebiop","upgradebiop","timetreat","trtrec"))

map(subtbl_trt,~.x %<>% mutate_if(is.numeric, ~ round(.,2)))

