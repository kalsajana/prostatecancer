library(magrittr)
library(tidyverse)
library(janitor)
library(lubridate)

# Import -------------
rm(list = ls())
nestdb <- readRDS("rdsobj/nestdb_g1.RDS")
nestdb_md5 <- tools::md5sum("rdsobj/nestdb_g1.RDS")

coln <- read.csv('reference/col_names.csv', header = FALSE)

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


## Treatment received percentage
trtrec <- nestdb %>% 
  unnest(data.trt) %>% 
  select(ID,Trt.Mod) %>% 
  distinct() %>% 
  group_by(Trt.Mod) %>% 
  summarise(n = n(),
            pct = n/(nrow(nestdb)) * 100)


## Median upgrading free survival time while in GGG1
medupfreesurv <- nestdb %>%
  mutate(first.biop = map_dbl(data.biop, ~ .x %>%
                              pull(Biopsy.Dat) %>%
                              min())) %>% 
  mutate(max.biop = map_dbl(data.biop, ~ .x %>%
                              filter(!Biopsy.GGG %in% c(1,NA)) %>%
                              pull(Biopsy.Dat) %>%
                              min())) %>%
  mutate(datdiff = as_date(first.biop) %--% as_date(max.biop)/years(1))%>% 
  filter(!datdiff %in% c(0,NA)) %>% 
  summarise(
    med = median(datdiff, na.rm = TRUE),
    max = max(datdiff, na.rm = TRUE),
    min = min(datdiff, na.rm = TRUE))

## Max GGG obtained
maxggg <- nestdb %>% 
  mutate(mg = map_int(data.biop, ~ .x %>%
                        pull(Biopsy.GGG) %>%
                        max(na.rm = TRUE))) %>% 
  group_by(mg) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

## Upgraded from GGG1 to GGG***
nextggg <- nestdb %>% 
  mutate(mg = map_dbl(data.biop, ~ .x %>%
                        pull(Biopsy.GGG) %>%
                        na.omit() %>% 
                        {ifelse(max(.) == 1,1,.[. %in% 2:5] %>% min())})) %>% 
  group_by(mg) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

##Wait time for treatment
wait <- nestdb %>% 
  mutate(firsttrtdat = data.trt %>% 
           map_dbl(~ .x %>% pull(Trt.Dat) %>% min())) %>% 
  mutate(lastbiopsydate = map2_dbl(data.biop, firsttrtdat,
                                   ~ .x %>% 
                                     filter(Biopsy.Dat < .y) %>% 
                                     pull(Biopsy.Dat) %>% max())) %>% 
  mutate(datdiff =  as_date(lastbiopsydate) %--% as_date(firsttrtdat)/years(1)) %>% 
  summarise(
    med = median(datdiff, na.rm = TRUE),
    max = max(datdiff, na.rm = TRUE),
    min = min(datdiff, na.rm = TRUE))

##Interval between first biopsy and first treatment
inttrt <- nestdb %>% 
  mutate(firsttrtdat =  map_dbl(data.trt, ~ .x %>% pull(Trt.Dat) %>% min())) %>% 
  mutate(firstbiopsydat = map_dbl(data.biop, ~ .x %>% pull(Biopsy.Dat) %>% min())) %>% 
  mutate(datdiff =  as_date(firstbiopsydat) %--% as_date(firsttrtdat)/years(1)) %>% 
  summarise(
    med = median(datdiff, na.rm = TRUE),
    max = max(datdiff, na.rm = TRUE),
    min = min(datdiff, na.rm = TRUE))

##First treatment modality
firsttrtmod <- nestdb %>% 
  #Check for empty tables
  mutate(emp = map_dbl(data.trt, ~ .x %>% janitor::remove_empty("rows") %>% nrow()) %>% {ifelse(. > 0,FALSE,TRUE)}) %>%
  mutate(firstmod = map_chr(data.trt, ~ .x %>% arrange(Trt.Dat) %>% pull(Trt.Mod) %>% nth(1))) %>%
  mutate(secmod = map_chr(data.trt, ~ .x %>% arrange(Trt.Dat) %>% pull(Trt.Mod) %>% nth(2))) %>%
  ### NOTE - If LHRH followed by RT or RP, uses RT or RP as first treatment modality.
  mutate(cormod = case_when(
    ((firstmod == "lhrh") & (secmod == "rt")) ~ "rt",
    ((firstmod == "lhrh") & (secmod == "rp")) ~ "rp",
    emp == TRUE ~ "no_treat",
    emp == FALSE ~ firstmod)) %>%
  group_by(cormod) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n) * 100)

# Create a list of all the sub-tables ---------------------

subtbl_trt <- list("Length of follow up" = lenfu,
              "Number of follow up biopsies" = numfubiop,
              "Time between each biopsy" = timebiop,
              "Median time before upgrade from GGG1" = medupfreesurv,
              "Maximal GGG on biopsy" = maxggg,
              "Upgraded GGG from GGG1" = nextggg,
              "Treatment modalities used" = trtrec,
              "Wait time until treatment" = wait,
              "Interval between first diagnostic biopsy & treatment" = inttrt,
              "First treatment modality" = firsttrtmod) %>% 
  map(~ .x %>% mutate_if(is.numeric, ~ round(.,2)))

print(subtbl_trt)
