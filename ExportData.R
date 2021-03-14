library(tidyverse)
library(magrittr)
library(glue)
library(janitor)
library(jsonlite)

# Import & Create required directories ---------------------------------------------
rm(list = ls())
nestdb <- readRDS("rdsobj/nestdb.RDS")
nestdb_md5 <- tools::md5sum("rdsobj/nestdb.RDS")

## Create folder for outputs if it does not exist
dir.create("output")

# Universal edits ---------------------------------------------------------------
tempdb <- nestdb 

## Remove TURPS from the biopsies
tempdb %<>% 
  mutate(data.biop = map(data.biop, ~ .x %>% 
                           filter(Biopsy.Dat.Orig == "biopsy") %>% 
                           select(!Biopsy.Dat.Orig)))

## Remove the names of persons who had a family hx
tempdb %<>% 
  mutate(posFamHx = map_lgl(data.fhx,~ .x %>% janitor::remove_empty("rows") %>% {ifelse(nrow(.) == 0,FALSE,TRUE)}),.before = "LastFU") %>% 
  select(!data.fhx)

# Remove LastFUOrig & dat.oth
tempdb %<>% select(!c("LastFU.Orig","data.oth"))

# WIDE CSV-----------------------------------------------------------------------
tempdb_wide <- tempdb

## Unnest Treatment stat
tempdb_wide %<>% 
  mutate(data.trt = map(data.trt, ~ .x %>%
                          mutate(across(everything(), ~ .x %>%  as.character() %>% replace_na(NaN))) %>% 
                          mutate(ID.Trt = glue('Treatment.{row_number()}')) %>% 
                          pivot_longer(!ID.Trt,names_to = "Type",values_to = "Cases") %>% 
                          mutate(ID.Trt = glue('{ID.Trt}_{Type}')) %>% 
                          select(!Type)
  )) %>% 
  unnest(data.trt, keep_empty = TRUE) %>% 
  group_by(ID) %>% 
  pivot_wider(names_from = ID.Trt, values_from = Cases)

## Unnest Biopsy stat
tempdb_wide %<>% 
  mutate(data.biop = map(data.biop, ~ .x %>%
                           mutate(across(everything(), ~ .x %>%  as.character() %>% replace_na(NaN))) %>% 
                           mutate(ID.Biop = glue('Biopsy.{row_number()}')) %>% 
                           pivot_longer(!ID.Biop,names_to = "Type",values_to = "Cases") %>% 
                           mutate(ID.Biop = glue('{ID.Biop}_{Type}')) %>% 
                           select(!Type)
  )) %>% 
  unnest(data.biop, keep_empty = TRUE) %>% 
  group_by(ID) %>% 
  pivot_wider(names_from = ID.Biop, values_from = Cases)

write_csv(tempdb_wide, file = "output/DB_Wide.csv")

# SEPERATE CSV ---------------------------------------------------------------------
patInfo <- tempdb %>% 
  select(!c(data.biop,data.trt))

trtInfo <- tempdb %>% 
  select(c(ID,data.trt)) %>% 
  unnest(data.trt, keep_empty = TRUE)

biopsyInfo <- tempdb %>% 
  select(c(ID,data.biop)) %>% 
  unnest(data.biop, keep_empty = TRUE) 

dir.create("output/DB_Split")
write_csv(patInfo, file = "output/DB_Split/DBSplit_PatInfo.csv")
write_csv(trtInfo, file = "output/DB_Split/DBSplit_TrtInfo.csv")
write_csv(biopsyInfo, file = "output/DB_Split/DBSplit_BiopsyInfo.csv")

# JSON -----------------------------------------------------------------------------
exportJSON <-toJSON(tempdb,pretty = TRUE)
write(exportJSON,"output/DB_Nested.json")

#RDS --------------------------------------------------------------------------------
write_rds(tempdb,"output/DB_Nested.rds")
