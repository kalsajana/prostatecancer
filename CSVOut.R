library(tidyverse)
library(magrittr)
library(glue)
library(janitor)

# Import -------------
rm(list = ls())
nestdb <- readRDS("rdsobj/nestdb.RDS")
nestdb_md5 <- tools::md5sum("rdsobj/nestdb.RDS")

coln <- read.csv('col_names.csv', header = FALSE)

# Other edits -------------------
tempdb <- nestdb 

## Remove TURPS from the biopsies
tempdb %<>% 
  mutate(data.biop = map(data.biop, ~ .x %>% 
                           filter(Biopsy.Dat.Orig == "biopsy") %>% 
                           select(!Biopsy.Dat.Orig)))

## Remove the names of persons who had a family hx
tempdb %<>% 
  mutate(posFamHx = map_lgl(data.fhx,~ .x %>% janitor::remove_empty("rows") %>% {ifelse(nrow(.) == 0,FALSE,TRUE)})) %>% 
  select(!data.fhx)

# Remove LastFUOrig & dat.oth
tempdb %<>% select(!c("LastFU.Orig","data.oth"))

# WIDENING----------------
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

widedb <- tempdb_wide; rm(tempdb_wide)
write_csv(widedb, file = "output/widetbl.csv")

# SEP_Tables grouped by ID ---------------------
patInfo <- tempdb %>% 
  select(!c(data.biop,data.trt))

trtInfo <- tempdb %>% 
  select(c(ID,data.trt)) %>% 
  unnest(data.trt, keep_empty = TRUE)

biopsyInfo <- tempdb %>% 
  select(c(ID,data.biop)) %>% 
  unnest(data.biop, keep_empty = TRUE) 

dir.create("output/nesteddf")
write_csv(patInfo, file = "output/nesteddf/patInfo.csv")
write_csv(trtInfo, file = "output/nesteddf/trtInfo.csv")
write_csv(biopsyInfo, file = "output/nesteddf/biopsyInfo.csv")
