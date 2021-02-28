library(magrittr)
library(tidyverse)
library(janitor)

# Import -------------
rm(list = ls())
nestdb <- readRDS("rdsobj/nestdb.RDS")
nestdb_md5 <- tools::md5sum("rdsobj/nestdb.RDS")

coln <- read.csv('col_names.csv', header = FALSE)

# Analysis -------------------

issues <- nestdb %>% 
  mutate(treatdat = data.trt %>% 
           map_dbl(~ .x %>% filter(rank(Trt.Dat, ties.method = "first")==1) %>% .[[1]]) %>%
           as_date()) %>%
  mutate(firstpostdat = data.biop %>% 
           map_dbl(~ .x %>% filter(Biopsy.Dat.Orig == "biopsy_posttreatment") %>% filter(rank(Biopsy.Dat, ties.method = "first")==1) %>% .[[1]]) %>% 
           as_date()) %>%
  mutate(test = firstpostdat %--% treatdat/years(1)) %>%
  drop_na()


alltrt <- nestdb %>% 
  mutate(has_trt = data.trt %>% 
           map_int(~ .x %>% janitor::remove_empty("rows") %>% nrow())) %>% 
  filter(has_trt > 0)

which(pull(issues,ID) != pull(alltrt,ID))


# Plot creations -------------------------


new_nestdb <- mutate(nestdb,
                  data.biop = map(data.biop, ~ .x %>% arrange(Biopsy.Dat) %>% drop_na(Biopsy.GGG)))
mod_data <- mutate(new_nestdb,
                  ED = map_dbl(data.biop, ~ .x %>%
                                 filter(Biopsy.GGG != 1) %>% 
                                 pull(Biopsy.Dat) %>% first()) %>% as_date(),
                  CONS = map_dbl(data.biop, ~ .x %>%
                                   slice_tail() %>%
                                   filter(Biopsy.GGG == 1) %>%
                                   pull(Biopsy.Dat) %>% first()) %>% as_date(),
                  ST =  map_dbl(data.biop, ~ .x %>%
                                  pull(Biopsy.Dat) %>% first()) %>% as_date()) %>% 
  mutate(status = ifelse(is.na(CONS),1,0),
         combo = ifelse(is.na(CONS),ED,CONS) %>% as_date(),
         time = ST %--% combo/years(1))

km_surv <- survfit(Surv(time,status) ~ 1,data = mod_data)
ggsurvplot(km_surv, 
           risk.table = FALSE,
           xlab = "Probability of maintaining GGG1",
           ylab = "Time since first GGG1 biopsy (Years)",
           ggtheme = theme_light())

