library(tidyverse)
library(janitor)

# Import  -------------------
coln <- read.csv('col_names.csv', header = FALSE)
db <- read.csv('data/21-Jan-2021_AS_258_Cohort.csv',
               header = TRUE,
               col.names = coln[[2]],
               colClasses = coln[[3]],
               strip.white = TRUE,
               na.strings = c(""," ","NA")) %>% tibble()

# Correct columns ------------------------
##Remove years from column
db$Dx.to.LastFU <- str_remove_all(db$Dx.to.LastFU," years") %>% as.double()

##Removing any negatives.
discarded_rows <- filter_if(db, is.numeric, any_vars(. < 0))
db[db < 0] <- NaN

##Change all string to simple case
db <- mutate(db,across(where(is.character), ~ str_to_lower(.x)))


# Nesting --------------

##Demographics
tmp0 <- db %>% 
  select(ID, AgeDx, Eth, LastFU, LastFU.Orig, Dx.to.LastFU) %>% 
  janitor::remove_empty("rows")

##Family history
tmp1 <- db %>% 
  select(ID, FamHx) %>%
  janitor::remove_empty("rows") %>%
  fill(ID,.direction = "down") %>%
  group_by(ID) %>% 
  nest(.key = "data.fhx")

##Biopsy information
tmp2 <- db %>% 
  select(ID, Biopsy.Dat:Biopsy.PSA, Biopsy.Vol:Biopsy.PosCores) %>% 
  janitor::remove_empty("rows") %>%
  fill(ID,.direction = "down") %>%
  group_by(ID) %>% 
  nest(.key = "data.biop")

##PSA closest to biopsy
tmp3 <- db %>% 
  select(ID, BioClosest.Dat:BioClosest.PSA) %>% 
  janitor::remove_empty("rows") %>%
  fill(ID,.direction = "down") %>%
  group_by(ID) %>% 
  nest(.key = "data.oth")

##Treatment modality
tmp4 <- db %>% 
  select(ID, Trt.Dat:Trt.Mod) %>% 
  janitor::remove_empty("rows") %>%
  fill(ID,.direction = "down") %>%
  group_by(ID) %>% 
  nest(.key = "data.trt")

##Combine into single object
nestdb <- full_join(tmp0,tmp1,by = "ID") %>% 
  full_join(tmp2, by = "ID") %>% 
  full_join(tmp3, by = "ID") %>% 
  full_join(tmp4, by = "ID")
rm(tmp0,tmp1,tmp2,tmp3,tmp4)

##Arrange by ascending ID
nestdb <- arrange(nestdb, ID)


# Special edits -------------

##Change all LHRH or ADT, and remove any duplicates
nestdb <- nestdb %>% 
  mutate(data.trt = data.trt %>% 
           map(~ .x %>% 
                 mutate(Trt.Mod = str_replace_all(Trt.Mod,"adt","lhrh")) %>% 
                 distinct()
               )
         )
  
# Save to RDS ---------------
saveRDS(nestdb, file = "rdsobj/nestdb.RDS")
