library(tidyverse)
library(janitor)

# Import  ---------------------------------------------------------------
rm(list = ls())

coln <- read_csv('reference/col_names.csv', col_names = FALSE)
db <- read.csv('data/21-Jan-2021_AS_258_Cohort.csv',
               header = TRUE,
               col.names = coln[[2]],
               colClasses = coln[[3]],
               strip.white = TRUE,
               na.strings = c(""," ","NA")) %>% tibble()

# Correct columns ---------------------------------------------------
##Remove years from column
db$Dx.to.LastFU <- str_remove_all(db$Dx.to.LastFU," years") %>% as.double()

##Removing any negatives.
discarded_rows <- filter_if(db, is.numeric, any_vars(. < 0))
db[db < 0] <- NaN

##Change all string to simple case
db <- mutate(db,across(where(is.character), ~ str_to_lower(.x)))


# Nesting ---------------------------------------------------

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
  arrange(Biopsy.Dat, .by_group = TRUE) %>% 
  nest(.key = "data.biop")

##PSA closest to biopsy
tmp3 <- db %>% 
  select(ID, BioClosest.Dat:BioClosest.PSA) %>% 
  janitor::remove_empty("rows") %>%
  fill(ID,.direction = "down") %>%
  group_by(ID) %>% 
  arrange(BioClosest.Dat, .by_group = TRUE) %>% 
  nest(.key = "data.oth")

##Treatment modality
tmp4 <- db %>% 
  select(ID, Trt.Dat:Trt.Mod) %>% 
  janitor::remove_empty("rows") %>%
  fill(ID,.direction = "down") %>%
  group_by(ID) %>% 
  arrange(Trt.Dat, .by_group = TRUE) %>% 
  nest(.key = "data.trt")

##Combine into single object
nestdb <- full_join(tmp0,tmp1,by = "ID") %>% 
  full_join(tmp2, by = "ID") %>% 
  full_join(tmp3, by = "ID") %>% 
  full_join(tmp4, by = "ID")
rm(tmp0,tmp1,tmp2,tmp3,tmp4)

##Arrange by ascending ID
nestdb <- arrange(nestdb, ID)


# Special edits ----------------------------------------------------------

##Change all LHRH or ADT, and remove any duplicates
nestdb <- nestdb %>% 
  mutate(data.trt = data.trt %>% 
           map(~ .x %>% 
                 mutate(Trt.Mod = str_replace_all(Trt.Mod,"adt","lhrh")) %>% 
                 distinct()
               ))

## Changes all "biopsy.*****" to "biopsy"
nestdb <- nestdb %>% 
  mutate(data.biop = data.biop %>% 
           map(~ .x %>% 
                 mutate(Biopsy.Dat.Orig = str_replace(Biopsy.Dat.Orig,"biopsy_diagnostic","biopsy"),
                        Biopsy.Dat.Orig = str_replace(Biopsy.Dat.Orig,"biopsy_posttreatment","biopsy"))
               ))

## Change ethnicity
nestdb <- nestdb %>% 
  mutate(Eth = str_replace(Eth,"latin","hispanic"),
         Eth = str_replace(Eth,"south east asian","asian"),
         Eth = str_replace(Eth,"native american","first nations"))

# Alternative file without G1 start --------------------------------------------
### Drop rows all leading rows with NA for Biopsy.GGG
replace_na <- function(df){
  while(is.na(df[1,"Biopsy.GGG"])==TRUE){
    df=df[-1,]
  }
  return(df)
}

### Then extract the first GGG. Then filter based on this value
nestdb_g1 <- nestdb %>% 
  mutate(data.biop = map(data.biop, ~ .x %>% 
                           arrange(Biopsy.Dat) %>% 
                           replace_na())) %>% 
  mutate(firstggg = map_chr(data.biop, ~ pull(.x, Biopsy.GGG) %>% .[[1]])) %>%
  {. ->> temp} %>% 
  filter(firstggg == 1) %>% select(ID:data.trt)

nestdb_gx <- temp %>% filter(!firstggg == 1) %>% select(ID:data.trt)

# Save to RDS ------------------------------------------------------------------
dir.create("rdsobj")
saveRDS(nestdb, file = "rdsobj/nestdb.RDS")
saveRDS(nestdb_g1, file = "rdsobj/nestdb_g1.RDS")
saveRDS(nestdb_gx, file = "rdsobj/nestdb_gx.RDS")
