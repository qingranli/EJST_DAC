# Data Prep for Figures
# 1. generate crosswalks between census tract maps (2010-2019-2020)
# 2. cross match DAC designations between federal tools 
# 3. cross match DAC designations within NY state
# 4. get population density and demographics 
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
library(bit64) # for long interger in GEOID
library(sf)
library(tidycensus) # get census data
library(ggpubr)

rm(list=ls())
gc()

#####################################################################
# 1. generate crosswalks between census tract maps (2010-2019-2020)
#####################################################################
# get state FIPS codes for 50 states + DC
fips_codes <- tigris::fips_codes
fips_st = fips_codes %>% select(state, state_code, state_name) %>% 
  unique() %>% 
  filter(state_code < 60)

# match census tracts 2019 vs. 2010 =================================
# load tracts19 and tracts10 
US_tracts = list() #73056 tracts
for (s in 1:nrow(fips_st)) {
  state_fips = fips_st$state_code[s]
  tr_save = tracts(state = state_fips, year = 2019)
  US_tracts[[s]] = tr_save %>% data.table() %>% 
    select(STATEFP, GEOID, ALAND, AWATER) 
  print(fips_st$state[s])
}
dt_GEOID19 = rbindlist(US_tracts)
dt_GEOID19$GEOID19 = dt_GEOID19$GEOID
rm(US_tracts, tr_save)

US_tracts = list() #73057 tracts
for (s in 1:nrow(fips_st)) {
  state_fips = fips_st$state_code[s]
  tr_save = tracts(state = state_fips, year = 2010)
  US_tracts[[s]] = tr_save %>% data.table() %>% 
    select(STATEFP10, GEOID10, ALAND10, AWATER10) 
  print(fips_st$state[s])
}
dt_GEOID10 = rbindlist(US_tracts)
rm(US_tracts, tr_save)

# match by GEOID
all_tracts = merge(dt_GEOID10, dt_GEOID19, 
                   by.x = "GEOID10", by.y = "GEOID", all = TRUE)
# 73031 matched tracts with same GEOID in 2010 and 2019
matched_tracts = all_tracts %>% filter(!is.na(STATEFP10), !is.na(STATEFP))
rest_tracts = fread("rest_tracts_2019and2010.txt", 
                    colClasses=list(character=c("GEOID10","GEOID19",
                                                "STATEFP10","STATEFP"),
                                    numeric=c("ALAND10","ALAND",
                                              "AWATER10","AWATER")))

# save crosswalk (2019 vs 2010) =====================================
crosswalk_2019 = rbind(matched_tracts, rest_tracts)
crosswalk_2019 <- crosswalk_2019 %>% 
  select(GEOID10, GEOID19, STATEFP) %>% arrange(GEOID10)
saveRDS(crosswalk_2019, "crosswalk_tr2010_tr2019.rds")

rm(all_tracts, matched_tracts, rest_tracts)
# match census tracts 2020 vs. 2010 =================================
# load tracts20 
US_tracts = list() #84414 tracts
for (s in 1:nrow(fips_st)) {
  state_fips = fips_st$state_code[s]
  tr_save = tracts(state = state_fips, year = 2020)
  US_tracts[[s]] = tr_save %>% data.table() %>% 
    select(STATEFP, GEOID, ALAND, AWATER) 
  print(fips_st$state[s])
}
dt_GEOID20 = rbindlist(US_tracts)
dt_GEOID20$GEOID20 = dt_GEOID20$GEOID
rm(US_tracts, tr_save)

# get crosswalk (from 2020 to 2010)
crosswalk_2020 = fread("nhgis_tr2020_tr2010.csv",
                       colClasses=list(character=c("tr2020ge","tr2010ge"))
                       )
crosswalk_2020 = crosswalk_2020 %>% select(tr2020ge, tr2010ge, parea) %>% 
  rename(tr2020ge_source = tr2020ge,
         tr2010ge_target = tr2010ge)

# check: all GEOID20 are reported in crosswalk
setdiff(dt_GEOID20$GEOID20, unique(crosswalk_2020$tr2020ge_source))
# check: all GEOID10 are reported in crosswalk
setdiff(dt_GEOID10$GEOID10, unique(crosswalk_2020$tr2010ge_target))

crosswalk_2020 <- crosswalk_2020 %>% 
  filter(tr2020ge_source %in% dt_GEOID20$GEOID20,
         tr2010ge_target %in% dt_GEOID10$GEOID10)

length(unique(crosswalk_2020$tr2020ge_source)) #84414
length(unique(crosswalk_2020$tr2010ge_target)) #73057

# parea = proportion of 2020-tract's land area laying in 2010-tracts
# keep the matching tracts with largest area proportion
crosswalk_2020 = crosswalk_2020 %>% group_by(tr2020ge_source) %>% 
  mutate(num_tr2010=n(), area_max = max(parea)) %>% 
  filter(parea == area_max)

crosswalk_2020 <- crosswalk_2020 %>% 
  rename(GEOID20 = tr2020ge_source, GEOID10 = tr2010ge_target) %>% 
  select(GEOID20, GEOID10, parea)

# add State FIPS and GEOID19 (matched to 2020 tracts)
crosswalk_2020_save = left_join(crosswalk_2020, crosswalk_2019,
                                by = "GEOID10")

# save crosswalk (2020 vs 2010) =====================================
setDT(crosswalk_2020_save)
saveRDS(crosswalk_2020_save, "crosswalk_tr2020_tr2010.rds")

#####################################################################
# 2. cross match DAC designations between federal tools 
#####################################################################
rm(crosswalk_2020, fips_codes, dt_GEOID10, dt_GEOID19)

# import DAC designations
CEJST = readRDS("EJ_index_DAC_CEJST_binary.rds")
DOE = readRDS("EJ_index_DAC_DOE.rds")
DOT = readRDS("EJ_index_DAC_DOT.rds")

# extract tractID and DAC (binary indicator)
CEJST1 <- CEJST %>% select(tractID, DAC) %>% 
  rename(GEOID10_num = tractID, DAC_cj = DAC) %>% 
  left_join(crosswalk_2019 %>% 
              mutate(GEOID10_num = as.integer64(GEOID10)) %>% 
              select(GEOID10_num, GEOID10), by = "GEOID10_num")

DOE1 <- DOE %>% select(tractID, DAC) %>% 
  rename(GEOID19_num = tractID, DAC_doe = DAC) %>% 
  left_join(crosswalk_2019 %>% 
              mutate(GEOID19_num = as.integer64(GEOID19)) %>% 
              select(GEOID19_num, GEOID19), by = "GEOID19_num")

DOT1 <- DOT %>% select(tractID, DAC) %>% 
  rename(GEOID20_num = tractID, DAC_dot = DAC) %>% 
  left_join(crosswalk_2020_save %>% 
              mutate(GEOID20_num = as.integer64(GEOID20)) %>% 
              select(GEOID20_num, GEOID20), by = "GEOID20_num")

# use 2020 as benchmark, merge DAC designations
DAC_US_merged = left_join(crosswalk_2020_save, 
                          DOT1 %>% select(-GEOID20_num), by = "GEOID20")

DAC_US_merged = left_join(DAC_US_merged, 
                          DOE1 %>% select(-GEOID19_num), by = "GEOID19")
DAC_US_merged = left_join(DAC_US_merged, 
                          CEJST1 %>% select(-GEOID10_num), by = "GEOID10") %>%
  select(-parea)

# add land and water areas
DAC_US_merged <- DAC_US_merged %>% 
  left_join(dt_GEOID20 %>% select(GEOID20, ALAND, AWATER),
            by = "GEOID20")
summary(DAC_US_merged)

saveRDS(DAC_US_merged, "DAC_US_merged.rds")

#####################################################################
# 3. cross match DAC designations within NY state
#####################################################################
NYS = readRDS("EJ_index_DAC_NYS.rds")
NYS1 = NYS %>% select(tractID, DAC) %>% 
  rename(GEOID19_num = tractID, DAC_ny = DAC) %>% 
  left_join(crosswalk_2019 %>% 
              mutate(GEOID19_num = as.integer64(GEOID19)) %>% 
              select(GEOID19_num, GEOID19), by = "GEOID19_num")

# 5411 census tracts (NY state) in 2020 map
DAC_NY_merged = DAC_US_merged %>% filter(STATEFP == 36) %>% 
  left_join(NYS1 %>% select(-GEOID19_num), by = "GEOID19")

summary(DAC_NY_merged)
saveRDS(DAC_NY_merged, "DAC_NY_merged.rds")

#####################################################################
# 4. get population density and demographics 
#####################################################################
vars_2020 <- load_variables(2020, "pl")
# get population from 2020 census
dt_pop = list() #84414 tracts
for (s in 1:nrow(fips_st)) {
  state_fips = fips_st$state_code[s]
  pop20 <- get_decennial(geography = "tract",
                         variables = c("P1_001N", # total
                                       "P1_003N", # white 
                                       "P1_004N", # black
                                       "P1_005N", # native
                                       "P1_006N", # asian
                                       "P1_007N"), # hawaii
                         year = 2020,
                         state = state_fips, 
                         geometry = TRUE, 
                         output = "wide")
  
  pop20 <- pop20 %>% 
    rename(GEOID20 = GEOID, pop = P1_001N,
           white = P1_003N, black = P1_004N,
           native = P1_005N, asian = P1_006N, hawaii = P1_007N
    ) %>% 
    mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6,  # Convert to km2
           pop_density = pop / area_km2,
           white_share = white/pop,
           black_share = black/pop,
           native_share = native/pop,
           asian_share = asian/pop,
           hawaii_share = hawaii/pop) %>% 
    data.table() %>% select(-NAME, -geometry)
  
  dt_pop[[s]] = pop20
  print(fips_st$state[s])
}

dt_pop_US = rbindlist(dt_pop)
saveRDS(dt_pop_US, "US_tr2020_demography.rds")



