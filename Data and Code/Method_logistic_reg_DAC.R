# generate post-lasso logistic regression result (4 EJSTs)
# Qingran Li (last edit Oct 2024)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
# library(tigris)
library(bit64) # for long interger in GEOID
# library(openxlsx)
library(dplyr)
library(MASS)
library(glmnet)
library(caret)
library(progress)

rm(list=ls())
gc()

`%notin%` <- Negate(`%in%`)

# extract tracts that are identified as tribal DACs in CEJST ========
dt_tribal = readRDS("tracts2010_tribal_CEJST.rds")
cw1019 = readRDS("crosswalk_tr2010_tr2019.rds")
cw1020 = readRDS("crosswalk_tr2020_tr2010.rds")
cw1019 <- cw1019 %>% mutate(GEOID2010 = as.integer64(GEOID10)) %>% 
  left_join(dt_tribal, by = "GEOID2010")
cw1020 <- cw1020 %>% mutate(GEOID2010 = as.integer64(GEOID10)) %>% 
  left_join(dt_tribal, by = "GEOID2010")

dt10_tribal <- dt_tribal %>% filter(DAC_Tribal == TRUE) %>% 
  rename(tractID = GEOID2010) %>% dplyr::select(tractID, Tribal_Percent)
dt19_tribal <- cw1019 %>% filter(DAC_Tribal == TRUE) %>% 
  mutate(tractID = as.integer64(GEOID19)) %>% dplyr::select(tractID, Tribal_Percent)
dt20_tribal <- cw1020 %>% filter(DAC_Tribal == TRUE) %>% 
  mutate(tractID = as.integer64(GEOID20)) %>% dplyr::select(tractID, Tribal_Percent)
rm(cw1019, cw1020, dt_tribal)

#####################################################################
# 0. Prepare data for regression 
#####################################################################
toolName = "NYS"   # select tool (CEJST, DOE, DOT, NYS)

# import data (DAC and indicators) 
# import indicator name and category labels from "dictionary" files
# exclude tribal DAC tracts from all EJSTs before regression
if (toolName == "CEJST") {
  dt0 = readRDS("EJ_index_DAC_CEJST_binary.rds") # *****CEJST
  dict = fread("CEJST_data_cols_binary.csv")
  dt <- dt0 %>% filter(tractID %notin% dt10_tribal$tractID)
} else if (toolName == "DOE") {
  dt0 = readRDS("EJ_index_DAC_DOE_v2.rds") # *****DOE
  dict = fread("DOE_data_cols.csv")
  dt <- dt0 %>% filter(tractID %notin% dt19_tribal$tractID)
} else if (toolName == "DOT") {
  dt0 = readRDS("EJ_index_DAC_DOT_v2.rds") # *****DOT
  dict = fread("DOT_data_cols.csv")
  dt <- dt0 %>% filter(tractID %notin% dt20_tribal$tractID)
} else if (toolName == "NYS") {
  dt0 = readRDS("EJ_index_DAC_NYSERDA.rds") #*****NY State
  dict = fread("NYS_data_cols.csv")
  dt <- dt0 %>% filter(tractID %notin% dt19_tribal$tractID)
}

summary(dt)
colnames(dt)

#####################################################################
# 1. post-LASSO: logistic regression (lambda use 1SE rule)
#####################################################################
dt1 = na.omit(dt) # remove data containing NA values
y = dt1$DAC
# change column index accordingly
if (toolName == "CEJST") {
  x = data.matrix(dt1[,5:36]) # *****CEJST (32 indicators)
} else if (toolName == "DOE") {
  x = data.matrix(dt1[,5:40]) # ***DOE (36 indicators)
} else if (toolName == "DOT") {
  x = data.matrix(dt1[,5:44]) # ***DOT (40 indicators)
} else if (toolName == "NYS") {
  x = data.matrix(dt1[,4:48]) # ***NY State (45 indicators)
}

# step 1: select lambda ---------------------------------------------
# use the one-standard-error (1SE) rule: The std err of the CV estimate
# ... is calculated for each fold. Use the most parsimonious model where the 
# ... CV error is within 1 SE of the minimum CV error.
list_lambda = c(1:10)
pb <- progress_bar$new(total = length(list_lambda))

time.start = Sys.time()
set.seed(7)
for(i in 1:length(list_lambda)){
  cv.model <- cv.glmnet(x,y,family = "binomial", alpha = 1)
  list_lambda[i] = cv.model$lambda.1se
  pb$tick()
}
print(Sys.time()-time.start) # ~5min

best_lambda = mean(list_lambda) # get the average value from 10 iterations
print(best_lambda)
#CEJST best lambda: 0.001119173
#DOE best lambda: 0.001177192
#DOT best lambda: 0.0007000028
#NYS best lambda: 0.001193789

# step 2: lasso regression ------------------------------------------
regfit_lasso <- glmnet(x, y, family = "binomial", 
                      alpha = 1, 
                      lambda = best_lambda)
est.lasso = coef(regfit_lasso) # get coefficient

# get list of variables post lasso regression
est.dt = data.table(col_name = rownames(est.lasso)[-1],
                    estLasso = est.lasso[-1]) %>% 
  filter(estLasso != 0)

# step 3: post-lasso logistic regression ----------------------------
selected_cols = c("DAC",est.dt$col_name)
dt_post <- dt[, ..selected_cols]
regfit <- glm(DAC ~., data = dt_post, family = "binomial")


# save regression object by tool ********************************
if (toolName == "CEJST") {
  regfit_cejst = regfit
} else if (toolName == "DOE") {
  regfit_doe = regfit
} else if (toolName == "DOT") {
  regfit_dot = regfit
} else if (toolName == "NYS") {
  regfit_nys = regfit
}

# save result to list object ======================================
regfitList = list()
regfitList[[1]] = regfit_cejst 
regfitList[[2]] = regfit_doe 
regfitList[[3]] = regfit_dot
regfitList[[4]] = regfit_nys
# saveRDS(regfitList, file="regfit_postLasso_1SE.RData")


#####################################################################
# 2. post-LASSO: logistic regression (lambda use min-Error rule)
#####################################################################
dt1 = na.omit(dt) # remove data containing NA values
y = dt1$DAC
# change column index accordingly
if (toolName == "CEJST") {
  x = data.matrix(dt1[,5:36]) # *****CEJST (32 indicators)
} else if (toolName == "DOE") {
  x = data.matrix(dt1[,5:40]) # ***DOE (36 indicators)
} else if (toolName == "DOT") {
  x = data.matrix(dt1[,5:44]) # ***DOT (40 indicators)
} else if (toolName == "NYS") {
  x = data.matrix(dt1[,4:48]) # ***NY State (45 indicators)
}

# step 1: select lambda ---------------------------------------------
# Select lambda that minimizes cross-validation error.
list_lambda = c(1:10)
pb <- progress_bar$new(total = length(list_lambda))

time.start = Sys.time()
set.seed(7)
for(i in 1:length(list_lambda)){
  cv.model <- cv.glmnet(x,y,family = "binomial", alpha = 1)
  list_lambda[i] = cv.model$lambda.min
  pb$tick()
}
print(Sys.time()-time.start) # ~5min

best_lambda = mean(list_lambda) # get the average value from 10 iterations
print(best_lambda)
#CEJST best lambda: 0.0001404431
#DOE best lambda: 8.266589e-05
#DOT best lambda: 0.0001107997
#NYS best lambda: 0.000269811

# step 2: lasso regression ------------------------------------------
regfit_lasso <- glmnet(x, y, family = "binomial", 
                       alpha = 1, 
                       lambda = best_lambda)
est.lasso = coef(regfit_lasso) # get coefficient

# get list of variables post lasso regression
est.dt = data.table(col_name = rownames(est.lasso)[-1],
                    estLasso = est.lasso[-1]) %>% 
  filter(estLasso != 0)

# step 3: post-lasso logistic regression ----------------------------
selected_cols = c("DAC",est.dt$col_name)
dt_post <- dt[, ..selected_cols]
regfit <- glm(DAC ~., data = dt_post, family = "binomial")

# save regression object by tool ********************************
if (toolName == "CEJST") {
  regfit_cejst = regfit
} else if (toolName == "DOE") {
  regfit_doe = regfit
} else if (toolName == "DOT") {
  regfit_dot = regfit
} else if (toolName == "NYS") {
  regfit_nys = regfit
}

# save result to list object ======================================
regfitList = list()
regfitList[[1]] = regfit_cejst 
regfitList[[2]] = regfit_doe 
regfitList[[3]] = regfit_dot
regfitList[[4]] = regfit_nys
# saveRDS(regfitList, file="regfit_postLasso_minE.RData")
