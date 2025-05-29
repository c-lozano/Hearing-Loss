library(readr)
library(dplyr)
library(nhanesA)
library(rstudioapi)
overwrite <- T

setwd(dirname(getActiveDocumentContext()$path))

# Data Wrangling ###

AUQ99 <- nhanes('AUQ',includelabels = T) |> tibble()
OCQ99 <- nhanes('OCQ',includelabels = T) |> tibble()
DEMO99 <- nhanes('DEMO',includelabels = T) |> tibble()
AUX99 <- nhanes('AUX1',includelabels = T) |> tibble()
# 92-107

AUQ01 <- nhanes('AUQ_B',includelabels = T) |> tibble()
OCQ01 <- nhanes('OCQ_B',includelabels = T) |> tibble()
DEMO01 <- nhanes('DEMO_B',includelabels = T) |> tibble()
AUX01 <- nhanes('AUX_B',includelabels = T) |> tibble()
# 40-55

AUQ03 <- nhanes('AUQ_C',includelabels = T) |> tibble()
OCQ03 <- nhanes('OCQ_C',includelabels = T) |> tibble()
DEMO03 <- nhanes('DEMO_C',includelabels = T) |> tibble()
AUX03 <- nhanes('AUX_C',includelabels = T) |> tibble()
# 39-54

AUQ11 <- nhanes('AUQ_G',includelabels = T) |> tibble()
DEMO11 <- nhanes('DEMO_G',includelabels = T) |> tibble()
AUX11 <- nhanes('AUX_G',includelabels = T) |> tibble()
# 38-53

AUQ15 <- nhanes('AUQ_I',includelabels = T) |> tibble()
# SEQN, AUQ054, AUQ310, AUQ370,    AUQ331, AUQ340, AUQ381
DEMO15 <- nhanes('DEMO_I',includelabels = T) |> tibble()
AUX15 <- nhanes('AUX_I',includelabels = T) |> tibble()
# 38-53

demos <- list(a=DEMO99,b=DEMO01,c=DEMO03,d=DEMO11,e=DEMO15)
auqs <- list(a=AUQ99,b=AUQ01,c=AUQ03,d=AUQ11,e=AUQ15)
ocqs <- list(a=OCQ99,b=OCQ01,c=OCQ03)
auxs <- list(a=AUX99,b=AUX01,c=AUX03,d=AUX11,e=AUX15)

for (c in 1:2){
  demos[[c]] <- demos[[c]] |> 
    select(SEQN, RIAGENDR, RIDAGEYR, WTINT4YR, WTMEC4YR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, INDHHINC, RIDRETH1, DMDEDUC2)
}

demos[[3]] <- demos[[3]] |> 
  select(SEQN, RIAGENDR, RIDAGEYR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, INDHHINC, RIDRETH1, DMDEDUC2)

for (c in 4:length(demos)){
  demos[[c]] <- demos[[c]] |> 
    select(SEQN, RIAGENDR, RIDAGEYR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, INDHHIN2, RIDRETH1, DMDEDUC2)
}

for (c in c('a','b','c')){
  auqs[[c]] <- auqs[[c]] |> 
    select(SEQN, AUQ130, AUQ210, AUQ230)
}
auqs[['d']] <- auqs[['d']] |> 
  select(SEQN, AUQ054, AUQ310, AUQ370, AUQ330, AUQ340, AUQ380)
auqs[['e']] <- auqs[['e']] |> 
  select(SEQN, AUQ054, AUQ310, AUQ370, AUQ331, AUQ340, AUQ381)

ocqs[[1]] <- ocqs[[1]] |> 
  select(SEQN, OCQ150, OCQ340, OCQ350, OCQ390G, OCQ360, OCQ420, OCQ430, OCQ440)
for (c in 2:length(ocqs)){
  ocqs[[c]] <- ocqs[[c]] |> 
    select(SEQN, OCD150, OCQ340, OCQ350, OCD390G, OCQ360, OCQ420, OCQ430, OCQ440)
}

auxsinds <- list(a=92:107, b=40:55, c=39:54, d=38:53, e=38:53, f=55:70)
for(c in 1:length(auxs)){
  auxs[[c]] <- auxs[[c]] |> 
    select(SEQN, auxsinds[[c]])
}

joined <- list()
cohorts <- c(a='1999',b='2001',c='2003',d='2011',e='2015')
for(c in c('a','b','c')){
  joined[[c]] <- demos[[c]] |> 
    full_join(auqs[[c]],by='SEQN') |> 
    full_join(ocqs[[c]],by='SEQN') |> 
    full_join(auxs[[c]],by='SEQN') |> 
    mutate(Cohort=cohorts[c],.before=2)
}
for(c in c('d','e')){
  joined[[c]] <- demos[[c]] |> 
    full_join(auqs[[c]],by='SEQN') |> 
    full_join(auxs[[c]],by='SEQN') |> 
    mutate(Cohort=cohorts[c])
}

dataset <- bind_rows(joined)
if(overwrite) {
  colTypes <- dataset |> head() |> collect() |> lapply(class)
  folderpath <- paste0(getwd(),'/data/')
  if(!dir.exists(folderpath)) dir.create(folderpath)
  write_csv(dataset,paste0(folderpath,'cleaned data.csv'))
  write_csv(as.data.frame(colTypes),paste0(folderpath,'cleaned data column types.csv'))
  dataset <- read_csv(paste0(folderpath,'cleaned data.csv'),col_types=as.list(read_csv(paste0(folderpath,'cleaned data column types.csv'),col_types='c')))
}