# Setup ####
library(dplyr)
library(tidyr)
library(forcats)
library(survey)
library(knitr)
library(ggeffects)
library(pacman)
library(readr)
library(ggplot2)

getFreshData <- F
if(getFreshData) source('data-collection.R')
rm(list=ls())

overwrite <- F
makePlots <- F

# Data wrangling ####
folderpath <- paste0(getwd(),'/data/')
ds <- read_csv(paste0(folderpath,'cleaned data.csv'),col_types=as.list(read_csv(paste0(folderpath,'cleaned data column types.csv'),col_types='c')))

## Constructing sampling weights ####
ds99_01 <- ds |> 
  filter(Cohort==1999 | Cohort==2001) |> 
  mutate(WTMEC10YR=(4/10)*WTMEC4YR) |> 
  mutate(WTINT10YR=(4/10)*WTINT4YR) |> 
  select(!ends_with('4YR'))

ds <- ds |> 
  filter(Cohort!=1999 & Cohort!=2001) |> 
  mutate(WTMEC10YR=(2/10)*WTMEC2YR) |> 
  mutate(WTINT10YR=(2/10)*WTINT2YR) |> 
  bind_rows(ds99_01) |> 
  arrange(SEQN)

rm(ds99_01)

## Time variable ####
ds <- ds |> 
  mutate(Cohort=as.numeric(Cohort))

## OELN ####
ds <- ds |> 
  mutate(OCQ150=if_else(is.na(OCQ150),OCD150,OCQ150),
         OCQ390G=if_else(is.na(OCQ390G),OCD390G,OCQ390G)) |> 
  select(!c(OCD150,OCD390G))

dstemp <- ds |> 
  select(Cohort,starts_with('OCQ'), starts_with('AUQ')) |> 
  mutate(across(!c(Cohort,OCQ360,OCQ440), ~if_else(is.na(.x) | .x %in% c('Refused','Don\'t know'),'na',.x)),
         across(c(OCQ360,OCQ440), ~if_else(is.na(.x) | .x %in% c(7777), 9999,.x)),
         OCQ150=if_else(OCQ150 %in% c('Working at a job or business,','With a job or business but not at work,'), 'Yes',OCQ150),
         OCQ150=if_else(OCQ150 %in% c('Not working at a job or business?','Looking for work, or'), 'No',OCQ150),
         OCQ390G=if_else(OCQ390G %in% c('Enter occupation','Armed forces'), 'Other',OCQ390G),
         across(c(AUQ330,AUQ331), ~if_else(.x %in% c('Never worked'),'No',.x)),
         AUQ340=if_else(!(AUQ340 %in% c('Less than 3 months','na')), 'Yes',AUQ340),
         AUQ340=if_else(AUQ340 %in% c('Less than 3 months'), 'No',AUQ340))

OELNvec <- rep(NA,nrow(ds))
for(i in 1:nrow(ds)){
  cht <- dstemp$Cohort[i]
  o150 <- dstemp$OCQ150[i]
  o340 <- dstemp$OCQ340[i]
  o350 <- dstemp$OCQ350[i]
  o360 <- dstemp$OCQ360[i]
  o390 <- dstemp$OCQ390G[i]
  o420 <- dstemp$OCQ420[i]
  o430 <- dstemp$OCQ430[i]
  o440 <- dstemp$OCQ440[i]
  
  a330 <- dstemp$AUQ330[i]
  a331 <- dstemp$AUQ331[i]
  a340 <- dstemp$AUQ340[i]
  
  if(cht %in% c(1999,2001,2003)){
    if(
      (o360>=4 & o360!=9999 & o350=='Yes' & o340=='Yes' & o150=='Yes') |
      (o440>=4 & o440!=9999 & o430=='Yes' & o390=='Other' & o360<4 & o350=='Yes' & o340=='Yes' & o150=='Yes') |
      (o440>=4 & o440!=9999 & o430=='Yes' & o390=='Other' & o350=='No' & o340=='Yes' & o150=='Yes') |
      (o440>=4 & o440!=9999 & o430=='Yes' & o420=='Yes' & o390=='Other' & o150=='No')
    ) OELNvec[i] <- 'Yes'
    
    else if(
      (o340=='No' & o150=='Yes') |
      (o390=='Never worked' & o150=='No') |
      (o420=='No' & o390=='Other' & o150=='No')
    ) OELNvec[i] <- 'No'
    
    else if(
      (o390=='Same as current occupation' & o360<4 & o350=='Yes' & o340=='Yes' & o150=='Yes') |
      (o390=='Same as current occupation' & o350=='No' & o340=='Yes' & o150=='Yes') |
      (o430=='No' & o390=='Other' & o360<4 & o350=='Yes' & o340=='Yes' & o150=='Yes') |
      (o430=='No' & o390=='Other' & o350=='No' & o340=='Yes' & o150=='Yes') |
      (o430=='No' & o420=='Yes' & o390=='Other' & o150=='Yes') |
      (o440<4 & o430=='Yes' & o390=='Other' & o360<4 & o350=='Yes' & o340=='Yes' & o150=='Yes') |
      (o440<4 & o430=='Yes' & o390=='Other' & o350=='No' & o340=='Yes' & o150=='Yes') |
      (o440<4 & o430=='Yes' & o420=='Yes' & o390=='Other' & o150=='Yes')
    ) OELNvec[i] <- '?'
    
    else OELNvec[i] <- NA
  }
  else if(cht==2011){
    if (a340=='Yes' & a330=='Yes') OELNvec[i] <- 'Yes'
    else if (a330=='No') OELNvec[i] <- 'No'
    else OELNvec[i] <- NA
  }
  else{
    if (a340=='Yes' & a331=='Yes') OELNvec[i] <- 'Yes'
    else if (a331=='No') OELNvec[i] <- 'No'
    else OELNvec[i] <- NA
  }
}

ds <- ds |> 
  mutate(OELN=OELNvec)

## Hearing condition ####
### Self-reported ####

hearVec <- rep(NA,nrow(ds))
for(i in 1:nrow(ds)){
  a130 <- ds$AUQ130[i]
  a054 <- ds$AUQ054[i]
  temp <- c('n','n')
  
  if ((a130 %in% c('Refused','Don\'t know',NA))) temp[1] <- 'n'
  else if(a130=='Good') temp[1] <- 'f'
  else temp[1] <- 't'
  
  if ((a054 %in% c('Refused','Don\'t know',NA))) temp[2] <- 'n'
  else if(a054 %in% c('Excellent','Good')) temp[2] <- 'f'
  else temp[2] <- 't'
  
  if (any(temp=='t')) hearVec[i] <- 'Yes'
  else if (any(temp=='f')) hearVec[i] <- 'No'
  else hearVec[i] <- NA
}

ds <- ds |> 
  mutate(hearingLossSR=hearVec)

### Audiometry ####
hearVec <- rep(NA,nrow(ds))
for (i in 1:nrow(ds)){
  a1k1r <- ds$AUXU1K1R[i]
  a1k2r <- ds$AUXU1K2R[i]
  a1k1l <- ds$AUXU1K1L[i]
  a1k2l <- ds$AUXU1K2L[i]
  a500r <- ds$AUXU500R[i]
  a500l <- ds$AUXU500L[i]
  a2kr <- ds$AUXU2KR[i]
  a2kl <- ds$AUXU2KL[i]
  a4kr <- ds$AUXU4KR[i]
  a4kl <- ds$AUXU4KL[i]
  
  temp <- c(a1k1r, a1k2r, a1k1l, a1k2l, a500r, a500l,a2kr,a2kl,a4kr,a4kl)
  if (any(is.na(temp))) next
  for (j in 1:length(temp)){
    if(temp[j] %in% c(666,888,NA)) temp[j] <- NA
  }
  
  a1kr <- mean(c(a1k1r, a1k2r))
  a1kl <- mean(c(a1k1l, a1k2l))
  
  pta <- min( mean(c(a500r,a1kr,a2kr,a4kr)),
              mean(c(a500l,a1kl,a2kl,a4kl)) )
  
  if(is.na(pta)) hearVec[i] <- NA
  else if(pta>=25) hearVec[i] <- 'Yes'
  else hearVec[i] <- 'No'
}

ds <- ds |>
  mutate(hearingLossAE=hearVec)



hearVec <- rep(NA,nrow(ds))
for (i in 1:nrow(ds)){
  a3kr <- ds$AUXU3KR[i]
  a3kl <- ds$AUXU3KL[i]
  a4kr <- ds$AUXU4KR[i]
  a4kl <- ds$AUXU4KL[i]
  a6kr <- ds$AUXU6KR[i]
  a6kl <- ds$AUXU6KL[i]
  a8kr <- ds$AUXU8KR[i]
  a8kl <- ds$AUXU8KL[i]
  
  temp <- c(a3kr,a3kl, a4kr,a4kl, a6kr,a6kl, a8kr,a8kl)
  if (any(is.na(temp))) next
  for (j in 1:length(temp)){
    if(temp[j] %in% c(666,888,NA)) temp[j] <- NA
    next
  }
  
  pta <- min( mean(temp[c(1,3,5,7)]),
              mean(temp[c(2,4,6,8)]) )
  
  if(is.na(pta)) hearVec[i] <- NA
  else if(pta>=25) hearVec[i] <- 'Yes'
  else hearVec[i] <- 'No'
}

ds <- ds |>
  mutate(hearingLossAE_HF=hearVec)

## Demographics ####

### Ethnicity ####
ds <- ds |>
  mutate(RIDRETH1=as.character(RIDRETH1)) |> 
  mutate(RIDRETH1=case_when(
    RIDRETH1 %in% c('Mexican American','Other Hispanic') ~ 'Hispanic',
    RIDRETH1 %in% 'Non-Hispanic Black' ~ 'Black',
    RIDRETH1 %in% 'Non-Hispanic White' ~ 'White',
    RIDRETH1 %in% 'Other Race - Including Multi-Racial' ~ 'Other',
    .default=RIDRETH1
  )) |> 
  mutate(RIDRETH1=factor(RIDRETH1,levels=c('White','Black','Hispanic','Other')))


### Income ####
temp <- ds |> 
  select(INDHHINC) |> 
  pull() |> 
  as.character()

uqTemp <- unique(temp)

for (i in 1:length(temp)){
  if(is.na(temp[i])) next
  if(temp[i] %in% uqTemp[c(1,5,8,11,15)]) temp[i] <- 'Under $20,000'
  else if(temp[i] %in% uqTemp[c(7,3,9)]) temp[i] <- '$20,000 to $44,999'
  else if(temp[i] %in% uqTemp[c(2,14,10)]) temp[i] <- '$45,000 to $74,999'
  else if(temp[i] %in% uqTemp[4]) temp[i] <- temp[i]
  else temp[i] <- 'Refused/unsure'
}

temp2 <- ds |> 
  select(INDHHIN2) |> 
  pull() |> 
  as.character()

uqTemp2 <- unique(temp2)

for (i in 1:length(temp2)){
  if(is.na(temp2[i])) temp2[i] <- temp[i]
  else if(temp2[i] %in% uqTemp2[c(14,8,13,3,11)]) temp2[i] <- 'Under $20,000'
  else if(temp2[i] %in% uqTemp2[c(10,16,17)]) temp2[i] <- '$20,000 to $44,999'
  else if(temp2[i] %in% uqTemp2[c(5,9,12)]) temp2[i] <- '$45,000 to $74,999'
  else if(temp2[i] %in% uqTemp2[c(2,4)]) temp2[i] <- '$75,000 and Over'
  else temp2[i] <- 'Refused/unsure'
}

ds <- ds |> 
  mutate(Income=factor(temp2))

### Education ####
temp <- ds |> 
  select(DMDEDUC2) |> 
  pull() |> 
  as.character()

uqTemp <- unique(temp)

for (i in 1:length(temp)){
  if(is.na(temp[i])) next
  if(temp[i] %in% uqTemp[c(3,6,12,13)]) temp[i] <- 'Less than high school'
  else if(temp[i] %in% uqTemp[c(4,9)]) temp[i] <- 'High school graduate'
  else if(temp[i] %in% uqTemp[c(2,5,10,11)]) temp[i] <- 'Some college or more'
  else temp[i] <- NA
}

ds <- ds |> 
  mutate(Education=factor(temp))


## Cleaning and prep for subsetting ####

ds <- ds |> 
  select(SEQN,Cohort,SDMVPSU,SDMVSTRA,WTINT2YR,WTMEC2YR,WTINT10YR,WTMEC10YR,RIAGENDR,RIDAGEYR,RIDRETH1,Income,Education, OELN,hearingLossSR,hearingLossAE,hearingLossAE_HF) |> 
  mutate(OELN=if_else(OELN %in% c('?'),'No',OELN)) |> 
  rename(Gender=RIAGENDR,
         Age=RIDAGEYR,
         Ethnicity=RIDRETH1,
         'OELN'=OELN,
         'SRHL'=hearingLossSR,
         'AEHL'=hearingLossAE,
         'AEHFHL'=hearingLossAE_HF) |> 
  mutate(inAnalysis=(Age>=20 & Age<70 & !is.na(Income) & !is.na(Education) & !is.na(OELN) & !is.na(SRHL) & !(!is.na(AEHL) & is.na(AEHFHL))),
         inAnalysis_AE=(inAnalysis & !is.na(AEHL) & !is.na(AEHFHL)),
         OELN=factor(OELN),
         SRHL=factor(SRHL),
         AEHL=factor(AEHL),
         AEHFHL=factor(AEHFHL))

# Descriptive statistics ####

dsSubUW <- ds |> 
  filter(inAnalysis)
dsSubUW_AE <- ds |> 
  filter(inAnalysis_AE)

demogs <- dsSubUW |> 
  group_by(Cohort) |> 
  summarise(
    n=as.character(n()),
    Age=paste0(round(mean(Age),2),' (',round(sd(Age)/sqrt(n()),2),')'),
    Female=paste0(sum(Gender=='Female'),' (',round(100*sum(Gender=='Female')/n(),1),')'),
    White=paste0(sum(Ethnicity=='White'),' (',round(100*sum(Ethnicity=='White')/n(),1),')'),
    Black=paste0(sum(Ethnicity=='Black'),' (',round(100*sum(Ethnicity=='Black')/n(),1),')'),
    Hispanic=paste0(sum(Ethnicity=='Hispanic'),' (',round(100*sum(Ethnicity=='Hispanic')/n(),1),')'),
    Other=paste0(sum(Ethnicity=='Other'),' (',round(100*sum(Ethnicity=='Other')/n(),1),')'),
    'Under $20,000'=paste0(sum(Income=='Under $20,000'),' (',round(100*sum(Income=='Under $20,000')/n(),1),')'),
    '$20,000 to $44,999'=paste0(sum(Income=='$20,000 to $44,999'),' (',round(100*sum(Income=='$20,000 to $44,999')/n(),1),')'),
    '$45,000 to $74,999'=paste0(sum(Income=='$45,000 to $74,999'),' (',round(100*sum(Income=='$45,000 to $74,999')/n(),1),')'),
    '$75,000 and Over'=paste0(sum(Income=='$75,000 and Over'),' (',round(100*sum(Income=='$75,000 and Over')/n(),1),')'),
    'Refused/unsure'=paste0(sum(Income=='Refused/unsure'),' (',round(100*sum(Income=='Refused/unsure')/n(),1),')'),
    'Less than high school'=paste0(sum(Education=='Less than high school'),' (',round(100*sum(Education=='Less than high school')/n(),1),')'),
    'High school graduate'=paste0(sum(Education=='High school graduate'),' (',round(100*sum(Education=='High school graduate')/n(),1),')'),
    'Some college or more'=paste0(sum(Education=='Some college or more'),' (',round(100*sum(Education=='Some college or more')/n(),1),')'),
    'History of OELN'=paste0(sum(OELN=='Yes'),' (',round(100*sum(OELN=='Yes')/n(),1),')'),
    'Self-reported HL'=paste0(sum(SRHL=='Yes'),' (',round(100*sum(SRHL=='Yes')/n(),1),')')
  ) |> 
  mutate(Cohort=c('1999--2000','2001--2002','2003--2004','2011--2012','2015--2016'))
  
demogs <- dsSubUW |> 
  summarise(
    n=as.character(n()),
    Age=paste0(round(mean(Age),2),' (',round(sd(Age)/sqrt(n()),2),')'),
    Female=paste0(sum(Gender=='Female'),' (',round(100*sum(Gender=='Female')/n(),1),')'),
    White=paste0(sum(Ethnicity=='White'),' (',round(100*sum(Ethnicity=='White')/n(),1),')'),
    Black=paste0(sum(Ethnicity=='Black'),' (',round(100*sum(Ethnicity=='Black')/n(),1),')'),
    Hispanic=paste0(sum(Ethnicity=='Hispanic'),' (',round(100*sum(Ethnicity=='Hispanic')/n(),1),')'),
    Other=paste0(sum(Ethnicity=='Other'),' (',round(100*sum(Ethnicity=='Other')/n(),1),')'),
    'Under $20,000'=paste0(sum(Income=='Under $20,000'),' (',round(100*sum(Income=='Under $20,000')/n(),1),')'),
    '$20,000 to $44,999'=paste0(sum(Income=='$20,000 to $44,999'),' (',round(100*sum(Income=='$20,000 to $44,999')/n(),1),')'),
    '$45,000 to $74,999'=paste0(sum(Income=='$45,000 to $74,999'),' (',round(100*sum(Income=='$45,000 to $74,999')/n(),1),')'),
    '$75,000 and Over'=paste0(sum(Income=='$75,000 and Over'),' (',round(100*sum(Income=='$75,000 and Over')/n(),1),')'),
    'Refused/unsure'=paste0(sum(Income=='Refused/unsure'),' (',round(100*sum(Income=='Refused/unsure')/n(),1),')'),
    'Less than high school'=paste0(sum(Education=='Less than high school'),' (',round(100*sum(Education=='Less than high school')/n(),1),')'),
    'High school graduate'=paste0(sum(Education=='High school graduate'),' (',round(100*sum(Education=='High school graduate')/n(),1),')'),
    'Some college or more'=paste0(sum(Education=='Some college or more'),' (',round(100*sum(Education=='Some college or more')/n(),1),')'),
    'History of OELN'=paste0(sum(OELN=='Yes'),' (',round(100*sum(OELN=='Yes')/n(),1),')'),
    'Self-reported HL'=paste0(sum(SRHL=='Yes'),' (',round(100*sum(SRHL=='Yes')/n(),1),')')
  ) |> 
  mutate(Cohort='Overall',.before=1) |> 
  bind_rows(demogs) |> 
  pivot_longer(-1) |> 
  pivot_wider(names_from=1,values_from=value)

demogs_AE <- dsSubUW_AE |> 
  group_by(Cohort) |> 
  summarise(
    n=as.character(n()),
    Age=paste0(round(mean(Age),2),' (',round(sd(Age)/sqrt(n()),2),')'),
    Female=paste0(sum(Gender=='Female'),' (',round(100*sum(Gender=='Female')/n(),1),')'),
    White=paste0(sum(Ethnicity=='White'),' (',round(100*sum(Ethnicity=='White')/n(),1),')'),
    Black=paste0(sum(Ethnicity=='Black'),' (',round(100*sum(Ethnicity=='Black')/n(),1),')'),
    Hispanic=paste0(sum(Ethnicity=='Hispanic'),' (',round(100*sum(Ethnicity=='Hispanic')/n(),1),')'),
    Other=paste0(sum(Ethnicity=='Other'),' (',round(100*sum(Ethnicity=='Other')/n(),1),')'),
    'Under $20,000'=paste0(sum(Income=='Under $20,000'),' (',round(100*sum(Income=='Under $20,000')/n(),1),')'),
    '$20,000 to $44,999'=paste0(sum(Income=='$20,000 to $44,999'),' (',round(100*sum(Income=='$20,000 to $44,999')/n(),1),')'),
    '$45,000 to $74,999'=paste0(sum(Income=='$45,000 to $74,999'),' (',round(100*sum(Income=='$45,000 to $74,999')/n(),1),')'),
    '$75,000 and Over'=paste0(sum(Income=='$75,000 and Over'),' (',round(100*sum(Income=='$75,000 and Over')/n(),1),')'),
    'Refused/unsure'=paste0(sum(Income=='Refused/unsure'),' (',round(100*sum(Income=='Refused/unsure')/n(),1),')'),
    'Less than high school'=paste0(sum(Education=='Less than high school'),' (',round(100*sum(Education=='Less than high school')/n(),1),')'),
    'High school graduate'=paste0(sum(Education=='High school graduate'),' (',round(100*sum(Education=='High school graduate')/n(),1),')'),
    'Some college or more'=paste0(sum(Education=='Some college or more'),' (',round(100*sum(Education=='Some college or more')/n(),1),')'),
    'History of OELN'=paste0(sum(OELN=='Yes'),' (',round(100*sum(OELN=='Yes')/n(),1),')'),
    'Self-reported'=paste0(sum(SRHL=='Yes'),' (',round(100*sum(SRHL=='Yes')/n(),1),')'),
    'Exam-based'=paste0(sum(AEHL=='Yes'),' (',round(100*sum(AEHL=='Yes')/n(),1),')'),
    'Exam-based, HF'=paste0(sum(AEHFHL=='Yes'),' (',round(100*sum(AEHFHL=='Yes')/n(),1),')')
  ) |> 
  mutate(Cohort=c('1999--2000','2001--2002','2003--2004','2011--2012','2015--2016'))

demogs_AE <- dsSubUW_AE |> 
  summarise(
    n=as.character(n()),
    Age=paste0(round(mean(Age),2),' (',round(sd(Age)/sqrt(n()),2),')'),
    Female=paste0(sum(Gender=='Female'),' (',round(100*sum(Gender=='Female')/n(),1),')'),
    White=paste0(sum(Ethnicity=='White'),' (',round(100*sum(Ethnicity=='White')/n(),1),')'),
    Black=paste0(sum(Ethnicity=='Black'),' (',round(100*sum(Ethnicity=='Black')/n(),1),')'),
    Hispanic=paste0(sum(Ethnicity=='Hispanic'),' (',round(100*sum(Ethnicity=='Hispanic')/n(),1),')'),
    Other=paste0(sum(Ethnicity=='Other'),' (',round(100*sum(Ethnicity=='Other')/n(),1),')'),
    'Under $20,000'=paste0(sum(Income=='Under $20,000'),' (',round(100*sum(Income=='Under $20,000')/n(),1),')'),
    '$20,000 to $44,999'=paste0(sum(Income=='$20,000 to $44,999'),' (',round(100*sum(Income=='$20,000 to $44,999')/n(),1),')'),
    '$45,000 to $74,999'=paste0(sum(Income=='$45,000 to $74,999'),' (',round(100*sum(Income=='$45,000 to $74,999')/n(),1),')'),
    '$75,000 and Over'=paste0(sum(Income=='$75,000 and Over'),' (',round(100*sum(Income=='$75,000 and Over')/n(),1),')'),
    'Refused/unsure'=paste0(sum(Income=='Refused/unsure'),' (',round(100*sum(Income=='Refused/unsure')/n(),1),')'),
    'Less than high school'=paste0(sum(Education=='Less than high school'),' (',round(100*sum(Education=='Less than high school')/n(),1),')'),
    'High school graduate'=paste0(sum(Education=='High school graduate'),' (',round(100*sum(Education=='High school graduate')/n(),1),')'),
    'Some college or more'=paste0(sum(Education=='Some college or more'),' (',round(100*sum(Education=='Some college or more')/n(),1),')'),
    'History of OELN'=paste0(sum(OELN=='Yes'),' (',round(100*sum(OELN=='Yes')/n(),1),')'),
    'Self-reported'=paste0(sum(SRHL=='Yes'),' (',round(100*sum(SRHL=='Yes')/n(),1),')'),
    'Exam-based'=paste0(sum(AEHL=='Yes'),' (',round(100*sum(AEHL=='Yes')/n(),1),')'),
    'Exam-based, HF'=paste0(sum(AEHFHL=='Yes'),' (',round(100*sum(AEHFHL=='Yes')/n(),1),')')
  ) |> 
  mutate(Cohort='Overall',.before=1) |> 
  bind_rows(demogs_AE) |> 
  pivot_longer(-1) |> 
  pivot_wider(names_from=1,values_from=value)


# Sample weighting ####

ds <- ds |> 
  mutate(across(c(OELN,SRHL,AEHL,AEHFHL), ~fct_relevel(.x, 'No'))) |> 
  mutate(Gender=fct_relevel(Gender,'Male'),
         Income=fct_relevel(Income,'Under $20,000'),
         Education=fct_relevel(Education,'Less than high school'))




designCombined <- svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTINT10YR,
                            nest = T,
                            data = as.data.frame(ds))
designByCohort <- svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTINT2YR,
                            nest = T,
                            data = as.data.frame(ds))
designCombined_AE <- svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC10YR,
                            nest = T,
                            data = as.data.frame(ds))
designByCohort_AE <- svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC2YR,
                            nest = T,
                            data = as.data.frame(ds))

dsSubCombined <- subset(designCombined,inAnalysis)
dsSubByCohort <- subset(designByCohort,inAnalysis)
dsSubCombined_AE <- subset(designCombined_AE,inAnalysis_AE)
dsSubByCohort_AE <- subset(designByCohort_AE,inAnalysis_AE)

varNames <- c('Gender','Age','Ethnicity','Income','Education','OELN','SRHL','AEHL','AEHFHL')
options(scipen=999)
demoMeansAll <- as.data.frame(svymean(~Age+Gender+Ethnicity+Income+Education+OELN+SRHL,dsSubCombined))
demoMeansCohort <- t(as.data.frame(svyby(~Age+Gender+Ethnicity+Income+Education+OELN+SRHL,~Cohort,dsSubByCohort,svymean)))[-1,]

demogsWeighted <- tibble(name=rownames(demoMeansAll),
                         Overall=rep('',nrow(demoMeansAll)),
                      '1999--2000'=rep('',nrow(demoMeansAll)),
                      '2001--2002'=rep('',nrow(demoMeansAll)),
                      '2003--2004'=rep('',nrow(demoMeansAll)),
                      '2011--2012'=rep('',nrow(demoMeansAll)),
                      '2015--2016'=rep('',nrow(demoMeansAll)))
demogsWeighted[1,2] <- paste0(round(demoMeansAll[1,1],2),' ','(',round(demoMeansAll[1,2],2),')')
for (i in 2:nrow(demoMeansAll)){
  demogsWeighted[i,2] <- paste0(
    round(100*demoMeansAll[i,1],2),' ','(',round(100*demoMeansAll[i,2],2),')'
  )
} 

for(j in 3:ncol(demogsWeighted)){
  demogsWeighted[1,j] <- paste0(round(demoMeansCohort[1,j-2],2),' ','(',round(demoMeansCohort[1+19,j-2],2),')')
  for(i in 2:nrow(demogsWeighted)){
    demogsWeighted[i,j] <- paste0(
      round(100*demoMeansCohort[i,j-2],2),' ','(',round(100*demoMeansCohort[i+19,j-2],2),')'
    )
  }
}



demoMeansAll_AE <- as.data.frame(svymean(~Age+Gender+Ethnicity+Income+Education+OELN+SRHL+AEHL+AEHFHL,dsSubCombined_AE))
demoMeansCohort_AE <- t(as.data.frame(svyby(~Age+Gender+Ethnicity+Income+Education+OELN+SRHL+AEHL+AEHFHL,~Cohort,dsSubByCohort_AE,svymean)))[-1,]

demogsWeighted_AE <- tibble(name=rownames(demoMeansAll_AE),
                         Overall=rep('',nrow(demoMeansAll_AE)),
                         '1999--2000'=rep('',nrow(demoMeansAll_AE)),
                         '2001--2002'=rep('',nrow(demoMeansAll_AE)),
                         '2003--2004'=rep('',nrow(demoMeansAll_AE)),
                         '2011--2012'=rep('',nrow(demoMeansAll_AE)),
                         '2015--2016'=rep('',nrow(demoMeansAll_AE)))
demogsWeighted_AE[1,2] <- paste0(round(demoMeansAll_AE[1,1],2),' ','(',round(demoMeansAll_AE[1,2],2),')')
for (i in 2:nrow(demoMeansAll_AE)){
  demogsWeighted_AE[i,2] <- paste0(
    round(100*demoMeansAll_AE[i,1],2),' ','(',round(100*demoMeansAll_AE[i,2],2),')'
  )
} 

for(j in 3:ncol(demogsWeighted_AE)){
  demogsWeighted_AE[1,j] <- paste0(round(demoMeansCohort_AE[1,j-2],2),' ','(',round(demoMeansCohort_AE[1+22,j-2],2),')')
  for(i in 2:nrow(demogsWeighted_AE)){
    demogsWeighted_AE[i,j] <- paste0(
      round(100*demoMeansCohort_AE[i,j-2],2),' ','(',round(100*demoMeansCohort_AE[i+22,j-2],2),')'
    )
  }
}
options(scipen=9999)

# Regressions ####

## SR ####
varNames <- c('Cohort','Age','Gender','Ethnicity','Income','Education','OELN','Cohort*OELN')
coefNames <- list(Cohort='Cohort',Age='Age',Gender='Female',
                  Ethnicity=c('Black','Hispanic','Other'),
                  Income=c('$20,000 to $44,999', '$45,000 to $74,000','$75,000 and Over','Refused/unsure'),
                  Education=c('High school graduate','Some college or more'),
                  OELN='Yes','Cohort*OELN'='Cohort*OELN')
odds_SR <- tibble('Variable'=unlist(coefNames),
                  'UOR coef'=rep('',length(Variable)),
                  'UOR CI'=`UOR coef`,
                  'MOR coef'=`UOR coef`,
                  'MOR CI'=`UOR coef`,
                  'MOR Int coef'=`UOR coef`,
                  'MOR Int CI'=`UOR coef`)

for (i in 1:(length(varNames)-1)){
  reg <- svyglm(reformulate(varNames[i],'SRHL'),dsSubByCohort,family=quasibinomial)
  for (c in 1:length(unlist(coefNames[i]))){
    eff <- exp(coef(reg))[c+1]
    effCI <- exp(confint(reg))[c+1,]
    pval <- summary(reg)$coefficients[c+1,4]
    if (pval<0.05 & pval>=0.01) pval <- '*'
    else if (pval<0.01) pval <- '**'
    else pval <- ''
    odds_SR[which(unlist(coefNames)==unlist(coefNames[i])[c]),2] <- paste0(format(round(eff,2),nsmall=2),pval)
    odds_SR[which(unlist(coefNames)==unlist(coefNames[i])[c]),3] <- paste0('[',format(round(effCI[1],2),nsmall=2),'--',format(round(effCI[2],2),nsmall=2),']')
  }
}

reg <- svyglm(reformulate(varNames[-length(varNames)],'SRHL'),dsSubByCohort,family=quasibinomial)
eff <- exp(coef(reg))
effCI <- exp(confint(reg))
pvals <- summary(reg)$coefficients[,4]
for (c in 1:length(unlist(coefNames)[-length(unlist(coefNames))])){
  pval <- pvals[c+1]
  if (pval<0.05 & pval>=0.01) pval <- '*'
  else if (pval<0.01) pval <- '**'
  else pval <- ''
  odds_SR[which(unlist(coefNames)==unlist(coefNames)[c]),4] <- paste0(format(round(eff[c+1],2),nsmall=2),pval) 
  odds_SR[which(unlist(coefNames)==unlist(coefNames)[c]),5] <- paste0('[',format(round(effCI[c+1,1],2),nsmall=2),'--',format(round(effCI[c+1,2],2),nsmall=2),']')
}

regSR <- svyglm(reformulate(varNames,'SRHL'),dsSubByCohort,family=quasibinomial)
eff <- exp(coef(regSR))
effCI <- exp(confint(regSR))
pvals <- summary(regSR)$coefficients[,4]
for (c in 1:length(unlist(coefNames))){
  pval <- pvals[c+1]
  if (pval<0.05 & pval>=0.01) pval <- '*'
  else if (pval<0.01) pval <- '**'
  else pval <- ''
  if(c==13) {
    odds_SR[which(unlist(coefNames)==unlist(coefNames)[c]),6] <- paste0(format(eff[c+1],scientific=T,digits=1),pval)
    odds_SR[which(unlist(coefNames)==unlist(coefNames)[c]),7] <- paste0('[',format(effCI[c+1,1],scientific=T,digits=1),'--',format(effCI[c+1,2],scientific=T,digits=1),']')
  }
  else {
    odds_SR[which(unlist(coefNames)==unlist(coefNames)[c]),6] <- paste0(format(round(eff[c+1],2),nsmall=2),pval)
    odds_SR[which(unlist(coefNames)==unlist(coefNames)[c]),7] <- paste0('[',format(round(effCI[c+1,1],2),nsmall=2),'--',format(round(effCI[c+1,2],2),nsmall=2),']')
  }
}

## AE ####
odds_AE <- tibble('Variable'=unlist(coefNames),
                  'UOR coef'=rep('',length(Variable)),
                  'UOR CI'=`UOR coef`,
                  'MOR coef'=`UOR coef`,
                  'MOR CI'=`UOR coef`,
                  'MOR Int coef'=`UOR coef`,
                  'MOR Int CI'=`UOR coef`)

for (i in 1:(length(varNames)-1)){
  reg <- svyglm(reformulate(varNames[i],'AEHL'),dsSubByCohort_AE,family=quasibinomial)
  for (c in 1:length(unlist(coefNames[i]))){
    eff <- exp(coef(reg))[c+1]
    effCI <- exp(confint(reg))[c+1,]
    pval <- summary(reg)$coefficients[c+1,4]
    if (pval<0.05 & pval>=0.01) pval <- '*'
    else if (pval<0.01) pval <- '**'
    else pval <- ''
    odds_AE[which(unlist(coefNames)==unlist(coefNames[i])[c]),2] <- paste0(format(round(eff,2),nsmall=2),pval)
    odds_AE[which(unlist(coefNames)==unlist(coefNames[i])[c]),3] <- paste0('[',format(round(effCI[1],2),nsmall=2),'--',format(round(effCI[2],2),nsmall=2),']')
  }
}

reg <- svyglm(reformulate(varNames[-length(varNames)],'AEHL'),dsSubByCohort_AE,family=quasibinomial)
eff <- exp(coef(reg))
effCI <- exp(confint(reg))
pvals <- summary(reg)$coefficients[,4]
for (c in 1:length(unlist(coefNames)[-length(unlist(coefNames))])){
  pval <- pvals[c+1]
  if (pval<0.05 & pval>=0.01) pval <- '*'
  else if (pval<0.01) pval <- '**'
  else pval <- ''
  odds_AE[which(unlist(coefNames)==unlist(coefNames)[c]),4] <- paste0(format(round(eff[c+1],2),nsmall=2),pval) 
  odds_AE[which(unlist(coefNames)==unlist(coefNames)[c]),5] <- paste0('[',format(round(effCI[c+1,1],2),nsmall=2),'--',format(round(effCI[c+1,2],2),nsmall=2),']')
}

regAE <- svyglm(reformulate(varNames,'AEHL'),dsSubByCohort_AE,family=quasibinomial)
eff <- exp(coef(regAE))
effCI <- exp(confint(regAE))
pvals <- summary(regAE)$coefficients[,4]
for (c in 1:length(unlist(coefNames))){
  pval <- pvals[c+1]
  if (pval<0.05 & pval>=0.01) pval <- '*'
  else if (pval<0.01) pval <- '**'
  else pval <- ''
  if(c==13) {
    odds_AE[which(unlist(coefNames)==unlist(coefNames)[c]),6] <- paste0(format(eff[c+1],scientific=T,digits=1),pval)
    odds_AE[which(unlist(coefNames)==unlist(coefNames)[c]),7] <- paste0('[',format(effCI[c+1,1],scientific=T,digits=1),'--',format(effCI[c+1,2],scientific=T,digits=1),']')
  }
  else {
    odds_AE[which(unlist(coefNames)==unlist(coefNames)[c]),6] <- paste0(format(round(eff[c+1],2),nsmall=2),pval)
    odds_AE[which(unlist(coefNames)==unlist(coefNames)[c]),7] <- paste0('[',format(round(effCI[c+1,1],2),nsmall=2),'--',format(round(effCI[c+1,2],2),nsmall=2),']')
    }
}

## AEHF ####
odds_AEHF <- tibble('Variable'=unlist(coefNames),
                  'UOR coef'=rep('',length(Variable)),
                  'UOR CI'=`UOR coef`,
                  'MOR coef'=`UOR coef`,
                  'MOR CI'=`UOR coef`,
                  'MOR Int coef'=`UOR coef`,
                  'MOR Int CI'=`UOR coef`)

for (i in 1:(length(varNames)-1)){
  reg <- svyglm(reformulate(varNames[i],'AEHFHL'),dsSubByCohort_AE,family=quasibinomial)
  for (c in 1:length(unlist(coefNames[i]))){
    eff <- exp(coef(reg))[c+1]
    effCI <- exp(confint(reg))[c+1,]
    pval <- summary(reg)$coefficients[c+1,4]
    if (pval<0.05 & pval>=0.01) pval <- '*'
    else if (pval<0.01) pval <- '**'
    else pval <- ''
    odds_AEHF[which(unlist(coefNames)==unlist(coefNames[i])[c]),2] <- paste0(format(round(eff,2),nsmall=2),pval)
    odds_AEHF[which(unlist(coefNames)==unlist(coefNames[i])[c]),3] <- paste0('[',format(round(effCI[1],2),nsmall=2),'--',format(round(effCI[2],2),nsmall=2),']')
  }
}

reg <- svyglm(reformulate(varNames[-length(varNames)],'AEHFHL'),dsSubByCohort_AE,family=quasibinomial)
eff <- exp(coef(reg))
effCI <- exp(confint(reg))
pvals <- summary(reg)$coefficients[,4]
for (c in 1:length(unlist(coefNames)[-length(unlist(coefNames))])){
  pval <- pvals[c+1]
  if (pval<0.05 & pval>=0.01) pval <- '*'
  else if (pval<0.01) pval <- '**'
  else pval <- ''
  odds_AEHF[which(unlist(coefNames)==unlist(coefNames)[c]),4] <- paste0(format(round(eff[c+1],2),nsmall=2),pval) 
  odds_AEHF[which(unlist(coefNames)==unlist(coefNames)[c]),5] <- paste0('[',format(round(effCI[c+1,1],2),nsmall=2),'--',format(round(effCI[c+1,2],2),nsmall=2),']')
}

regAEHF <- svyglm(reformulate(varNames,'AEHFHL'),dsSubByCohort_AE,family=quasibinomial)
eff <- exp(coef(regAEHF))
effCI <- exp(confint(regAEHF))
pvals <- summary(regAEHF)$coefficients[,4]
for (c in 1:length(unlist(coefNames))){
  pval <- pvals[c+1]
  if (pval<0.05 & pval>=0.01) pval <- '*'
  else if (pval<0.01) pval <- '**'
  else pval <- ''
  if(c==13) {
    odds_AEHF[which(unlist(coefNames)==unlist(coefNames)[c]),6] <- paste0(format(eff[c+1],scientific=T,digits=1),pval)
    odds_AEHF[which(unlist(coefNames)==unlist(coefNames)[c]),7] <- paste0('[',format(effCI[c+1,1],scientific=T,digits=1),'--',format(effCI[c+1,2],scientific=T,digits=1),']')
  }
  else {
    odds_AEHF[which(unlist(coefNames)==unlist(coefNames)[c]),6] <- paste0(format(round(eff[c+1],2),nsmall=2),pval)
    odds_AEHF[which(unlist(coefNames)==unlist(coefNames)[c]),7] <- paste0('[',format(round(effCI[c+1,1],2),nsmall=2),'--',format(round(effCI[c+1,2],2),nsmall=2),']')
  }
}

# Predicted probability plots ####

if(makePlots){
  effSR <- predict_response(regSR,term = c('Cohort','OELN'),margin='empirical',weights=regSR$prior.weights)
  effAE <- predict_response(regAE,term = c('Cohort','OELN'),margin='empirical',weights=regAE$prior.weights)
  effAEHF <- predict_response(regAEHF,term = c('Cohort','OELN'),margin='empirical',weights=regAEHF$prior.weights)
  
  effComb <- bind_rows(list(tibble(effSR),tibble(effAE),tibble(effAEHF))) |> 
    mutate(Model=factor(rep(c('Self-reported','Exam-based, speech-frequency','Exam-based, high-frequency'),each=nrow(effSR)),levels=c('Self-reported','Exam-based, speech-frequency','Exam-based, high-frequency'))) |> 
    mutate(group=factor(group,levels=c('Yes','No'))) |> 
    rename('Self-reported OELN'=group)
  
  plotComb <- effComb |> 
    ggplot(aes(x=x,y=predicted,color=`Self-reported OELN`,fill=`Self-reported OELN`))+
    facet_wrap(~Model)+
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.15,linetype=0)+
    geom_line(linewidth=0.8)+
    scale_x_continuous(breaks=c(1999,2001,2003,2011,2015))+
    scale_y_continuous(labels=scales::percent)+
    scale_color_brewer(palette='Set1',direction=-1)+
    scale_fill_brewer(palette='Set1',direction=-1)+
    labs(x='Cohort',
         y='Hearing loss probability',
         title='Average predicted probabilities of hearing loss',
         subtitle='Separated by metric of hearing loss')+
    theme_bw()+
    theme(panel.grid.minor.x=element_blank())
}

# Writing to file ####

## Data and LaTeX tables ####
if(overwrite){
  folderpath <- paste0(getwd(),'/data/')
  if(!dir.exists(folderpath)) dir.create(folderpath)
  write_csv(ds,paste0(folderpath,'cleaned data with custom variables.csv'))
  
  folderpath <- paste0(getwd(),'/LaTeX tables/')
  if(!dir.exists(folderpath)) dir.create(folderpath)
  write_file(kable(demogs,'latex'),paste0(folderpath,'demogs.txt'))
  write_file(kable(demogs_AE,'latex'),paste0(folderpath,'demogs_AE.txt'))
  write_file(kable(odds_SR,'latex'),paste0(folderpath,'oddsSR.txt'))
  write_file(kable(odds_AE,'latex'),paste0(folderpath,'oddsAE.txt'))
  write_file(kable(odds_AEHF,'latex'),paste0(folderpath,'oddsAEHF.txt'))
  
  ## Package citations ####
    write_bib(file=paste0(folderpath,"bibliography of packages.bib"))
    
    packages <- tibble('Package name' = character(),
                       Version = character(),
                       Maintainer = character())
    
    for (pkg in p_loaded()){
      packages <- packages %>%
        add_row(
          'Package name' = pkg,
          Version = as.character(packageVersion(pkg)),
          Maintainer = maintainer(pkg)
        )
    }
    
    write_file(kable(packages,'latex'),paste0(folderpath,'packages.txt'))

  ## Figures ####
  
  if(makePlots){
    figs <- c('plotComb')
    # 
    # fignames <- c('Absorbances',
    #               'Fold changes',
    #               'Absorbances separated',
    #               'Fold changes separated',
    #               'Fold changes by dose',
    #               'Fold changes by dose expanded',
    #               'Population relative to untreated by dose',
    #               'Population relative to untreated by treatment',
    #               'Population relative to untreated')
    
    folderpath <- paste(getwd(),'/figures/', sep='')
    if(!dir.exists(folderpath)) dir.create(folderpath)
    
    widths <- 3400
    heights <- 1300
    
    for (i in 1:length(figs)) {
      fig <- figs[i]
      png(filename=paste(folderpath, fig, '.png', sep=''), width=widths[i], height=heights[i], res=300)
      print(get(fig))
      dev.off()
    }
  }
}

# Bat ####

#               _..-'(                       )`-.._
#            ./'. '||\\.       (\_/)       .//||` .`\.
#         ./'.|'.'||||\\|..    )O O(    ..|//||||`.`|.`\.
#      ./'..|'.|| |||||\`````` '`"'` ''''''/||||| ||.`|..`\.
#    ./'.||'.|||| ||||||||||||.     .|||||||||||| |||||.`||.`\.
#   /'|||'.|||||| ||||||||||||{     }|||||||||||| ||||||.`|||`\
#  '.|||'.||||||| ||||||||||||{     }|||||||||||| |||||||.`|||.`
# '.||| ||||||||| |/'   ``\||``     ''||/''   `\| ||||||||| |||.`
# |/' \./'     `\./         \!|\   /|!/         \./'     `\./ `\|
# V    V         V          }' `\ /' `{          V         V    V
# `    `         `               V               '         '    '