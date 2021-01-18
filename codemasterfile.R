# Load relevant packages
library(dplyr)
library(car)

# Load dataset
getwd()

knitr::opts_knit$set(root.dir = '/Users/mellyzors/Desktop/fathering') #modify to your wd
options(scipen=999)

## this is the master file
path <- file.path("/Users/mellyzors/Desktop/fathering/master.csv") #modify to your path

read.csv(path, stringsAsFactors = FALSE)

master <- read.csv(path, stringsAsFactors = FALSE)

#create a recoded master sheet to contain all modified variables
master_r <- master



## Social Support Questionnaire for Children (SSQC) - Gordon-Hollingsworth et al., 2011; 50 items
#### 4 point likert scale: 0 (Never or Rarely True), 1 (Sometimes True), 2 (Often or Very True), or 3 (Always True). For sibling component, a NA option was available

##### Parent (8, 15, 21, 22, 23, 24, 30, 32, 34, 36) 10 items.
##### Relative (1, 4, 9, 11, 17, 25, 29, 35, 39, 49) 10 items.
##### Adult (12, 27, 28, 31, 37, 40, 41, 46, 48, 50) 10 items.
##### Sibling (2, 3, 7, 13, 20, 26, 33, 38, 45, 47) 10 items.
##### Peer (5, 6, 10, 14, 16, 18, 19, 42, 43, 44) 10 items.
##### Parent, relative, adult, sibling, and peer are summated to obtain total social support scores; higher scores indicate more support

###### Notes: The SSQC has high internal consistency for the 5 subscales and total scale with Cronbach’s alphas of .89 to .97

#recode all sibling questions so that 4 = NA
master_r <- master_r %>%
   mutate_at(vars(SSQC2,SSQC3,SSQC7,SSQC13,SSQC20,SSQC26,SSQC33,SSQC38,SSQC45,SSQC47), ~ na_if(., 4))

#compute subscale scores
master_r <-master_r %>%
  rowwise() %>%
  mutate(SSQC_PA = mean(c(SSQC8,SSQC15,SSQC21,SSQC22,SSQC23,SSQC24,SSQC30,SSQC32,SSQC34,SSQC36), na.rm=TRUE)*10) %>%
  mutate(SSQC_RE = mean(c(SSQC1,SSQC4,SSQC9,SSQC11,SSQC17,SSQC25,SSQC29,SSQC35,SSQC39,SSQC49), na.rm=TRUE)*10) %>%
  mutate(SSQC_AD = mean(c(SSQC12,SSQC27,SSQC28,SSQC31,SSQC37,SSQC40,SSQC41,SSQC46,SSQC48,SSQC50), na.rm=TRUE)*10) %>%
  mutate(SSQC_SI = mean(c(SSQC2,SSQC3,SSQC7,SSQC13,SSQC20,SSQC26,SSQC33,SSQC38,SSQC45,SSQC47), na.rm=TRUE)*10) %>%
  mutate(SSQC_PE = mean(c(SSQC5,SSQC6,SSQC10,SSQC14,SSQC16,SSQC18,SSQC19,SSQC42,SSQC43,SSQC44), na.rm = TRUE)*10) %>%
  mutate(SSQC_TOT = sum(SSQC_PA:SSQC_PE, na.rm = TRUE))


## Inventory of parent and peer attachment (IPPA) - Armsden & Greensberg, 1989; (scale: 0, 1, 2)

#### Parent attachment
##### Trust (10 items) - 1,2,3r,4,10r,13,14,21,23,24
##### Communication (10 items) - 5r,6,7r,8,15r, 16,17,20,26,28
##### Alienation (8 items) - 9r,11r,12r,18r,19r,22r,25r,27r

#### Peer attachment
##### Trust (10 items) - 33r, 34, 36, 40, 41, 42, 43, 47, 48, 49
##### Communication (8 items) - 29, 30, 31, 35, 44, 45, 52, 53
##### Alienation (7 items) - 32r, 37r, 38r, 39r, 46r, 50r, 51r

##### Sum response values for each subscale
##### Can sum up subscales for total attachment scores for parent and peer

###### Notes: Three week test-retest reliabilities for a sample of 27 18- to 20-year-olds were .93 for parent and .86 for peer attachment

#compute scores
master_r <- master_r %>%
    mutate_at(vars(IPPAR3,IPPAR5,IPPAR7,IPPAR9,IPPAR10,IPPAR11,IPPAR12,IPPAR15,IPPAR18,IPPAR19,IPPAR22,IPPAR25,IPPAR27,IPPAR32,IPPAR33,IPPAR37,IPPAR38,IPPAR39,IPPAR46,IPPAR50,IPPAR51), car::recode, "0=2; 1=1; 2=0")
  rowwise() %>%
  mutate(IPPA_PTRUST = mean(c(IPPAR1,IPPAR2,IPPAR3,IPPAR4,IPPAR10,IPPAR13,IPPAR14,IPPAR21,IPPAR23,IPPAR24), na.rm=TRUE)*10) %>%
  mutate(IPPA_PCOM = mean(c(IPPAR5,IPPAR6,IPPAR7,IPPAR8,IPPAR15,IPPAR16,IPPAR17,IPPAR20,IPPAR26,IPPAR28), na.rm=TRUE)*10) %>%
  mutate(IPPA_PALIEN = mean(c(IPPAR9,IPPAR11,IPPAR12,IPPAR18,IPPAR19,IPPAR22,IPPAR25,IPPAR27), na.rm=TRUE)*8) %>%
  mutate(IPPA_PA_tot = sum(IPPA_PTRUST, IPPA_PCOM,IPPA_PALIEN, na.rm=TRUE)) %>%
  mutate(IPPA_PETRUST = mean(c(IPPAR33,IPPAR34,IPPAR36,IPPAR40,IPPAR41,IPPAR42,IPPAR43,IPPAR47,IPPAR48,IPPAR49), na.rm=TRUE)*10) %>%
  mutate(IPPA_PECOM = mean(c(IPPAR29,IPPAR30,IPPAR31,IPPAR35,IPPAR44,IPPAR45,IPPAR52,IPPAR53), na.rm=TRUE)*8) %>%
  mutate(IPPA_PEALIEN = mean(c(IPPAR32,IPPAR37,IPPAR38,IPPAR39,IPPAR46,IPPAR50,IPPAR51), na.rm=TRUE)*7) %>%
  mutate(IPPA_PE_tot = sum(IPPA_PETRUST, IPPA_PECOM, IPPA_PALIEN, na.rm=TRUE))


## Network of Relationship Inventory - Quality Version (NRI-QV) - Furman &  Buhrmester, 2009; (scale: 1-5 likert scale)

#### Positive Subscales
##### Companionship (COM): Items 1,11,21 for each relationship
##### Intimate disclosure (ID): Items 2, 12, 22
##### Satisfaction (SAT): 4, 14, 24
##### Emotional Support (ES): 6, 16, 26
##### Approval (APP): 8, 18, 28

##### Negative subscales
##### Pressure (PRS): 3, 13, 23
##### Conflict (CON): 5, 15, 25
##### Criticism (CRIT): 7, 17, 27
##### Dominance (DOM): 9, 19, 29
##### Exclusion (EX): 10, 20, 30

###### Notes:
###### If using all items, can derive 2 additional factor scores: Closeness (C):  the mean of the companionship, disclosure, emotional support,  approval, and satis-faction scales.
###### Discord (D):  the mean of the conflict, criticism, pressure, exclusion and dominance scales. *retain all items to derive factor scores
###### Individuals rated: A. same sex best friend, B. opposite sex best friend, C. significant other, D. sibling, E. mom, F. dad.
###### Reverse coded: 10a thru 10f, 30a thru 30f
###### The NRI-RQV has high internal consistency for its 12 scales with Cronbach’s alphas of .49 to .93.


#recode
##because dplyr also is loaded, you want to specify car::recode to prevent masking
master_r <- master_r %>%
   mutate_at(vars(NRIRQV10a:NRIRQV10f, NRIRQV30a:NRIRQV30f), car::recode, "1=5; 2=4; 3=3; 4=2; 5=1") #specified variables are adjacent, thus matrix can be specified

#POSITIVE SUBSCALES
#companionship
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_COMa = mean(c(NRIRQV1a, NRIRQV11a, NRIRQV21a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_COMb = mean(c(NRIRQV1b, NRIRQV11b, NRIRQV21b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_COMc = mean(c(NRIRQV1c, NRIRQV11c, NRIRQV21c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_COMd = mean(c(NRIRQV1d, NRIRQV11d, NRIRQV21d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_COMe = mean(c(NRIRQV1e, NRIRQV11e, NRIRQV21e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_COMf = mean(c(NRIRQV1f, NRIRQV11f, NRIRQV21f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_COM_tot = sum(NRIRQV_COMa, NRIRQV_COMb, NRIRQV_COMc, NRIRQV_COMd, NRIRQV_COMe, NRIRQV_COMf,na.rm=TRUE))

#intimate disclosure
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_IDa = mean(c(NRIRQV2a, NRIRQV12a, NRIRQV22a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_IDb = mean(c(NRIRQV2b, NRIRQV12b, NRIRQV22b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_IDc = mean(c(NRIRQV2c, NRIRQV12c, NRIRQV22c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_IDd = mean(c(NRIRQV2d, NRIRQV12d, NRIRQV22d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_IDe = mean(c(NRIRQV2e, NRIRQV12e, NRIRQV22e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_IDf = mean(c(NRIRQV2f, NRIRQV12f, NRIRQV22f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_ID_tot = sum(NRIRQV_IDa, NRIRQV_IDb, NRIRQV_IDc, NRIRQV_IDd, NRIRQV_IDe, NRIRQV_IDf,na.rm=TRUE))

#satisfaction
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_SATa = mean(c(NRIRQV4a, NRIRQV14a, NRIRQV24a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_SATb = mean(c(NRIRQV4b, NRIRQV14b, NRIRQV24b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_SATc = mean(c(NRIRQV4c, NRIRQV14c, NRIRQV24c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_SATd = mean(c(NRIRQV4d, NRIRQV14d, NRIRQV24d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_SATe = mean(c(NRIRQV4e, NRIRQV14e, NRIRQV24e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_SATf = mean(c(NRIRQV4f, NRIRQV14f, NRIRQV24f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_SAT_tot = sum(NRIRQV_SATa, NRIRQV_SATb, NRIRQV_SATc, NRIRQV_SATd, NRIRQV_SATe, NRIRQV_SATf,na.rm=TRUE))

#emotional support
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_ESa = mean(c(NRIRQV6a, NRIRQV16a, NRIRQV26a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_ESb = mean(c(NRIRQV6b, NRIRQV16b, NRIRQV26b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_ESc = mean(c(NRIRQV6c, NRIRQV16c, NRIRQV26c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_ESd = mean(c(NRIRQV6d, NRIRQV16d, NRIRQV26d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_ESe = mean(c(NRIRQV6e, NRIRQV16e, NRIRQV26e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_ESf = mean(c(NRIRQV6f, NRIRQV16f, NRIRQV26f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_ES_tot = sum(NRIRQV_ESa, NRIRQV_ESb, NRIRQV_ESc, NRIRQV_ESd, NRIRQV_ESe, NRIRQV_ESf,na.rm=TRUE))

#appreciation
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_APPa = mean(c(NRIRQV8a, NRIRQV18a, NRIRQV28a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_APPb = mean(c(NRIRQV8b, NRIRQV18b, NRIRQV28b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_APPc = mean(c(NRIRQV8c, NRIRQV18c, NRIRQV28c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_APPd = mean(c(NRIRQV8d, NRIRQV18d, NRIRQV28d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_APPe = mean(c(NRIRQV8e, NRIRQV18e, NRIRQV28e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_APPf = mean(c(NRIRQV8f, NRIRQV18f, NRIRQV28f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_APP_tot = sum(NRIRQV_APPa, NRIRQV_APPb, NRIRQV_APPc, NRIRQV_APPd, NRIRQV_APPe, NRIRQV_APPf,na.rm=TRUE))

#aggregate for closeness
master_r <-master_r %>%
  rowwise() %>%
  mutate(NRIRQV_CLO_tot = sum(NRIRQV_COM_tot,NRIRQV_ID_tot,NRIRQV_SAT_tot,NRIRQV_ES_tot, NRIRQV_APP_tot, na.rm=TRUE))

#NEGATIVE SUBSCALES
#pressure
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_PRSa = mean(c(NRIRQV3a, NRIRQV13a, NRIRQV23a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_PRSb = mean(c(NRIRQV3b, NRIRQV13b, NRIRQV23b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_PRSc = mean(c(NRIRQV3c, NRIRQV13c, NRIRQV23c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_PRSd = mean(c(NRIRQV3d, NRIRQV13d, NRIRQV23d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_PRSe = mean(c(NRIRQV3e, NRIRQV13e, NRIRQV23e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_PRSf = mean(c(NRIRQV3f, NRIRQV13f, NRIRQV23f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_PRS_tot = sum(NRIRQV_PRSa, NRIRQV_PRSb, NRIRQV_PRSc, NRIRQV_PRSd, NRIRQV_PRSe, NRIRQV_PRSf,na.rm=TRUE))

#conflict
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_CONa = mean(c(NRIRQV5a, NRIRQV15a, NRIRQV25a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CONb = mean(c(NRIRQV5b, NRIRQV15b, NRIRQV25b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CONc = mean(c(NRIRQV5c, NRIRQV15c, NRIRQV25c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CONd = mean(c(NRIRQV5d, NRIRQV15d, NRIRQV25d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CONe = mean(c(NRIRQV5e, NRIRQV15e, NRIRQV25e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CONf = mean(c(NRIRQV5f, NRIRQV15f, NRIRQV25f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CON_tot = sum(NRIRQV_CONa, NRIRQV_CONb, NRIRQV_CONc, NRIRQV_CONd, NRIRQV_CONe, NRIRQV_CONf,na.rm=TRUE))

#criticism
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_CRITa = mean(c(NRIRQV7a, NRIRQV17a, NRIRQV27a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CRITb = mean(c(NRIRQV7b, NRIRQV17b, NRIRQV27b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CRITc = mean(c(NRIRQV7c, NRIRQV17c, NRIRQV27c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CRITd = mean(c(NRIRQV7d, NRIRQV17d, NRIRQV27d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CRITe = mean(c(NRIRQV7e, NRIRQV17e, NRIRQV27e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CRITf = mean(c(NRIRQV7f, NRIRQV17f, NRIRQV27f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_CRIT_tot = sum(NRIRQV_CRITa, NRIRQV_CRITb, NRIRQV_CRITc, NRIRQV_CRITd, NRIRQV_CRITe, NRIRQV_CRITf,na.rm=TRUE))

#dominance
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_DOMa = mean(c(NRIRQV9a, NRIRQV19a, NRIRQV29a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_DOMb = mean(c(NRIRQV9b, NRIRQV19b, NRIRQV29b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_DOMc = mean(c(NRIRQV9c, NRIRQV19c, NRIRQV29c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_DOMd = mean(c(NRIRQV9d, NRIRQV19d, NRIRQV29d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_DOMe = mean(c(NRIRQV9e, NRIRQV19e, NRIRQV29e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_DOMf = mean(c(NRIRQV9f, NRIRQV19f, NRIRQV29f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_DOM_tot = sum(NRIRQV_DOMa, NRIRQV_DOMb, NRIRQV_DOMc, NRIRQV_DOMd, NRIRQV_DOMe, NRIRQV_DOMf,na.rm=TRUE))

#exclusion
master_r <- master_r %>%
  rowwise() %>%
  mutate(NRIRQV_EXa = mean(c(NRIRQV10a, NRIRQV20a, NRIRQV30a), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_EXb = mean(c(NRIRQV10b, NRIRQV20b, NRIRQV30b), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_EXc = mean(c(NRIRQV10c, NRIRQV20c, NRIRQV30c), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_EXd = mean(c(NRIRQV10d, NRIRQV20d, NRIRQV30d), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_EXe = mean(c(NRIRQV10e, NRIRQV20e, NRIRQV30e), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_EXf = mean(c(NRIRQV10f, NRIRQV20f, NRIRQV30f), na.rm=TRUE)*3) %>%
  mutate(NRIRQV_EX_tot = sum(NRIRQV_EXa, NRIRQV_EXb, NRIRQV_EXc, NRIRQV_EXd, NRIRQV_EXe, NRIRQV_EXf,na.rm=TRUE))

#aggregate for discord
master_r <-master_r %>%
  rowwise() %>%
  mutate(NRIRQV_DIS_tot = sum(NRIRQV_PRS_tot,NRIRQV_CON_tot,NRIRQV_CRIT_tot,NRIRQV_DOM_tot, NRIRQV_EX_tot, na.rm=TRUE))


## Social Adversity

#### Social adversity SYNTAX FOR WAVE 3
#### Includes the following: Divorced/single parents, teenage mother, mental health of mom, mental health of dad,
#### physical health of mom, physical health of dad, crowded home, large family, parent ever arrested, public housing
#### 0 = no endorsement, 1 = endorsement

###### Notes: In WAVE 1 original 'social adversity' variable may be different. Please double check lab's prev. papers.


#recode 0 for married, all else considered endorsement
master_r$divorcedparents <- master_r$DEMO34

master_r <-master_r %>%
  mutate_at(vars(divorcedparents), car::recode, "1=0; 2=1; 3=1; 4=1; 5=1")

#teenage mother if birthage: age <19.
master_r$teenagemother <- master_r$DEMO35
master_r$teenagemother [master_r$DEMO35 > 19] <- 0
master_r$teenagemother [master_r$DEMO35 <= 19] <- 1

#mphysical: mom physical health problems.
master_r$mphysillness <- master_r$DEMO55
master_r$fphysillness <- master_r$DEMO56
master_r$mmentalillness <- master_r$DEMO57
master_r$fmentalillness <- master_r$DEMO58

master_r <-master_r %>%
  mutate_at(vars(mphysillness:fmentalillness), car::recode, "2=0; 1=1")

#crowded home variable and large family variables.
#peopleperroom and numbersiblings only used for the calc of crowded home, not for SA
#divide people living in home by rooms total for crowded home
#if family greater than 5, considered large
master_r <-master_r %>%
  mutate(peopleperroom = DEMO49/DEMO50, na.rm = TRUE) %>%
  rowwise() %>%
  mutate(numbersiblings = sum(DEMO38A,DEMO38B,DEMO40A,DEMO40B, na.rm = TRUE))

master_r$crowdedhome <- master_r$peopleperroom
master_r$crowdedhome [master_r$peopleperroom < 5] <- 0
master_r$crowdedhome [master_r$peopleperroom >= 5] <- 1

master_r$largefamily <- master_r$numbersiblings
master_r$largefamily [master_r$numbersiblings < 5] <-0
master_r$largefamily [master_r$numbersiblings >=5] <-1

#either parent ever arrested
master_r$arrestedmother <- master$DEMO67A
master_r$arrestedfather <- master$DEMO71A

master_r <-master_r %>%
  mutate_at(vars(arrestedmother, arrestedfather), car::recode, "2=0; 1=1") %>%
  rowwise() %>%
  mutate(arrestsum = sum(arrestedfather,arrestedmother, na.rm=TRUE))

master_r$arrestedparents <- master_r$arrestsum
master_r$arrestedparents [master_r$arrestsum >= 1] <-1
master_r$arrestedparents [master_r$arrestsum < 1] <-0

#govt home or public housing
master_r$publichouse <- car::recode(master$DEMO47, "2=0; 1=1")

master_r <-master_r %>%
  rowwise() %>%
    mutate(SA3 = mean(c(crowdedhome,largefamily,divorcedparents,teenagemother,mphysillness,fphysillness,mmentalillness,fmentalillness,arrestedparents,publichouse), na.rm=TRUE)*10)


## Collective efficacy - Sampson, 1997, higher scores indicate higher collective efficacy

##### Informal social control: 5-item 5-point Likert scale ranging from 1 to 5 (very unlikely to very likely)
##### Average values to get ISC score -Warner Swartz and Hawk, 2015.

##### Social cohesion and trust: 5-item 5-point Likert scale ranging from 1 to 5 (strongly disagree to strongly agree)
##### Average values to get SCT score -Robinette et al., 2013

###### Notes: This is part of the "neighborhood characteristics" portion of caregiver packet

#recode reversed variables
master_r <- master_r %>%
   mutate_at(vars(social_cohesion4, social_cohesion5), car::recode, "1=5; 2=4; 3=3; 4=2; 5=1")

master_r <- master_r %>%
  rowwise() %>%
  mutate(NC_ISC = mean(c(social_control1,social_control2,social_control3,social_control4,social_control5), na.rm=TRUE)*10) %>%
  mutate(NC_SCT = mean(c(social_cohesion1,social_cohesion2,social_cohesion3,social_cohesion4r,social_cohesion5r), na.rm=TRUE)*10) %>%
  mutate(NC_CE = sum(NC_ISC, NC_SCT, na.rm=TRUE))


  ## Selfishness Questionnaire (SQ) Raine & Uh, 2018 - 24 items; (scale: 0 through 2)

  ### Egocentric subscale: (2, 6, 10, 13, 15, 18, 21, 23)
  ### Adaptive subscale: (1, 4, 7, 9, 12, 17, 20, 24)
  ### Pathologic subscale: (3, 5, 8, 11, 14, 16, 19, 22)
  ### Total: sum of all 3

  selfish_data <- selfish_data %>%
    rowwise() %>%
    mutate(EGO_SQ = mean(c(SQ2, SQ6, SQ10, SQ13, SQ15, SQ18, SQ21, SQ23), na.rm=TRUE)*8) %>%
    mutate(ADAPT_SQ = mean(c(SQ1, SQ4, SQ7, SQ9, SQ12, SQ17, SQ20, SQ24), na.rm=TRUE)*8) %>%
    mutate(PATHO_SQ = mean(c(SQ3, SQ5, SQ8, SQ11, SQ14, SQ16, SQ19, SQ22), na.rm=TRUE)*8) %>%
    mutate(TOTAL_SQ = sum(EGO_SQ, ADAPT_SQ, PATHO_SQ, na.rm=TRUE))


  ## Reactive Proactive Aggression Questionnaire (RPQ) Raine et al., 2006; 23 items (scale: 0 through 2)

  ### Proactive subscale: (2, 4, 6, 9, 10, 12, 15, 17, 18, 20, 21, 23)  12 items.
  ### Reactive subscale: (1, 3, 5, 7, 8, 11, 13, 14, 16, 19, 22) 11 items.
  #### Summate  to form proactive and reactive scales
  #### Both Proactive and reactive scale scores are summated to obtain total aggression scores. Raine et al., 2006

  selfish_data <- selfish_data %>%
    rowwise() %>%
    mutate(PA3 = mean(c(RPQ2,RPQ4,RPQ6,RPQ9,RPQ10,RPQ12,RPQ15,RPQ17,RPQ18,RPQ20,RPQ21,RPQ23), na.rm=TRUE)*12) %>%
    mutate(RA3 = mean(c(RPQ1,RPQ3,RPQ5,RPQ7,RPQ8,RPQ11,RPQ13,RPQ14,RPQ16,RPQ19,RPQ22), na.rm=TRUE)*11) %>%
    mutate(RPQ = sum(PA3, RA3, na.rm=TRUE))


  ## Alabama Parenting Questionnaire (APQ) Frick, 1991; 42 items
  ### 5 point likert scale: 1 (Never), 1 (Almost Never), 3 (Sometimes), 4 (Often), 5 (Always).

  ### Involvement (1, 4, 7, 9, 11, 14, 15, 20, 23, 26) 10 items.
  ### Positive Parenting (2, 5, 13, 16, 18, 27) 6 items.
  ### Poor Monitoring/Supervision (6, 10, 17, 19, 21, 24, 28, 29, 30, 32) 10 items.
  ### Inconsistent Discipline (3, 8, 12, 22, 25, 31) 6 items.
  ### Corporal Punishment (33, 35, 38) 3 items.
  ### Other Discipline Practices* (34, 36, 37, 39, 40, 41, 42) 7 items.

  #### No reverse coding necessary.
  #### Sum all items in the scale to obtain a total scale score
  #### You may subtract this score by the number of items in the subscale so that the score range begins at zero)
  #### Higher scores in the positive scales (involvement, positive parenting) show efficient parenting practices
  #### Higher scores in the negative scales indicate inefficient practices
  #### Other Discipline Practices is not a scale, but provides information on an item by item basis

  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(APQ_INV = mean(c(APQ1, APQ4, APQ7, APQ9, APQ11, APQ14, APQ15, APQ20, APQ23, APQ26), na.rm=TRUE)*10) %>%
      mutate(APQ_PP = mean(c(APQ2, APQ5, APQ13, APQ16, APQ18, APQ27), na.rm=TRUE)*6) %>%
      mutate(APQ_PMS = mean(c(APQ6, APQ10, APQ17, APQ19, APQ21, APQ24, APQ28, APQ29, APQ30, APQ32), na.rm=TRUE)*10) %>%
      mutate(APQ_ID = mean(c(APQ3, APQ8, APQ12, APQ22, APQ25, APQ31), na.rm=TRUE)*6) %>%
      mutate(APQ_CP = mean(c(APQ33, APQ35, APQ38), na.rm=TRUE)*3) %>%
      mutate(APQ_OT = mean(c(APQ34, APQ36, APQ37, APQ39, APQ40, APQ41, APQ42), na.rm=TRUE)*7) %>%
      mutate(APQ_POSITIVE = sum(APQ_INV, APQ_PP, na.rm=TRUE)) %>%
      mutate(APQ_NEGATIVE = sum(APQ_PMS, APQ_ID, APQ_CP, na.rm=TRUE))


  ### Child Behavior Checklist (CBCL) Achenbach, 2001 (scale: 0 through 2)

  ## PROFILE AGGRESSIVE BEHAVIOR 18 ITEMS; ALPHA = .847; DELINQUENCY 17 ITEMS, ALPHA = .647
  ### Agg: CBCL3 CBCL16 CBCL19 CBCL20 CBCL21 CBCL22 CBCL23 CBCL37 CBCL57 CBCL68 CBCL86 CBCL87 CBCL88 CBCL89 CBCL94 CBCL95 CBCL97 CBCL104 (18 items)
  ### Del: CBCL2 CBCL26 CBCL28 CBCL39 CBCL43 CBCL63 CBCL67 CBCL72 CBCL73 CBCL81 CBCL82 CBCL90 CBCL96 CBCL99 CBCL101 CBCL105 CBCL106 (17 items)

  ## FAST TRACK PROJECT TECH REPORT: AGGRESSIVE BEHAVIOR 20 ITEMS; ALPHA = .856; DELINQUENCY 13 ITEMS; ALPHA = .570
  ### Agg: CBCL3 CBCL7 CBCL16 CBCL19 CBCL20 CBCL21 CBCL22 CBCL23 CBCL27 CBCL37 CBCL57 CBCL68 CBCL74 CBCL86 CBCL87 CBCL93 CBCL94 CBCL95 CBCL97 CBCL104 (20 items)
  ### Del: CBCL26 CBCL39 CBCL43 CBCL63 CBCL67 CBCL72 CBCL81 CBCL82 CBCL90 CBCL96 CBCL101 CBCL105 CBCL106 (13 items)

  #profile cbcl scores
  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(CBC_AGG = mean(c(CBC3, CBC16, CBC19, CBC20, CBC21, CBC22, CBC23, CBC37, CBC57, CBC68, CBC86, CBC87, CBC88, CBC89, CBC94, CBC95, CBC97, CBC104), na.rm=TRUE)*18) %>%
      mutate(CBC_DEL = mean(c(CBC2, CBC26, CBC28, CBC39, CBC43, CBC63, CBC67, CBC72, CBC73, CBC81, CBC82, CBC90, CBC96, CBC99, CBC101, CBC105, CBC106), na.rm=TRUE)*17) %>%
      mutate(CBC_EXTER = sum(CBC_AGG, CBC_DEL, na.rm=TRUE))

  #fast track cbcl scores
  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(CBC_AGG_FT = mean(c(CBC3,CBC7,CBC16,CBC19,CBC20,CBC21,CBC22,CBC23,CBC27,CBC37,CBC57, CBC68,CBC74,CBC86,CBC87,CBC93,CBC94,CBC95,CBC97,CBC104), na.rm=TRUE)*20) %>%
      mutate(CBC_DEL_FT = mean(c(CBC26,CBC39,CBC43,CBC63,CBC67,CBC72,CBC81,CBC82,CBC90,CBC96, CBC101,CBC105,CBC106), na.rm=TRUE)*13) %>%
      mutate(CBC_EXTER_FT = sum(CBC_AGG_FT, CBC_DEL_FT, na.rm=TRUE))


  ## Inventory of Callous Unemotional Traits (ICU) Frick, 2004; 24 items(scale: 0 through 3)

  ## Original:
  ### Callous: 2, 4, 7, 8, 9, 10, 12, 18, 11, 20, 21 (11 items)
  ### Uncaring: 3r, 5r, 13r, 15r, 16r, 17r, 23, 24 (8 items)
  ### Unemotional: 1r, 6, 14r, 19r, 22 (5 items)
  #### Sum for total

  ## New factor structure:
  ### Callous: 4, 7, 9, 11, 12, 18, 20 (7 items)
  ### Uncaring: 3r, 5r, 13r, 15r, 16r, 17r, 23r, 24r (8 items)
  ### Unemotional: 1r, 14r, 19r, 22 (4 items)
  #### Sum for total

  #CAREGIVER VERSION
  #reverse code
  selfish_data$P_ICU1r <- car::recode(selfish_data$P_ICU1, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU3r <- car::recode(selfish_data$P_ICU3, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU5r <- car::recode(selfish_data$P_ICU5, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU8r <- car::recode(selfish_data$P_ICU8, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU13r <- car::recode(selfish_data$P_ICU13, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU14r <- car::recode(selfish_data$P_ICU14, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU15r <- car::recode(selfish_data$P_ICU15, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU16r <- car::recode(selfish_data$P_ICU16, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU17r <- car::recode(selfish_data$P_ICU17, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU19r <- car::recode(selfish_data$P_ICU19, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU23r <- car::recode(selfish_data$P_ICU23, "0=3; 1=2; 2=1; 3=0")
  selfish_data$P_ICU24r <- car::recode(selfish_data$P_ICU24, "0=3; 1=2; 2=1; 3=0")

  #original
  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(pICU_CA = mean(c(P_ICU4,P_ICU8r,P_ICU9,P_ICU18,P_ICU11,P_ICU21,P_ICU7,P_ICU20,P_ICU2,P_ICU10,P_ICU12), na.rm=TRUE)*11) %>%
      mutate(pICU_UC = mean(c(P_ICU15r,P_ICU23r,P_ICU16r,P_ICU3r,P_ICU17r,P_ICU24r,P_ICU13r,P_ICU5r), na.rm=TRUE)*8) %>%
      mutate(pICU_UE = mean(c(P_ICU1r,P_ICU19r,P_ICU6,P_ICU22,P_ICU14r), na.rm=TRUE)*5) %>%
      mutate(pICU = sum(pICU_CA,pICU_UC,pICU_UE, na.rm=TRUE))

  #new factor
  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(pICU_CA_newf = mean(c(P_ICU4,P_ICU7,P_ICU9,P_ICU11,P_ICU12,P_ICU18,P_ICU20), na.rm=TRUE)*7) %>%
      mutate(pICU_UC_newf = mean(c(P_ICU3r,P_ICU5r,P_ICU13r,P_ICU15r,P_ICU16r,P_ICU17r,P_ICU23r,P_ICU24r), na.rm=TRUE)*8) %>%
      mutate(pICU_UE_newf = mean(c(P_ICU1r,P_ICU14r,P_ICU19r,P_ICU22), na.rm=TRUE)*4) %>%
      mutate(pICU_newf = sum(pICU_CA_newf,pICU_UC_newf,pICU_UE_newf, na.rm=TRUE))


  #SELF-REPORT (YOUTH) VERSION
  #CAREGIVER VERSION
  #reverse code
  selfish_data$ICU1r <- car::recode(selfish_data$ICU1, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU3r <- car::recode(selfish_data$ICU3, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU5r <- car::recode(selfish_data$ICU5, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU8r <- car::recode(selfish_data$ICU8, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU13r <- car::recode(selfish_data$ICU13, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU14r <- car::recode(selfish_data$ICU14, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU15r <- car::recode(selfish_data$ICU15, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU16r <- car::recode(selfish_data$ICU16, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU17r <- car::recode(selfish_data$ICU17, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU19r <- car::recode(selfish_data$ICU19, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU23r <- car::recode(selfish_data$ICU23, "0=3; 1=2; 2=1; 3=0")
  selfish_data$ICU24r <- car::recode(selfish_data$ICU24, "0=3; 1=2; 2=1; 3=0")

  #original
  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(ICU_CA = mean(c(ICU4,ICU8r,ICU9,ICU18,ICU11,ICU21,ICU7,ICU20,ICU2,ICU10,ICU12), na.rm=TRUE)*11) %>%
      mutate(ICU_UC = mean(c(ICU15r,ICU23r,ICU16r,ICU3r,ICU17r,ICU24r,ICU13r,ICU5r), na.rm=TRUE)*8) %>%
      mutate(ICU_UE = mean(c(ICU1r,ICU19r,ICU6,ICU22,ICU14r), na.rm=TRUE)*5) %>%
      mutate(ICU = sum(ICU_CA,ICU_UC,ICU_UE, na.rm=TRUE))

  #new factor
  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(ICU_CA_newf = mean(c(ICU4,ICU7,ICU9,ICU11,ICU12,ICU18,ICU20), na.rm=TRUE)*7) %>%
      mutate(ICU_UC_newf = mean(c(ICU3r,ICU5r,ICU13r,ICU15r,ICU16r,ICU17r,ICU23r,ICU24r), na.rm=TRUE)*8) %>%
      mutate(ICU_UE_newf = mean(c(ICU1r,ICU14r,ICU19r,ICU22), na.rm=TRUE)*4) %>%
      mutate(ICU_newf = sum(ICU_CA_newf,ICU_UC_newf,ICU_UE_newf, na.rm=TRUE))


  ## APSD (Antisocial Process Screening Device) Frick & Hare, 2001; 12 items (scale: 0 through 2)

  ### Narcissism: 4, 6, 8, 11, 12, 13 (7 items)
  ### Impulsivity: 1, 3, 7, 10, 14 (5 items)

  #CAREGIVER VERSION
  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(pAPSD_NARC = mean(c(P_APSD4,P_APSD6,P_APSD8,P_APSD9,P_APSD11,P_APSD12,P_APSD13), na.rm=TRUE)*7) %>%
      mutate(pAPSD_IMP = mean(c(P_APSD1,P_APSD3,P_APSD7,P_APSD10,P_APSD14), na.rm=TRUE)*5)

  #SELF-RERPORT (YOUTH) VERSION
  selfish_data <-selfish_data %>%
    rowwise() %>%
      mutate(APSD_NARC = mean(c(APSD4,APSD6,APSD8,APSD9,APSD11,APSD12,APSD13), na.rm=TRUE)*7) %>%
      mutate(APSD_IMP = mean(c(APSD1,APSD3,APSD7,APSD10,APSD14), na.rm=TRUE)*5)
