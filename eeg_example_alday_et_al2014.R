library(lme4)
library(effects)
library(lattice)

eeg.item.data <- read.table("Data//ggitem_N400P600.tab",header = TRUE)
source("ggmempreproc_common.R")
cz <- subset(eeg.item.data, chan == "CZ")

# delete columns we don't care about
cz$cond <- NULL
cz$chan <- NULL
cz$roi <- NULL

# delete big data frame
rm("eeg.item.data")
# and free up memory not currently being used ("garbage collect")
gc()


cz.n400 <- subset(cz, win=="N400")

big.model <- lmer(mean ~ ambiguity*wordOrder*np1type*np2type 
                      + (1|subj) 
                      + (1|item) 
                  ,data=cz.n400
                  ,REML=FALSE)
massive.model <- lmer(mean ~ ambiguity*wordOrder*np1type*np2type
                          + (1+ambiguity*wordOrder*np1type*np2type|subj)
                          + (1+ambiguity*wordOrder|item)
                      ,data=cz.n400
                      ,REML=FALSE)
