#! /usr/bin/env Rscript

# Copyright 2011-2014, Phillip Alday
# This file is part of Ginnungagap-Code.
#
# Ginnungagap-Code is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Ginnungagap-Code is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Ginnungagap-Code.  If not, see <http://www.gnu.org/licenses/>.

# ROIs Region of Interest -- we divide the scalp up into regions
roi <- c(rep("0",length(eeg.item.data$subj)))
roi[eeg.item.data$chan == "F7"] <- "Left-Anterior"
roi[eeg.item.data$chan == "F3"] <- "Left-Anterior"
roi[eeg.item.data$chan == "FC5"] <- "Left-Anterior"
roi[eeg.item.data$chan == "FC1"] <- "Left-Anterior"
roi[eeg.item.data$chan == "F8"] <- "Right-Anterior"
roi[eeg.item.data$chan == "F4"] <- "Right-Anterior"
roi[eeg.item.data$chan == "FC6"] <- "Right-Anterior"
roi[eeg.item.data$chan == "FC2"] <- "Right-Anterior"
roi[eeg.item.data$chan == "P7"] <- "Left-Posterior"
roi[eeg.item.data$chan == "P3"] <- "Left-Posterior"
roi[eeg.item.data$chan == "CP5"] <- "Left-Posterior"
roi[eeg.item.data$chan == "CP1"] <- "Left-Posterior"
roi[eeg.item.data$chan == "P8"] <- "Right-Posterior"
roi[eeg.item.data$chan == "P4"] <- "Right-Posterior"
roi[eeg.item.data$chan == "CP6"] <- "Right-Posterior"
roi[eeg.item.data$chan == "CP2"] <- "Right-Posterior"
roi[eeg.item.data$chan == "FZ"] <- "Midline"
roi[eeg.item.data$chan == "FCZ"] <- "Midline"
roi[eeg.item.data$chan == "CPZ"] <- "Midline"
roi[eeg.item.data$chan == "CZ"] <- "Midline"
roi[eeg.item.data$chan == "PZ"] <- "Midline"

# conditions -- we can reextract the condition from the item number
cond <- c(rep(" ",length(eeg.item.data$subj)))
cond[eeg.item.data$item %/% 100 == 10] <- "SANN"
cond[eeg.item.data$item %/% 100 == 11] <- "OANN"
cond[eeg.item.data$item %/% 100 == 12] <- "SUNN"
cond[eeg.item.data$item %/% 100 == 13] <- "OUNN"
cond[eeg.item.data$item %/% 100 == 14] <- "SANP"
cond[eeg.item.data$item %/% 100 == 15] <- "OANP"
cond[eeg.item.data$item %/% 100 == 16] <- "SUNP"
cond[eeg.item.data$item %/% 100 == 17] <- "OUNP"
cond[eeg.item.data$item %/% 100 == 18] <- "SAPN"
cond[eeg.item.data$item %/% 100 == 19] <- "OAPN"
cond[eeg.item.data$item %/% 100 == 20] <- "SUPN"
cond[eeg.item.data$item %/% 100 == 21] <- "OUPN"
cond[eeg.item.data$item %/% 100 == 22] <- "SAPP"
cond[eeg.item.data$item %/% 100 == 23] <- "OAPP"
cond[eeg.item.data$item %/% 100 == 24] <- "SUPP"
cond[eeg.item.data$item %/% 100 == 25] <- "OUPP"

# seperate the item from the condition code
# every item appears in every condition, so it makes sense to betracht them separately-
eeg.item.data$item <- eeg.item.data$item %% 100

# put it all together
eeg.item.data <- cbind(eeg.item.data,roi,cond)
# and get rid of all the lines without an ROI value
eeg.item.data <- eeg.item.data[eeg.item.data$roi != "0",]

rm(cond, roi)

# now we add in the conditions as paramtetric factors -- a bit redundant,
# but it allows for ease of use while manipulating the data

cond.to.parameter <- function(someframe){
    wordOrder = c(rep(" ",length(someframe$subj)))
    wordOrder[substr(someframe$cond,1,1) == "O"] <- "object"
    wordOrder[substr(someframe$cond,1,1) == "S"] <- "subject"

    ambiguity = c(rep(" ",length(someframe$subj)))
    ambiguity[substr(someframe$cond,2,2) == "U"] <- "unambig"
    ambiguity[substr(someframe$cond,2,2) == "A"] <- "ambig"

    np1type = c(rep(" ",length(someframe$subj)))
    np1type[substr(someframe$cond,3,3) == "N"] <- "noun"
    np1type[substr(someframe$cond,3,3) == "P"] <- "pronoun"

    np2type = c(rep(" ",length(someframe$subj)))
    np2type[substr(someframe$cond,4,4) == "N"] <- "noun"
    np2type[substr(someframe$cond,4,4) == "P"] <- "pronoun"

    # put it all together
    cbind(someframe,wordOrder,ambiguity,np1type,np2type)
}

eeg.item.data <- cond.to.parameter(eeg.item.data)

eeg.item.data$win <- as.character(eeg.item.data$win)

n400.pro <- "+300..+500" 
p600.pro <-"+600..+800" 
n400.noun <- "+400..+600" 
p600.noun <- "+700..+900"

eeg.item.data$win[eeg.item.data$win == n400.pro & eeg.item.data$np2type == "pronoun"] <- "N400"
eeg.item.data$win[eeg.item.data$win == n400.noun & eeg.item.data$np2type == "noun"] <- "N400"

eeg.item.data$win[eeg.item.data$win == p600.pro & eeg.item.data$np2type == "pronoun"] <- "P600"
eeg.item.data$win[eeg.item.data$win == p600.noun & eeg.item.data$np2type == "noun"] <- "P600"

eeg.item.data$item <- factor(eeg.item.data$item)
eeg.item.data$win <- factor(eeg.item.data$win)
eeg.item.data$roi <- factor(eeg.item.data$roi)
eeg.item.data$ambiguity <- factor(eeg.item.data$ambiguity)
eeg.item.data$wordOrder <- factor(eeg.item.data$wordOrder)
eeg.item.data$np1type <- factor(eeg.item.data$np1type)
eeg.item.data$np2type <- factor(eeg.item.data$np2type)

eeg.item.data <- subset(eeg.item.data, win %in% c("N400","P600"))
eeg.item.data$win <- factor(eeg.item.data$win)
