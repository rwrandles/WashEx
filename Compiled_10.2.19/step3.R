library(dplyr) ## For easy data frame manipulation
library(tidyr) ## For data frame cleaning
library(DBI) ## For SQLite database functions
library(RSQLite) ## For SQLite database functions
library(lubridate) ## For calendar and date related functions
library(zoo) ## For the function na.locf() -- fills in NA values to most recent non-NA value
library(snakecase) ## For renaming column names to lower camel case

setwd(" ")
source("WashEx-source-functions.R")
WashEx <- dbConnect(RSQLite::SQLite(), "WashEx-db.sqlite")
master_comms <- dbReadTable(WashEx, "Committees")
master_sponsors <- dbReadTable(WashEx, "Sponsors")
master_bills <- dbReadTable(WashEx, "Legislation")
master_actions <- dbReadTable(WashEx, "Actions")

changeFile <- read.csv("step2.csv", header = TRUE, stringsAsFactors = FALSE)

for(comm in 1:nrow(changeFile)){
  master_actions$locId[master_actions$locId == 
                         master_comms$commId[master_comms$name == changeFile$badComm[comm] & 
                                               master_comms$biennium == changeFile$biennium[comm]]] = master_comms$commId[master_comms$name == changeFile$goodComm[comm] &
                                                                                                            master_comms$biennium == changeFile$biennium[comm] & master_comms$agency == changeFile$agency[comm]]
  
  master_comms$active[master_comms$name == changeFile$badComm[comm] & master_comms$biennium == changeFile$biennium[comm]] = FALSE
}

master_actions$sessionAct <- rep(NA, nrow(master_actions))

sessDates <- read.csv("sessDates.csv", header = TRUE, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8-BOM")

for(i in 1:nrow(sessDates)){
  master_actions$sessionAct[which(ymd(master_actions$actionDate) %within%
                                    interval(ymd(sessDates$dateStart[i]),
                                             ymd(sessDates$dateEnd[i])))] = sessDates$sessTitle[i]
}

##

membership <- read.csv("commLeaders.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")

master_bills$firstRef <- mapply(getReferral, billId = master_bills$billId, biennium = master_bills$biennium)

for(i in 1:nrow(master_bills)){
  info <- memInfor(master_bills$biennium[i], master_comms$longName[master_comms$commId == master_bills$firstRef[i]], master_bills$primeSponsorId[i])
  
  master_bills$isChair[i] <- info$chair
  master_bills$isVice[i] <- info$vice
  master_bills$isMem[i] <- info$member
}

dbWriteTable(WashEx, "Legislation", master_bills, overwrite = TRUE)
dbWriteTable(WashEx, "Sponsors", master_sponsors, overwrite = TRUE)
dbWriteTable(WashEx, "Committees", master_comms, overwrite = TRUE)
dbWriteTable(WashEx, "Actions", master_actions, overwrite = TRUE)
dbWriteTable(WashEx, "Sessions", sessDates, overwrite = TRUE)

dbDisconnect(WashEx)