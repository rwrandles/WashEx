## Load libraries here ##
library(lubridate)
library(RSQLite)
library(ini)
library(snakecase)
library(dplyr)
library(tidyr)
library(zoo)
library(dbplyr)
library(RMySQL)
##

## Set working directory
setwd("")
##

## Load source files ##
options(warn = -1)
tryCatch(source("WashEx-buildHandler.R"), 
         error = function(e){
           e$message <- "buildHandler.R not found. Code will abort."
           stop(e)
         })

tryCatch(source("WashEx-source-functions.R"),
         error = function(e){
           throwCode(001)
         })
##

## Load WashEx-db.sqlite
WashEx <- tryCatch(dbConnect(RSQLite::SQLite(), "WashEx-db.sqlite"), error = function(e){ throwCode(002) })

live_actions <- dbReadTable(WashEx, "Actions")
live_bills <- dbReadTable(WashEx, "Legislation")
live_comms <- dbReadTable(WashEx, "Committees")
live_sponsors <- dbReadTable(WashEx, "Sponsors")
sessDates <- dbReadTable(WashEx, "Sessions")
##

## Load config settings
cfg <- tryCatch(read.ini("config.ini"),
                error = function(e){
                  throwCode(003)
                })
if(cfg$base$endDate == "0"){ cfg$base$endDate <- as.character(today())}
from <- strtoi(substr(cfg$base$startDate,1,4))
to <- strtoi(substr(cfg$base$endDate,1,4))
now <- paste(strtoi(substr(today(),1,4)) + strtoi(substr(today(),1,4))%%2 - 1,
             substr(strtoi(substr(today(),1,4)) + strtoi(substr(today(),1,4))%%2,3,4), sep = "-")
##

## GENERATE COMMITTEE TABLES
bi_comms <- tryCatch(getCommittees(now), error = function(e){ throwCode(101) })
if(nrow(bi_comms) == 0){ throwCode(102) }

names(bi_comms) <- to_lower_camel_case(names(bi_comms))
bi_comms <- bi_comms %>% mutate(biennium = now, id = strtoi(id))
bi_comms$acronym <- trimws(bi_comms$acronym) ## Removes whitespace from the acronym column

if(nrow(bi_comms %>% filter(!(longName %in% (live_comms %>% filter(biennium == now))$longName))) > 0){
  
  new_comms <- data.frame()
  
  ## Generating committee Ids that are unique to the committee AND biennium
  
  bi_comms$commId <- NA
  commCounter <- 0
  
  ## For each row in the master committee table:
  ##
  ## Assign an letter for each biennium (1991-92 = "A", 1993-94 = "B", etc.)
  ##
  ## Within that biennium, assign the first committee number "0".
  ## Continue adding the next number so long as the current committee is in the same biennium as the previous
  ##
  ## Once a new biennium is reached, change the letter and begin the count over at "0"
  ##
  ## Committee Ids should end as "A0", "A1", "A2", "B0", "B1", "B2", "C0", "C1", "C2", etc.
  
  
  for(counter in 1:nrow(bi_comms)){
    bi_comms$commId[counter] <- (strtoi(substr(bi_comms$biennium[counter], 1, 4)) - 1991) / 2 + 65
    
    bi_comms$commId[counter] <- intToUtf8(bi_comms$commId[counter])
    
    if(counter == 1){
      bi_comms$commId[counter] <- paste(bi_comms$commId[counter], as.character(commCounter), sep = "")
    }
    else if(bi_comms$biennium[counter] == bi_comms$biennium[counter-1]){
      commCounter <- commCounter + 1
      
      bi_comms$commId[counter] <- paste(bi_comms$commId[counter], as.character(commCounter), sep = "")
    }
    else{
      commCounter <- 0
      
      bi_comms$commId[counter] <- paste(bi_comms$commId[counter], as.character(commCounter), sep = "")
    }
  }
  
  bi_comms$active <- rep(1, nrow(bi_comms))
  
  ## Remove columns except the following: committe id, biennium, acronym, agency (House or Senate), name, long name, and active status
  
  bi_comms <- bi_comms %>% select(commId, biennium, acronym, agency, name, longName, active)
  
  ## Deactivating rules committees
  
  bi_comms$active[bi_comms$name == "Rules"] = 0
  
  ##
  
  ## Removing duplicate committee entries
  new_comms <- bi_comms %>% filter(!(commId %in% live_comms$commId))
  
  live_comms <- distinct(bind_rows(live_comms, new_comms))
  ## 
} else{ bi_comms <- live_comms %>% filter(biennium == now) }

## GENERATE SPONSOR TABLES
bi_sponsors <- tryCatch(getSponsors(now), error = function(e){ throwCode(201) })
if(nrow(bi_sponsors) == 0){ throwCode(202) }

names(bi_sponsors) <- to_lower_camel_case(names(bi_sponsors))
bi_sponsors$biennium <- now
bi_sponsors$repId <- paste(bi_sponsors$id, substr(now, 3,4), sep = "_")
bi_sponsors$type <- ifelse(bi_sponsors$agency == "House", "rep", "sen")
bi_sponsors$leg <- length(seq(1889,substr(now,1,4),2))
bi_sponsors <- bi_sponsors %>% filter(firstName != "")

gender_table <- tryCatch(read.csv("Source Data/sponsor_gender.csv", header = TRUE, stringsAsFactors = FALSE),
                         error = function(e){ throwCode(203) })

gender_table$id <- as.character(gender_table$id)

missing_gender <- bi_sponsors %>% filter(!(id %in% gender_table$id), id != "GOV", id != "PPL")

if(nrow(missing_gender) > 0){
  missing_gender$gender = NA
  missing_gender <- missing_gender %>% mutate(firstYear = now) %>% select(id, firstYear, firstName, lastName, gender)
  if(file.exists("Source Data/gender_fixfile.csv")){
    fix <- read.csv("Source Data/gender_fixfile.csv", header = TRUE, stringsAsFactors = FALSE, colClasses = c(rep("character", 4), "integer"))
    missing_gender <- missing_gender %>% filter(!(id %in% fix$id))
    if(sum(is.na(fix$gender)) - length(fix$gender) == 0){
      throwCode(901)
      fix <- bind_rows(fix, missing_gender)
      write.csv(fix, "Source Data/gender_fixfile.csv", row.names = FALSE)
    }
    else{
      gender_table <- bind_rows(gender_table, fix %>% filter(!is.na(gender)))
      fix <- fix %>% filter(!(id %in% gender_table$id))
      missing_gender <- bi_sponsors %>% filter(!(id %in% gender_table$id), id != "GOV", id != "PPL")
      missing_gender$gender = rep(NA, nrow(missing_gender))
      missing_gender <- missing_gender %>% mutate(firstYear = now) %>% select(id, firstYear, firstName, lastName, gender)
      fix <- bind_rows(fix, missing_gender)
      if(nrow(fix) > 0){ throwCode(901) } else{ file.remove("gender_fixfile.csv") }
      write.csv(fix, "Source Data/gender_fixfile.csv", row.names = FALSE)
    }
  }
  else{
    throwCode(901)
    write.csv(missing_gender, "Source Data/gender_fixfile.csv", row.names = FALSE)
  }
}


bi_sponsors$gender = rep(NA, nrow(bi_sponsors))

for(spons in bi_sponsors$id[which(bi_sponsors$id %in% gender_table$id)]){
  bi_sponsors$gender[which(bi_sponsors$id == spons)] = 
    ifelse(unique(gender_table$gender[which(gender_table$id == spons)]) == "0", "M", "F") 
}

write.csv(gender_table, "Source Data/sponsor_gender.csv", row.names = FALSE)

bi_sponsors <- bi_sponsors %>% mutate(isEastDistrict = ifelse(district %in% c("3","4","6","7","8","9","12","13","14","15","16"),1,0),
                                      isSNDistrict = ifelse(district %in% c("1","5","11","24","30","31","32","33","34","36","37","39","40","41","43","45","46","47","48"),1,0),
                                      class = ifelse(district %in% c("1","2","3","4","5","9","10","11","12","14","16","17","18","19","20","22","23","24","25","27","28","39","40","41","49"),"A","B"))

bi_sponsors <- bi_sponsors %>% arrange(lastName) %>% select(repId, id, leg, firstName, lastName, type, district, party, gender, biennium, isEastDistrict, isSNDistrict, class)

WAgovs <-tryCatch(read.csv("Source Data/WAgov.csv", header = TRUE, stringsAsFactors = FALSE),
                  error = function(e){ throwCode(204) })

if(!(now %in% WAgovs$biennium)){ throwCode(205) }

bi_sponsors <- add_row(bi_sponsors, repId = paste("GOV", substr(now,3,4), sep = "_"), id = "GOV", leg = length(seq(1889,substr(now,1,4),2)), firstName = WAgovs$firstName[WAgovs$biennium == now], lastName = WAgovs$lastName[WAgovs$biennium == now], type = "gov", district = "0", party = WAgovs$party[WAgovs$biennium == now], gender = WAgovs$gender[WAgovs$biennium == now], biennium = now, isEastDistrict = 0, isSNDistrict = 0)
bi_sponsors <- add_row(bi_sponsors, repId = paste("PPL", substr(now,3,4), sep = "_"), id = "PPL", leg = length(seq(1889,substr(now,1,4),2)), firstName = "The", lastName = "People", type = "ppl", district = "0", party = "0", gender = "0", biennium = now, isEastDistrict = 0, isSNDistrict = 0, class = NA)
##

live_sponsors <- distinct(bind_rows((live_sponsors %>% filter(biennium != now)), bi_sponsors))

new_sponsors <- bi_sponsors %>% filter(!(repId %in% live_sponsors$repId))

live_sponsors$numTerms <- rep(NA, nrow(live_sponsors))

for(i in 1:nrow(live_sponsors)){
  temp <- live_sponsors %>% filter(id == live_sponsors$id[i], type == live_sponsors$type[i]) %>% arrange(repId)
  
  if(live_sponsors$type[i] == "rep"){ live_sponsors$numTerms[i] <- which(temp$repId == live_sponsors$repId[i]) }
  else if(live_sponsors$type[i] == "sen"){ 
    biCount <- which(temp$repId == live_sponsors$repId[i])
    
    if(biCount %% 2 != 0){ live_sponsors$numTerms[i] <- (biCount + 1)/2 }
    else{
      init <- temp$biennium[1]
      init <- substr(init,nchar(init)-1,nchar(init))
      init <- as.integer(init)
      
      if(init %% 4 == 0){ live_sponsors$numTerms[i] <- ifelse(live_sponsors$class[i] == "A", (biCount + 2)/2, biCount/2) }
      else{ live_sponsors$numTerms[i] <- ifelse(live_sponsors$class[i] == "A", biCount/2, (biCount + 2)/2) }
    }
  }
}

##GENERATE BILL TABLES

bi_bills <- tryCatch(getLegislationByYear(substr(today(), 1,4)), error = function(e){ throwCode(301) })

names(bi_bills) <- to_lower_camel_case(names(bi_bills))

bi_bills <- distinct(bi_bills)

bi_bills_long <- live_bills %>% filter(biennium == now)

new_bills <- bi_bills %>% 
  filter(!(billId %in% bi_bills_long$billId)) %>% 
  select(biennium, billId, billNumber)

if(nrow(new_bills) > 0){
  
  temp_bills <- data.frame()
  
  for(bill in 1:length(new_bills$billNumber)){
    current_bill <- tryCatch(getLegislation(now, new_bills$billNumber[bill]), error = function(e){ throwCode(302) })
    
    names(current_bill) <- to_lower_camel_case(names(current_bill))
    
    current_bill$primeSponsorId = paste(ifelse(grepl("I", current_bill$billId), "PPL", 
                                               ifelse(grepl("GA", current_bill$billId), "GOV", 
                                                      current_bill$primeSponsorId)), substr(now, 3,4), sep = "_")
    
    if(grepl("GA", current_bill$billId)){ current_bill$longDescription <- paste("Appointment: ", current_bill$shortDescription, sep = "") }
    
    temp_bills <- bind_rows(temp_bills, current_bill)
    
    if(bill %% 100 == 0){ print(bill) }
  }
  
  temp_bills <- distinct(temp_bills)
  
  temp_bills <- temp_bills %>% mutate(billUrl = 
                                        paste("http://app.leg.wa.gov/billsummary?billNumber=",
                                              billNumber, "&Year=", 
                                              substr(introducedDate, 1, 4),
                                              "&Initiative=", ifelse(grepl("I", current_bill$billId), "true", "false"), sep = ""))
  
  temp_bills$billType <- gsub(" .*$", "", temp_bills$billId)
  temp_bills$billType <- substring(temp_bills$billType, nchar(temp_bills$billType) - 1)
  temp_bills$billType[which(temp_bills$billType=="CR")] <- "Concurrent Resolution"
  temp_bills$billType[which(temp_bills$billType=="GA")] <- "Appointment"
  temp_bills$billType[which(temp_bills$billType=="JM")] <- "Joint Memorial"
  temp_bills$billType[which(temp_bills$billType=="JR")] <- "Joint Resolution"
  temp_bills$billType[which(temp_bills$billType=="HR" | temp_bills$billType == "SR")] <- "Resolution"
  temp_bills$billType[which(temp_bills$billType=="HB" | temp_bills$billType == "SB")] <- "Bill"
  temp_bills$billType[which(temp_bills$billType=="HI" | temp_bills$billType == "SI")] <- "Initiative"
  
  majority_df <- tryCatch(read.csv("Source Data/legMajorities.csv", header = TRUE, stringsAsFactors = FALSE), error = function(e){ throwCode(303) })
  if(!(substr(today(),1,4) %in% majority_df$year)){ throwCode(304) }
  
  partyTable <- bi_sponsors$party
  names(partyTable) <- bi_sponsors$repId
  
  temp_bills$majSpons <- unname(mapply(isMajority, party = unname(partyTable[temp_bills$primeSponsorId]), year = substr(temp_bills$introducedDate,1,4), billId = temp_bills$billId))
  
  temp_bills$isMinor <- ifelse(grepl("Acknowledging|Honoring|Observing|Celebrating|Congratulating|Proclaiming|Renaming|Naming|Asking|Urging|Requesting|Calling|Recognizing|Commending|Declaring|Designating|Thanking|Objecting to|technical correction", 
                                     temp_bills$legalTitle),1,0)
  
  temp_bills <- temp_bills %>% select(billId, billType, billNumber, 
                                      
                                      biennium, legalTitle, introducedDate, primeSponsorId, majSpons, isMinor, longDescription, billUrl)
  
  live_bills <- distinct(bind_rows(live_bills, temp_bills))
}

##GENERATE ACTION TABLES

bi_actions <- data.frame()

bi_bills_long <- live_bills %>% filter(biennium == now)

for(bill in 1:nrow(bi_bills_long)){
  current_action <- tryCatch(getStatusChanges(now, bi_bills_long$billNumber[bill],
                                              paste(substr(now,1,4), "-01-01", sep = ""),
                                              today()),
                             error = function(e){ throwCode(401) })
  
  names(current_action) <- to_lower_camel_case(names(current_action))
  
  current_action <- current_action %>% mutate(biennium = now)
  
  bi_actions <- bind_rows(bi_actions, current_action)
  
  if(bill %% 100 == 0){ print(bill) }
}

bi_actions$actionDate <- substr(bi_actions$actionDate, 1, 10)

bi_actions <- distinct(bi_actions)

#####################################################
## C - Parses actions into congressional locations ##
#####################################################

dupnames <- bi_comms[duplicated(bi_comms %>% select(name, biennium)),] %>% select(name, biennium)

tableCopy <- bi_actions %>% filter(!(grepl("retained", bi_actions$historyLine)))

tableCopy$locId <- rep(NA, nrow(tableCopy))
tableCopy$agency <- rep(NA, nrow(tableCopy))

duplicates <- dupnames %>% filter(name != "Rules")

tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "irst reading|ntroduce")))] <- ifelse(unlist(lapply(tableCopy$billId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "irst reading|ntroduce")))], grepl, pattern = "HB|HJM|HJR|HCR|HR|HI")), "H", "S")

Hnames <- bi_comms$name[substr(bi_comms$longName,1,1) == "H" & bi_comms$biennium == now & !(bi_comms$name %in% duplicates$name) & bi_comms$name != "Rules"]
Snames <- bi_comms$name[substr(bi_comms$longName,1,1) == "S" & bi_comms$biennium == now & !(bi_comms$name %in% duplicates$name) & bi_comms$name != "Rules"]
Hacr <- bi_comms$acronym[substr(bi_comms$longName,1,1) == "H" & bi_comms$biennium == now]
Sacr <- bi_comms$acronym[substr(bi_comms$longName,1,1) == "S" & bi_comms$biennium == now]

for(comm in Snames){
  code <- bi_comms$commId[bi_comms$name == comm]
  
  tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                      "assed to ",
                                                                                      "efer to "), comm, "\\>", sep = "", collapse = "|"))))] = code
  tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                       "assed to ",
                                                                                       "efer to "), comm, "\\>", sep = "", collapse = "|"))))] = "S"
}
for(comm in Hnames){
  code <- bi_comms$commId[bi_comms$name == comm]
  
  tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                      "assed to ",
                                                                                      "efer to "), comm, "\\>", sep = "", collapse = "|"))))] = code
  tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                       "assed to ",
                                                                                       "efer to "), comm, "\\>", sep = "", collapse = "|"))))] = "H"
}

for(acr in Sacr){
  tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(acr, "\\>", sep = ""))))] = bi_comms$commId[bi_comms$acronym == acr]
  tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(acr, "\\>", sep = ""))))] = "S"
}
for(acr in Hacr){
  tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(acr, "\\>", sep = ""))))] = bi_comms$commId[bi_comms$acronym == acr]
  tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(acr, "\\>", sep = ""))))] = "H"
}

tableCopy$agency[which(gsub(" .*$", "", tableCopy$historyLine) == "House")] = "H"
tableCopy$agency[which(gsub(" .*$", "", tableCopy$historyLine) == "Senate")] = "S"

tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "House Rules")))] = "HRL"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "House Rules")))] = "H"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Senate Rules")))] = "SRL"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Senate Rules")))] = "S"

tableCopy$agency = na.locf(tableCopy$agency)

tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(c("eferred to", "assed to", "efer to", "by"), "Rules\\>", collapse = "|"))))] = paste(tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(c("eferred to", "assed to", "efer to", "by"), "Rules\\>", collapse = "|"))))], "RL", sep = "")

tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "hird reading|Third Reading|inal passage")))] = paste(tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "hird reading|Third Reading|inal passage")))], "FL", sep = "")

dupList <- which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "hird reading, passed")))
dupList <- dupList[which(tableCopy$billId[dupList] == tableCopy$billId[dupList + 1])]
agencyList <- tableCopy$agency[dupList]
agencyList <- unlist(lapply(agencyList, houseSwap))
tableCopy$agency[dupList + 1] <- agencyList

for(comm in duplicates$name){
  lines <- which(unlist(lapply(tableCopy$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                               "assed to ",
                                                                               "efer to "), comm, "\\>", sep = "", collapse = "|"))))
  
  tableCopy$locId[which(!is.na(tableCopy$agency))[which(!is.na(tableCopy$agency)) %in% lines]] = 
    (live_comms %>% filter(name == comm, biennium == now))$commId[match(tableCopy$agency[which(!is.na(tableCopy$agency))[which(!is.na(tableCopy$agency)) %in% lines]], substr((live_comms %>% filter(name == comm, biennium == now))$longName,1,1))]
  
}

tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Delivered to Governor")))] = "DSK"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Delivered to Governor")))] = "G"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Governor signed")))] = "SGN"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Governor signed")))] = "G"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Governor vetoed")))] = "VET"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Governor vetoed")))] = "G"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Governor partially vetoed")))] = "PVT"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Governor partially vetoed")))] = "G"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "overridden")))] = "OVR"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "overridden")))] = "0"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Speaker signed")))] = "SPK"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "Speaker signed")))] = "H"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "President signed")))] = "PRE"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "President signed")))] = "S"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "onference")))] = "CNF"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "onference")))] = "C"
tableCopy$locId[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "ffective date|hapter")))] = "LAW"
tableCopy$agency[which(unlist(lapply(tableCopy$historyLine, grepl, pattern = "ffective date|hapter")))] = "L"

tableCopy$locId[which(grepl("GA", tableCopy$billId) & unlist(lapply(tableCopy$historyLine, grepl, pattern = "confirmation calendar")))] = "SFL"
tableCopy$agency[which(grepl("GA", tableCopy$billId) & unlist(lapply(tableCopy$historyLine, grepl, pattern = "confirmation calendar")))] = "S"
tableCopy$locId[which(grepl("GA", tableCopy$billId) & unlist(lapply(tableCopy$historyLine, grepl, pattern = "onfirmed")))] = "CON"
tableCopy$agency[which(grepl("GA", tableCopy$billId) & unlist(lapply(tableCopy$historyLine, grepl, pattern = "onfirmed")))] = "O"
tableCopy$locId[which(grepl("CR|JM|JR", tableCopy$billId) & unlist(lapply(tableCopy$historyLine, grepl, pattern = "Filed with")))] = "FIL"
tableCopy$agency[which(grepl("CR|JM|JR", tableCopy$billId) & unlist(lapply(tableCopy$historyLine, grepl, pattern = "Filed with")))] = "F"
tableCopy$locId[which(grepl("HR|SR", tableCopy$billId) & unlist(lapply(tableCopy$historyLine, grepl, pattern = "Adopted")))] = "ADP"
tableCopy$agency[which(grepl("HR|SR", tableCopy$billId) & unlist(lapply(tableCopy$historyLine, grepl, pattern = "Adopted")))] = "A"

tableCopy <- tableCopy[complete.cases(tableCopy[, "locId"]) ,] %>% select(billId, biennium, actionDate, locId, historyLine)


##

comm_changes <- tryCatch(read.csv("Source Data/comm_changefile.csv", header = TRUE, stringsAsFactors = FALSE),
                         error = function(e){ throwCode(501) })

if(nrow(comm_changes) > 0){
  for(comm in 1:nrow(comm_changes)){
    tableCopy$locId[tableCopy$locId == 
                      live_comms$commId[live_comms$name == comm_changes$badComm[comm] & 
                                          live_comms$biennium == comm_changes$biennium[comm]]] = live_comms$commId[live_comms$name == comm_changes$goodComm[comm] &
                                                                                                                     live_comms$biennium == comm_changes$biennium[comm] & live_comms$agency == comm_changes$agency[comm]]
    
    live_comms$active[live_comms$name == comm_changes$badComm[comm] & live_comms$biennium == comm_changes$biennium[comm]] = FALSE
  }
}

##IMPLEMENTING SESSIONS

tableCopy$sessionAct <- rep(NA, nrow(tableCopy))

sessDates <- tryCatch(read.csv("Source Data/sessDates.csv", header = TRUE, stringsAsFactors = FALSE,
                               fileEncoding = "UTF-8-BOM"),
                      error = function(e){ throwCode(601) })
for(i in 1:nrow(sessDates)){
  tableCopy$sessionAct[which(ymd(tableCopy$actionDate) %within%
                               interval(ymd(sessDates$dateStart[i]),
                                        ymd(sessDates$dateEnd[i])))] = sessDates$sessTitle[i]
}

##MEMBERSHIP FLAGS

membership <- tryCatch(read.csv("Source Data/commLeaders.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1"),
                       error = function(e){ throwCode(701) })

if(length(unique((membership %>% filter(biennium == now))$committee)) != nrow(live_comms %>% filter(biennium == now))){ throwCode(702) }

if(nrow(new_bills) > 0){
  for(bill in temp_bills$billId){
    billIndex <- which(live_bills$billId == bill & live_bills$biennium == now)
    
    live_bills$firstRef[billIndex] <-
      tableCopy$locId[which(tableCopy$billId == bill)][1]
    
    if(!(live_bills$firstRef[billIndex] %in% bi_comms$commId)){
      live_bills$isChair[billIndex] <- NA
      live_bills$isVice[billIndex] <- NA
      live_bills$isMem[billIndex] <- NA
    }
    else{
      info <- memInfor(bi_sponsors, membership, now, bi_comms$longName[bi_comms$commId == live_bills$firstRef[billIndex]], live_bills$primeSponsorId[billIndex])
      
      live_bills$isChair[billIndex] <- info$chair
      live_bills$isVice[billIndex] <- info$vice
      live_bills$isMem[billIndex] <- info$member
    }
  }
}

##PROGRESS FLAGS

for(bill in bi_bills_long$billId){
  live_bills$passedHouse[live_bills$billId == bill & live_bills$biennium == now] <- ifelse(sum(grepl("passed", tableCopy$historyLine[tableCopy$locId == "HFL" & tableCopy$billId == bill])) > 0,1,0)
  live_bills$passedSen[live_bills$billId == bill & live_bills$biennium == now] <- ifelse(sum(grepl("passed", tableCopy$historyLine[tableCopy$locId == "SFL" & tableCopy$billId == bill])) > 0,1,0)
  live_bills$becameLaw[live_bills$billId == bill & live_bills$biennium == now] <- ifelse("LAW" %in% tableCopy$locId[tableCopy$billId == bill],1,0)
  live_bills$bounces[live_bills$billId == bill & live_bills$biennium == now] <- nrow(tableCopy[tableCopy$billId == bill,])
}

live_actions <- distinct(bind_rows(live_actions, tableCopy))

##WRITE TO DATABASE

dbWriteTable(WashEx, "Legislation", live_bills, overwrite = TRUE)
dbWriteTable(WashEx, "Sponsors", live_sponsors, overwrite = TRUE)
dbWriteTable(WashEx, "Committees", live_comms, overwrite = TRUE)
dbWriteTable(WashEx, "Actions", live_actions, overwrite = TRUE)
dbWriteTable(WashEx, "Sessions", sessDates, overwrite = TRUE)

dbDisconnect(WashEx)

WashEx_AWS <- src_mysql(dbname = cfg$db$dbname, host = cfg$db$host, port = as.integer(cfg$db$port), 
                        user = cfg$db$user, password = cfg$db$password)

sql_bills <- make_mysql_compatible(live_bills)
sql_sponsors <- make_mysql_compatible(live_sponsors)
sql_comms <- make_mysql_compatible(live_comms)
sql_actions <- make_mysql_compatible(live_actions)
sql_sessions <- make_mysql_compatible(sessDates)

copy_to(WashEx_AWS, df = sql_bills, name = "Legislation", temporary = FALSE, overwrite = TRUE)
copy_to(WashEx_AWS, df = sql_sponsors, name = "Sponsors", temporary = FALSE, overwrite = TRUE)
copy_to(WashEx_AWS, df = sql_comms, name = "Committees", temporary = FALSE, overwrite = TRUE)
copy_to(WashEx_AWS, df = sql_actions, name = "Actions", temporary = FALSE, overwrite = TRUE)
copy_to(WashEx_AWS, df = sql_sessions, name = "Sessions", temporary = FALSE, overwrite = TRUE)

WashEx_backup <- tryCatch(dbConnect(RSQLite::SQLite(), "Data Backups/live-backup.sqlite"), error = function(e){ throwCode(002) })

dbWriteTable(WashEx_backup, "Legislation", live_bills, overwrite = TRUE)

dbWriteTable(WashEx_backup, "Sponsors", live_sponsors, overwrite = TRUE)
dbWriteTable(WashEx_backup, "Committees", live_comms, overwrite = TRUE)
dbWriteTable(WashEx_backup, "Actions", live_actions, overwrite = TRUE)
dbWriteTable(WashEx_backup, "Sessions", sessDates, overwrite = TRUE)

throwCode(000)
