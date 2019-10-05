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

master_sponsors$termStart <- paste(substr(master_sponsors$biennium,1,4), "01", "01", sep = "-")
master_sponsors$termEnd <- paste(strtoi(substr(master_sponsors$biennium,1,4)) + 1, "12", "31", sep = "-")

changeFile <- read.csv("step1.csv", header = TRUE, stringsAsFactors = FALSE)
outgoing <- changeFile %>% filter(!is.na(endDate))
incoming <- changeFile %>% filter(!is.na(startDate))

for(spons in 1:nrow(outgoing)){
  dateChange(outgoing$biennium[spons], outgoing$type[spons], outgoing$repId[spons], outgoing$endDate[spons], FALSE)
}
for(spons in 1:nrow(incoming)){
  dateChange(incoming$biennium[spons], incoming$type[spons], incoming$repId[spons], incoming$startDate[spons], TRUE)
}

raw_bills <- data.frame() 
master_bills <- data.frame()

## For each biennium:
##
## Split the biennium into two years, since the Legislative Web Service handles requests for legislation
## in individual years rather than biennia.
## Create two objects to hold legislative information for the years. Scrape the Web Service using
## the getLegislationByYear() function. 
##
## Next, bind both rows together to create a set of bills for the biennium
##
## Finally, bind this set of bills to the existing raw frame for bills, and select only the variables for
## biennium, bill id, and bill number.

for(biennium in biennium_list){
  year1 <- strtoi(substr(biennium,1,4))
  year2 <- year1 + 1
  
  year1_bills <- getLegislationByYear(as.character(year1))
  year2_bills <- getLegislationByYear(as.character(year2))
  
  biennium_bills <- bind_rows(year1_bills, year2_bills)
  
  names(biennium_bills) <- to_lower_camel_case(names(biennium_bills))
  
  raw_bills <- bind_rows(raw_bills, biennium_bills) %>% select(biennium, billId, billNumber)
}

raw_bills <- distinct(raw_bills)

## Set the sample size for random sampling. This will choose n bills from EACH biennium (currently 10 biennia).

## For each biennium:
## 
## Subset the raw bills frame into only those bills from the particular biennium
## Take a random sample of n bills
##
## For each sampled bill:
## 
##   Use the getLegislation() function to get summary information on the bill
##   Bind this information to the existing master frame for bills

for(biennium in biennium_list){
  year_bills <- raw_bills[which(raw_bills$biennium == biennium) ,]
  
  for(bill in 1:nrow(year_bills)) {
    current_bill <- getLegislation(year_bills$biennium[bill], year_bills$billNumber[bill])
    
    names(current_bill) <- to_lower_camel_case(names(current_bill))
    
    current_bill$primeSponsorId = paste(ifelse(grepl("I", current_bill$billId), "PPL", 
                                               ifelse(grepl("GA", current_bill$billId), "GOV", 
                                                      current_bill$primeSponsorId)), substr(biennium, 3,4), sep = "_")
    
    master_bills <- bind_rows(master_bills, current_bill)
    
    if(bill %% 100 == 0){ print(bill) }
  }
}

master_bills <- master_bills %>% mutate(billUrl = 
                                          paste("http://app.leg.wa.gov/billsummary?billNumber=",
                                                billNumber, "&Year=", 
                                                substr(introducedDate, 1, 4),
                                                "&Initiative=", ifelse(grepl("I", current_bill$billId), "true", "false"), sep = ""))

master_bills$billType <- gsub(" .*$", "", master_bills$billId)

majority_df <- read.csv("legMajorities.csv", header = TRUE, stringsAsFactors = FALSE)

master_bills$majSpons <- unname(mapply(isMajority, year = substr(master_bills$introducedDate,1,4), billId = master_bills$billId))

## Remove all columns except: Bill id, Bill number, biennium, legal title, date of introduction, sponsor,
## sponsor id, and long description.

master_bills <- master_bills %>% select(billId, billType, billNumber, 
                                        
                                        biennium, legalTitle, introducedDate, primeSponsorId, majSpons, longDescription, billUrl)

master_bills <- distinct(master_bills)

## Generate action table

raw_actions <- data.frame() ## start with blank table

## For each bill in the master bills table:
##
## use the getStatusChanges() function to get the legislative actions history on the bill
## Then, add the biennium onto the row and bind with the raw actions table

for(bill in 1:nrow(master_bills)){
  current_action <- getStatusChanges(master_bills$biennium[bill], master_bills$billNumber[bill],
                                     paste("01-01-", substr(master_bills$biennium[bill],1,4), sep = ""),
                                     paste("12-31-", as.character(strtoi(substr(master_bills$biennium[bill],1,4)) + 1), sep = ""))
  
  names(current_action) <- to_lower_camel_case(names(current_action))
  
  current_action <- current_action %>% mutate(biennium = master_bills$biennium[bill])
  
  raw_actions <- bind_rows(raw_actions, current_action)
  
  if(bill %% 100 == 0){ print(bill) }
}

raw_actions$actionDate <- substr(raw_actions$actionDate, 1, 10)

raw_actions <- distinct(raw_actions)

#####################################################
## C - Parses actions into congressional locations ##
#####################################################

dupnames <- master_comms[duplicated(master_comms %>% select(name, biennium)),] %>% select(name, biennium)

tableCopy <- raw_actions %>% filter(!(grepl("retained", raw_actions$historyLine)))

tableCopy$locId <- rep(NA, nrow(tableCopy))
tableCopy$agency <- rep(NA, nrow(tableCopy))

tableCopyFinal <- data.frame()

for(bi in biennium_list){
  tableCopy_bi <- tableCopy[tableCopy$biennium == bi,]
  
  duplicates <- dupnames %>% filter(biennium == bi, name != "Rules")
  
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "irst reading|ntroduce")))] <- ifelse(unlist(lapply(tableCopy_bi$billId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "irst reading|ntroduce")))], grepl, pattern = "HB|HJM|HJR|HCR|HR|HI")), "H", "S")
  
  Hnames <- master_comms$name[substr(master_comms$longName,1,1) == "H" & master_comms$biennium == bi & !(master_comms$name %in% duplicates$name) & master_comms$name != "Rules"]
  Snames <- master_comms$name[substr(master_comms$longName,1,1) == "S" & master_comms$biennium == bi & !(master_comms$name %in% duplicates$name) & master_comms$name != "Rules"]
  Hacr <- master_comms$acronym[substr(master_comms$longName,1,1) == "H" & master_comms$biennium == bi]
  Sacr <- master_comms$acronym[substr(master_comms$longName,1,1) == "S" & master_comms$biennium == bi]
  
  for(comm in Snames){
    code <- master_comms$commId[master_comms$name == comm & master_comms$biennium == bi]
    
    tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                              "assed to ",
                                                                                              "efer to "), comm, "\\>", sep = "", collapse = "|"))))] = code
    tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                               "assed to ",
                                                                                               "efer to "), comm, "\\>", sep = "", collapse = "|"))))] = "S"
  }
  for(comm in Hnames){
    code <- master_comms$commId[master_comms$name == comm & master_comms$biennium == bi]
    
    tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                              "assed to ",
                                                                                              "efer to "), comm, "\\>", sep = "", collapse = "|"))))] = code
    tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                               "assed to ",
                                                                                               "efer to "), comm, "\\>", sep = "", collapse = "|"))))] = "H"
  }
  
  for(acr in Sacr){
    tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(acr, "\\>", sep = ""))))] = master_comms$commId[master_comms$acronym == acr & master_comms$biennium == bi]
    tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(acr, "\\>", sep = ""))))] = "S"
  }
  for(acr in Hacr){
    tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(acr, "\\>", sep = ""))))] = master_comms$commId[master_comms$acronym == acr & master_comms$biennium == bi]
    tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(acr, "\\>", sep = ""))))] = "H"
  }
  
  for(comm in duplicates$name){
    lines <- which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(c("eferred to ",
                                                                                    "assed to ",
                                                                                    "efer to "), comm, "\\>", sep = "", collapse = "|"))))
    
    tableCopy_bi$locId[which(!is.na(tableCopy_bi$agency))[which(!is.na(tableCopy_bi$agency)) %in% lines]] = 
      (master_comms %>% filter(name == comm, biennium == bi))$commId[match(tableCopy_bi$agency[which(!is.na(tableCopy_bi$agency))[which(!is.na(tableCopy_bi$agency)) %in% lines]], substr((master_comms %>% filter(name == comm, biennium == bi))$longName,1,1))]
    
  }
  
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "House Rules")))] = "HRL"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "House Rules")))] = "H"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Senate Rules")))] = "SRL"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Senate Rules")))] = "S"
  
  tableCopy_bi$agency = na.locf(tableCopy_bi$agency)
  
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(c("eferred to", "assed to", "efer to", "by"), "Rules\\>", collapse = "|"))))] = paste(tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = paste(c("eferred to", "assed to", "efer to", "by"), "Rules\\>", collapse = "|"))))], "RL", sep = "")
  
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "hird reading|Third Reading|inal passage")))] = paste(tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "hird reading|Third Reading|inal passage")))], "FL", sep = "")
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Delivered to Governor")))] = "DSK"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Delivered to Governor")))] = "G"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Governor signed")))] = "SGN"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Governor signed")))] = "G"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Governor vetoed")))] = "VET"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Governor vetoed")))] = "G"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Governor partially vetoed")))] = "PVT"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Governor partially vetoed")))] = "G"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "overridden")))] = "OVR"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "overridden")))] = "0"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Speaker signed")))] = "SPK"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "Speaker signed")))] = "H"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "President signed")))] = "PRE"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "President signed")))] = "S"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "onference")))] = "CNF"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "onference")))] = "C"
  tableCopy_bi$locId[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "ffective date")))] = "LAW"
  tableCopy_bi$agency[which(unlist(lapply(tableCopy_bi$historyLine, grepl, pattern = "ffective date")))] = "L"
  
  tableCopyFinal <- bind_rows(tableCopyFinal, tableCopy_bi)
}

master_actions <- tableCopyFinal[complete.cases(tableCopyFinal[, "locId"]) ,] %>% select(billId, biennium, actionDate, locId, historyLine)

output_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(output_df) <- c("biennium", "badComm", "goodComm", "agency")

write.csv(output_df, "step2.csv")

dbWriteTable(WashEx, "Sponsors", master_sponsors, overwrite = TRUE)
dbWriteTable(WashEx, "Legislation", master_bills, overwrite = TRUE)
dbWriteTable(WashEx, "Actions", master_actions, overwrite = TRUE)

dbDisconnect(WashEx)
