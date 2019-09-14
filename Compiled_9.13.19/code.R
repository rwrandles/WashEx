## Loading necessary packages

library(httr) ## For scraping XML data from Washington Legislative Web Services page
library(XML) ## For parsing XML data into data frames
library(dplyr) ## For easy data frame manipulation
library(tidyr) ## For data frame cleaning
library(DBI) ## For SQLite database functions
library(RSQLite) ## For SQLite database functions
library(lubridate) ## For calendar and date related functions
library(zoo) ## For the function na.locf() -- fills in NA values to most recent non-NA value
library(snakecase) ## For renaming column names to lower camel case

setwd(" ") ## Set working directory to locId of WashEx folder

#################################
## A - Data scraping functions ##
#################################

url <- "http://wslwebservices.leg.wa.gov/" ## URL prefix for all GET requests on the wsl service.

## Function: getLegislation
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe of legislative summary information including billId, introduction date, bill titles/descriptions

getLegislation <- function(biennium, billNumber){
  
  path <- paste("legislationservice.asmx/GetLegislation?biennium=", 
                gsub(" ", "%20", biennium), "&billNumber=", gsub(" ", "%20", billNumber), sep="")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getLegislationByYear
## Arguments: year - a character string of the format "XXXX"
##
## Returns: a dataframe containing a list of all of the bills introduced during the year

getLegislationByYear <- function(year){
  
  path <- paste("legislationservice.asmx/GetLegislationByYear?year=", gsub(" ", "%20", year), sep="")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getCurrentStatus
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe with the bill's current status
##          **NOTE** THIS IS THE BILL'S STATUS AS OF TODAY. IF A BILL WAS NEVER PASSED, IT LISTS THE MOST RECENT STATUS

getCurrentStatus <- function(biennium, billNumber){
  path <- paste("legislationservice.asmx/GetCurrentStatus?biennium=", 
                gsub(" ", "%20", biennium), "&billNumber=", gsub(" ", "%20", billNumber), sep="")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getStatusChanges
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##            beginDate - a character string of the format "YYYY-MM-DD"
##            endDate - a character string of the format "YYYY-MM-DD"
##
## Returns: a dataframee of all status changes occurring on the bill between the dates listed

getStatusChanges <- function(biennium, billNumber, beginDate, endDate){
  path <- paste("legislationservice.asmx/GetLegislativeStatusChangesByBillNumber?biennium=", 
                gsub(" ", "%20", biennium), "&billNumber=", gsub(" ", "%20", billNumber), 
                "&beginDate=", beginDate, "&endDate=", endDate, sep="")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getAmendment
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe of all amendment actions (accepted and rejected) on the particular bill, including URL to the amendment text

getAmendments <- function(biennium, billNumber){
  path <- paste("legislationservice.asmx/GetAmendmentsForbiennium?biennium=", 
                biennium, "&billNumber=", billNumber, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
  
}

## Function: getHearings
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe of dates, locations, and descriptions of committee hearings on the particular bill

getHearings <- function(biennium, billNumber){
  path <- paste("legislationservice.asmx/GetHearings?biennium=",
                biennium, "&billNumber=", billNumber, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getRollCalls
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe of roll call information on the particular bill
##          **NOTE** BECAUSE XML ALLOWS NESTED LISTS, THE XML PACKAGE CANNOT COMPLETELY PARSE SOME PORTIONS INTO A COMPREHENSIBLE DATA FRAME
##          **NOTE** BECAUSE OF THIS, THE CURRENT IMPLEMENTATION OF THIS FUNCTION IS INCOMPLETE

getRollCalls <- function(biennium, billNumber){
  path <- paste("legislationservice.asmx/GetRollCalls?biennium=",
                biennium, "&billNumber=", billNumber, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getSponsors
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##
## Returns: a dataframe of all of the sponsors (all congressmembers) for that biennium

getSponsors <- function(biennium){
  path <- paste("sponsorservice.asmx/GetSponsors?biennium=",
                biennium, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getBillSponsors
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billId - a character string of the format "XX YYYY" where XX is the prefix (HB, SB, etc.) and YYYY is the bill number
##
## Returns: a dataframe of all sponsors on the bill, including co-sponsors

getBillSponsors <- function(biennium, billId){
  path <- paste("legislationservice.asmx/GetSponsors?biennium=",
                biennium, "&billId=", billId, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getLegislationSigned
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            agency - either "House" or "Senate"
##
## Returns: a dataframe containing all of the bills that originated in the specified chamber and were eventually signed into law

getLegislationSigned <- function(biennium, agency){
  path <- paste("legislationservice.asmx/GetLegislationGovernorSigned?biennium=", 
                biennium, "&agency=", agency, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getCommittees
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##
## Returns: a dataframe of all of the committees and their corresponding committee code that were active in that biennium

getCommittees <- function(biennium){
  path <- paste("CommitteeService.asmx/GetCommittees?biennium=",
                biennium, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: isMajority
## Arguments: year - a character string representing the year in question
##            billId - a character string of the format "XX YYYY" where XX is the prefix (HB, SB, etc.) and YYYY is the bill number
##
## Returns: a boolean indicating whether the primary sponsor of bill with billId was a member of the majority party in the given year of introduction

isMajority <- function(year, billId){
  if(year == "0001") { return(NA) }
  else{
    if(grepl("HB|HJM|HJR|HCR|HR", master_bills$billType[master_bills$billId == billId & grepl(year, master_bills$introducedDate)])){
      if(master_sponsors$party[master_sponsors$repId == master_bills$primeSponsorId[master_bills$billId == billId & grepl(year, master_bills$introducedDate)] & master_sponsors$type == "rep"] == majority_df$hMaj[majority_df$year == year]){ return(TRUE) }
      else{ return(FALSE) }
    }
    else if(grepl("SB|SJM|SJR|SCR|SR", master_bills$billType[master_bills$billId == billId & grepl(year, master_bills$introducedDate)])){
      if(master_sponsors$party[master_sponsors$repId == master_bills$primeSponsorId[master_bills$billId == billId & grepl(year, master_bills$introducedDate)] & master_sponsors$type == "sen"] == majority_df$sMaj[majority_df$year == year]){ return(TRUE) }
      else{ return(FALSE) }
    }
    else{ return(NA) }
  }
}

## Function: dateChange
## Arguments: biennium - a character string of the format "XXXX-YY" indicating the years of the legislature in question
##            type - a character string of either "rep" or "sen" indicating the type of legislator
##            oldId - the repId of the outgoing legislator
##            endDate - the date of the end of the outgoing legislator's term
##            newId - the repId of the incoming legislator
##            startDate - the date of the beginning of the incoming legislator's term
##
## Returns: does not return a value; this function makes a change in the master_sponsors table to the startDate and endDate columns
##          to account for shifting legislators throughout the term

dateChange <- function(biennium, type, oldId, endDate, newId, startDate){
  master_sponsors$termStart[master_sponsors$biennium == biennium & master_sponsors$type == type & master_sponsors$repId == newId] <<- startDate
  master_sponsors$termEnd[master_sponsors$biennium == biennium & master_sponsors$type == type & master_sponsors$repId == oldId] <<- endDate
}

## Function: getReferral
## Arguments: billId - a character string of the format "XX YYYY" where XX is the prefix (HB, SB, etc.) and YYYY is the bill number
##            biennium - a character string of the format "XXXX-YY" indicating the years of the legislature in question
##
## Returns: returns the long name of the committee of original referral for bill with billId in the given biennium. If no such committee is found,
##          returns NA

getReferral <- function(billId, biennium){
  id = master_actions$locId[master_actions$billId == billId & master_actions$biennium == biennium][1]
  if(!is.na(id)){
    ifelse(length(id) > 0, return(id), return(NA))
  }
  else{ return(NA) }
}

## Function: memInfor
## Arguments: biennium - a character string of the format "XXXX-YY" indicating the years of the legislature in question
##            comm - long name of the committee in question
##            sponsorId - repId of teh legislator in question
##
## Returns: returns a 1x3 data frame containing booleans for whether the legislator was the chair, vice-chair, and/or member of 
##          the committee in the given biennium

memInfor <- function(biennium, comm, sponsorId){
  sponsor <- paste(unique(master_sponsors$firstName[master_sponsors$repId == sponsorId & master_sponsors$biennium == biennium]),
                   unique(master_sponsors$lastName[master_sponsors$repId == sponsorId & master_sponsors$biennium == biennium]))
  
  mbr <- sum(grepl(sponsor, membership$name[membership$biennium == biennium & membership$committee == comm])) > 0
  if(mbr){
    chr <- as.logical(membership$isChair[membership$biennium == biennium & membership$committee == comm & membership$name == sponsor])
    vce <- as.logical(membership$isVice[membership$biennium == biennium & membership$committee == comm & membership$name == sponsor])
  }
  else{
    chr <- FALSE
    vce <- FALSE
  }
  
  return(data.frame(chair = chr, vice = vce, member = mbr))
}

###############################################
## B - Generation of Legislative data tables ##
###############################################

## Generate list of biennia

## Create a vector containing strings of the form "XXXX-YY" corresponding to the two year biennia for 
## a particular congressional session, for the years between 2001 and 2019

biennium_list <- paste(seq(2015, 2017, 2), substr(seq(2015, 2017, 2) + 1, 3, 4), sep = "-")

## Generate master committee table

master_comms <- data.frame() ## begin with a blank data frame

## For each biennium in the list of biennia:
## 
## Use the getCommittees() function to return all committee information for that session
## Following, add a column containing the biennium and reclassify the id variable as an integer
## Finally, bind this set of committees to the existing master frame for comittees

for(biennium in biennium_list){
  year_comm <- getCommittees(biennium)
  
  names(year_comm) <- to_lower_camel_case(names(year_comm))
  
  year_comm <- year_comm %>% mutate(biennium = biennium, id = strtoi(id))
  
  master_comms <- bind_rows(master_comms, year_comm)
}

master_comms$acronym <- trimws(master_comms$acronym) ## Removes whitespace from the acronym column

## Generating committee Ids that are unique to the committee AND biennium

master_comms$commId <- NA
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


for(counter in 1:nrow(master_comms)){
  master_comms$commId[counter] <- (strtoi(substr(master_comms$biennium[counter], 1, 4)) - 1991) / 2 + 65
  
  master_comms$commId[counter] <- intToUtf8(master_comms$commId[counter])
  
  if(counter == 1){
    master_comms$commId[counter] <- paste(master_comms$commId[counter], as.character(commCounter), sep = "")
  }
  else if(master_comms$biennium[counter] == master_comms$biennium[counter-1]){
    commCounter <- commCounter + 1
    
    master_comms$commId[counter] <- paste(master_comms$commId[counter], as.character(commCounter), sep = "")
  }
  else{
    commCounter <- 0
    
    master_comms$commId[counter] <- paste(master_comms$commId[counter], as.character(commCounter), sep = "")
  }
}

master_comms$active <- rep(TRUE, nrow(master_comms))

## Remove columns except the following: committe id, biennium, acronym, agency (House or Senate), name, long name, and active status

master_comms <- master_comms %>% select(commId, biennium, acronym, agency, name, longName, active)

## Deactivating rules committees

master_comms$active[master_comms$name == "Rules"] = FALSE

## Generate master sponsors table

master_sponsors <- data.frame()

## For each biennium:
##
## Use the getSponsors() function to get all information on legislators for that session.
## Add a new column containing the biennium, change the id variable to an integer, create 
## a new variable containing "rep" if in the House and "sen" if in the Senate, and
## create a new variable containing the number of the congressional session.
##
## Finally, bind this set of legislators to the existing master frame for legislators

majority_df <- data.frame(year = 2001:2019,
                          hMaj = c("N", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D"),
                          sMaj = c("D", "D", "R", "R", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "R", "R", "D", "D", "D"))

## sponsor gender assignment
## 1 FOR FEMALE, 0 FOR MALE

genders <- read.csv("sponsor_gender.csv", header = TRUE, stringsAsFactors = FALSE)

for(term in biennium_list){
  year_spons <- getSponsors(term)
  
  names(year_spons) <- to_lower_camel_case(names(year_spons))
  
  gender_list <- c()
  
  for(spons in 1:nrow(year_spons)){
    gender_list[spons] <- ifelse(unique(genders$gender[which(genders$repId == year_spons$id[spons])]) == "0", "M", "F") }
  
  year_spons <- year_spons %>% mutate(gender = gender_list)
  
  year_spons <- year_spons %>% mutate(biennium = term, repId = paste(id, substr(term, 3,4), sep = "_"), type = ifelse(agency == "House", "rep", "sen"))
  year_spons <- year_spons %>% mutate(leg = length(seq(1889,substr(term,1,4),2)))
  
  master_sponsors <- bind_rows(master_sponsors, year_spons)
}

## Sort legislators by last name, and give each entry a unique numerical id
## Then remove columns except the following: id, representative id, congressional session, first name, last name, type, district, party, and biennium

master_sponsors <- master_sponsors %>% arrange(lastName) %>% select(repId, leg, firstName, lastName, type, district, party, gender, biennium)

master_sponsors$termStart <- paste(substr(master_sponsors$biennium,1,4), "01", "01", sep = "-")
master_sponsors$termEnd <- paste(strtoi(substr(master_sponsors$biennium,1,4)) + 1, "12", "31", sep = "-")

##
## Manually adjusting for mid-term legislator changes
##

dateChange("2015-16", "rep", "10249_15", "2015-10-22", "17204_15", "2015-10-22")
dateChange("2015-16", "rep", "18517_15", "2016-02-02", "24075_15", "2016-02-16")
dateChange("2015-16", "rep", "21043_15", "2015-11-25", "22559_15", "2015-11-03")
dateChange("2015-16", "rep", "14325_15", "2016-01-07", "23902_15", "2016-01-07")
dateChange("2015-16", "rep", "630_15", "2016-04-09", "3476_15", "2016-06-08")
dateChange("2015-16", "rep", "8222_15", "2015-08-31", "21520_15", "2015-09-23")
dateChange("2015-16", "rep", "15098_15", "2015-04-30", "21490_15", "2015-05-08")

dateChange("2015-16", "sen", "173_15", "2015-10-22", "10249_15", "2015-10-22")
dateChange("2015-16", "sen", "644_15", "2015-12-31", "14325_15", "2016-01-07")

dateChange("2017-18", "rep", "1649_17", "2017-09-01", "27988_17", "2017-09-21")
dateChange("2017-18", "rep", "17307_17", "2017-05-31", "27975_17", "2017-06-12")
dateChange("2017-18", "rep", "11952_17", "2017-02-01", "14115_17", "2017-02-02")

dateChange("2017-18", "sen", "27981_17", "2017-07-23", "27981_17", "2017-07-18")
dateChange("2017-18", "sen", "5163_17", "2017-12-31", "28317_17", "2018-01-03")
dateChange("2017-18", "sen", "2140_17", "2017-11-29", "28022_17", "2017-11-29")
dateChange("2017-18", "sen", "18146_17", "2017-01-24", "11952_17", "2017-02-01")

##
## Manually adding governor and people's sponsors
##

master_sponsors <- add_row(master_sponsors, repId = "GOV_15", leg = 64, firstName = "Jay", lastName = "Inslee", type = "gov", district = "0", party = "D", gender = "M", biennium = "2015-16")
master_sponsors <- add_row(master_sponsors, repId = "GOV_17", leg = 64, firstName = "Jay", lastName = "Inslee", type = "gov", district = "0", party = "D", gender = "M", biennium = "2017-18")

master_sponsors <- add_row(master_sponsors, repId = "PPL_15", leg = 64, firstName = "The", lastName = "People", type = "ppl", district = "0", party = "0", gender = "0", biennium = "2015-16")
master_sponsors <- add_row(master_sponsors, repId = "PPL_17", leg = 64, firstName = "The", lastName = "People", type = "ppl", district = "0", party = "0", gender = "0", biennium = "2017-18")

## Generate legislation table

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

## Committee Reassignment

##
## 2017-18
##

master_actions$locId[master_actions$locId == 
                       master_comms$commId[master_comms$name == "Energy, Environment & Technology" & 
                                                  master_comms$biennium == "2017-18"]] = master_comms$commId[master_comms$name == "Energy, Environment & Telecommunications" &
                                                                                                                    master_comms$biennium == "2017-18"]

master_comms$active[master_comms$name == "Energy, Environment & Technology" & master_comms$biennium == "2017-18"] = FALSE

##

master_actions$locId[master_actions$locId == 
                       master_comms$commId[master_comms$name == "Higher Education & Workforce Development" & 
                                                  master_comms$biennium == "2017-18"]] = master_comms$commId[master_comms$name == "Higher Education" &
                                                                                                                    master_comms$biennium == "2017-18" & master_comms$agency == "Senate"]

master_comms$active[master_comms$name == "Higher Education & Workforce Development" & master_comms$biennium == "2017-18"] = FALSE

##

master_actions$locId[master_actions$locId == 
                       master_comms$commId[master_comms$name == "Human Services & Corrections" & 
                                                  master_comms$biennium == "2017-18"]] = master_comms$commId[master_comms$name == "Human Services, Mental Health & Housing" &
                                                                                                                    master_comms$biennium == "2017-18" & master_comms$agency == "Senate"]

master_comms$active[master_comms$name == "Human Services & Corrections" & master_comms$biennium == "2017-18"] = FALSE

##

master_actions$locId[master_actions$locId == 
                       master_comms$commId[master_comms$name == "Labor & Commerce" & 
                                                  master_comms$biennium == "2017-18"]] = master_comms$commId[master_comms$name == "Commerce, Labor & Sports" &
                                                                                                                    master_comms$biennium == "2017-18" & master_comms$agency == "Senate"]

master_comms$active[master_comms$name == "Labor & Commerce" & master_comms$biennium == "2017-18"] = FALSE

##

master_actions$locId[master_actions$locId == 
                       master_comms$commId[master_comms$name == "State Government, Tribal Relations & Elections" & 
                                                  master_comms$biennium == "2017-18"]] = master_comms$commId[master_comms$name == "State Government" &
                                                                                                                    master_comms$biennium == "2017-18" & master_comms$agency == "Senate"]

master_comms$active[master_comms$name == "State Government, Tribal Relations & Elections" & master_comms$biennium == "2017-18"] = FALSE

##

master_actions$locId[master_actions$locId == 
                       master_comms$commId[master_comms$name == "Health & Long Term Care" & 
                                                  master_comms$biennium == "2017-18"]] = master_comms$commId[master_comms$name == "Health Care" &
                                                                                                                    master_comms$biennium == "2017-18" & master_comms$agency == "Senate"]

master_comms$active[master_comms$name == "Health & Long Term Care" & master_comms$biennium == "2017-18"] = FALSE

##

master_actions$locId[master_actions$locId == 
                       master_comms$commId[master_comms$name == "Agriculture, Water, Natural Resources & Parks" & 
                                                  master_comms$biennium == "2017-18"]] = master_comms$commId[master_comms$name == "Agriculture, Water, Trade & Economic Development" &
                                                                                                                    master_comms$biennium == "2017-18" & master_comms$agency == "Senate"]

master_comms$active[master_comms$name == "Agriculture, Water, Natural Resources & Parks" & master_comms$biennium == "2017-18"] = FALSE

##

## Session dates

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

##########################
## D - SQLite Packaging ##
##########################

WashEx <- dbConnect(RSQLite::SQLite(), dbname = "WashEx-db.sqlite")

dbWriteTable(WashEx, "Legislation", master_bills, overwrite = TRUE)
dbWriteTable(WashEx, "Sponsors", master_sponsors, overwrite = TRUE)
dbWriteTable(WashEx, "Committees", master_comms, overwrite = TRUE)
dbWriteTable(WashEx, "Actions", master_actions, overwrite = TRUE)
dbWriteTable(WashEx, "Sessions", sessDates, overwrite = TRUE)

dbDisconnect(WashEx)

