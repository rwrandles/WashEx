## Loading necessary packages

library(httr) ## For scraping XML data from Washington Legislative Web Services page
library(XML) ## For parsing XML data into data frames
library(dplyr) ## For easy data frame manipulation
library(tidyr) ## For data frame cleaning
library(DBI) ## For SQLite database functions
library(RSQLite) ## For SQLite database functions
library(lubridate)

setwd("C:/Users/Rohnin/Documents/R Stuff/WashEx/Compiled_8.11.19/") ## Set working directory to location of WashEx folder

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
  path <- paste("legislationservice.asmx/GetAmendmentsForBiennium?biennium=", 
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

getMembers <- function(biennium, agency, committee){
  
  committee <- gsub(" ", "%20", committee)
  committee <- gsub("&", "%26", committee)
  
  path <- paste("/CommitteeService.asmx/GetCommitteeMembers?biennium=", 
                biennium, "&agency=", agency, "&committeeName=", committee, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

########################################
## B - Generation of Secondary tables ##
########################################

## Generate list of biennia

## Create a vector containing strings of the form "XXXX-YY" corresponding to the two year biennia for 
## a particular congressional session, for the years between 2001 and 2019

biennium_list <- c("2015-16", "2017-18")

#biennium_list = c()
#counterb = 1
#for (year in seq(2001, 2019, 2)){
#  biennium_list[counterb] = paste(year, "-", substr(year+1,3,4), sep="")
#  counterb = counterb + 1
#}

## Generate master committee table

master_comms <- data.frame() ## begin with a blank data frame

## For each biennium in the list of biennia:
## 
## Use the getCommittees() function to return all committee information for that session
## Following, add a column containing the biennium and reclassify the Id variable as an integer
## Finally, bind this set of committees to the existing master frame for comittees

for(biennium in biennium_list){
  year_comm <- getCommittees(biennium)
  
  year_comm <- year_comm %>% mutate(Biennium = biennium, Id = strtoi(Id))
  
  master_comms <- bind_rows(master_comms, year_comm)
}

master_comms$Acronym <- trimws(master_comms$Acronym) ## Removes whitespace from the Acronym column

## Generating committee Ids that are unique to the committee AND biennium

master_comms$CommitteeId <- NA
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
  master_comms$CommitteeId[counter] <- (strtoi(substr(master_comms$Biennium[counter], 1, 4)) - 1991) / 2 + 65
  
  master_comms$CommitteeId[counter] <- intToUtf8(master_comms$CommitteeId[counter])
  
  if(counter == 1){
    master_comms$CommitteeId[counter] <- paste(master_comms$CommitteeId[counter], as.character(commCounter), sep = "")
  }
  else if(master_comms$Biennium[counter] == master_comms$Biennium[counter-1]){
    commCounter <- commCounter + 1
    
    master_comms$CommitteeId[counter] <- paste(master_comms$CommitteeId[counter], as.character(commCounter), sep = "")
  }
  else{
    commCounter <- 0
    
    master_comms$CommitteeId[counter] <- paste(master_comms$CommitteeId[counter], as.character(commCounter), sep = "")
  }
}

master_comms$Active <- rep(TRUE, nrow(master_comms))

## Remove columns except the following: committe Id, Biennium, Acronym, Agency (House or Senate), Name, and Long Name

master_comms <- master_comms %>% select(CommitteeId, Biennium, Acronym, Agency, Name, LongName)

## Committee Reassignment

##
## 2017-18
##

master_comms$CommitteeId[master_comms$Name == "Energy, Environment & Technology" & 
                           master_comms$Biennium == "2017-18"] <- master_comms$CommitteeId[master_comms$Name == "Energy, Environment & Telecommunications" & 
                                                                                             master_comms$Biennium == "2017-18"]
master_comms$Active[master_comms$Name == "Energy, Environment & Telecommunications" & 
                           master_comms$Biennium == "2017-18"] = FALSE

##

master_comms$CommitteeId[master_comms$Name == "Higher Education" & 
                           master_comms$Biennium == "2017-18"] <- master_comms$CommitteeId[master_comms$Name == "Higher Education & Workforce Development" & 
                                                                                             master_comms$Biennium == "2017-18"]

master_comms$CommitteeId[master_comms$Name == "Higher Education & Workforce Development" & 
                           master_comms$Biennium == "2017-18"] = FALSE

##

master_comms$CommitteeId[master_comms$Name == "Human Services & Corrections" & 
                           master_comms$Biennium == "2017-18"] <- master_comms$CommitteeId[master_comms$Name == "Human Services, Mental Health & Housing" & 
                                                                                             master_comms$Biennium == "2017-18"]

master_comms$Active[master_comms$Name == "Human Services, Mental Health & Housing" & 
                           master_comms$Biennium == "2017-18"] = FALSE

##

master_comms$CommitteeId[master_comms$Name == "Labor & Commerce" & 
                           master_comms$Biennium == "2017-18"] <- master_comms$CommitteeId[master_comms$Name == "Commerce, Labor & Sports" & 
                                                                                             master_comms$Biennium == "2017-18"]

master_comms$Active[master_comms$Name == "Commerce, Labor & Sports" & 
                           master_comms$Biennium == "2017-18"] = FALSE

##

master_comms$CommitteeId[master_comms$Name == "State Government & Tribal Relations" & 
                           master_comms$Biennium == "2017-18"] <- master_comms$CommitteeId[master_comms$Name == "State Government" & 
                                                                                             master_comms$Biennium == "2017-18"]

master_comms$Active[master_comms$Name == "State Government" & 
                           master_comms$Biennium == "2017-18"] = FALSE

##

master_comms$CommitteeId[master_comms$Name == "Health & Long Term Care" & 
                           master_comms$Biennium == "2017-18"] <- master_comms$CommitteeId[master_comms$Name == "Health Care" & 
                                                                                             master_comms$Biennium == "2017-18"]

master_comms$Active[master_comms$Name == "Health Care" & 
                           master_comms$Biennium == "2017-18"] = FALSE

##

master_comms$CommitteeId[master_comms$Name == "Agriculture, Water, Natural Resources & Parks" & 
                           master_comms$Biennium == "2017-18"] <- master_comms$CommitteeId[master_comms$Name == "Agriculture, Water, Trade & Economic Development" & 
                                                                                             master_comms$Biennium == "2017-18"]

master_comms$Active[master_comms$Name == "Agriculture, Water, Trade & Economic Development" & 
                           master_comms$Biennium == "2017-18"] = FALSE


## Generate master sponsors table

master_sponsors <- data.frame()

## For each biennium:
##
## Use the getSponsors() function to get all information on legislators for that session.
## Add a new column containing the biennium, change the Id variable to an integer, create 
## a new variable containing "rep" if in the House and "sen" if in the Senate, and
## create a new variable containing the number of the congressional session.
##
## Finally, bind this set of legislators to the existing master frame for legislators

## Sponsor gender assignment
## 1FOR FEMALE, 0 FOR MALE

genders <- read.csv("sponsor_gender.csv", header = TRUE, stringsAsFactors = FALSE)

for(biennium in biennium_list){
  year_spons <- getSponsors(biennium)
  
  gender_list <- c()
  
  for(spons in 1:nrow(year_spons)){
    gender_list[spons] <- ifelse(genders$Gender[which(genders$repId == year_spons$Id[spons])] == "0", "M", "F") }
  
  year_spons <- year_spons %>% mutate(Gender = gender_list)
  
  year_spons <- year_spons %>% mutate(Biennium = biennium, repId = paste(Id, substr(biennium, 3,4), sep = "_"), type = ifelse(Agency == "House", "rep", "sen"))
  year_spons <- year_spons %>% mutate(cong = length(seq(1889,substr(biennium,1,4),2)))
  
  master_sponsors <- bind_rows(master_sponsors, year_spons)
}

## Sort legislators by last name, and give each entry a unique numerical Id
## Then remove columns except the following: Id, representative Id, congressional session, first name, last name, type, district, party, and biennium

master_sponsors <- master_sponsors %>% arrange(LastName) %>% select(repId, cong, FirstName, LastName, type, District, Party, Gender, Biennium)

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
## biennium, bill Id, and bill number.

for(biennium in biennium_list){
  year1 <- strtoi(substr(biennium,1,4))
  year2 <- year1 + 1
  
  year1_bills <- getLegislationByYear(as.character(year1))
  year2_bills <- getLegislationByYear(as.character(year2))
  
  biennium_bills <- bind_rows(year1_bills, year2_bills)
  
  raw_bills <- bind_rows(raw_bills, biennium_bills) %>% select(Biennium, BillId, BillNumber)
}

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
  year_bills <- raw_bills[which(raw_bills$Biennium == biennium) ,]
  
  for(bill in 1:nrow(year_bills)){
    current_bill <- getLegislation(year_bills$Biennium[bill], year_bills$BillNumber[bill])
    
    if(grepl("GA", bill$BillId)){
      bill$PrimeSponsorID = "GOV" }
    
    master_bills <- bind_rows(master_bills, current_bill)
  }
}

master_bills <- master_bills %>% mutate(billUrl = 
                                          paste("http://app.leg.wa.gov/billsummary?BillNumber=",
                                                BillNumber, "&Year=", 
                                                substr(IntroducedDate, 1, 4),
                                                "&Initiative=false", sep = ""),
                                        PrimeSponsorID = paste(PrimeSponsorID, substr(biennium, 3,4), sep = "_"))

## Remove all columns except: Bill Id, Bill number, biennium, legal title, date of introduction, sponsor,
## sponsor Id, and long description.

master_bills <- master_bills %>% select(BillId, BillNumber, 
                                        
                                        Biennium, LegalTitle, IntroducedDate, Sponsor, PrimeSponsorID, LongDescription, billUrl)

## Generate action table

raw_actions <- data.frame() ## start with blank table

## For each bill in the master bills table:
##
## use the getStatusChanges() function to get the legislative actions history on the bill
## Then, add the biennium onto the row and bind with the raw actions table

for(bill in 1:nrow(master_bills)){
  current_action <- getStatusChanges(master_bills$Biennium[bill], master_bills$BillNumber[bill],
                               paste("01-01-", substr(master_bills$Biennium[bill],1,4), sep = ""),
                               paste("12-31-", as.character(strtoi(substr(master_bills$Biennium[bill],1,4)) + 1), sep = ""))
  
  current_action <- current_action %>% mutate(Biennium = master_bills$Biennium[bill])
  
  raw_actions <- bind_rows(raw_actions, current_action)
}

#####################################################
## C - Parses actions into congressional locations ##
#####################################################

actionTableCopy <- distinct(raw_actions) ## first, create a copy of the raw actions table for modification
actionTableCopy$Status <- sapply(actionTableCopy$Status, trimws) ## remove all whitespace from the "status" column

len = nrow(actionTableCopy)
actionTableCopy$locId = rep(NA, len) # create a column called "locId" which will hold the alphanumeric code related to the location

actionTableFinal = data.frame() # the actionTable that all biennium-based actionTables are put into

for (biennium in biennium_list){
  counter = 1
  
  subset_bi_df = master_comms[which(master_comms$Biennium==biennium),] # committees in that biennium
  committee_names = subset_bi_df$Name # all the committees by name in  biennium
  committee_acronyms = subset_bi_df$Acronym # all comms by acronym in biennium
  
  actionTableBiennium = actionTableCopy[actionTableCopy$Biennium==biennium,] # actionTable lines that happen in biennium given
  
  descriptions = actionTableBiennium[,2] # the descriptions of bill's progress to iterate through
  
  location_flag = ""
  current_bill = actionTableBiennium$BillId[counter] # sets starting bill for biennium
  
  #sets initial location of bill
  sub <- substr(actionTableBiennium$BillId[counter], 1,1)
  if(sub == "H"){
    location_flag = "House"
  }
  else if(sub == "S"){
    location_flag = "Senate"
  }
  else{
    if (grepl("HB", actionTableBiennium$BillId[counter])){
      location_flag = "House"
    }
    if (grepl("SB", actionTableBiennium$BillId[counter])){
      location_flag = "Senate"
    }
  }
  
  for (d in descriptions){ #d is a single line of an event in the bill's history
    
    if (actionTableBiennium$BillId[counter] == current_bill){ #if d is in the same bill as previous line d-1
      
      if (grepl("eturned to Senate", d)){
        #this bill has been passed to Senate
        location_flag = "Senate" }
      
      if (grepl("eturned to House", d)){
        #this bill has been passed to House
        location_flag = "House" }
      
    }
    
    else if (actionTableBiennium$BillId[counter] != current_bill){ #we are now in a new bill
      current_bill = actionTableBiennium$BillId[counter] #update current bill
      
      #sets starting location
      if (grepl("HB", actionTableBiennium$BillId[counter])){
        location_flag = "House"
      }
      if (grepl("SB", actionTableBiennium$BillId[counter])){
        location_flag = "Senate"
      }
    }
    
    for (acronym in committee_acronyms){ 
      if (grepl(paste(acronym, ""), d)){
        temp4 = master_comms[which(master_comms$Acronym==acronym & master_comms$Biennium==biennium),]
        actionTableBiennium$locId[counter] = temp4$CommitteeId 
      }
      
      for (committee in committee_names){
        
        if (grepl(paste("eferred to", committee),d)){
          #if true, then d contains bill was "refereed to committee" after this action
          temp1 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym==acronym),]
          if(length(temp1$Name) > 0){
            actionTableBiennium$locId[counter] = temp1$CommitteeId 
          }
        }
        
        if (grepl(paste("eturned to", paste(location_flag, "Rules Committee")), d)){
          actionTableBiennium$locIdp[counter] = paste(substr(location_flag, 1, 1), "RL", sep = "") }
        
        if (grepl(paste("eturned to", paste(location_flag, "Rules \"X\" file")), d)){
          actionTableBiennium$locIdp[counter] = paste(substr(location_flag, 1, 1), "RL", sep = "") }
        
        if (grepl("by Rules Committee", d)){
          actionTableBiennium$locIdp[counter] = paste(substr(location_flag, 1, 1), "RL", sep = "") }
        
        if (grepl("passed to Rules|Passed to Rules", d)){
          actionTableBiennium$locIdp[counter] = paste(substr(location_flag, 1, 1), "RL", sep = "") }
        
        #House/Senate Rule Committee
        #if (grepl(paste("eturned to", paste(location_flag, "Rules Committee")), d)){ #looks for "returned to House/Senate Rules Committee"
        #  actionTableBiennium$locId[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))] }
        
        #if (grepl(paste("eturned to", paste(location_flag, "Rules \"X\" file")), d)){ #looks for "House/Senate Rules "X" File" 
        #  actionTableBiennium$locId[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))]  }
        
        #if (grepl("by Rules Committee", d)){ #looks for "by Rules Committee"
        #  actionTableBiennium$locId[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))]  }
        
        #if (grepl("passed to Rules|Passed to Rules", d)){ #looks for "passed to Rules"
        #  actionTableBiennium$locId[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))]  }
        ##########
        
        #Concur/Not Concur
        if (grepl("concur|concur.", d)){
          actionTableBiennium$locId[counter] = paste(substr(location_flag, 1,1), "FL", sep = "") 
          if(location_flag == "House"){
            location_flag = "Senate" 
            }
          else if(location_flag == "Senate"){
            location_flag = "House" 
            }
          }
        
        if (location_flag == "House"){
          #if true, then this description is in the house
          
          if (grepl("hird reading|Third Reading|Final passage", d)){
            actionTableBiennium$locId[counter] = "HFL" }
          
          if (grepl("Ways & Means", d)){
            temp7 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym=="WAYS"),]
            if (length(temp7$Name)>0){
              actionTableBiennium$locId[counter] = temp7$CommitteeId }}
          
          if (grepl("Higher Education", d)){
            temp10 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym=="HE"),]
            if (length(temp10$Name)>0){
              actionTableBiennium$locId[counter] = temp10$CommitteeId
            }}
        }
        
        if (location_flag == "Senate"){
          #if true, then this description is in the senate
          
          if (grepl("hird reading|Third Reading|Final passage", d)){
            actionTableBiennium$locId[counter] = "SFL" }
          
          if (grepl("Ways & Means", d)){
            temp8 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym=="WM"),]
            if (length(temp8$Name)>0){
              actionTableBiennium$locId[counter] = temp8$CommitteeId }}
          
          if (grepl("Higher Education", d)){
            temp11 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym=="HIE"),]
            if (length(temp11$Name)>0){
              actionTableBiennium$locId[counter] = temp11$CommitteeId }}
        }
        
      }
      
    }
    
    if (grepl("Delivered to Governor", d)){
      #if true, then bill was delivered to governor
      actionTableBiennium$locId[counter] = "DSK" }
    
    if (grepl("Governor signed", d)){
      #if true, then bill was signed
      actionTableBiennium$locId[counter] = "SGN" }
    
    if (grepl("Governor vetoed", d)){
      #if true, then governor vetoed bill
      actionTableBiennium$locId[counter] = "VET" }
    
    if (grepl("Governor partially vetoed", d)) {
      #if true, then governor partially vetoed bill
      actionTableBiennium$locId[counter] = "PVT" }
    
    if (grepl("Speaker signed", d)){
      #if true, then house speaker signed bill
      actionTableBiennium$locId[counter] = "SPK" }
    
    if (grepl("President signed", d)){
      #if true, then senate president signed bill
      actionTableBiennium$locId[counter] = "PRE" }
    
    if (grepl("onference", d)){
      actionTableBiennium$locId[counter] = "CNF" }
    
    if (grepl("Senate Rules", d)){
      actionTableBiennium$locId[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))]  }
    
    if (grepl("ffective date", d)){
      actionTableBiennium$locId[counter] = "LAW" }
    
    if (length((d == d[grep("Third reading, passed", d)])) >0){
      if (d == d[grep("Third reading, passed", d)]){
        #this bill has been passed to other chamber, so location_flag needs to change
        if (location_flag == "House"){
          location_flag = "Senate" }
        else if (location_flag == "Senate"){
          location_flag = "House" }
      }} 
    
    counter = counter + 1
    
    
  }
  actionTableFinal = rbind(actionTableFinal, actionTableBiennium)
  
}

master_actions <- actionTableFinal[complete.cases(actionTableFinal[, 10]) ,] %>% select(BillId, Biennium, ActionDate, locId, HistoryLine)

## Session dates

sessDates <- read.csv("sessDates.csv", header = TRUE, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8-BOM")

#######################
## D - Parsing votes ##
#######################

#billsVote <- master_actions$BillId[grepl("yeas", master_actions$HistoryLine)]


##########################
## E - SQLite Packaging ##
##########################

WashEx <- dbConnect(RSQLite::SQLite(), dbname = "WashEx-db.sqlite")

dbWriteTable(WashEx, "Legislation", master_bills, overwrite = TRUE)
dbWriteTable(WashEx, "Sponsors", master_sponsors, overwrite = TRUE)
dbWriteTable(WashEx, "Committees", master_comms, overwrite = TRUE)
dbWriteTable(WashEx, "Actions", master_actions, overwrite = TRUE)
dbWriteTable(WashEx, "Sessions", sessDates, overwrite = TRUE)

dbDisconnect(WashEx)

