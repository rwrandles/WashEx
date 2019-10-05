library(httr)
library(XML)

url <- "http://wslwebservices.leg.wa.gov/" ## URL prefix for all GET requests on the wsl service.
biennium_list <- paste(seq(2013, 2017, 2), substr(seq(2013, 2017, 2) + 1, 3, 4), sep = "-")

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
##            repId - the repId of the affected legislator
##            date - the date of the seat change in YYYY-MM-DD format
##            start - boolean value; if true, the date is for an incoming sponsor; if false, for an outgoing sponsor
##
## Returns: does not return a value; this function makes a change in the master_sponsors table to the startDate and endDate columns
##          to account for shifting legislators throughout the term

dateChange <- function(biennium, type, repId, date, start){
  if(start){
    master_sponsors$termStart[master_sponsors$biennium == biennium & master_sponsors$type == type & master_sponsors$repId == repId] <<- date
  }
  else if(!start){
    master_sponsors$termEnd[master_sponsors$biennium == biennium & master_sponsors$type == type & master_sponsors$repId == repId] <<- date
  }
  else{ return(NA) }
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