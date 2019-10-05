library(dplyr) ## For easy data frame manipulation
library(tidyr) ## For data frame cleaning
library(DBI) ## For SQLite database functions
library(RSQLite) ## For SQLite database functions
library(lubridate) ## For calendar and date related functions
library(zoo) ## For the function na.locf() -- fills in NA values to most recent non-NA value
library(snakecase) ## For renaming column names to lower camel case

setwd(" ")
source("WashEx-source-functions.R")

###############################################
## B - Generation of Legislative data tables ##
###############################################

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

WAgovs <- read.csv("WAgov.csv", header = TRUE, stringsAsFactors = FALSE)

for(term in biennium_list){
  master_sponsors <- add_row(master_sponsors, repId = paste("GOV", substr(term,3,4), sep = "_"), leg = length(seq(1889,substr(term,1,4),2)), firstName = WAgovs$firstName[WAgovs$biennium == term], lastName = WAgovs$lastName[WAgovs$biennium == term], type = "gov", district = "0", party = WAgovs$party[WAgovs$biennium == term], gender = WAgovs$gender[WAgovs$biennium == term], biennium = term)
  master_sponsors <- add_row(master_sponsors, repId = paste("PPL", substr(term,3,4), sep = "_"), leg = length(seq(1889,substr(term,1,4),2)), firstName = "The", lastName = "People", type = "ppl", district = "0", party = "0", gender = "0", biennium = term)
}

output_rep <- master_sponsors %>% filter(district != 0) %>% group_by(district, type, biennium) %>% filter(type == "sen" & length(district) > 1) %>% arrange(district)
output_sen <- master_sponsors %>% filter(district != 0) %>% group_by(district, type, biennium) %>% filter(type == "rep" & length(district) > 2) %>% arrange(district)
output_df <- bind_rows(output_rep, output_sen)

output_df$startDate <- NA
output_df$endDate <- NA

write.csv(output_df, "step1.csv")

WashEx <- dbConnect(RSQLite::SQLite(), "WashEx-db.sqlite")
dbWriteTable(WashEx, "Committees", master_comms, overwrite = TRUE)
dbWriteTable(WashEx, "Sponsors", master_sponsors, overwrite = TRUE)
dbDisconnect(WashEx)

dbDisconnect(WashEx)