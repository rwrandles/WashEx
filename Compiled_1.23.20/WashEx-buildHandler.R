## Load libraries here ##
library(mailR)
##

## Other important stuff ##
errCodes <- read.delim("error_codes.txt", header = FALSE, stringsAsFactors = FALSE, colClasses = "character")
##

throwCode <- function(code){
  err <- formatC(code, width = 3, format = "d", flag = "0")
  if(err == "000"){
    if(cfg$base$refEmail != "NA" & cfg$base$password != "NA"){
      send.mail(from = cfg$base$refEmail,
                to = cfg$base$refEmail,
                subject = "WashEx Nightly Build Complete",
                body = paste(timestamp(), "\n",
                             "WashEx nightly build completed successfully", sep = ""),
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = user, passwd = cfg$base$password, ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
    }
    else{
      timestamp()
      print("WashEx nightly build completed successfully")
    }
  }
  else if(substr(err,1,1) != "9"){
    
    ## Error has occurred
    ## Edit status flag here
    ## Send email here
    
    errMessage <- errCodes[which(errCodes$V1 == err),2] %>%
      gsub("%n", "\n", .) %>%
      gsub("%b%", now, .) %>%
      gsub("%y%", substr(today(),1,4), .)
    
    if(cfg$base$refEmail != "NA" & cfg$base$password != "NA"){
      send.mail(from = cfg$base$refEmail,
                to = cfg$base$refEmail,
                subject = "WashEx Nightly Build Error",
                body = paste(timestamp(), "\n",
                             "The following error occurred in the WashEx nightly build", "\n",
                             "Error code ", err, ": ", errMessage, sep = ""),
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = user, passwd = cfg$base$password, ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
    }
    
    stop(cat(paste(errMessage, "\n", sep = "")), paste("Code", err))
  }
  else{
    
    ## Warning (not critical)
    ## Do not edit status flag
    ## Send email maybe?
    
    errMessage <- errCodes[which(errCodes$V1 == err),2] %>%
      gsub("%n", "\n", .) %>%
      gsub("%b%", now, .) %>%
      gsub("%y%", substr(today(),1,4), .)
    
    if(cfg$base$refEmail != "NA" & cfg$base$password != "NA"){
      send.mail(from = cfg$base$refEmail,
                to = cfg$base$refEmail,
                subject = "WashEx Nightly Build Warning",
                body = paste(timestamp(), "\n",
                             "The following warning occurred in the WashEx nightly build", "\n",
                             "Warning ", err, ": ", errMessage, sep = ""),
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = user, passwd = cfg$base$password, ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
    }
    
    cat(paste(gsub("%n", "\n", errCodes[which(errCodes$V1 ==
                                                err),2]), "\n", sep = ""))
  }

}
