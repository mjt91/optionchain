##
## Script to save option price data from yahoo.finance
##

##
# author: Marius T.
# mail: marius.theiss@udo.edu
##


## load libs
if(!require(quantmod)){
  install.packages("quantmod")
  library(quantmod)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

# Maybe redundant since tibble is part of dplyr
if(!require(tibble)){
  install.packages("tibble")
  library(tibble)
}


## main function
saveOptionChain <- function(tic){
  
  # Function to save all Calls
  saveCalls <- function(Quantdata){
    
    # create empty df for saving
    df.calls <- tibble()
    
    # loop through all expiry dates
    for (name in names(QuantData)){
      
      # exception handling
      if (is.null(QuantData[[name]]$calls)) {
        message(paste("Call chain empty for", tic, "on", name))
        break 
      }
      
      # transform datetime
      expiryDate <- as.Date(x = name, format = "%b.%d.%Y")
      retrieveTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # temporary df in tibble format
      df.tmp <- tibble(symbol = tic,
                       type = "Call",
                       expiry = expiryDate,
                       strike = QuantData[[name]]$calls$Strike,
                       last = QuantData[[name]]$calls$Last,
                       chng = QuantData[[name]]$calls$Chg,
                       bid = QuantData[[name]]$calls$Bid,
                       ask = QuantData[[name]]$calls$Ask,
                       vol = QuantData[[name]]$calls$Vol,
                       open.interest = QuantData[[name]]$calls$OI,
                       retrived = retrieveTime
                       )
      
      if (name == names(QuantData)[1]){
        df.calls <- df.tmp
      } else {
        df.calls <- full_join(df.calls, df.tmp, by = c("symbol",
                                                       "type",
                                                       "expiry",
                                                       "strike",
                                                       "last",
                                                       "chng",
                                                       "bid",
                                                       "ask",
                                                       "vol",
                                                       "open.interest",
                                                       "retrived"
        ))
      }
      
    }
    
    # save to csv
    today <- format(Sys.Date(), "%Y%m%d")
    file_name = paste(tic, ".Calls_", today, ".csv", sep = "")
    write.table(x = df.calls, file = file_name, sep = ",", dec = ".")
    
    # goodbye message
    message(paste("Calls for", tic, "done.", "Saved as", file_name))

  }
  
  
  # Function to save all Puts
  savePuts <- function(Quantdata){
    
    # create empty df for saving
    df.puts <- tibble()
    
    # loop through all expiry dates
    for (name in names(QuantData)){
      
      # exception handling
      if (is.null(QuantData[[name]]$puts)) {
        message(paste("Put chain empty for", tic, "on", name))
        break 
      }
      
      # transform datetime
      expiryDate <- as.Date(x = name, format = "%b.%d.%Y")
      retrieveTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # temporary df in tibble format
      df.tmp <- tibble(symbol = tic,
                       type = "Put",
                       expiry = expiryDate,
                       strike = QuantData[[name]]$puts$Strike,
                       last = QuantData[[name]]$puts$Last,
                       chng = QuantData[[name]]$puts$Chg,
                       bid = QuantData[[name]]$puts$Bid,
                       ask = QuantData[[name]]$puts$Ask,
                       vol = QuantData[[name]]$puts$Vol,
                       open.interest = QuantData[[name]]$puts$OI,
                       retrieved = retrieveTime
      )
      

      if (name == names(QuantData)[1]){
        df.puts <- df.tmp
      } else {
        df.puts <- full_join(df.puts, df.tmp, by = c("symbol",
                                                      "type",
                                                      "expiry",
                                                      "strike",
                                                      "last",
                                                      "chng",
                                                      "bid",
                                                      "ask",
                                                      "vol",
                                                      "open.interest",
                                                      "retrieved"
        ))
      }
      
    }
    
    # save to csv
    today <- format(Sys.Date(), "%Y%m%d")
    file_name = paste(tic, ".Puts_", today, ".csv", sep = "")
    write.table(x = df.puts, file = file_name, sep = ",", dec = ".")
    
    # goodbye message
    message(paste("Puts for", tic, "done.", "Saved as", file_name))

  }
  
  # get data
  QuantData <- quantmod::getOptionChain(Symbols = tic, scr = "yahoo", Exp = NULL)
  
  # call nested save functions
  saveCalls(Quantdata = QuantData)
  savePuts(Quantdata = QuantData)
  
}

