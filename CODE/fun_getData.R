### functions collect equity data and process them for further technical analysis
############## these functions assume the GENERIC functions "funDevelop.R" are already beed sourced
library(Quandl)
library(xts)
#####################BEGIN: functions to source data################################################################
GetData.Quandl <- function(dictData){
  Check.ExistVarInDF(dictData, c("nameR", "dataSource", "dataId", "type"))
  # Check.StopIf(length(setdiff(dictData$dataSource, c("ECB", "FRED", "EUROSTAT_DSD", "INSEE", "YAHOO")))!=0, 
  #              "data source must be one of 'c(ECB, FRED, EUROSTAT_DSD, INSEE, YAHOO)'")
  Check.StopIf(Size(dictData)[1]!=1, "Must be a single row of data frame!")
  eval(parse(text=paste0(
    "tmp <- tryCatch(Quandl('", gsub(" ", "", dictData$dataSource), 
    "/", gsub(" ", "", dictData$dataId), "'), 
    error = function(e) {}, warning = function(w) {})"
    )  
  ))
  Check.StopIf(class(tmp)[1]=="NULL", paste0(dictData$dataId, " cannot be found in the source of ", dictData$dataSource))
  Check.StopIf(Size(tmp)[2]!=2, paste0("Expect output should be 2 column df"))
  tmp[, 1] <- as.Date(tmp[, 1])
  tmp[, 2] <- as.numeric(tmp[, 2])
  names(tmp) <- c("date", "value") #gsub(" ", "", dictData$nameR)
  Check.Unique(tmp$date)
  if (sum(sort(tmp$date)-tmp$date !=0 ) > 0){
    tmp <- tmp[match(sort(tmp$date), tmp$date), ] 
  }
  tmp <- tmp[match(sort(tmp$date), tmp$date), ] 
  ## prepare meta data
  dictData$dateBegin <- min(tmp$date)
  dictData$dateEnd <- max(tmp$date)
  dictData$dateFetchData <- as.Date(Sys.time())
  return(list(dfData=tmp, dataInfo=dictData))
}

############## different package
GetData.Pdfetch <- function(dictData){
  Check.ExistVarInDF(dictData, c("nameR", "dataSource", "dataId", "type"))
  Check.StopIf(length(setdiff(dictData$dataSource, c("ECB", "FRED", "EUROSTAT_DSD", "INSEE", "YAHOO")))!=0, 
               "data source must be one of 'c(ECB, FRED, EUROSTAT_DSD, INSEE, YAHOO)'")
  Check.StopIf(Size(dictData)[1]!=1, "Must be a single row of data frame!")
  eval(parse(text=paste0(
    "tmp <- tryCatch(pdfetch::pdfetch_", dictData$dataSource, "('", dictData$dataId, "'), error = function(e) {}, warning = function(w) {})"
    )
  ))
  Check.StopIf(class(tmp)[1]=="NULL", paste0(dictData$dataId, "cannot be found in the source of ", dictData$dataSource))
  Check.StopIf(Size(tmp)[2]>1, paste0("Expect output should be a vector"))
  names(tmp) <- gsub(" ", "", dictData$nameR)
  Check.StopIf(sum(sort(index(tmp))-index(tmp)!=0)>0, "Expect dates are sorted by the function!")
  ##### expand data if type is price, rate or index
  tmpType <- dictData$type
  if (toupperNoSpace(dictData$type)%in%c("INDEX", "PRICE")){
    namNew <- gsub("index", "rate", gsub("price", "rate", gsub(" ", "", dictData$nameR), ignore.case=T))
    eval(parse(text=paste0(
      "tmp$", namNew, " <- c(NA, as.numeric(tail(tmp[, 1], dim(tmp)[1]-1))/as.numeric(head(tmp[, 1], dim(tmp)[1]-1))-1)"
      )
    ))
    tmpType <- c(tmpType, "rate")
  } else if (toupperNoSpace(dictData$type)=="RATE"){
    tmpVal <- as.numeric(tmp[, 1])
    if (sum(is.na(tmpVal))>0){
      warning(paste0("There are ", sum(sum(is.na(tmpVal))), " NA in ", names(tmp[, 1])), ", which are replace by 0.")
      tmpVal[is.na(tmpVal)] <- 0
    }
    if (toupperNoSpace(dictData$unit)=="%") {
      tmpVal <- cumprod(1+ tmpVal/100)
    } else{
      stop("Please expand code for unit of rates other than %, here!!!")
    }
    eval(parse(text=paste0(
      "tmp$", gsub("rate", "index", gsub(" ", "", dictData$nameR), ignore.case=T), " <- tmpVal"
      )
    ))
    tmpType <- c(tmpType, "index")
  } else {
    warning(paste0(dictData$type, " is not defined (i.e, either price, index or rate). No additional var generated!"))
  }
  ## prepare meta data
  dictData$dateBegin <- min(index(tmp))
  dictData$dateEnd <- max(index(tmp))
  eval(parse(text=paste0(
      "desc <- data.frame(cbind(", paste(rep("t(dictData)", Size(tmp)[2]), collapse=",") , ")[-1, ], stringsAsFactors=F)"
    )
  ))
  names(desc) <- names(tmp)
  row.names(desc) <- row.names(t(dictData))[-1]
  return(list(dfData=tmp, dataInfo=desc))
}
##############################################################################################END: FUNCTIONS TO SOURCE DATA##############
