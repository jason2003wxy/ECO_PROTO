rm(list = ls())
source("CODE/fun_generic.R")
source("CODE/fun_getData.R")
library(dplyr)
######################################################################
######## here we source some data for illustration purpose ###########
######################################################################
### import pre-defined data dictionary
dict.data.raw <- DataP.DfFactor2Character(read.csv("DATA/dictDataImport.csv"))
###### import economic variable
for (idx in c(1:2, 4:9)){
  tmp <- GetData.Quandl(dict.data.raw[idx, ])
  Check.StopIf(Size(tmp$dfData)[2]!=2, paste0("Expect output should be 2 column df"))
  if (exists("tmp.eco")){
    eval(parse(text=paste0(
      "tmp.eco$", gsub(" ", "", dict.data.raw$nameR[idx]), " <- tmp$dfData"
    )
    ))
  } else{
    eval(parse(text=paste0(
      "tmp.eco <- list(", gsub(" ", "", dict.data.raw$nameR[idx]), "=tmp$dfData)"
    )
    ))
  }
  if (exists("dict.data")){
    dict.data <- rbind(dict.data, tmp$dataInfo)
  } else{
    dict.data <- tmp$dataInfo
  }
}

##############################
##### the US recession period, FIXED
## https://research.stlouisfed.org/fred2/help-faq/ or http://www.nber.org/cycles/cyclesmain.html
#############################
dateUsRecess = read.table(textConnection(
  "monBeg, monEnd
1857-06-01, 1858-12-01
1860-10-01, 1861-06-01
1865-04-01, 1867-12-01
1869-06-01, 1870-12-01
1873-10-01, 1879-03-01
1882-03-01, 1885-05-01
1887-03-01, 1888-04-01
1890-07-01, 1891-05-01
1893-01-01, 1894-06-01
1895-12-01, 1897-06-01
1899-06-01, 1900-12-01
1902-09-01, 1904-08-01
1907-05-01, 1908-06-01
1910-01-01, 1912-01-01
1913-01-01, 1914-12-01
1918-08-01, 1919-03-01
1920-01-01, 1921-07-01
1923-05-01, 1924-07-01
1926-10-01, 1927-11-01
1929-08-01, 1933-03-01
1937-05-01, 1938-06-01
1945-02-01, 1945-10-01
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


##############################
##### CLEAN the collected RAW data
#############################
### prepare dates of RAW data with monthely, quarterly or annual 
lst.eco <- vector("list", length(names(tmp.eco)))
names(lst.eco) <- names(tmp.eco)
for (idx in 1:length(names(tmp.eco))){
  tmp <- tmp.eco[[idx]]
  Check.ExistVarInDF(tmp, c("date", "value"))
  ## remove all missing values 
  if(sum(is.na(tmp$value)) > 0) {
    tmp <- filter(tmp, (!is.na(value)))
  }
  ## adjust dates if the data is freq of monthely, quarterly or annual
  tmpYrMon <- format(tmp$date-1, "%Y-%m") ### "-1" to correct FRED data, 
  if (length(tmpYrMon) == length(unique(tmpYrMon))){  
    ### "YYYY-MM" is unique! Thus, data fre must be monthely, quarterly or annual
    tmp$date <- as.Date(paste0(tmpYrMon, "-", 
                       GetMonthEndDay(as.numeric(format(tmp$date-1, "%m")))))
  }                   ### "-1" to correct FRED data,
  lst.eco[[idx]] <- tmp
}
### remove tmps
rm(tmp, tmp.eco, tmpYrMon)

##############################
##### save to Rdata if necesary
#############################
# save(dateUsRecess, lst.eco, dict.data, file="DATA/usEcoData2Test.Rdata")