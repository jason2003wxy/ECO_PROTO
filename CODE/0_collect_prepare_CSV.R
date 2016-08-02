rm(list = ls())
source("CODE/fun_generic.R")
source("CODE/fun_getData.R")

######################################################################
######## here we source some data for illustration purpose ###########
######################################################################
### import pre-defined data dictionary
dict.data.raw <- DataP.DfFactor2Character(read.csv("DATA/dictDataImport.csv"))
###### import economic variable
for (idx in 4:6){
  tmp <- DataP.DfFactor2Character(read.csv(paste0("DATA/", gsub(" ", "", dict.data.raw$dataId[idx]), ".csv"), header=TRUE))
  Check.StopIf(Size(tmp)[2]!=2, paste0("Expect output should be 2 column df"))
  tmp[, 1] <- as.Date(tmp[, 1])
  tmp[, 2] <- as.numeric(tmp[, 2])
  names(tmp) <- c("date", "value")
  Check.Unique(tmp$date)
  if (!identical(sort(tmp$date), tmp$date)){
    tmp <- tmp[match(sort(tmp$date), tmp$date), ] 
  }
  if (exists("lst.eco")){
    eval(parse(text=paste0(
      "lst.eco$", gsub(" ", "", dict.data.raw$nameR[idx]), " <- tmp"
    )
    ))
  } else{
    eval(parse(text=paste0(
      "lst.eco <- list(", gsub(" ", "", dict.data.raw$nameR[idx]), "=tmp)"
    )
    ))
  }
  if (exists("dict.data")){
    dict.data <- rbind(dict.data, dict.data.raw[idx, ])
  } else{
    dict.data <- dict.data.raw[idx, ]
  }
}
dict.data <- DataP.DfFactor2Character(dict.data)
rm(tmp, idx)

##############################
##### the US recession period, FIXED
## https://research.stlouisfed.org/fred2/help-faq/ or http://www.nber.org/cycles/cyclesmain.html
#############################

dateUsRecess <- read.table(textConnection(
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
##### save to Rdata if necesary
#############################
# save(dateUsRecess, lst.eco, dict.data, file="usEcoData2Test.Rdata")