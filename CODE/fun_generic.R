########################################################################################################################################################################
###################################################################### CHECK function development#################################################################
###################### check functions
Check.InputCoeff.VAR <- function(A){
  m <- dim(A)[1]
  p <- (dim(A)[2])/m
  At <- matrix(nrow = m * p, ncol = m * p)
  if (p == 1) {
    At <- A
  }
  else {
    At[seq(1, m), seq(1, m * p)] <- A
    At[seq(m + 1, m * p), seq(1, m * p - m)] <- diag(1, (p - 1) * m)
    At[seq(m + 1, m * p), seq(m * p - m + 1, m * p)] <- 0
  }
  l <- (eigen(At, only.values = TRUE))$values
  if ((any(Mod(l) > 1))) {
    stop("VAR model coefficient matrix leads to a unstable AR model")
  }
} 

Check.AllNA <- function(inp){
  ## only works for vector!
  if (sum(is.na(inp))==length(inp)){
    stop("The input vector is all NA")
  }
}

Check.IsScalar <- function(inp){
  #scalar <=> inp is a vector AND length is one#
  if(!( is.vector(inp) & length(inp)==1)){
    stop("inp is not a scalar")
  }
}

Check.StopIf <- function(conditionTrue,stopMessage){  
  Check.IsScalar(conditionTrue)  
  if(!is.logical(conditionTrue)){
    stop("'conditionTrue' input must be logical varaible")
  }
  if(!is.character(stopMessage)){
    stop("'stopMessage' input must be string")
  }
  if (conditionTrue){
    stop(stopMessage)
  }
}

Check.Unique <- function(inpVec){
  if (length(inpVec) != length(unique(inpVec))){
    stop("The input vector is not unique!")
  }
}

Check.ExistVarInDF <- function(inpDf,varNames){
  Check.StopIf(!(is.data.frame(inpDf)|is.matrix(inpDf)|sum(class(inpDf)%in%"xts")>0),"Must be df, matrix or xts")
  Check.StopIf(!(is.character(varNames)&is.vector(varNames)),"Must be character vector")
  Check.StopIf(length(unique(varNames))!=length(varNames),"varNames Must be unique")
  if (sum(names(inpDf) %in% varNames)!=length(varNames)){
    stop(paste0("These names cannot be found in inpDf ",setdiff(varNames,names(inpDf))))		 
  }  
}

Check.VerifyDictStr <- function(dictStra){
  Check.StopIf(!is.data.frame(dictStra),"Must be data frame!")
  Check.ExistVarInDF(dictStra,c("state","asset","weight"))
  uniState <- sort(unique(dictStra$state))
  uniAsset <- sort(unique(dictStra$asset))
  for (idx in 1:length(uniState)) {
    Check.StopIf(!identical(sort(unique(dictStra$asset[dictStra$state==uniState[idx]])),uniAsset),
                 paste0("In state of ",uniState[idx]," all asset class should be defined!"))
    Check.StopIf(sum(dictStra$weight[dictStra$state==uniState[idx]])>1,"Weights for each state should be less than 1!")
  }
}
##############################################END: check functions#########

#################### Begin: DATA PREPARE functions###############################
DataP.GetDfByCommonDict <- function(rawVal,rawVarName,dict){
  Check.StopIf(!is.character(rawVarName),"rawVarName must be character.")
  Check.StopIf(!is.data.frame(rawVal),"rawVal must be data frame.")
  Check.StopIf(!is.data.frame(dict),"dict must be data frame.")
  Check.ExistVarInDF(dict,"varXls")
  Check.ExistVarInDF(dict,"varR")
  ## check the name vector has the same size as the column of the data matrix
  Check.StopIf(length(rawVarName) != dim(rawVal)[2],"Mismatch between col names and #col in data input")
  ## check for empty col name vector
  Check.StopIf(sum(rawVarName == "") >0,"Col name vector has missing values!")
  rawVarName <- toupperNoSpace(rawVarName)
  dict$varXls <- toupperNoSpace(dict$varXls)
  Check.StopIf(identical(intersect(rawVarName,dict$varXls),character(0)),"One of rawVarName must present in dictNames!")
  ## names matching
  namComm <- intersect(rawVarName,dict$varXls)
  mapData2Nam <- match(namComm,rawVarName)
  res <- rawVal[,mapData2Nam]
  mapDict2Nam <- match(namComm,dict$varXls)
  names(res) <- dict$varR[mapDict2Nam]
  return(list(data=res,desc=dict[mapDict2Nam,]))
}

DataP.DfFactor2Character <- function(dfInp){  
  ## For a given data frame with factor class data fields, this function converts
  # the factor class to character class  
  if(!is.data.frame(dfInp)){
    stop("Input must be data frame!")
  }
  nCol <- dim(dfInp)[2]
  dataName <- names(dfInp)
  for(idx in 1:nCol){
    if(is.factor(dfInp[,idx])){
      dfInp[,idx] <- as.character(dfInp[,idx])
      print(paste0(dataName[idx]," has been converted to character class"))
    }
  }
  return(dfInp)
}

DataP.GetFieldsForUniId <- function(idDuplicated,ref2Judge,mat2Pick,condit2Pick){
  ## for each unique id of idDuplicated (i.e., ) pick a row of the associated mat2Pick according to min/max of the associated ref2Judge values
  ## this function can be used as max EAD rule!!! DataP.GetFieldsForUniId(idDuplicated,EAD,mat2Pick,"max")
  if (class(idDuplicated)!="Date" & class(mat2Pick)!="Date"){
    Check.StopIf(!(is.vector(idDuplicated)&is.vector(ref2Judge)),"Both ref2Judge and idDuplicated must be vector") 
  }
  Check.StopIf(!(is.vector(mat2Pick)|is.matrix(mat2Pick)|is.data.frame(mat2Pick)),"mat2Pick must be vector, matrix or data.frame")
  Check.StopIf(!(length(idDuplicated)==length(ref2Judge) & length(idDuplicated)==Size(mat2Pick)[1]),
               "ref2Judge, idDuplicated and mat2Pick must have same Nr rows")
  Check.IsScalar(condit2Pick)
  Check.StopIf(sum(toupperNoSpace(condit2Pick)%in%c("MIN","MAX"))!=1,"condit2Pick must be either MIN or MAX")
  
  uniId <- unique(idDuplicated)
  if (length(uniId)==length(idDuplicated)){
    print("The input idDuplicated is unique! Thus output is equal to mat2Pick!")
    return(list(idUnique=uniId,matPicked=mat2Pick))
  } else {   ### normal non-unique cases
    #### initialized the values of res
    ixSel <- idDuplicated==uniId[1]
    tmpSortIx <- match(sort(ref2Judge[ixSel]),ref2Judge[ixSel])
    ### logical values will be same over the loop
    logiConOfMin <- toupperNoSpace(condit2Pick)=="MIN"
    logiConOfMax <- toupperNoSpace(condit2Pick)=="MAX"
    logiTrueVector <- is.vector(mat2Pick)
    logiTrueMat <- is.data.frame(mat2Pick)|| is.matrix(mat2Pick)
    # deceide which row of the corresponding mat2Pick to choose
    if (logiConOfMin){
      posRow2Select <- tmpSortIx[1]
    } else if (logiConOfMax) {
      posRow2Select <- tail(tmpSortIx,1)
    } else {
      stop("value of condit2Pick should be either MIN or MAX!!!")
    }
    # treatment of mat2Pick given vecto or matrix/dataFrame
    if (logiTrueVector) {
      res <- mat2Pick[ixSel][posRow2Select]
    } else if (logiTrueMat) {
      res <- mat2Pick[ixSel,][posRow2Select,]
    } else {
      stop("Here: mat2Pick must be vector, matrix or data.frame")
    }
    if (length(uniId)>1) {
      for (idx in 2:length(uniId)){
        ixSel <- idDuplicated==uniId[idx]
        tmpSortIx <- match(sort(ref2Judge[ixSel]),ref2Judge[ixSel])
        #################BEGIN:  same treatment as initialization part###
        # deceide which row of the corresponding mat2Pick to choose
        if (logiConOfMin){
          posRow2Select <- tmpSortIx[1]
        } else if (logiConOfMax) {
          posRow2Select <- tail(tmpSortIx,1)
        } else {
          stop("value of condit2Pick should be either MIN or MAX!!!")
        }
        #############END###
        # treatment of mat2Pick given vecto or matrix/dataFrame
        if (logiTrueVector) {
          res <- c(res,mat2Pick[ixSel][posRow2Select])
        } else if (logiTrueMat) {
          res <- rbind(res,mat2Pick[ixSel,][posRow2Select,])
        } else {
          stop("Here: mat2Pick must be vector, matrix or data.frame")
        }
      }
    }
    Check.StopIf(length(uniId)!=Size(res)[1],"Failed quality control")
    return(list(idUnique=uniId,matPicked=res))
  }
}

DataP.PrepXts <- function(dfEq,order.by,fieldOut=names(dfEq),rm.na=TRUE,...){
  Check.StopIf(!(is.data.frame(dfEq)||is.matrix(dfEq)||is.vector(dfEq)),"dfEq should be in these classes, otherwise expand!")
  Check.StopIf(class(order.by)!="Date","Must be class of Date!")
  Check.Unique(order.by)
  Check.StopIf(!identical(order.by,sort(order.by)),"Must be sorted")
  
  xtsOut <- xts(dfEq,order.by=order.by,stringsAsFactors=FALSE,...)
  if(!identical(names(xtsOut),fieldOut)){
    Check.Unique(fieldOut)
    Check.StopIf(!(is.character(dfEq)&identical(setdiff(fieldOut,names(xtsOut)),character(0))),"Must be string and be subset of names(xtsOut)")
    xtsOut <- xtsOut[,match(fieldOut,names(xtsOut))]
  }
  if (rm.na){
    ixExcl <- apply(is.na(xtsOut),1,sum)>0
    if(sum(ixExcl)>0){
      print(paste0("There are ",sum(ixExcl)," rows contain at least one NA. These rows are REMOVED."))
      if (sum(ixExcl)==length(ixExcl)){
        stop("Every row of output contains a NA! Please consider set rm.na=FALSE! ")
      } else{
        xtsOut <- xtsOut[!ixExcl,]
      }
    }
  }
  return(xtsOut)
} 

DataP.xts2DataFrame <- function(xtsInp){
  Check.StopIf(sum(class(xtsInp)%in%"xts")==0,"xtsInp must be class of xts")
  if(sum(names(xtsInp)=="date")>0){
    print("xtsInp contains filed of date. However, which is replace by index(xtsInp)")
  } else {
    print("A filed named by date is created for output df")
  }
  res <- data.frame(xtsInp[,names(xtsInp)!="date"],stringsAsFactors=FALSE)
  res$date <- index(xtsInp)
  return(res)
}

DataP.GetPosBegEndTrueGroup <- function(vecLogical){
  Check.StopIf(!(is.vector(vecLogical)&class(vecLogical)=="logical"),"Must be logical vector!")
  if(sum(is.na(vecLogical))>0){
    print(paste0("There are ",sum(is.na(vecLogical))," NA in the input, which are replaced by FALSE!"))
    vecLogical[is.na(vecLogical)] <- FALSE
  }
  Check.StopIf(sum(vecLogical==FALSE)==length(vecLogical),"vecLogical only have FALSE! No need for this function")
  
  res <- data.frame(beg=0,end=0,stringsAsFactors=FALSE)
  idx <- 1
  for (ct in 1:length(vecLogical)){
    pBuy <- which(vecLogical[idx:length(vecLogical)])[1] + idx-1
    if(is.na(pBuy)) break
    pSell <- which(!vecLogical[pBuy:length(vecLogical)])[1]+pBuy-1
    idx <- pSell
    res <- rbind(res,c(pBuy,pSell))
    if(is.na(pSell)) break
  }  
  res <- res[-1,]
  res$end <- res$end -1
  if (is.na(tail(res$end,1))){
    res$end[length(res$end)] <- length(vecLogical)
  }
  Check.StopIf(sum(is.na(res))>0,"Do not expect any NA")
  return(res)
}
##############################################END: data prepare functions#########


####################################################################BEGIN: GENERIC functions############################################
toUpperNoSpace <- function(stringInput){ return(toupperNoSpace(stringInput)) }
toupperNoSpace <- function(stringInput){
  Check.StopIf(!is.character(stringInput),"The input must be a string vector")
  return( toupper(gsub(" ","",stringInput)) )
}

toNoSpace <- function(stringInput){
  Check.StopIf(!is.character(stringInput),"The input must be a string vector")
  return(gsub(" ","",stringInput) )
}

Out.GetPercentageString <- function(vecNumeric,nrDigits){
  Check.IsScalar(nrDigits)
  Check.StopIf(!is.numeric(nrDigits),"nrDigits must be numeric!")
  Check.StopIf(!(is.numeric(vecNumeric)&is.vector(vecNumeric)),"vecNumeric must be numeric and a vector!")
  return(paste0(round(vecNumeric*100, digits=nrDigits),"%"))
}

Size <- function(matOrVec){
  if (is.vector(matOrVec)){
    res <- length(matOrVec)
  } else if (is.matrix(matOrVec)|is.data.frame(matOrVec)|is.array(matOrVec)) {
    res <- dim(matOrVec)
  } else {
    stop("size function can only handle data frame, matrix, vector or array!")
  }
  return(res)
}

Match.Robust <- function(vec2Match,refUsed){
  Check.StopIf(!(is.vector(vec2Match)&is.vector(refUsed)),"Both inputs should be vector")
  Check.Unique(refUsed)
  if(length(intersect(vec2Match,refUsed))==0){
    stop("vec2Match and refUsed do not share any common value. It's useless to apply this function!")
  }
  tmp <- setdiff(vec2Match,refUsed)
  if(length(tmp)!=0){
    print(paste0("Some values of vec2Match cannot be found in refUsed: ",paste(tmp,collapse = ", ")))
  }
  return(match(vec2Match,refUsed))
}

GetMonthEndDay <- function(month.num){
  Check.StopIf(!is.numeric(month.num), "month.num must be numeric")
  Check.StopIf(length(setdiff(month.num, 1:12)) != 0, "month.num must be in 1:12")
  res <- rep(NA, length(month.num))
  res[month.num %in% c(seq(1, 7, by=2), seq(8, 12, by=2))] <- 31
  res[month.num %in% c(4, 6, 9, 11)] <- 30
  res[month.num == 2] <- 28
  return(res)
}

TestsStationaryPval <- function(inp.vec.mat, ...) {
  ### checks
  Check.StopIf(!(length(Size(inp.vec.mat)) %in% c(1,2)), "inp.vec.mat must be either vector or matrix")
  ### perform kpss.test with trend
  if (length(Size(inp.vec.mat)) == 1){
    tmp.adf <- as.character(tseries::adf.test(as.numeric(inp.vec.mat), "stationary", ...))
    tmp.pp <- as.character(PP.test(as.numeric(inp.vec.mat)))
    tmp.kpss.trend <- as.character(tseries::kpss.test(as.numeric(inp.vec.mat), null="Trend"))
    tmp.kpss.level <- as.character(tseries::kpss.test(as.numeric(inp.vec.mat), null="Level"))
    res <- data.frame(pVal.ADF=as.numeric(tmp.adf[4]), pVal.PP=as.numeric(tmp.pp[3]), 
                      pVal.KPSS.trend=as.numeric(tmp.kpss.trend[3]), 
                      pVal.KPSS.level=as.numeric(tmp.kpss.level[3]), stringsAsFactors=FALSE)
  } else{
    tmp.adf <- apply(inp.vec.mat, 2, function(x) as.character(tseries::adf.test(as.numeric(x), "stationary", ...)))
    tmp.pp <- apply(inp.vec.mat, 2, function(x) as.character(PP.test(as.numeric(x))))
    tmp.kpss.trend <- apply(inp.vec.mat, 2, function(x) as.character(tseries::kpss.test(as.numeric(x), null="Trend")))
    tmp.kpss.level <- apply(inp.vec.mat, 2, function(x) as.character(tseries::kpss.test(as.numeric(x), null="Level")))
    res <- cbind(tmp.adf[4, ], tmp.pp[3, ], tmp.kpss.trend[3, ], tmp.kpss.level[3, ])
    res <- data.frame(apply(res, 2, as.numeric), stringsAsFactors=FALSE)
    names(res) <- c("pVal.ADF", "pVal.PP", "pVal.KPSS.trend", "pVal.KPSS.level")
  }
  return(res)
}

Sim_VAR_N_Period <- function (w, A, C, N){
  m = dim(A)[1]
  p = (dim(A)[2])/m
  nd = 1000
  # U = chol(C)
  noisevec = MASS::mvrnorm(nd + N, rep(0, m), C)
  matw = rep(1, nd + N) %*% t(w)
  vec = noisevec + matw
  if (any(w != 0)) {
    B = diag(1, m)
    for (j in seq(1, p)) {
      B = B - A[, seq(m * j - m + 1, j * m)]
    }
    mproc = as.vector(solve(B) %*% w)
    xi = (matrix(1, nrow = p, ncol = 1)) %*% mproc
  }
  else {
    xi = matrix(nrow = p, ncol = m)
    xi[, ] = 0
  }
  u = matrix(nrow = p + nd + N, ncol = m)
  u[seq(1, p), seq(1, m)] = xi
  u[seq(p + 1, p + nd + N), seq(1, m)] = 0
  Atr = t(A)
  x = matrix(ncol = m, nrow = p)
  for (k in seq(p + 1, nd + N + p)) {
    for (j in seq(1, p)) {
      x[j, ] = u[k - j, ] %*% Atr[seq(m * j - m + 1, m * 
                                        j), ]
    }
    u[k, ] = as.matrix(apply(x, 2, sum) + vec[k - p, ])
  }
  v = u[seq(nd + p + 1, nd + p + N), ]
  simulated = data.frame(v[, seq(1, m)])
  return(simulated)
}
###################################################################################################END: GENERIC functions###############
