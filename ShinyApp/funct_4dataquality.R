
#--- Inconsistencies Bar Chart -----------------------------------------------------------------------------------------#

### Remove columns with too many inconsistencies

function.removeColumns <- function(resNas, df, pourcent, columnSelected){
  
  resColo <- 0
  
  for (i in names(resNas)){
    if (i == columnSelected){
      resColo[i] = i
    }
    else if (resNas[i] < pourcent){
      resColo[i] = i
    }
  }
  resColo <- resColo[-1]
  df <- df[,resColo]
  
  return(df)
  
}


### Data for bar chart inconsistency

function.barChartInconsistency <- function(matrixBool){
  res <- 0
  for (col in names(matrixBool)){
    column <- matrixBool[,col]
    res[col] <- round ( sum(column == 1) / length(column) * 100 , digits = 2 )
  }
  res <- res[-1]
  return(res)
}


#--- Consistency --------------------------------------------------------------------------------------------------------#


### 0/1 file inconsistency

function.matrixBooleanConsistency <- function(df,types,ranges){
  
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- data.frame(matrix (rep(0, n1*n2), n1, n2))
  names(a) <- names(df)
  rownames(a) <- rownames(df)
  
  for (col in names(df)) {
    typ <- types[,col]
    
    if (typ == "string"){
      df[,col] <- as.character(df[,col])
      rang <- ranges[,col]
    }
    else if (typ == "numeric" || typ == "integer") {
      df[,col] <- as.character(df[,col])
      df[,col] <- as.numeric(df[,col])
      rangMin <- ranges[1,col]
      rangMax <- ranges[2,col]
    }
    
    for (ligne in row.names(df)){
      
      val <- df[ligne,col]
      
      # TEST NUMERIC/INTEGER
      
      if (typ == "numeric" || typ == "integer") {
        if (! is.na(val)){
          if (val < rangMin || val > rangMax) {
            a[ligne,col] <- 1
          }
        }
        else a[ligne,col] <- 1
      }
      ###
      
      # TEST STRING
      
      else if (typ == "string") {
        if (! is.na(val)){
          if (val %in% rang && val != ""){}
          else a[ligne,col] <- 1
        }
        else a[ligne,col] <- 1
      }
      ###
    }
  }
  return(a)
}

function.removeConsistency <- function(df, a){
  rem <- 0
  for (row in row.names(a)) {
    if (1 %in% a[row,]) rem[row] = row
  }
  return(rem[-1])
}

function.nbInconsistenciesValues <- function(matrixBool){
  res <- 0
  for (col in names(matrixBool)){
    column <- matrixBool[,col]
    res[col] <- sum(column == 1)
  }
  res <- res[-1]
  return(sum(res))
}












