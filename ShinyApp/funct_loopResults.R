
function.tabRes <- function(tabRes, row,colRemoved, badValues, nbcol, nbrow, res, tabCosts){
  
  moy <- res$moy
  sens <- res$sensitivity
  spe <- res$specificity
  resCost <- res$restab$cost
  costs <- tabCosts$Cost
  
  tabRes[row,"ColumnRemoved"] <- colRemoved
  tabRes[row,"Inconsistency(%)"] <- badValues
  tabRes[row,"NbCol"] <- nbcol
  tabRes[row,"NbRow"] <- nbrow
  tabRes[row,"Accuracy(%)"] <- round(mean(moy), digits = 2)
  tabRes[row,"Sensitivity(%)"] <- round(mean(sens), digits = 2)
  tabRes[row,"Specificity(%)"] <- round(mean(spe), digits = 2)
  tabRes[row,"Cost"] <- round(sum(resCost * costs) * 5 / nbrow, digits = 2)
  
  return(tabRes)
  
}


function.loopResults <- function(df, dfPerfect, matrix , tabCosts, target, ranges, fold){
  
  tabRes <- data.frame()
  row <- "Data Base - Initial"
  
  dfNa <- sort(function.barChartInconsistency(matrix), decreasing = TRUE)
  nomCol <- names(which(dfNa != 0))
  
  # Initial
  
  dfClean <- df
  res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold,ranges)
  div <- nrow(dfClean)
  
  tabRes <- function.tabRes(tabRes, row, 
                            "",
                            "",
                            ncol(dfClean),
                            div,
                            res,
                            tabCosts
  )
  
  print(row)
  
  row <- "Data Quality 0"
  
  # DQ with all columns
  
  rowRemove <- function.removeConsistency(df,matrix)
  dfClean <- df[!row.names(df)%in%rowRemove , ]
  
  res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold,ranges)
  div <- nrow(dfClean)
  
  tabRes <- function.tabRes(tabRes, row, 
                            "",
                            "",
                            ncol(dfClean),
                            div,
                            res,
                            tabCosts
  )
  
  
  l <- "DQ"
  n <- 0
  row <- paste(l,n)
  
  print(row)
  
  for (col in nomCol) {
    
    n <- n + 1
    row = paste(l,n)
    
    df <- df[,!names(df)%in%col]
    matrix <- matrix[,!names(matrix)%in%col]
    
    rowRemove <- function.removeConsistency(df,matrix)
    dfClean <- df[!row.names(df)%in%rowRemove , ]
    
    res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold,ranges)
    div <- nrow(dfClean)
    
    tabRes <- function.tabRes(tabRes, row, 
                              col,
                              dfNa[col],
                              ncol(dfClean),
                              div,
                              res,
                              tabCosts
    )
    
    print(row)
    
  }
  
  row <- "Data Base - Fixed"
  
  if (! is.null(dfPerfect)){
    dfClean <- dfPerfect
    
    res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold,ranges)
    div <- nrow(dfClean)
    
    
    tabRes <- function.tabRes(tabRes, row, 
                              "",
                              "",
                              ncol(dfClean),
                              div,
                              res,
                              tabCosts
    )
    
    print(row)
  }
  
  return(tabRes)
  
}