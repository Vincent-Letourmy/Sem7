library(plotly)

source("ShinyApp/funct_4dataquality.R")
source("ShinyApp/funct_5CVNaiveBayes.R")

df <- read.csv("CSV_NoProblem/MyDataDQwithProblems.csv", header = TRUE, sep = ";")
df <- function.as_factor(df)

dfPerfect <- read.csv("CSV_NoProblem/MyDataDQ.csv", header = TRUE, sep = ",")
dfPerfect <- function.as_factor(dfPerfect)

types <- read.csv("CSV_NoProblem/TypesDataDQ.csv", header = TRUE, sep = ";")
ranges <- read.csv("CSV_NoProblem/RangesDataDQ.csv", header = TRUE, sep = ";")
matrix <- function.matrixBooleanConsistency(df,types,ranges)

target <- "Biopsy"
otherTargets <- c("Citology","Schiller","Hinselmann")
df <- df[,!names(df)%in%otherTargets]
dfPerfect <- dfPerfect[,!names(dfPerfect)%in%otherTargets]
matrix <- matrix[,!names(matrix)%in%otherTargets]

dfNa <- function.barChartInconsistency(matrix)
dfNa <- sort(dfNa, decreasing = TRUE)
nomCol <- names(which(dfNa != 0))

tabRes <- data.frame()
tabCosts <- function.tabNaiveBayes(df,target)
tabCosts[2,"Cost"] <- 50
tabCosts[3,"Cost"] <- 100
row <- 1

# Initial

dfClean <- df
res <- function.CVNaiveBayes(dfClean,target,tabCosts,10,ranges)
div <- nrow(dfClean)

tabRes[row,"ColumnRemoved"] <- "(Initial DataBase)"
tabRes[row,"%ofBadValues"] <- ""
tabRes[row,"NbCol"] <- ncol(dfClean)
tabRes[row,"NbRow"] <- div
tabRes[row,"Accuracy"] <- round(mean(res$moy), digits = 2)
tabRes[row,"Sensitivity"] <- round(mean(res$sensitivity), digits = 2)
tabRes[row,"Specificity"] <- round(mean(res$specificity), digits = 2)
tabRes[row,"Cost"] <- round(sum(res$restab$cost * tabCosts$Cost) * 5 / div, digits = 2)


row <- 2

# DQ with all columns

rowRemove <- function.removeConsistency(df,matrix)
dfClean <- df[!row.names(df)%in%rowRemove , ]

res <- function.CVNaiveBayes(dfClean,target,tabCosts,10,ranges)
div <- nrow(dfClean)

tabRes[row,"ColumnRemoved"] <- "(Dataquality, no column removed)"
tabRes[row,"%ofBadValues"] <- ""
tabRes[row,"NbCol"] <- ncol(dfClean)
tabRes[row,"NbRow"] <- div
tabRes[row,"Accuracy"] <- round(mean(res$moy), digits = 2)
tabRes[row,"Sensitivity"] <- round(mean(res$sensitivity), digits = 2)
tabRes[row,"Specificity"] <- round(mean(res$specificity), digits = 2)
tabRes[row,"Cost"] <- round(sum(res$restab$cost * tabCosts$Cost) * 5 / div, digits = 2)

for (col in nomCol) {
  
  row = row + 1
  df <- df[,!names(df)%in%col]
  matrix <- matrix[,!names(matrix)%in%col]
  
  rowRemove <- function.removeConsistency(df,matrix)
  dfClean <- df[!row.names(df)%in%rowRemove , ]
  
  res <- function.CVNaiveBayes(dfClean,target,tabCosts,10,ranges)
  div <- nrow(dfClean)

  tabRes[row,"ColumnRemoved"] <- col
  tabRes[row,"%ofBadValues"] <- dfNa[col]
  tabRes[row,"NbCol"] <- ncol(dfClean)
  tabRes[row,"NbRow"] <- div
  tabRes[row,"Accuracy"] <- round(mean(res$moy), digits = 2)
  tabRes[row,"Sensitivity"] <- round(mean(res$sensitivity), digits = 2)
  tabRes[row,"Specificity"] <- round(mean(res$specificity), digits = 2)
  tabRes[row,"Cost"] <- round(sum(res$restab$cost * tabCosts$Cost) * 5 / div, digits = 2)
  
}

row = row + 1
dfClean <- dfPerfect

res <- function.CVNaiveBayes(dfClean,target,tabCosts,10,ranges)
div <- nrow(dfClean)

tabRes[row,"ColumnRemoved"] <- "(Fixed database)"
tabRes[row,"%ofBadValues"] <- ""
tabRes[row,"NbCol"] <- ncol(dfClean)
tabRes[row,"NbRow"] <- div
tabRes[row,"Accuracy"] <- round(mean(res$moy), digits = 2)
tabRes[row,"Sensitivity"] <- round(mean(res$sensitivity), digits = 2)
tabRes[row,"Specificity"] <- round(mean(res$specificity), digits = 2)
tabRes[row,"Cost"] <- round(sum(res$restab$cost * tabCosts$Cost) * 5 / div, digits = 2)

  
print(tabRes)

x <- tabRes$ColumnRemoved
plot_ly(tabRes,x = factor(x,levels = x), y = ~tabRes$Accuracy, type = "scatter", mode = "lines")
plot_ly(tabRes,x = factor(x,levels = x), y = ~tabRes$Sensitivity, type = "scatter", mode = "lines")
plot_ly(tabRes,x = factor(x,levels = x), y = ~tabRes$Cost, type = "scatter", mode = "lines")







