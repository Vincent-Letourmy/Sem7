
# Result of accuracy, sens or spe with confidence interval

function.statBoxWithConfInterval <- function(tab, stat, name, iconName, colorName){
  
  res <- tab
  mean <- mean(res)
  error <- qt(0.975,df=length(res)-1)*sd(res)/sqrt(length(res))
  
  left <- mean - error
  right <- mean + error
  
  stat <- round(stat, digits = 2)
  valueBox(
    value = paste(name," : ",stat,"%")
    ,paste('Confidence Interval :',round(left,digits = 1),"%  /  ",round(right,digits = 1),"%")
    ,icon = icon(iconName,lib='glyphicon')
    ,color = colorName)
  
}


# Bar chart CV accuracy

function.accuracyCVBarChart <- function(accuracyTab, accuracy , fold){
  
  if (!is.null(accuracy)) {
    plot_ly(
      x = c(1:fold),
      y = c(accuracyTab),
      name = "Bar Chart",
      type = "bar"
    )
  }
  
}


# Bar chart box

function.BarChartBox <- function(accuracy,accCVBar){
  if (!is.null(accuracy)) {
    fluidRow(
      box( width = 12,
           title = "Accuracy Bar Chart"
           ,status = "primary"
           ,solidHeader = TRUE 
           ,collapsible = TRUE
           ,collapsed = TRUE
           ,plotlyOutput(accCVBar)
      )
    )
  }
}


# Result of cost

function.costsResultsValue <- function(resultData){
  result <- round(resultData, digits = 0)
  valueBox(
    value = paste("Cost : ",result)
    ,paste('Cost :',result)
    ,icon = icon("menu-hamburger",lib='glyphicon')
    ,color = "green")
}


# Tab details costs

function.tabCostsTotal <- function(resultats, costs, div){
  nouv <- 0
  for (row in 1:length(resultats)) {
    nouv[row] <- resultats[row] * costs[row] * 5 / div
  }
  return(nouv)
}


# Details box

function.detailsBox <- function(tabDetail){
  renderUI({
    box( 
      title = "Details of costs"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,tableOutput(tabDetail)
    )
  })
}




















