#' Building the Optimal Portfolio
#' 
#' DESCRIPTION
#' 
#' DETAILS
#'
#' @param tickers A character vector of valid stock tickers that will comprise
#'    the portfolio to optimize
#' @param print A boolean to specify whether to print portfolio details on the 
#'    R console. Default is FALSE.
#'
#' @return The default return value is an object of class StockReturns, though
#'    the user can use this function to optimize a portfolio in steps and each
#'    step can return a different return value (e.g. step 1 returns a list of 
#'    stock returns, step 2 returns a vector of weights, etc.). See details.
#' @export
#'
#' @examples
#' # EXAMPLES HERE
#' 
BuildPort <- function(tickers=NULL, print=F){
  
  # getReturns will return a custom class that we need later
  sRetObj   <- getReturns(tickers,
                          freq = "day",
                          start = Sys.Date()-365*5)
  
  # pull the returns out of sRetObj and save for user
  sReturns <- lapply(sRetObj$full, setDT)
  
  # Get stock model and feed into function to get optimal weights 
  portWeights <- optimalPort(stockModel(sRetObj))$X
  
  # Based on weights and historical data, get historical portfolio returns
  portReturns <- portReturn(stockModel(sRetObj), portWeights)
  
  if(print){
    cat("\n\n")
    
    cat("-------------------------------------------------------------\n")
    cat(" Stock Returns (first 6 rows of each)\n")
    cat("------------------------------------------\n")
    lapply(1:length(sReturns), function(i){
      print(paste0("Returns for: ", tickers[i]))
      cat("\n")
      print(head(sReturns[[i]]))
      cat("\n")
    })
    
    cat("\n------------------------------------------\n")
    cat(" Stock Weights\n")
    cat("------------------------------------------\n")
    print(portWeights)
    
    cat("\n------------------------------------------\n")
    cat(" Optimal Portfolio\n")
    cat("------------------------------------------\n")
    print(portReturns)
    cat("-------------------------------------------------------------\n\n")
  }
  
  return(portReturns)
}