############################################################################
# Function: RunSim - Simulate random portfolio returns over 1 year, n  times 
#

############################################################################

#' A Random Walk: A One Year Simulation
#' 
#' A function to simulate the random walk of a portfolio/stock/instrument given
#' an average return value, average risk (i.e. standard deviation of returns), and
#' the desired number of iterations to run. \code{RunSim} will simulate 1 year of 
#' returns.
#'
#' @details Simulation parameters are historical mu and sigma of portfolio based on
#'    historical mu and sigma of given stocks in our portfolio. Each stock is weighted 
#'    in the portfolio using the markowitz fundemental theory of portfolio optimization
#'
#'    By default, \code{RunSim} will average the results of all iterations of the 
#'    simulation for a given mu and sig and ultimately output 1-year random walks of 
#'    our portfolio (i.e. each day's generated return is actually an average of n 
#'    simulations of that day.
#' 
#'    If \code{fullsim = TRUE} then \code{RunSim} will further variate the calculated 
#'    mu and sigma according to a normal distribution to account for additional risk. The 
#'    number of new samples of \code{mu} and \sigma{sig} will default to 100 unless 
#'    specified in the optional \code{m} parameter
#' 
#' @section Warning:
#'    This function will run the simulation \code{n} times (function argument). However,
#'    if the argument \code{fullsim} is given as TRUE, the number of iterations is 
#'    \code{n * m (default is 100)}. Simulate responsibly...
#' 
#' @param mu A numeric value representing the average of returns for a financial 
#'    instrument (typically a portfolio)
#' @param sig A numeric value representing the standard deviation of returns (i.e. risk)
#' @param n A numeric value indicating how many iterations to run (n = 10: run 10 1-year
#'    simulations)
#' @param fullsim A boolean value indicating whether to run the full simulation, which 
#'    involves an additional layer that randomly samples the given \code{mu} and 
#'    \code{sig} and then runs another simulation.
#' @param m An optional numeric value specifying how many samples of \code{mu} and 
#'    \code{sig} to take (this determines how many times to repeat the initial 
#'    simulation)
#'
#' @export
#'
#' @examples
RunSim <- function(mu, sig, n=100, fullsim=FALSE, m=100){
  # ------------------- Random walk parameters
  h   <- 1/260            # dist of each step (1year = 261 trading days)
  s0  <- 1                 # since measuring returns, start at 100%
  
  s <- matrix(0, n, 261)
  s[,1] <- s0
  
  for(j in 2:261)
    s[,j] <- s[,j-1]*exp((mu-.5*sig^2)*h+sig*rnorm(n)*sqrt(h))
  
  apply(s,2,mean)         # return day average across n sim s
}