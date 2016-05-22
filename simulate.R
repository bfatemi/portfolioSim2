#This code requires the following two packages
library(stockPortfolio)
library(googleVis)
library(data.table)

options(scipen = 10000)

#######################################################################
# Efficient Portfolio Parameters
#######################################################################

# Get stock returns 
stockReturns   <- getReturns(ticker = c("MSFT", "C", "BAC"),
                             freq = "day",
                             start = Sys.Date()-365*5)

# Get stock model and feed into function to get optimal weights 
opPort_Weights <- optimalPort(stockModel(stockReturns))$X

# Based on weights and historical data, get historical portfolio returns
portReturns <- portReturn(stockModel(s), opPort_Weights)

# Get historical average return and risk of portfolio
mu  <- portReturns$R
sig <- sqrt(portReturns$V)

# number of iterations
n   <- 100  

############################################################################
# Function: RunSim - Simulate random portfolio returns over 1 year, n  times 
#
# Simulation parameters are historical mu and sigma of portfolio based on
# historical mu and sigma of given stocks in our portfolio, weighted by 
# markowitz fundemental theory to derive optimal weights (weights that land
# our portfolio on the efficient frontier
#
# Function will average across all iteration of the simulation for a given mu and sigma,
# so the function output will 1 year random walk of portfolio where each day's
# generated return is actually an average of n simulations of that day.
#
# But we will further variate the calculated mu and sigma to account for 
# additional risk, so we will run the function another m times
############################################################################

RunSim <- function(vec){
  # ------------------- Random walk parameters
  mu  <- vec[1]           # first position of input vector is mu for this run
  sig <- vec[2]           # second pos is sig for this run
  h   <- 1/260            # dist of each step (1year = 261 trading days)
  s0  <- 1                # since measuring returns, start at 100%
  
  s <- matrix(0, n, 261)
  s[,1] <- s0
  
  for(j in 2:261)
    s[,j] <- s[,j-1]*exp((mu-.5*sig^2)*h+sig*rnorm(n)*sqrt(h))
  
  apply(s,2,mean)         # return day average across n sim s
}

#--------------------------------------------------------------------------
# For the hist mu and sig of our portfolio, we will randomly  
# sample them with uniform distribution ranging from .8 to 1.2
# of derived value
#--------------------------------------------------------------------------

m <- 10
dt.simArgs <- data.table(x = runif(m,  mu*.8,  mu*1.2),
                         y = runif(m, sig*.8, sig*1.2))
m <- 10
dt.simArgs <- data.table(x = runif(m,  mu*.8,  mu*1.2),
                         y = runif(m, sig*.8, sig*1.2))

#--------------------------------------------------------------------------
# Two step process:
#       1. Run sim function m times by feeding each row by row our simArgs 
#         data.table where first element is mu, and second is sig for 
#         that sim run
#       2. The return result will be a 261 X M matrix where each column is 
#          one run. Finally, we will average accross all runs for each 
#          mu and sig, and end up with a 262 X 1 vector for that mu, sigma
#--------------------------------------------------------------------------
simRun_Res <- apply(dt.simArgs,    MARGIN = 1, RunSim)
aveAll_Res <- apply(simRun_Res, MARGIN = 1, mean)
View(simRun_Res)

#--------------------------------------------------------------------------
# Get labels and information for plotting
#--------------------------------------------------------------------------
dates       <- seq(Sys.Date(), by=1, len=365)
tradingDays <- dates[!wday(dates) %in% c(1,7)] 
dayOfWeek   <- weekdays(tradingDays)
weekOfYr    <- week(tradingDays)
qtrOfYr     <- quarter(tradingDays)
monthOfYr   <- month.name[month(tradingDays)]

dt <- data.table(Date      = tradingDays,
                 WeekDay   = dayOfWeek,
                 Week      = weekOfYr,
                 Quarter   = qtrOfYr,
                 Month     = monthOfYr,
                 SimReturn = aveAll_Res)
setkey(dt, Month)

# Probability that increase with time
dt[, summary(lm(SimReturn ~ Date))]
dt[, summary(lm(SimReturn ~ Month))]

#average return by month
dt[, .(AveReturn = mean(SimReturn)-1), by=Month]

# conditioned on negative returns, probability pos returns next day
# conditioned on negative returns, probability pos returns next month
# conditioned on negative returns, probability pos returns next qtr

# same results for either random samples or trends in Mu-Sig
View(dt)
# Ave sim returns by month
# Ave sim returns by day
# Probability that X month will have pos return if X-1 was negative


#store the dataframe I just created with info for the legend of my chart, and a size option
Line <- gvisLineChart(dframe,xvar = colnames(dframe),
                      options=list(width=1200, 
                                   height=600,
                                   vAxis="{format:'#,###%'}"))

#Display chart
plot(Line)
htmlcode <- unlist(Line$html)

#Create Google Gadget
#cat(createGoogleGadget(Motion), file="motionchart.xml")