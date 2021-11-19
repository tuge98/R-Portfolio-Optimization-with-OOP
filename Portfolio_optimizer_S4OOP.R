####github: https://github.com/tuge98
####Author: Tuukka Pohjonen
####Email: tuukka.pohjonen@outlook.com


rm(list = ls())
#install.packages("tidyquant")
#install.packages("timetk")
#install.packages("tidyverse")
#install.packages("PortfolioAnalytics")
#install.packages("ROI")
#install.packages("ROI.plugin.glpk")
#install.packages("ROI.plugin.quadprog")

#libraries used in this script
library(tidyquant)
library(timetk)
library(tidyverse)
library(PortfolioAnalytics)





#print(R.version)
#should be working in R 4.0.0 ---> (atleast on that version)
#S4 OOP implementation of simple portfolio-optimizer
#Uses Yahoo finance API to query asset returns 




#declaring data class for obtaining asset returns from Yahoo finance --------------------------------------------


setClass("portfoliodata",
         representation(tick="character", from = "character", to = "character", df = "data.frame"))



setGeneric(name="getapiData",
           def=function(theObject)
           {
             standardGeneric("getapiData")
           }
)


#Generic method "getapiData"


setMethod(f="getapiData",
          signature = "portfoliodata",
          definition = function(theObject)
            {
            price_data <- tq_get(theObject@tick,
                                 from = theObject@from,
                                 to = theObject@to,
                                 get = 'stock.prices')
            
        
            ret_tidy <- price_data %>%
              group_by(symbol) %>%
              tq_transmute(select = adjusted,
                           mutate_fun = periodReturn,
                           period = 'daily',
                           col_rename = 'ret')
            
            
            ret_xts <- ret_tidy %>%
              spread(symbol, value = ret) %>%
              tk_xts()
            returnvector_length <- length(theObject@tick)
            theObject@df <- as.data.frame(ret_xts[, c(1:returnvector_length)])
            return(theObject)
            
          })



#Declaring portfolio optimizer Class ---------------------------------------------


setClass("portfolio_optimizer",
         representation(data = "xts", weights = "list",
                        weightboundaries = "character",
                        type = "character",
                        name = "character",
                        type2 = "character",
                        name2 = "character",
                        method = "character",
                        minboundary = "numeric",
                        maxboundary = "numeric",
                        choice = "numeric"))

















#Method to optimize minimumVariance portfolio with ROI solver

setGeneric(name="optimizemethod",
           def=function(theObject)
           {
             standardGeneric("optimizemethod")
           }
)




setMethod(f="optimizemethod",
          signature = "portfolio_optimizer",
          definition = function(theObject)
            {
            asset_returns = theObject@data
            port_spec <- portfolio.spec(colnames(asset_returns))
            port_spec <- add.constraint(portfolio =port_spec, type = "full_investment")
            port_spec <- add.constraint(portfolio = port_spec,
                                        type = "box",
                                        min = theObject@minboundary,
                                        max = theObject@maxboundary)
            
            if(theObject@choice == 1) {
              port_spec <- add.constraint(portfolio = port_spec, type = theObject@weightboundaries)
              port_spec <- add.objective(portfolio = port_spec, type = theObject@type, name = theObject@name)
              opt <- optimize.portfolio(asset_returns, portfolio = port_spec, optimize_method = theObject@method)
              theObject@weights <- list(extractWeights(opt))
              
            }else{
              port_spec <- add.constraint(portfolio = port_spec, type = theObject@weightboundaries)
              port_spec <- add.objective(portfolio = port_spec, type = theObject@type, name = theObject@name)
              port_spec <- add.objective(portfolio = port_spec, type = theObject@type2, name = theObject@name2)
              opt <- optimize.portfolio(asset_returns, portfolio = port_spec, optimize_method = theObject@method)
              theObject@weights <- list(extractWeights(opt))
              
            }

            return(theObject@weights)
            
          })



#Defining tickers. You can change tickers or date parameters -----------------------------------------------

#ticker <- c('AMZN', 'AAPL', 'NFLX', 'XOM', 'T')
ticker <- c('AMZN', 'AAPL', 'NFLX')

#creating instance of portfoliodata class with parameters.
portfoliodatainstance <- new("portfoliodata", tick = ticker, from = '2016-01-01', to = '2018-05-31')






#calling getapiData method and saving data as "XTS" datatype into a variable
apimethod = getapiData(portfoliodatainstance)


#transforming instance of a class into xts datatype, as it can't be defined in S4 structure (only base datatypes allowed)
x1 = as.xts(apimethod@df)






#creating instance of portfolio_optimizer class and passing instance of
#getapiClass method from data class as parameter to it







#example of getting global minimum variance portfolio

portfolioo = new("portfolio_optimizer", data = x1, weightboundaries = "long_only",
                 type = "risk",
                 name = "var",
                 method = "ROI",
                 minboundary = 0.0,
                 maxboundary = 0.7,
                 choice = 1)
                 


#example of mean-variance portfolio
#type2 and name2 are extra constraints which are needed if more than 1 variable is minimized/maximized. In this case
#mean of sample returns is maximized, whereas standard deviation is minimized
#choice will be != 1 if more than 2 constaints are needed

portfolio2 = new("portfolio_optimizer", data = x1, weightboundaries = "long_only",
                 type = "risk",
                 name = "StdDev",
                 type2 = "return",
                 name2 = "mean",
                 method = "random",
                 minboundary = 0.0,
                 maxboundary = 0.7,
                 choice = 2)



#calculating portfolio weights and saving those to the variable.
weightss = optimizemethod(portfolioo)




#this might take some time if full_investment constraint is not relaxed
weights2 = optimizemethod(portfolio2)


#printing weights
print(weightss)
print(weights2)







#testing S4 structure
#### isS4(portfolioo)
#TRUE
####
