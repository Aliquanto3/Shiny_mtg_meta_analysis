#install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='aliquanto', 
                          token='63479145A1D77E51D90B58835F383E5D', 
                          secret='xy6jU60BAZGtxzY3u7G8GIpR5cdXhiY1890qJQLd')
#install.packages(c('ggplot2', 'shiny'))
library(shiny)
runApp()
deployApp()
#terminateApp("demo")