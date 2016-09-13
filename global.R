library(shiny)
library(ggplot2)
library(scales)
library(DT)
library(quantreg)
library(tidyr)
library(dplyr)
library(Hmisc)
library(synapseClient)

synapseLogin()

options(shiny.maxRequestSize=100*1024^2) 
options(shiny.reactlog=TRUE) 

stat_sum_df <- function(fun, geom="point", ...) {
  stat_summary(fun.data=fun,  geom=geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun,  geom=geom,  ...)
}
