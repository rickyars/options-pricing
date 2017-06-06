library(shiny)

library(fOptions)
library(quantmod)
library(plotly)
library(sde)
library(tidyverse)

source("R/bsm-cones.R")

# NOTES:
# r (risk free rate), d (dividend yield), b (cost-of-carry rate).
# d = r - b 
