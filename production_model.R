setwd("/Users/michaelbostwick/Documents/Galapagos")

library(tidyverse)
library(ggplot2)
library(readxl)
library(glmnet)
library(mpath)
library(reshape2)
library(gridExtra)
library(leaps)
library(bestsubset)

source("modeling.function.R")

load("BASES_CENSO_UPA_spss/clean_data.RData")
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")

production_include <- vars[which(is.na(vars$`UPA Production`)),1]$`Variable Name`
production_x.df <- subset(reduced_data[reduced_data$productivity > 0,], select = production_include)

log_productivity <- log10(reduced_data[reduced_data$productivity > 0, 'productivity'])

zero_prod <- reduced_data[reduced_data$productivity ==0,]
par(mfrow = c(2,1))
raw_prod_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = productivity))
log_prod_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = log10(productivity)))
lower_prod_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = productivity)) +
  xlim(0, 10000) + xlab("productivity (0-10,000)")

pdf("Paper/images/production_histograms.pdf",width=12,height=8)
grid.arrange(raw_prod_hist, lower_prod_hist, log_prod_hist, nrow = 1)
dev.off()

production.models <- fit.models(model.name = "production", x.data = production_x.df, 
                                y.response = log_productivity, response.family = "gaussian")
