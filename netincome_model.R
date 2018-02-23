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

netincome_include <- vars[which(is.na(vars$`Net Income`)),1]$`Variable Name`



full_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = netincome))
reduced_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = netincome)) + 
  scale_x_continuous(limits = c(-5e+04, 5e+04))
pdf("Paper/images/netincome_histograms.pdf",width=12,height=8)
grid.arrange(full_hist, reduced_hist, nrow = 1)
dev.off()


netincome_x.df <- subset(reduced_data[abs(reduced_data$netincome) < 5e+04,], select = netincome_include)
netincome <- reduced_data[abs(reduced_data$netincome) < 5e+04, 'netincome']

netincome.models <- fit.models(model.name = "netincome", x.data = netincome_x.df, 
                                y.response = netincome, response.family = "gaussian")
