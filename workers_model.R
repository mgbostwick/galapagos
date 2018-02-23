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
library(bestglm)

source("modeling.function.R")

load("BASES_CENSO_UPA_spss/clean_data.RData")
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")

workers_include <- vars[which(is.na(vars$`Number workers supported`)),1]$`Variable Name`

workers_nonzero <- reduced_data[reduced_data$fulltimework > 0, 'fulltimework']
reduced_data$workers_binary <- (reduced_data$fulltimework > 0)*1

workers_x.df <- subset(reduced_data, select = workers_include)

all_workers_hist <- ggplot(data = reduced_data) + geom_bar(mapping = aes(x = factor(workers_binary)))
nonzero_hist <- ggplot(data = reduced_data[reduced_data$fulltimework > 0,]) + geom_histogram(mapping = aes(x = fulltimework))

pdf("Paper/images/worker_histograms.pdf",width=12,height=8)
grid.arrange(all_workers_hist, nonzero_hist, nrow = 1)
dev.off()

workers.models <- fit.models(model.name = "workers", x.data = workers_x.df, 
                                y.response = reduced_data$workers_binary, response.family = "binomial")
