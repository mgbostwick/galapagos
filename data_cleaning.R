setwd("/Users/michaelbostwick/Documents/Galapagos")

library(tidyverse)
library(ggplot2)
library(readxl)

load("BASES_CENSO_UPA_spss/prelim_data.RData")

# Reduce dataset to subset of possible variables for consideration
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")
var_list <- vars$`Variable Name`
for (i in 1:nrow(vars)){
  result <- tryCatch(reduced_data <-  data[,var_list[i]],
                     error = function(e) {print(paste(i,var_list[i]))})
}
reduced_data <-  data[,var_list]

# Where appropriate fill NA values with 0
binary_vars <- vars[which(vars$Binary==1),1]$`Variable Name`
reduced_data[,binary_vars][is.na(reduced_data[,binary_vars])] <- 0

colnames(reduced_data)[colSums(is.na(reduced_data)) > 0]

fill_na_zero <- c("r3","p7","p8","p9","p26","CosechaLibras","VentaLibras","num_cultivo","t28","libras_cosechada",
                  "libras_vendida","t8","t10","pc6","ad6","ad4","ad11","produccion_en_libras_producto_cosechado_autoconsumo",
                  "produccion_en_libras_producto_vendido","v30_a","v44","v49","v53_a","v53_b","v54","af6","e29",
                  "e30_f","to56_e","to57_e","to57_a","to57_b","to57_c","to57_d","to57_f")

reduced_data[,fill_na_zero][is.na(reduced_data[,fill_na_zero])] <- 0

# Add None level for Farms without Pasture
levels(reduced_data$pc4) <- c(levels(reduced_data$pc4), "None")
reduced_data[,"pc4"][is.na(reduced_data[,"pc4"])] <- "None"

# Convert mistaken factor variables to numeric variables
reduced_data$e29_f <- as.numeric(reduced_data$e29_f)
reduced_data$v45 <- as.numeric(reduced_data$v45)

# Check to make sure there are no more mistaken factor variables
reduced_data[1,sapply(reduced_data, is.factor)]

# Output clean dataset
save(reduced_data, file='BASES_CENSO_UPA_spss/clean_data.RData')
