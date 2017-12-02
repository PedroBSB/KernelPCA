library(mlRFinance)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)

nasdaq <- data.table(read_csv2("data/sp.csv",
                               skip = 1))
nomes_colunas <- names(read_csv2("data/sp.csv",n_max = 2))

colnames(nasdaq) <- nomes_colunas
nomes_colunas_transform <- setdiff(nomes_colunas,"X1")

setDT(nasdaq)[, (nomes_colunas_transform):= lapply(.SD,  function(x) as.numeric(gsub(",",".",x))),
              .SDcols=nomes_colunas_transform]
saveRDS(nasdaq,"data/nasdaq.rds")
