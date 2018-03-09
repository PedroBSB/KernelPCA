library(data.table)
library(dplyr)
library(bit64)
library(stringr)
library(readxl)
library(reshape2)
library(knitr)
library(ggplot2)
library(plyr)
library(gplots)
library(lubridate)
library(tcltk)
library(googlesheets)
library(gsheet)
library(corrplot)
library(xtable)
library(tidyverse)

nasdaq <- data.table(read_csv2("data/sp.csv",
                               skip = 1))
nomes_colunas <- names(read_csv2("data/sp.csv",n_max = 2))

colnames(nasdaq) <- nomes_colunas
nomes_colunas_transform <- setdiff(nomes_colunas,"X1")

setDT(nasdaq)[, (nomes_colunas_transform):= lapply(.SD,  function(x) as.numeric(gsub(",",".",x))),
              .SDcols=nomes_colunas_transform]


#saveRDS(nasdaq,"data/nasdaq.rds")
nasdaq <- readRDS("data/nasdaq.rds")

## counting NA

b <- nasdaq[, lapply(.SD, function(x) sum(is.na(x)))]
summary(t(b))#165


## Respeitando regra 
select_vars <- names(b)[which(b<=165)]
set.seed(1635190218)
select_vars <- union("X1",sample(select_vars,200,replace = F))
nasdaq <- data.table(nasdaq %>% select(select_vars))

nasdaq[, NA_freq := Reduce(`+`, lapply(.SD,function(x) is.na(x)))]
nasdaq <- nasdaq %>% filter(NA_freq==0)
nasdaq <- data.table(nasdaq)
nasdaq <- nasdaq[,`:=`(year = gsub("\\d{2}.\\d{2}.(\\d{4})","\\1",X1),
                   month = gsub("\\d{2}.(\\d{2}).\\d{4}","\\1",X1),
                   day = gsub("(\\d{2}).\\d{2}.\\d{4}","\\1",X1))]

nasdaq <- nasdaq[,last_day := max(day), by = .(year,month)]
# nasdaq <- nasdaq[(day == last_day),]
nasdaq <- nasdaq[,DT:=as.Date(paste0(day,"/",month,"/",year),format ="%d/%m/%Y")]
nasdaq <- nasdaq[order(DT)]


var_ativos <- select_vars[-1]

# Calculando retorno dos ativos
nasdaq <- nasdaq[, c(var_ativos):=lapply(.SD, function(x) (x/shift(x, 1)-1)*100),.SDcols =var_ativos ]

nasdaq <- nasdaq %>% select(c("DT",var_ativos))


## ref: J. Bouchaud, M. Potters, Theory of Financial Risks—From Statistical Physics to Risk Management, Cambridge University Press, UK, 2000.
Q <- nrow(nasdaq)/(ncol(nasdaq) - 1)
lamda_max <- (1 + 1/Q + (1/Q)^.5)


nasdaq <- data.table(gather(nasdaq,"var","value",-DT)%>%filter(!is.na(value)))
nasdaq[,m:=mean(value,na.rm=T),by = "var"]
nasdaq[,v:=sd(value,na.rm=T),by = "var"]
nasdaq[,value:=(value-m)/v]

nasdaq <- spread(nasdaq %>% select(-m,-v),"var","value")


cor0 <- cor(nasdaq[,-1])
eigen0 <- eigen(cor0)

pdf(file.path("eigen.pdf"),height=8,width = 12)


plot(eigen0$values,type="l",xlim = c(0,25),col=1,lwd = 2,xlab = "Eigenvalue Index",ylab="Eigenvalue")
abline(h=lamda_max,col=2,lty=3)

g1 <- eigen0$values[eigen0$values>=lamda_max]
g2 <- eigen0$values[eigen0$values<lamda_max]
eigen1<- diag(c(g1,rep(mean(g2),length(g2))))
cor1 <- eigen0$vectors %*% eigen1 %*% t(eigen0$vectors)
points(c(g1,rep(mean(g2),length(g2))),col=4,type="l",lwd = 2)



dev.off()

