
#READ THE DATA
# setwd("~/Dropbox (CEDAR Lab)/PMSe(R)/ANALYSIS")
# library(readr)
# pms=read_csv("pms_clean.csv")


##READ YOHYOH - UNCOMMENT ABOVE AND COMMENT THIS CHUNK
rm(list=ls())
#yohyoh_dir='/Users/yohyohwang/Downloads'
yohyoh_dir='/Users/yohyoh.wang/Downloads'
setwd(yohyoh_dir)
pms=read.csv('pms_clean.csv')
##

#convert to a usable dataframe for ridgeplot
# pms_ridge=data.frame(matrix(ncol=length(unique(pms$id)), #initialize dataframe with number of columns = number of unique subject IDs
#                             nrow=max(max(pms[pms$instrument=="drsp_v2",]$mens_day)))) #number of rows = maximum cycle length
# colnames(pms_ridge)=unique(pms$id)
# 
# for(subject in unique(pms$id)){
#   pms_ridge[,unique(pms$id)==subject]=c(pms[pms$id==subject & pms$instrument=="drsp_v2" & pms$cycle_number==2,]$drsp_sum,
#         rep(NA,35-length(pms[pms$id==subject & pms$instrument=="drsp_v2" & pms$cycle_number==2,]$drsp_sum)))
# }

library(ggridges)
library(ggplot2)


pms_ridge1=pms[pms$cycle_number==2 & pms$instrument=="drsp_v2",]
pms_ridge1=pms_ridge1%>%select(id,drsp_sum,mens_day)


# ggplot(pms_ridge1, aes(x = drsp_sum, y = id))
# ggplot(pms_ridge1, aes(x = mens_day, y = drsp_sum))

# xrange=c(1,nrow(pms_ridge))
# yrange=c(0,max(pms_ridge,na.rm=TRUE))

xrange=c(1,max(pms_ridge1$mens_day))
yrange=c(min(pms_ridge1$drsp_sum,na.rm=TRUE),max(pms_ridge1$drsp_sum,na.rm=TRUE))
plot(xrange,yrange,type="n",xlab="Menstrual Cycle Day",ylab="Sum of DRSP Symptoms 1-21")

idSet=unique(pms_ridge1$id)

for (subject in idSet) {
  s <- subset(pms_ridge1, id==subject)
  lines(s$mens_day, s$drsp_sum, type="l", lwd=0.7, col=rainbow(length(idSet))[idSet==subject])
}