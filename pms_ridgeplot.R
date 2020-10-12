
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

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(dplyr)


pms_r=pms[pms$cycle_number==2 & pms$instrument=="drsp_v2",]
pms_r=pms_r%>%select(id,drsp_sum,mens_day)

#to make this dataset compatible with ridgeplot (histogram), convert sum into repetitions of days

pms_ridge=data.frame(matrix(ncol=2,nrow=0))

for(subject in unique(pms_r$id)){ #go through each subject
  for(mens_day in unique(pms_r[pms_r$id==subject & !is.na(pms_r$drsp_sum),]$mens_day)){ #for each menstrual cycle day
    pms_mens=data.frame(matrix(ncol=2,nrow=0))
    colnames(pms_mens)=c("id","mens_reps")
    mens_reps=rep(mens_day,pms_r[pms_r$id==subject & pms_r$mens_day==mens_day,]$drsp_sum)
    id_reps=rep(subject,pms_r[pms_r$id==subject & pms_r$mens_day==mens_day,]$drsp_sum)
    combined_reps=matrix(c(id_reps,mens_reps),ncol=2)
    
    pms_ridge=rbind(pms_ridge,combined_reps)
  }
}

colnames(pms_ridge)=c("id","mens_reps")


ggplot(pms_r, aes(x=mens_day, y=as.character(id)), height=drsp_sum) + 
  geom_density_ridges(stat="identity", scale=1.5, aes(x=mens_day, y=as.character(id), height=drsp_sum))

ggplot(pms_r, aes(x=mens_day, y=as.character(id)), height=drsp_sum) + 
  geom_density_ridges(stat="identity", aes(height=drsp_sum)) 


# ggplot(pms_ridge, aes(x=mens_reps, y=as.character(id))) + 
#   geom_density_ridges(aes(x=mens_reps, y=as.character(id))) +
#   geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01)