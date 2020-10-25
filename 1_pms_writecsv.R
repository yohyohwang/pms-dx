#READ THE DATA
# setwd("~/Dropbox (CEDAR Lab)/PMSe(R)/ANALYSIS")
# library(readr)
# pms=read_csv("PMSe.csv")


-----------------------------------------------------
#READ YOHYOH - UNCOMMENT ABOVE AND COMMENT THIS CHUNK
rm(list=ls())
setwd('/Users/yohyoh.wang/Downloads')
pms=read.csv('data.csv')
-----------------------------------------------------

library(dplyr)
pms=pms%>%select(id, redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance, drsp_v2_timestamp, drsp_1,  drsp_2, drsp_3, drsp_4, drsp_5, drsp_6,
                 drsp_7, drsp_8, drsp_9, drsp_10, drsp_11, drsp_12, drsp_13, drsp_14, drsp_15, drsp_16, drsp_17, drsp_18,
                 drsp_19, drsp_20, drsp_21, drsp_22, drsp_23, drsp_24, menstrualflow, premom_screenshot_timestamp, tc_ratio, 
                 tc_cat, actual_d1,lh_peak) #keep variables we want

library(data.table)
oldN=c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance","drsp_v2_timestamp")
newN=c("cycle_number","instrument","mens_day","mens_date")
setnames(pms,oldN,newN) #rename some columns for ease

pms=pms%>%filter(id>=1500000 & id<=1600000) #numerical IDs only (exclude DNQ)

pms$cycle_number=gsub("cycle_1_arm_1","1",pms$cycle_number)
pms$cycle_number=gsub("cycle_2_arm_1","2",pms$cycle_number)
pms$cycle_number=gsub("cycle_3_arm_1","3",pms$cycle_number)
pms$cycle_number=gsub("cycle_4_arm_1","4",pms$cycle_number)


pms$mens_date=as.Date(pms$mens_date,"%m/%d/%y")
pms$premom_screenshot_timestamp=as.Date(pms$premom_screenshot_timestamp,"%m/%d/%y")
pms$actual_d1=as.Date(pms$actual_d1,"%m/%d/%y")
pms$lh_peak=as.Date(pms$lh_peak,"%m/%d/%y")

pms=pms[pms$cycle_number=="1" | pms$cycle_number=="2" | pms$cycle_number=="3" | pms$cycle_number=="4",] #keep cycles only
pms=pms[pms$instrument!="cortisol" & pms$instrument!="premom_screenshot" & pms$instrument!="anticipated_period_start",] #dont need cortisol or screenshot for PMS dx

for(subject in unique(pms$id)){ #for every unique subject number
  if("drsp_v2" %in% pms[pms$id==subject,c("instrument")]){
  
  s=pms[pms$id==subject,]
  dates=s[!is.na(s$actual_d1),c("cycle_number","actual_d1","lh_peak")] #anchor dates
  
  for(cycle in unique(pms[pms$id==subject & pms$instrument=="drsp_v2",]$cycle_number)){
    day1=as.Date(dates[dates$cycle_number==cycle,c("actual_d1")])
    day1rep=rep(day1,nrow(s[s$cycle_number==cycle & s$instrument=="drsp_v2",]))
    d=s[s$cycle_number==cycle & s$instrument=="drsp_v2",]$mens_day-1+day1rep
    s[s$cycle_number==cycle & s$instrument=="drsp_v2",c("mens_date")]=factor(d)
    
    pms[pms$id==subject & pms$cycle_number==cycle & pms$instrument=="drsp_v2",]=s[s$cycle_number==cycle & s$instrument=="drsp_v2",]
  }
  }
}

pms$drsp_sum=rowSums(subset(pms, select=drsp_1:drsp_21)) #sum of DRSP symptoms per day
pms$drsp_22_24_g3=pms$drsp_22>=3 | pms$drsp_23>=3 | pms$drsp_24>=3 #whether DRSP 22-24 symptoms were >3 for each DRSP day

write.csv(pms,"pms_clean.csv")
