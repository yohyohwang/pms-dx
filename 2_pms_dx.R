#Calculate the total (net) score for days -5 to -1 vs. +5 to +10. 
#Let's call these periods LP (luteal period) vs. FP (follicular period).
#If LP is greater than or equal to 50% than FP, the person has PMS. 

#This pattern has to be present on at least 2 cycles (cycles 2 and 3). 

#Any of the DRSP Items 22 through 24 have to be 3 or above in LP. 





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

lrange=c(-5,-1) #day range (relative to d1 of menses) for luteal phase
frange=c(5,10) #follicular phase range

library(dplyr)

pms=pms%>%select(id, cycle_number, instrument, mens_day, mens_date, 
                 actual_d1, lh_peak, drsp_sum, drsp_22_24_g3) #select variables of interest


##Initialize some important dataframes and variables
export_names=c("id","pms_dx","c1_lsum","c1_fsum","c2_lsum","c2_fsum","c1_lhpeakdetected","c2_lhpeakdetected","drsp_g3")
export_col=length(export_names)

pms_4cycles=data.frame(matrix(ncol=export_col,nrow=0))
colnames(pms_4cycles)=export_names

a=data.frame(matrix(ncol=export_col,nrow=1))
colnames(a)=export_names

count4=1 #initialize index for subjects with 4 cycles
##

##PMS Diagnosis Loop
for(subject in unique(pms$id)){ #for every unique subject number
    if(4 %in% pms[pms$id==subject,]$cycle_number & #length(pms[pms$subject==subject & pms$cycle_number==4,]$actual_d1)!=0 & 
       "drsp_v2" %in% pms[pms$id==subject & pms$cycle_number==3 & pms$instrument=="drsp_v2",]$instrument){ #if there are 4 cycles in this record and DRSP is recorded for cycles 2-3
    
      drsp_g3=0
      s=pms[pms$id==subject,] #use "s" as a duplicate structure
    
      dates=s[!is.na(s$actual_d1),c("cycle_number","actual_d1","lh_peak")] #anchor dates
      
      if(4 %in% dates$cycle_number){#if d1 for 4th cycle was reported
      
      #initialize some variables
      dx=0
      lh_peak_detected=c(0,0)
      lsum_total=c(0,0)
      fsum_total=c(0,0)
      
      for(cycle in c(2,3)){ #look at cycles 2 and 3
        
        day1=as.Date(dates[dates$cycle_number==cycle,c("actual_d1")]) #set day1 date
        lhpeak=as.Date(dates[dates$cycle_number==cycle,c("lh_peak")]) #set LH peak date
        
        lrange_dates=c((day1)+lrange[1],(day1)+lrange[2]) #date range for luteal phase
        frange_dates=c((day1)+frange[1],(day1)+frange[2]) #date range for follicular phase
        
        lsum=sum(s[as.Date(s$mens_date)>=lrange_dates[1] #sum of DRSP responses for luteal phase
                   & as.Date(s$mens_date)<=lrange_dates[2],]$drsp_sum,
                 na.rm=TRUE)

        fsum=sum(s[as.Date(s$mens_date)>=frange_dates[1] #sum of DRSP responses for follicular phase
                   & as.Date(s$mens_date)<=frange_dates[2],]$drsp_sum,
                 na.rm=TRUE)
        
        #lsum=sum(s[s$mens_date>=lrange_dates[1] & s$mens_date<=lrange_dates[2],c("drsp_sum")],na.rm=TRUE)
        #fsum=sum(s[s$mens_date>=frange_dates[1] & s$mens_date<=frange_dates[2],c("drsp_sum")],na.rm=TRUE)
        
        
        if(lsum>=fsum/2){ #is the sum of DRSP responses for the luteal phase ≥ sum of follicular phase?
          dx=dx+1 #add 1 to dx variable
        }
        
        lsum_total[cycle-1]=lsum
        fsum_total[cycle-1]=fsum
        
        if(TRUE %in% s[as.Date(s$mens_date)>=lrange_dates[1] & as.Date(s$mens_date)<=lrange_dates[2],]$drsp_22_24_g3){
          drsp_g3=1# Any of the DRSP Items 22 through 24 have to be 3 or above in LP.
        }
        
        lh_peak_detected[cycle-1]=!is.na(dates[dates$cycle_number==cycle,c("lh_peak")]) #was LH peak detected? Returns true/false
        
      }
      
      a$id=subject #save subject ID
      a$pms_dx=dx==2 & drsp_g3==1 #save diagnosis
      a$c1_lsum=lsum_total[1] #save lsum (luteal phase of c1)
      a$c2_lsum=lsum_total[2] #save lsum (luteal phase of c2)
      a$c1_fsum=fsum_total[1] #save fsum (follicular phase of c2)
      a$c2_fsum=fsum_total[2] #save fsum (follicular phase of c3)
      a$c1_lhpeakdetected=lh_peak_detected[1] #save if LH peak was non-NA
      a$c2_lhpeakdetected=lh_peak_detected[2]
      a$drsp_g3=drsp_g3
      
      pms_4cycles=rbind(pms_4cycles,a)
      
      count4=count4+1
      }
  }
}
##
write.csv(pms_4cycles,"pms_dx.csv") #export results
