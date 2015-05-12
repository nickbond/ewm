library(dplyr)
library(lubridate)
library(ggplot2)

logger_cal<-read.csv("/Users/nickbond/Dropbox/metabolism/raw_data/logger_cal_2014_15.csv")
setwd(raw.data.dir)

#read in light files
light.files <- list.files(pattern="*light*")

all.light <-data.frame()

for(i in light.files) {
  site<-gsub("Site Name \t,","",readLines(i, n=1))
light.df <- read.table(i,  header=FALSE, skip=8)
light.df<-data.frame(site,light.df)
names(light.df)<-c("site", "Scan", "Date", "Time", "raw.values", "cal.values")
light.df$datetime<-strptime(paste(light.df$Time, light.df$Date, sep=" "), format="%H:%M:%S %d/%m/%Y", tz = "Australia/Brisbane")
light.df$datetime<-as.POSIXct(light.df$datetime)
all.light<-rbind_list(all.light, light.df)
}

#read in atmos pressure data
atmo.filename <- list.files(pattern="*atmo*")
atmo.df <- read.csv(atmo.filename, sep="\t", header=TRUE)
names(atmo.df)<- c("datetime", "atmo.pressure")
atmo.df$datetime<-strptime(atmo.df$datetime, format = "%m/%d/%y %H:%M", tz = "Etc/GMT-10")
atmo.df$datetime<-as.POSIXct(atmo.df$datetime)
#this is temporary
print("check atmo dates and formats")

atmo.df$datetime<-seq(ymd_hms('2014-08-10 09:00:00'),ymd_hms('2014-09-02 12:00:00'), by = as.difftime(hours(1)), tz= "Etc/GMT-10")

#read in DO files
DO.files<-list.files(pattern="\\.dat")

all.DO.data<-data.frame()

for(i in DO.files) {
site<-gsub(".dat","",i)
DO.header <- readLines(i, n=6)
DO.df<-read.csv(i, skip=6, sep=",")
names(DO.df)<-c("Date", "Time", "Battery", "tempC", "DO.perc", "DO.meas")


DO.df<-data.frame(logger.no=gsub("^.*: ","", DO.header[3]), site=gsub("^.*: ","", DO.header[6]), DO.df, salinity=0)
DO.df$datetime<-strptime(paste(DO.df$Time, DO.df$Date, sep=" "), format="%H:%M:%S %d/%m/%y", tz = "Etc/GMT-10")
DO.df$datetime<-as.POSIXct(DO.df$datetime)
DO.df <- left_join(DO.df, atmo.df, by="datetime")
DO.df <- left_join(DO.df, light.df, by="datetime")

DO.df<-DO.df %>%
  mutate(Date=as.Date(datetime, tz="Etc/GMT-10"), 
         hour=hour(datetime), 
         Time=format(datetime, format="%H:%M%:%S"), 
         I = raw.values / 8.771) %>%
      #   atmo.pressure=atmo.pressure / 0.0680459639) %>%
  group_by(Date, hour) %>%
  mutate(atmo.pressure=mean(atmo.pressure, na.rm=TRUE)) %>%
  ungroup()%>%
 # arrange(datetime)%>%
  select(datetime, Date, Time, I, tempC, DO.meas, atmo.pressure, salinity) 


all.DO.data<-rbind_list(all.DO.data, data.frame(site=site, DO.df))


DO.ts<-ts(DO.df$DO.meas, frequency=144, start=c(1,144))
DO.ts.decomp<-stl(DO.ts, "periodic")

pdf(paste(site, "_DO_ts_decomp.pdf", sep=""), 7, 5)
plot(DO.ts.decomp)
dev.off()



DO.df<-na.omit(DO.df)

DO.df<-DO.df %>%
  group_by(Date) %>%
  mutate(n=n())
  
DO.df<-filter(DO.df, n==144 | (Date==max(Date) & as.character(Time)=="00:00:00")) %>%
  select(-datetime, -n)

print(head(DO.df))
print(tail(DO.df))


  
unique.dates<-unique(DO.df$Date)
  
  for (i in 1:(length(unique.dates)-1)) {
    min.row<-min(which(DO.df$Date==unique.dates[i]))
    max.row<-max(which(DO.df$Date==unique.dates[i]))
    subset.data<-DO.df[min.row:(max.row+1),]
    write.csv(subset.data, file=paste(file.out.dir, site, "_", unique.dates[i],".csv",sep=""), , row.names=F)
  }


}

#look for a file with "logger_cal" in the title in the raw_data directory (this will be month specific)
# THis file should hae 3 columns named "site", "initial_DO_cal", "final_DO_cal". without the quotes. values in each 
# column should be self explanatory.
logger_cal_data<-list.files(pattern="logger_cal")

#read in the logger cal file.
logger_cal <- read.csv(logger_cal_data)

#calculate the change in saturated DO in the calibration solution from start to finish and subset the file to just the 
#site and difference column.
logger_cal <- logger_cal %>% mutate(cal_diff = initial_DO_cal - final_DO_cal) %>% select(site, cal_diff)

#join the calibration summary file to the logger data files.

all.DO.data <- left_join(all.DO.data, logger_cal, by= "site")

#create a new column (corrected DO for now), which results from 
#applying the following formula.
# Corrected DO = Raw DO + (calibration_difference/length of sequence)*(row_position_in_sequence-1)
#This means the first row is corrected by 0, and the last row corrected by the difference in the 
#initial and final calibration values. A linear correction is applied between these.
#

all.DO.data <- all.DO.data %>%
    group_by(site) %>%
      mutate(DO_orig = DO.meas, DO.meas = DO.meas+((cal_diff/n())*(row_number()-1)))
      

ggplot(all.DO.data, aes(x=datetime, y=DO.meas))+geom_point() + facet_grid(site~.)
ggsave("DO_data_sept_2014_all_sites.pdf")





