library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
#setwd(raw.data.dir)
#setwd("~/Dropbox/metabolism/raw_data")


#read in light files
light.files <- list.files(path=raw.data.dir, pattern=".*light.*\\.csv$", recursive=TRUE, full.names=TRUE)

light.data <-data.frame()

for(i in light.files) {
  site <- strsplit(i, split = "_", fixed=TRUE)[[1]][3]

  light.df <- read.csv(i, header=FALSE, skip=9)
  names(light.df)<-c("Scan", "Date", "Time", "raw.values", "cal.values")
  light.df<-data.frame(site=site,light.df)

  light.df$datetime<-strptime(paste(light.df$Time, light.df$Date, sep=" "), format="%H:%M:%S %d/%m/%Y", tz="UTC")
  light.df$datetime<-as.POSIXct(light.df$datetime, tz="UTC")
  minute(light.df$datetime) <- floor(minute(light.df$datetime)/10)*10
  second(light.df$datetime) <- 0

  light.data<-rbind_list(light.data, light.df)
}
#for now select only zone2site4...

light.data <- light.data  %>% filter(site == "zone2site4") %>% select(datetime, raw.values)

 #read in atmos pressure data
atmo.files <- list.files(path=raw.data.dir, pattern=".*atmo.*\\.csv$", recursive=TRUE, full.names=TRUE)
atmo.data <- data.frame()

for(i in atmo.files) {
  site <- strsplit(i, split="_", fixed=TRUE)[[1]][2]
  atmo.df <- read.csv(i, skip=2 , header=FALSE)
  atmo.df <- atmo.df[,c(2:4)]
  names(atmo.df)<- c("datetime", "atmo.psi", "atmo.tempC")

  atmo.df$datetime<-strptime(atmo.df$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
  atmo.df$datetime<-as.POSIXct(atmo.df$datetime, tz = "UTC")
  atmo.data <- bind_rows(atmo.data, atmo.df)
  atmo.data<-na.omit(atmo.data)
}



#read in DO files
DO.files<-list.files(path=raw.data.dir, pattern=".*DO.*\\.dat$", recursive=TRUE, full.names = TRUE)
DO.data<-data.frame()


#calculate the change in saturated DO in the calibration solution from start to finish and subset the file to just the
#site and difference column.
#logger_cal<-read_csv(paste0(folder.location, "logger_cal_2014_15.csv")
logger_cal<-read_csv(list.files(path="calibration_files", pattern="logger_cal*", full.names=TRUE))


for(i in DO.files) {
  site <- strsplit(i, split="_", fixed=TRUE)[[1]][3]
  DO.header <- readLines(i, n=6)
  DO.df<-read.csv(i, skip=7, header=FALSE)
  names(DO.df)<-c("Date", "Time", "Battery", "wtempC", "DO.perc", "DO.meas")


  DO.df<-data.frame(logger.no=gsub("^.*: ","", DO.header[3]), site=site, DO.df)
  DO.df$datetime<-strptime(paste(DO.df$Time, DO.df$Date, sep=" "), format="%H:%M:%S %d/%m/%y", tz="UTC")
  #round to the nearest 10 minute block - in case loggers were set up without checking the clock.
  DO.df$datetime <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + floor(as.numeric(DO.df$datetime)/600)*600
  DO.df$datetime<-as.POSIXct(DO.df$datetime, tz="UTC")
  DO.df <- DO.df %>% select(logger.no, site, datetime, Battery, wtempC, DO.perc, DO.meas)
  DO.df <- left_join(DO.df, atmo.data, by="datetime")
  #fill in empty rows using locf and nocb
    DO.df$atmo.psi <- na.locf(DO.df$atmo.psi, na.rm = FALSE)
  DO.df$atmo.tempC <- na.locf(object = DO.df$atmo.tempC, na.rm = FALSE)
  DO.df$atmo.psi <- na.locf(DO.df$atmo.psi, na.rm = FALSE, fromLast=TRUE)
  DO.df$atmo.tempC <- na.locf(object = DO.df$atmo.tempC, na.rm = FALSE, fromLast=TRUE)
  
  #DO.df$atmo.tempC[is.na(DO.df$atmo.tempC)]<-25

  DO.df <- left_join(DO.df, light.data, by="datetime")

  DO.df<-DO.df %>%
    mutate(Date=as.Date(datetime),
           hour=hour(datetime),
           Time=format(datetime, format="%H:%M%:%S"),
           I = raw.values / 8.771,
           atmo.mbar = atmo.psi/ 0.0145037738007,
           atmo.atm = atmo.mbar * 0.00098692326671601)
   #atmo.mbar= ifelse(is.na(atmo.psi), 1000, atmo.psi / 0.0145037738007))

  local_cal <- logger_cal[which(logger_cal$sample_date == setdir & match(logger_cal$site,site[1])==TRUE),] %>%
    mutate(cal_diff = final_DO_cal-initial_DO_cal, salinity=ifelse(is.na(salinity),0,salinity)) %>%
    select(site, cal_diff, salinity, outliers_to_exclude)


  DO.df$site<-as.character(DO.df$site)
  DO.df <- left_join(DO.df, local_cal, by= "site")

  DO.df<- DO.df %>%
    mutate(n=row_number(), ntot=n()) %>%
  mutate(DO.perc.orig = DO.perc, drift.corrected.DO.perc = (DO.perc - ((cal_diff/ntot)*n-1)), DO.corrected.mgL = ifelse((is.na(atmo.mbar) | is.na(I)),NA,O2.saturation(salinity, wtempC, atmo.mbar, drift.corrected.DO.perc, corrected.DO.only=TRUE)))

  file.NAs<-local_cal$outliers_to_exclude
  if (!is.na(file.NAs)) {

  file.NAs<-as.character(file.NAs[1])
  DO.df[eval(parse(text=file.NAs)), "DO.corrected.mgL"] <- NA
  DO.df$DO.corrected.mgL.infilled <- na.spline(DO.df$DO.corrected.mgL, maxgap=10, na.rm=F)

  } else {

  DO.df$DO.corrected.mgL.infilled <- DO.df$DO.corrected.mgL
  }

    #group_by(Date, hour) %>%
    #mutate(site=site[1], atmo.pressure=mean(atmo.pressure, na.rm=TRUE)) %>%
    #ungroup()%>%
    # arrange(datetime)%>%



  #join the calibration summary file to the logger data files.







  DO.data<-rbind_list(DO.data, select(DO.df, site, datetime, DO.meas, DO.corrected.mgL, DO.corrected.mgL.infilled))


  DO.ts<-ts(na.omit(DO.df$DO.corrected.mgL.infilled), frequency=144, start=c(1,144))
  DO.ts.decomp<-stl(DO.ts, "periodic")

  pdf(paste0(model.output.dir, "DO_ts_decomp_", site, "_", setdir, ".pdf", sep=""), 7, 5)
  plot(DO.ts.decomp)
  dev.off()

  DO.df <-DO.df %>%
    select(site, datetime, Date, Time, atmo.atm, I, salinity, wtempC, DO.corrected.mgL.infilled) %>%
    rename(., DO.meas = DO.corrected.mgL.infilled, tempC = wtempC, atmo.pressure=atmo.atm)



  DO.df<-na.omit(DO.df)

#  #Create DOPTO file
  DO.df %>%
    write.csv(., file=file.path("combined_data", paste0(setdir, "_", site, "_DO_data.csv")), row.names=FALSE)

  #####


  DO.df<-DO.df %>%
    group_by(Date) %>%
    mutate(n=n(), row_number=row_number())

  DO.df<-filter(DO.df, n==144 | (Date==max(Date) & as.character(Time)=="00:00:00")) %>%
    select(-datetime, -n, -row_number)


  DO.df %>% select(site, Date, Time, I, tempC, DO.meas, atmo.pressure) %>%
    rename(.,SamplePointName=site, temperatureC = tempC, logDate=Date, logTime=Time, light=I, dissolvedOxygen = DO.meas, atmosPressure=atmo.pressure) %>%
    mutate(., EvaluationCode=NA, Comments=NA, SampleDate=NA) %>%
    select(., SamplePointName, SampleDate, logDate, logTime, light, temperatureC, dissolvedOxygen, atmosPressure, EvaluationCode, Comments) %>%
  write_csv(., path = file.path("upload_data", paste0(setdir, site, "_logger_upload_data.csv")), na="")



#split data up into daily data
  unique.dates<-unique(DO.df$Date)

  for (k in 1:(length(unique.dates)-1)) {
    min.row<-min(which(DO.df$Date==unique.dates[k]))
    max.row<-max(which(DO.df$Date==unique.dates[k]))
    subset.data<-DO.df[min.row:(max.row+1),]
    write_csv(subset.data, path = file.path(model.input.dir, paste0(site, "_", unique.dates[k],".csv")))

  }

  print(i)

}

DO.data  %>% select(site, datetime, DO.corrected.mgL, DO.meas, DO.corrected.mgL.infilled) %>% gather(series, reading, -c(site, datetime)) %>% ggplot(., aes(x=datetime, y=reading, colour=series)) + geom_point(size=0.5) + facet_grid(site~.)

ggsave(paste0(model.output.dir,"DO_all_sites_", setdir, ".pdf"))







