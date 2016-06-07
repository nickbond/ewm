#setwd("/Users/nickbond/Dropbox/metabolism")
##You need to set the working directory to the year for which you want to run the scripts. All paths are specified relative ot this directory. E.g. setwd("~/Dropbox/metabolism/2016")
source(file.path("R_Scripts","metabolism_helper_functions.R"))

library(zoo)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


raw.data.dirs <- list.dirs(path = file.path("raw_data"), full.names = FALSE, recursive=FALSE)
#d<- raw.data.dirs[7]

for (d in raw.data.dirs) {
  setdir <- d

####set source and output directories
raw.data.dir <-file.path("raw_data", setdir)

model.input.dir <- file.path("model_input", setdir) 

if(dir.exists(model.input.dir)==FALSE) { dir.create(path = model.input.dir, showWarnings=FALSE, recursive=TRUE) }

model.output.dir <- file.path("model_output_3_param", setdir) 

if(dir.exists(model.output.dir)==FALSE) { dir.create(path = model.output.dir, showWarnings=FALSE, recursive=TRUE) }



#folder.location <- "//psf/Dropbox/metabolism/"
#folder.location <- "/Users/nickbond/Dropbox/metabolism/"

combined.data.dir <- file.path("combined_data")

if(dir.exists("combined_data")==FALSE) { dir.create(path = "combined_data", showWarnings=FALSE, recursive=TRUE) }

if(dir.exists("upload_data")==FALSE) { dir.create(path = "upload_data", showWarnings=FALSE, recursive=TRUE) }


####



source("R_Scripts/Script 1. File import.R")

print(d)
}


write.csv(bind_rows(lapply(list.files(pattern=".csv", path = "upload_data/", full.names = TRUE), read.csv)), file="upload_data/all_upload_logger_data.CSV", row.names=FALSE)


#raw.data.dirs <- list.dirs(path = "/Users/nickbond/Dropbox/metabolism/raw_data/", full.names = FALSE, recursive=FALSE)
wd<-"U:/Dropbox/metabolism"
setwd(wd)

model.input.dirs <- list.dirs(path = "raw_data/", full.names = FALSE, recursive=FALSE)
#d<- model.input.dirs[7]
model.input.dirs<-model.input.dirs[-c(1:3)]

for (d in model.input.dirs) {
  setwd(wd)
  
  setdir <- d
  
  model.input.dir <- paste0("model_input/", setdir) 

  model.output.dir <- paste0("model_output_3_param/", setdir) 

  
  folder.location <- getwd()

  
  source("R_Scripts/script-1_run-openbugs_3_param.R")
}



#combine outputs and plot as demonstration.

all.gpp.data <- data.frame()

folder.location <- "model_output_3_param/"

base.files <- list.files(path = folder.location, pattern = "BASE", recursive=TRUE, full.names = TRUE)
for (i in base.files) {

  sample.period<-strsplit(i,split = "/")
  sample.period <- as.numeric(unlist(sample.period))
  sample.period <- unique(sample.period[!is.na(sample.period)])
  
  data <- read_csv(i)
  data <- data.frame(sample.period, data)
  data$File <- gsub(".csv","", data$File)
  data <- separate(data=data, col = File, into = c("site", "date"), sep="_")
  all.gpp.data <- rbind_list(all.gpp.data, data)
}

all.gpp.data <- all.gpp.data %>%
  mutate(PR.ratio = GPP.mean / ER.mean) %>%
  mutate(sample.period = as.character(sample.period))


write_csv(all.gpp.data, paste0("all_gpp_data_3_param.csv"))


###
#mess with data here 




wd<-"/Users/nickbond/Dropbox/metabolism"
#folder.location <- getwd(wd)

all.gpp.data<-read_csv(paste0(wd, "/all_gpp_data_3_param.csv"))

fixed.DO.files <-list.files(paste0(wd, "/combined_data"), pattern = "DO_data.csv", full.names = TRUE)

fixed.DO.data <- data.frame()
for(i in fixed.DO.files) {
  fixed.DO <- read_csv(i, col_types = "cTDtddddd")
  fixed.DO.data <- bind_rows(fixed.DO.data, fixed.DO)
}

fixed.DO.data <- fixed.DO.data %>%
  group_by(site, Date) %>%
  summarise(avg.DO = mean(DO.meas, na.rm=TRUE),
            avg.atmo.pressure = mean(atmo.pressure, na.rm=TRUE), 
            avg.tempC = mean(tempC, na.rm=TRUE),
            avg.salinity = mean(salinity, na.rm = TRUE), 
            sum.I = sum(I, na.rm = TRUE))

fixed.DO.data$Date<-as.Date(as.POSIXct(fixed.DO.data$Date, tz="UTC"), tz="UTC")

all.gpp.data$date <- as.Date(as.POSIXct(all.gpp.data$date, tz="UTC"), tz="UTC")

all.hydro.data <- read_csv("hydrological_data_2014_15.csv")
all.hydro.data$date <- as.Date(dmy(all.hydro.data$date, tz = "UTC"), tz="UTC")

site.zones <- read_csv("sites_zones.csv")

all.hydro.data <- left_join(all.hydro.data, filter(site.zones, GPP == 1))



#add zone column to gpp data
all.gpp.data$zone <- stringr::str_match(string = all.gpp.data$site, "zone[:digit:]")

all.gpp.data <- left_join(all.hydro.data, all.gpp.data[1:1130,], by = c('date'='date', 'zone'='zone', 'sitecode.2014'='site')) #reference all rows in GPP data due to bug in dplyr left_join
all.gpp.data <- left_join(all.gpp.data, fixed.DO.data, by = c('sitecode.2014'='site', 'date' = 'Date'))


write_csv(filter(all.gpp.data, !is.na(GPP.mean)), path = paste0(wd, "/all_gpp_data_combined_3_param.csv"))


#write base output file

base_output_df<- all.gpp.data %>% 
  filter(!is.na(GPP.mean)) %>%
  select(sitecode.2014, date, GPP.mean, GPP.sd, ER.mean, ER.sd, K.mean, K.sd, R2, avg.tempC, sum.I) %>%
  rename(samplePointName=sitecode.2014, sampleDate=date, GPP=GPP.mean, GPPSD=GPP.sd, ER=ER.mean, ERSD=ER.sd, K=K.mean, KSD=K.sd, r2=R2, aveDailyTemp=avg.tempC, totDailyLight=sum.I) %>% 
  mutate(meetsUseCriteria=ifelse(r2>0.9 & GPPSD/GPP<0.5, 'Yes', 'No'), evalCode=NA, Comment=NA) %>%
  write_csv(., path=paste0(wd, "/_EDWK_MET_BASE.csv"), na = "")




all.gpp.data %>%
  mutate(sample.period=factor(sample.period)) %>%
  group_by(sitecode.2014, sample.period) %>%
  summarise(PR.ratio.m = mean(PR.ratio, na.rm=T), PR.ratio.sd = sd(PR.ratio, na.rm=T), n=n(), PR.ratio.se = PR.ratio.sd/sqrt(n)) %>%
  ggplot(., aes(x=sample.period, y=PR.ratio.m)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black",
           fill="grey50", # Use black outlines,
           size=.3) +
  geom_errorbar(aes(ymin = PR.ratio.m - PR.ratio.se, ymax = PR.ratio.m + PR.ratio.se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) + 
  xlab("Date") +
  ylab("P/R ratio") + 
  facet_grid(sitecode.2014~.)


ggplot(all.gpp.data, aes(x=date, y=discharge.ml.d)) + geom_line() + facet_wrap(~sitecode.2014) 
ggplot(all.gpp.data, aes(x=date, y= PR.ratio)) + geom_bar(stat="identity") + facet_wrap(~sitecode.2014)
