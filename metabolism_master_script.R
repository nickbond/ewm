#source("/Users/nickbond/Dropbox/metabolism/R_Scripts/metabolism_helper_functions.R")

source("//psf/Dropbox/metabolism/R_Scripts/metabolism_helper_functions.R")

library(zoo)
library(stringr)


#raw.data.dirs <- list.dirs(path = "/Users/nickbond/Dropbox/metabolism/raw_data/", full.names = FALSE, recursive=FALSE)
raw.data.dirs <- list.dirs(path = "//psf/Dropbox/metabolism/raw_data/", full.names = FALSE, recursive=FALSE)
#d<- raw.data.dirs[7]

for (d in raw.data.dirs) {
  setdir <- d

####set source and output directories
raw.data.dir <-paste0("//psf/Dropbox/metabolism/raw_data/", setdir)
#raw.data.dir <- paste0("/Users/nickbond/Dropbox/metabolism/raw_data/", setdir)

model.input.dir <- paste0("//psf/Dropbox/metabolism/model_input/", setdir) 
#model.input.dir <- paste0("/Users/nickbond/Dropbox/metabolism//model_input/", setdir)
if(dir.exists(model.input.dir)==FALSE) { dir.create(path = model.input.dir, showWarnings=FALSE) }

model.output.dir <- paste0("//psf/Dropbox/metabolism/model_output_5_param/", setdir) 
#model.output.dir <- paste0("/Users/nickbond/Dropbox/metabolism//model_output/", setdir)
if(dir.exists(model.output.dir)==FALSE) { dir.create(path = model.output.dir, showWarnings=FALSE) }



folder.location <- "//psf/Dropbox/metabolism/"
#folder.location <- "/Users/nickbond/Dropbox/metabolism/"

combined.data.dir <- paste0(folder.location, "combined_data/") 
#model.output.dir <- paste0("/Users/nickbond/Dropbox/metabolism//model_output/", setdir)
if(dir.exists(combined.data.dir)==FALSE) { dir.create(path = combined.data.dir, showWarnings=FALSE) }



####


#source("/Users/nickbond/Dropbox/metabolism/R_Scripts/Script 1. File import.R")
source("//psf/Dropbox/metabolism/R_Scripts/Script 1. File import.R")

print(d)
}





#raw.data.dirs <- list.dirs(path = "/Users/nickbond/Dropbox/metabolism/raw_data/", full.names = FALSE, recursive=FALSE)
model.input.dirs <- list.dirs(path = "//psf/Dropbox/metabolism/raw_data/", full.names = FALSE, recursive=FALSE)
#d<- raw.data.dirs[7]
model.input.dirs<-model.input.dirs

for (d in model.input.dirs) {
  setdir <- d
  
  model.input.dir <- paste0("//psf/Dropbox/metabolism/model_input/", setdir) 
  #model.input.dir <- paste0("/Users/nickbond/Dropbox/metabolism//model_input/", setdir)
  
  model.output.dir <- paste0("//psf/Dropbox/metabolism/model_output_5_param/", setdir) 
  #model.output.dir <- paste0("/Users/nickbond/Dropbox/metabolism//model_output/", setdir)
  
  
  folder.location <- "//psf/Dropbox/metabolism/"
  #folder.location <- "/Users/nickbond/Dropbox/metabolism/"
  
  source("//psf/Dropbox/metabolism/R_Scripts/script-1_run-openbugs.R")
}

#combine outputs and plot as demonstration.

all.gpp.data <- data.frame()
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
  mutate(PR.ratio = GPP.mean / ER.mean)

write_csv(all.gpp.data, paste0(folder.location, "all_gpp_data_5_param.csv"))


###
#mess with data here 




folder.location <- "//psf/Dropbox/metabolism/"

fixed.DO.files <-list.files(paste0(folder.location, "combined_data"), full.names = TRUE)

fixed.DO.data <- data.frame()
for(i in fixed.DO.files) {
  fixed.DO <- read_csv(i)
  fixed.DO.data <- bind_rows(fixed.DO.data, fixed.DO)
}

fixed.DO.data <- fixed.DO.data %>%
  group_by(Date) %>%
  summarise(avg.DO = mean(DO.meas, na.rm=TRUE),
            avg.atmo.pressure = mean(atmo.pressure, na.rm=TRUE), 
            avg.tempC = mean(tempC, na.rm=TRUE),
            avg.salinity = mean(salinity, na.rm = TRUE), 
            sum.I = sum(I, na.rm = TRUE))
fixed.DO.data$Date<-as.Date(as.POSIXct(fixed.DO.data$Date))

all.gpp.data <- read_csv(paste0(folder.location, "all_gpp_data.csv"), )
all.gpp.data$date <- as.Date(as.POSIXct(all.gpp.data$date))

all.hydro.data <- read_csv("//psf/Dropbox/metabolism/hydrological_data_2014_15.csv")
all.hydro.data$date <- as.Date(dmy(all.hydro.data$date))


#add zone column to gpp data
all.gpp.data$zone <- stringr::str_match(string = all.gpp.data$site, "zone[:digit:]")

all.gpp.data <- left_join(all.gpp.data, all.hydro.data, by = c('date'='date', 'zone'='zone'))
all.gpp.data <- left_join(all.gpp.data, fixed.DO.data, by = c('date' = 'Date'))

write_csv(all.gpp.data, path = paste0(folder.location, "all_gpp_data_combined_5_param.csv"))

all.gpp.data %>%
  mutate(PR.ratio = GPP.mean / ER.mean) %>%
  group_by(site, sample.period) %>%
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
  facet_grid(site~.)


