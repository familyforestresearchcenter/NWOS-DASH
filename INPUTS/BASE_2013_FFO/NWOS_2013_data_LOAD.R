#loads raw estimates data, manipulates and saves to RData file
#for use in Shiny visualization

#packages
library(sp)
library(rgdal)
library(rgeos)

#changing global settings
options(stringsAsFactors = FALSE)

# #loading reference tables
rg <- read.csv("INPUTS/BASE_2013_FFO/REF_GEO.csv")
rg$GEO_ABB[rg$GEO_LEVEL %in% c('REGION','SUBREGION')] <- paste('US',rg$GEO_ABB[rg$GEO_LEVEL %in% c('REGION','SUBREGION')],sep="_")
# rt <- read.csv("INPUTS/BASE_2013_FFO/REF_TABLE.csv")
# rv <- read.csv("INPUTS/BASE_2013_FFO/REF_VARIABLE.csv")
rl <- read.csv("INPUTS/BASE_2013_FFO/REF_LABEL.csv")
# 
rl$LABEL <- gsub("$","",rl$LABEL,fixed=T) #remove dollar signs
# 
# #create _REF_VIZ_TABLE
# rtz <- rv[,c("VARIABLE","ï..TABLE")]
# rtz$DROPDOWN <- paste(rtz$VARIABLE,
#                       rt$TABLE_NAME[match(rtz$ï..TABLE,rt$TABLE)],
#                       sep=': ') #dropdown labels
# rtz$DESCRIPTION <- rt$DESCRIPTION[match(rtz$ï..TABLE,rt$TABLE)] #title text
# rtz <- rtz[rtz$VARIABLE!="",]
# #write.csv(rtz,"INPUTS/BASE_2013_FFO/REF_TABLE_VIZ.csv",row.names=F,na='')

rtz <- read.csv("INPUTS/BASE_2013_FFO/REF_TABLE_VIZ.csv") #!!!!!!!!!!!!!!!!!!just edit this one
  
#loading estimates
et <- readRDS("INPUTS/BASE_2013_FFO/NWOS_2013_FFO_TEN_PLUS.RDS")
et$DOMAIN <- "10+"
et$STRATUM <- "FFO"
et$YEAR <- "2013"
et$TNUM <- 'NOT PUBLISHED'

et2 <- readRDS("INPUTS/BASE_2013_FFO/NWOS_2013_FFO_HUNDRED_PLUS.RDS")
et2$DOMAIN <- "100+"
et2$STRATUM <- "FFO"
et2$YEAR <- "2013"
et2$TNUM <-'NOT PUBLISHED'

et3 <- readRDS("INPUTS/BASE_2013_FFO/NWOS_2013_FFO_ONE_PLUS.RDS")
et3$DOMAIN <- "1+"
et3$STRATUM <- "FFO"
et3$YEAR <- "2013"
et3$TNUM <- 'NOT PUBLISHED'

et4 <- readRDS("INPUTS/BASE_2013_FFO/NWOS_2013_FFO_THOUSAND_PLUS.RDS")
et4$DOMAIN <- "1000+"
et4$STRATUM <- "FFO"
et4$YEAR <- "2013"
et4$TNUM <- 'NOT PUBLISHED'

et <- as.data.frame(rbind(et,et2,et3,et4))
et$STATE <- rg$GEO_NAME[match(et$GEO_ABB,rg$GEO_ABB)] #add state
et$TABLE <- rtz$DROPDOWN[match(et$VARIABLE,rtz$VARIABLE)] #add table
et$DESCRIPTION <- rtz$DESCRIPTION[match(et$VARIABLE,rtz$VARIABLE)] #add description
#add level labels and order to et
UK <- paste(et$VARIABLE,et$LEVEL,sep="_") #primary key
UK2 <- paste(rl$VARIABLE,rl$LEVEL,sep="_")
et$LABEL <- rl$LABEL[match(UK,UK2)] #add labels
et$ORDER <- rl$ORDER[match(UK,UK2)] #add order
et <- et[!is.na(et$TABLE) & !is.na(et$LABEL),] #remove records with null table/labels

#order
TORDER <- match(et$TABLE,rtz$DROPDOWN) #table order (same as metadata)
et <- et[order(et$DOMAIN,et$STATE,TORDER,et$ORDER),]

#rescale totals in terms of thousands and proportions of terms of percentages
et$VALUE <- ifelse(et$STATISTIC=='TOTAL',
                  et$VALUE/1000,
                  ifelse(et$STATISTIC=='PROPORTION',
                        et$VALUE*100,
                        et$VALUE))

#rescale variances to match
et$VARIANCE <- ifelse(et$STATISTIC=='TOTAL',
                   sqrt(et$VARIANCE)/1000,
                   ifelse(et$STATISTIC=='PROPORTION',
                          sqrt(et$VARIANCE)*100,
                          et$VARIANCE))

et$VALUE <- ifelse(et$VALUE<1,0,round(et$VALUE)) #round those over 1
et$VARIANCE <- ifelse(et$VARIANCE<1,0,round(et$VARIANCE))

et$UNITS <- ifelse(et$STATISTIC != 'N', #update units
                   paste('THOUSAND',et$UNITS),et$UNITS)

names(et)[names(et)=='VARIANCE'] <- 'SE' #rename variance column

et$UNITS[et$STATISTIC=='N'] <- NA #no units for sample size

et$STATISTIC[et$STATISTIC=='PROPORTION'] <- 'PERCENTAGE' #proportions are percentages

#reduce to those with adequate sample size
ssz <- et[et$STATISTIC=='N' & et$VARIABLE=='AC_WOOD_CAT',] #just sample size
ssz <- aggregate(VALUE~GEO_ABB+DOMAIN,data=ssz,FUN='sum')
ssz <- ssz[ssz$VALUE>=50,] #arbitrary cutoff, to retain in dashboard
table(ssz$DOMAIN) #geographies in dashboard by domain

#column identifying whether to include NWOS-DASH
et$INC <- ifelse(paste(et$GEO_ABB,et$DOMAIN) %in% 
                   paste(ssz$GEO_ABB,ssz$DOMAIN),1,0)

#save estimates table (w/ metadata)
save(list=c("et"),file="INPUTS/BASE_2013_FFO/BASE_2013_FFO_DATA.RData")

#combine with existing estimates table
et.2013 <- et #rename
load("DEPLOY/NWOS_dashboard_DATA.RData") #load old
et <- rbind(et,et.2013) #combine

#save estimates table (w/ metadata) and states layer in DEPLOY folder
save(list=c("et","states"),file="DEPLOY/NWOS_dashboard_DATA.RData")
