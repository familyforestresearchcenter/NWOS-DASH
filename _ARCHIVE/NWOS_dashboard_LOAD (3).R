#loads raw estimates data, manipulates and saves to RData file
#for use in Shiny visualization

#packages
library(sp)
library(rgdal)
library(rgeos)

#changing global settings
options(stringsAsFactors = FALSE)

#location of tabling inputs
dir <- "C:/Users/jessecaputo/Dropbox (FFRC)/NWOS/PRODUCTS/TOOLS/TABLING/INPUTS/"

#loading reference tables
rg <- read.csv(paste(dir,"REF/REF_GEO.csv",sep=""))
rt <- read.csv(paste(dir,"REF/REF_TABLE.csv",sep=""))
rv <- read.csv(paste(dir,"REF/REF_VARIABLE.csv",sep=""))
rl <- read.csv(paste(dir,"REF/REF_LABEL.csv",sep=""))

#create _REF_VIZ_TABLE
rtz <- rv[,c("VARIABLE","ï..TABLE")]
rtz$DROPDOWN <- paste(rtz$VARIABLE,
                      rt$TABLE_NAME[match(rtz$ï..TABLE,rt$TABLE)],
                      sep=': ') #dropdown labels
rtz$DESCRIPTION <- rt$DESCRIPTION[match(rtz$ï..TABLE,rt$TABLE)] #title text
rtz <- rtz[rtz$VARIABLE!="",]
#write.csv(rtz,"_REF_TABLE_VIZ.csv",row.names=F,na='')
rtz <- read.csv("_REF_TABLE_VIZ.csv")

#function to determine published table number
TNUM <- function(x){
  TAB <- rv$ï..TABLE[match(x$VARIABLE,rv$VARIABLE)]
  NUM <- rt$ï..TABLE_NUMBER[match(TAB,rt$TABLE)]
  ABB <- rg$GEO_ABB[match(x$GEO_CD,rg$GEO_CD)]
  TNUM <- paste(ABB,round(as.numeric(NUM)),sep='-')
  return(TNUM)
}
  
#loading estimates
et <- readRDS(paste(dir,"ESTIMATES/NWOS_2018_FFO_TENPLUS_ESTIMATES.RDS",sep=""))
et$DOMAIN <- "10+"
et$STRATUM <- "FFO"
et$YEAR <- "2018"
et$TNUM <- TNUM(et)

et2 <- readRDS(paste(dir,"ESTIMATES/NWOS_2018_FFO_HUNDREDPLUS_ESTIMATES.RDS",sep=""))
et2$DOMAIN <- "100+"
et2$STRATUM <- "FFO"
et2$YEAR <- "2018"
et2$TNUM <- TNUM(et2)

et3 <- readRDS(paste(dir,"ESTIMATES/NWOS_2018_FFO_ONEPLUS_ESTIMATES.RDS",sep=""))
et3$DOMAIN <- "1+"
et3$STRATUM <- "FFO"
et3$YEAR <- "2018"
et3$TNUM <- TNUM(et3)

et4 <- readRDS(paste(dir,"ESTIMATES/NWOS_2018_FFO_THOUSANDPLUS_ESTIMATES.RDS",sep=""))
et4$DOMAIN <- "1000+"
et4$STRATUM <- "FFO"
et4$YEAR <- "2018"
et4$TNUM <- TNUM(et4)

et <- as.data.frame(rbind(et,et2,et3,et4))
et$STATE <- rg$GEO_NAME[match(et$GEO_CD,rg$GEO_CD)] #add state
et$TABLE <- rtz$DROPDOWN[match(et$VARIABLE,rtz$VARIABLE)] #add table
et$DESCRIPTION <- rtz$DESCRIPTION[match(et$VARIABLE,rtz$VARIABLE)] #add description
#add level labels and order to et
UK <- paste(et$VARIABLE,et$LEVEL,sep="_") #primary key
UK2 <- paste(rl$ï..VARIABLE,rl$LEVEL,sep="_")
et$LABEL <- rl$LABEL[match(UK,UK2)] #add labels
et$ORDER <- rl$ORDER[match(UK,UK2)] #add order
et <- et[!is.na(et$TABLE) & !is.na(et$LABEL),] #remove records with null table/labels

#order
TORDER <- match(et$TABLE,rtz$DROPDOWN) #table order (same as metadata)
et <- et[order(et$DOMAIN,et$STATE,TORDER,et$ORDER),]

#null out tables that are not published
#i.e. individual states for 1+ and 1000+
et$TNUM <- ifelse(et$DOMAIN %in% c('1+','1000+') &
                 !grepl('United',et$STATE),'NOT PUBLISHED',
               et$TNUM)

#load states layer
load("C:/Users/jessecaputo/Dropbox (FFRC)/GIS/DATA/STATECD_NWOS/NWOS_STATES.RData")
format(object.size(st),units="Mb")
states <- st
states <- gSimplify(states,tol=0.05)
states <- SpatialPolygonsDataFrame(states,data=st@data)
format(object.size(states),units="Mb")

#save estimates table (w/ metadata) and states layer
save(list=c("et","states"),file="DEPLOY/NWOS_dashboard_DATA.RData")
