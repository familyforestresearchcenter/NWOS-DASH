#loads raw estimates data, manipulates and saves to RData file
#for use in Shiny visualization

#changing global settings
options(stringsAsFactors = FALSE)

#loading reference tables
setwd("C:/Users/jessecaputo/Dropbox (FFRC)/NWOS/PRODUCTS/TOOLS/TABLING/INPUTS/REF")
rg <- read.csv("REF_GEO.csv")
rt <- read.csv("REF_TABLE.csv")
rv <- read.csv("REF_VARIABLE.csv")
rl <- read.csv("REF_LABEL.csv")

#concatenating table and subtable
rt$VIZTABLE <- ifelse(rt$SUBTABLE!='',
                      paste(rt$TABLE,rt$SUBTABLE,sep='; '),
                      rt$TABLE)
rv$VIZTABLE <- ifelse(rv$SUBTABLE!='',
                      paste(rv$ï..TABLE,rv$SUBTABLE,sep='; '),
                      rv$ï..TABLE)

#add visualization header (!!should come from somewhere else!!)
rt$VIZ_HEADER <- ifelse(rt$TABLE_NAME==rt$HEADER,rt$HEADER,
                        paste(rt$TABLE_NAME,tolower(rt$HEADER),
                              sep='; '))

#loading estimates
setwd("../ESTIMATES")

et <- readRDS("NWOS_2018_FFO_TENPLUS_ESTIMATES.RDS")
et$DOMAIN <- "10+"
et$STRATUM <- "FFO"
et$YEAR <- "2018"

et2 <- readRDS("NWOS_2018_FFO_HUNDREDPLUS_ESTIMATES.RDS")
et2$DOMAIN <- "100+"
et2$STRATUM <- "FFO"
et2$YEAR <- "2018"

et <- as.data.frame(rbind(et,et2))
rm(et2)
et$STATE <- rg$GEO_NAME[match(et$GEO_CD,rg$GEO_CD)] #add state
et$TABLE <- rv$VIZTABLE[match(et$VARIABLE,rv$VARIABLE)] #add table
et <- na.omit(et) #remove records with nulls

#save
save.image("C:/Users/jessecaputo/Dropbox (FFRC)/NWOS/PRODUCTS/NWOS_2018_FFO/NWOS 2018 dashboard/NWOS_dashboard_data.RData")
