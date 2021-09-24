rl <- rl[rl$VARIABLE %in% et$VARIABLE,]
UK <- paste(rl$VARIABLE,rl$LEVEL)
UK2 <- paste(et$VARIABLE,et$LEVEL)
table(UK %in% UK2)
rl <- rl[UK %in% UK2,]
rl <- rl[order(rl$VARIABLE,rl$LEVEL),]
write.csv(rl,'INPUTS/BASE_2006_FFO/REF_LABEL.csv',na="",row.names=F)

rtz <- rtz[rtz$VARIABLE %in% et$VARIABLE,]
write.csv(rtz,'INPUTS/BASE_2006_FFO/REF_TABLE_VIZ.csv',na="",row.names=F)
