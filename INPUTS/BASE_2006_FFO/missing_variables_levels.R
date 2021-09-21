table(rtz$VARIABLE %in% et$VARIABLE) #should be TRUE
table(rl$VARIABLE %in% et$VARIABLE)

all.var.levs <- unique(et[c('VARIABLE','LEVEL')])
all.var.levs <- all.var.levs[is.na(all.var.levs$LEVEL)|
                               all.var.levs$LEVEL!='-1',]

HAS <- paste(all.var.levs$VARIABLE,all.var.levs$LEVEL) %in%
  paste(rl$VARIABLE,rl$LEVEL) #identifies if level / variable is included

sum(HAS)==nrow(rl) #should be TRUE

all.var.levs <- all.var.levs[!HAS,]

write.csv(all.var.levs,"INPUTS/BASE_2006_FFO/missing_variables_levels.csv")
