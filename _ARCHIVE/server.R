shinyServer(function(input, output, session) {
  
  #make sure LEVEL always corresponds with TABLE (mapping tab)
  observeEvent(tm(),{
    updateSelectInput(session,"l","Level",unique(et$LABEL[et$TABLE==tm()]))
  })
  
  #selects desired table
  t <- reactive({
    input$t
  })
  tm <- reactive({ #for mapping tab
    input$tm
  })
  
  
  #selects desired units
  u <- reactive({
    input$u
  })
  um <- reactive({ #for mapping tab
    input$um
  })
  
  #selects desired level for mapping tab
  l <- reactive({
    input$l
  })
  
  #selects desired units for plots and tables
  unit <- reactive({
    if (grepl('Percent',input$u)){
      'percent'
    } else {
      paste('thousand',tolower(input$u))
    }
  })
  unit.map <- reactive({ #for mapping
    if (grepl('Percent',input$um)){
      'percent'
    } else {
      paste('thousand',tolower(input$u))
    }
  })
  
  #function to subset et based on choices, main tab
  subets <- function(){
    ets <- subset(et,TABLE==input$t & STATE==input$s
                  & DOMAIN==input$d & STRATUM==input$p & YEAR==input$y)
    lv <- ets$LABEL[order(ets$ORDER)] #levels in order
    ets$LABEL <- ordered(ets$LABEL,levels=lv)
    #select data based on selected units
    if (input$u=='Acres'){
      ets$VALUE <- ets$ACRES/1000
      ets$VAR <- sqrt(ets$ACRES_VARIANCE)/1000
    } else if (input$u=='Ownerships'){
      ets$VALUE <- ets$OWNERSHIPS/1000
      ets$VAR <- sqrt(ets$OWNERSHIPS_VARIANCE)/1000
    } else if (input$u=='Percent of Acres'){
      ets$VALUE <- ets$ACRES_PROPORTION*100
      ets$VAR <- sqrt(ets$ACRES_PROPORTION_VARIANCE)*100
    } else if (input$u=='Percent of Ownerships'){
      ets$VALUE <- ets$OWNERSHIPS_PROPORTION*100
      ets$VAR <- sqrt(ets$OWNERSHIPS_PROPORTION_VARIANCE)*100
    }
    return(ets)
  }
  
  #function to subset et based on choices
  subets.map <- function(){
    ets <- subset(et,TABLE==input$tm & LABEL==input$l
                  & DOMAIN==input$dm & STRATUM==input$pm & YEAR==input$ym)
    #select data based on selected units
    if (input$um=='Acres'){
      ets$VALUE <- ets$ACRES/1000
      ets$VAR <- sqrt(ets$ACRES_VARIANCE)/1000
    } else if (input$um=='Ownerships'){
      ets$VALUE <- ets$OWNERSHIPS/1000
      ets$VAR <- sqrt(ets$OWNERSHIPS_VARIANCE)/1000
    } else if (input$um=='Percent of Acres'){
      ets$VALUE <- ets$ACRES_PROPORTION*100
      ets$VAR <- sqrt(ets$ACRES_PROPORTION_VARIANCE)*100
    } else if (input$um=='Percent of Ownerships'){
      ets$VALUE <- ets$OWNERSHIPS_PROPORTION*100
      ets$VAR <- sqrt(ets$OWNERSHIPS_PROPORTION_VARIANCE)*100
    }
    return(ets)
  }
  
  #creates function to find caption for main tab
  maketitle <- function() {
    
    ets <- subets() #subset et
    
    tunits <- ifelse(grepl("Percent",u()),'Proportion of','Number of') #title units
    tunits2 <- ifelse(grepl("Acres",u()),'acres by','ownerships by') #title units 2
    des <- et$DESCRIPTION[match(t(),et$TABLE)] #description
    title <- paste(tunits,tunits2,tolower(des)) #create title
    title <- paste(title," (Table: ",ets$TNUM[1],")") #add table number
    
    if (!is.na(title)){
      title #prints question text
    }
    
  }
  
  #creates function to generate caption for mapping tab
  maketitle.map <- function() {
    
    ets <- subets.map() #subset et
    
    tunits <- ifelse(grepl("Percent",um()),'Proportion of','Number of') #title units
    tunits2 <- ifelse(grepl("Acres",um()),'acres by','ownerships by') #title units 2
    des <- et$DESCRIPTION[match(tm(),et$TABLE)] #description
    title <- paste(tunits,tunits2,tolower(des)) #create title
    title <- paste(title, " (",tolower(ets$LABEL[1]),')',sep="")
    #title <- paste(title," (Table: ",ets$TNUM[1],")",sep="") #add table number
    
    if (!is.na(title)){
      title #prints question text
    }
    
  }
  
  #creates function to make plot
  makeplot <- function() {
    
    ets <- subets() #subset et
    
    #plotting element for use in plotting error bars
    limits <- aes(ymax=(VALUE)+2*(VAR),
                  ymin=(VALUE)-2*(VAR))
    
    #prints plot
    if (nrow(ets)>0){
      pl<-ggplot(data=ets,aes(x=LABEL,y=VALUE))+
        geom_bar(stat="identity")+
        geom_errorbar(limits,width=0,size=0.5,position=position_dodge(width=0.90))+
        labs(x="ROW",y=unit())+
        scale_x_discrete(labels=wrap_format(10))+
        scale_y_continuous(label=comma)+
        theme(text=element_text(size=12),
              legend.position="none",
              axis.title.x=element_blank(),
              panel.background=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_rect(fill="transparent"),
              axis.ticks=element_line(color="black"),
              axis.text=element_text(color="black"))
    } 
    if(nrow(ets)>0){
      ggplotly(pl,tooltip=c("y"))
    }
    
  }
  
  #creates function to make table
  maketable <- function() {
    
    ets <- subets() #subset et  
    
    #prints table
    if (nrow(ets)>0){
      ets <- ets[,c("LABEL","VALUE","VAR","N")]
      names(ets) <- c('Category','Value','StdErr','N')
      ets$Value <- paste(comma(ets$Value,accuracy=0.01), unit())
      ets$StdErr <- paste(comma(ets$StdErr,accuracy=0.01), unit())
      ets
    }
    
  }
  
  #creates function to make table
  makemap <- function() {
    
    ets <- subets.map() #subset et
    #attach value and standard error to states layer
    states@data$VALUE <- ets$VALUE[match(states@data$STATE_NWOS_ALPHA,
                                         ets$STATE)]
    states@data$VAR <- ets$VAR[match(states@data$STATE_NWOS_ALPHA,
                                     ets$STATE)]
    
    #create bins
    nonull <- na.omit(states@data$VALUE)
    if (length(nonull)>0){
      bins <- c(floor(min(nonull)),
                1:8*((max(nonull)-min(nonull))/9)+
                  min(nonull),
                ceiling(max(nonull)))
      bins <- unique(bins) #unique bins
    } else {
      bins <- 0 #if there are no values in bin, create one
    }
    if (all(bins==0)){bins <- c(0:1)} #make sure at least 1 bin 
    pal <- colorBin("YlOrRd", domain = states$VALUE, bins = bins)
    
    #create labels
    labform <- "<strong>%s</strong><br/>%g UNIT<br/>SE = %g UNIT"
    labform <- gsub("UNIT",unit.map(),labform)
    labels <- sprintf(
      labform,
      states$STATE_NWOS_ALPHA, round(states$VALUE,1), round(states$VAR,1)
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addPolygons(fillColor = ~pal(VALUE),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "1",
                  fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal,
                values = ~VALUE,
                opacity = 0.7,
                title = unit.map(),
                position = "bottomright")
    
    return(m)
    
  }
  
  #RENDERS TITLE ON SCREEN
  output$caption <- renderText({
    
    maketitle()
    
  })
  
  #RENDERS TITLE ON SCREEN
  output$caption.map <- renderText({
    
    maketitle.map()
    
  })
  
  #RENDERS PLOT ON SCREEN
  output$plot <- renderPlotly({
    
    makeplot()
    
  })
  
  #RENDERS TABLE ON SCREEN
  output$table <- renderDataTable({
    
    datatable(maketable(),rownames=F)
    
  })
  
  #RENDERS MAP ON SCREEN
  output$map <- renderLeaflet({
    
    makemap()
    
  })
  
  #SAVES TABLE TO CSV FILE
  output$downloadTable <- downloadHandler(filename = function() {paste('NWOS2018', Sys.time(),'.csv', sep="")},
                                          content = function(file) { 
                                            write.csv(subets(),file)
                                            
                                          }
  )
  
})