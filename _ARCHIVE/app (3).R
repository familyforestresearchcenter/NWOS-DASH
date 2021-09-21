library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(DT)
library(plotly)
library(leaflet)
library(sp)
library(jsonlite)

#changing global settings
options(stringsAsFactors = FALSE)

#loads formatted estimates and reference tables
load("NWOS_dashboard_DATA.RData")

ui <- navbarPage("National Woodland Owner Survey Dashboard (NWOS-DASH)",
                 windowTitle="NWOS-DASH",
                 theme = shinytheme("cosmo"),
  
  
  tabPanel("Univariate Results",
  
    tags$head(
       tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }",
                ".btn-primary, .btn-primary:hover, .btn-primary:active, .btn-primary:visited, .btn-primary:focus, .btn-primary:active:focus { background-color: #1d76d9; border-color: #1d76d9; }")
    ),       
           
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput("t", "Variable", unique(et$TABLE), selected="Ownership type"),
        
        selectInput("u", "Unit", c('Acres','Ownerships','Percent of Acres','Percent of Ownerships')),
        
        selectInput("s", "State", unique(et$STATE), selected="United States"),
        
        selectInput("p", "Population", unique(et$STRATUM)),
        
        selectInput("d", "Domain", unique(et$DOMAIN), selected="10+"),
        
        selectInput("y", "NWOS Cycle", unique(et$YEAR)),
        
        downloadButton('downloadCSV', 'CSV', class = "btn btn-primary"),
        downloadButton('downloadJSON', 'JSON', class = "btn btn-primary"),
        br(),
        HTML("<font size='-1'><i>Note: Clicking download buttons will initiate the download of external file(s).</i></font>")
      ),
      
      
      mainPanel(
        h4(textOutput("caption"),align="center"),
        br(),
        tabsetPanel(type = "tabs",
                    tabPanel("Plot",plotlyOutput("plot", height=450)),
                    tabPanel("Table",br(),dataTableOutput("table"))
        ),
        br(),
        h5(textOutput("tabref"),align="justify")
        
      )
    )
  ),
  
  tabPanel("Mapped Results",
           
             tags$head(
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }")
             ),       
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput("tm", "Variable", unique(et$TABLE), selected="Ownership type"),
                 
                 selectInput("l", "Level", unique(et$LABEL)),
                 
                 selectInput("um", "Unit", c('Percent of Acres','Percent of Ownerships','Acres','Ownerships')),

                 selectInput("pm", "Population", unique(et$STRATUM)),
                 
                 selectInput("dm", "Domain", c('10+','100+')),
                 
                 selectInput("ym", "NWOS Cycle", unique(et$YEAR)),
                 
                 downloadButton('downloadCSVMap', 'CSV', class = "btn btn-primary"),
                 downloadButton('downloadJSONMap', 'JSON', class = "btn btn-primary"),
                 br(),
                 HTML("<font size='-1'><i>Note: Clicking download buttons will initiate the download of external file(s).</i></font>")
                 
               ),
               
               
               mainPanel(
                 h4(textOutput("caption.map"),align="center"),
                 br(),
                 leafletOutput("map", height=600)
                 
               )
             )
           ),
  
  navbarMenu("More",
             tabPanel("About",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          selectInput("yall", "Download complete NWOS summary dataset, by year", unique(et$YEAR)),
                          
                          selectInput("sall", "and State", unique(et$STATE), selected="United States"),
                          
                          downloadButton('downloadCSVAll', 'CSV', class = "btn btn-primary"),
                          downloadButton('downloadJSONAll', 'JSON', class = "btn btn-primary"),
                          br(),
                          HTML("<font size='-1'><i>Note: Clicking download buttons will initiate the download of external file(s).</i></font>")
                        ),
                        
                      mainPanel(
                        HTML("<p lang='en'><h4>U.S. Forest Service, National Woodland Owner Survey</h4></p>
                        <p lang='en'>There are over 800 million acres of forestland in the U.S. and most of this land is privately owned. Approximately 36% of the forestland is owned by families and individuals, 31% by the federal government, 19% by corporations, 9% by state governments, 2% by tribal groups, 2% by other private groups, and 2% by local governments.</p>
                        <p lang='en'>The National Woodland Owner Survey (NWOS) is aimed at increasing our understanding of private forest owners, including:</p>
                        <ul><li>How many private forest owners there are?</li>
                        <li>Why they own land?</li>
                        <li>What they have done with their forests in the past?</li>
                        <li>What they plan to do with their forests in the future?</li></ul>
                        <p lang='en'>Summary information from the NWOS is used by people who provide, design, and implement services and policies that impact private forest owners, including government agencies, landowner organizations and other non-governmental organizations, private service providers, business analysts, forest industry companies, and academic researchers.</p>
                        <p lang='en'>The National Woodland Owner Survey is implemented by the <a href='http://www.familyforestresearchcenter.org/' target='_blank'>Family Forest Research Center (opens in new window)</a>, a joint venture between the USDA Forest Service and the University of Massachusetts Amherst. More information on the NWOS, including summary tables and survey instruments can be found on the <a href='https://www.fia.fs.fed.us/nwos/' target='_blank'>NWOS website (opens in new window)</a>.</p>"),
                        HTML(paste("<p lang='en'><b>Suggested citation: Caputo, J. and B. Butler. National Woodland Owner Survey Dashboard (NWOS-DASH) version 1.0. Accessed ", Sys.Date(), ".</b></p>",sep=""))
                      )
                    )
                  ),
             tabPanel("Help",
                      HTML("<p lang='en'><h4>How to use NWOS-DASH?</h4></p>
                      <p lang='en'>NWOS-DASH is the official data visualization and retrieval tool for the National Woodland Owner Survey (NWOS). Here you can obtain population-level estimates of landowners' objectives, activities, demographics and more &mdash; summarized by state and region.</p>
                      <p lang='en'>The <b>Univariate Results</b> tab on the navigation bar allows a number of options for summarizing individual variables. You begin by selecting the variable, statistical unit, state/region, population, domain, and survey year from the selection menu. An interactive plot based on your custom selection will be generated. By mousing over the bars, you will be able to see exact values. Buttons on the top allow you to zoom, change axes and other plot attributes, and finally to export the plot as a PNG file. Within this pane, the TABLE and PLOT tabs allow you to toggle between the plot view and an interactive table view. You can also download the underlying data as CSV or JSON using the buttons on the bottom of the selection menu.</p>
                      <p lang='en'>The data on the Univariate Results tab correspond to tables that are published in official USDA publications. Underneath each plot/table is listed the full citation for the data. Additional information about methods, terminology, and results for the corresponding study can be found in the cited publication.</p>
                      <p lang='en'>The <b>Mapped Results</b> tab provides the same data as the Univariate Results tab, but displayed on an interactive map of the United States. Instead of choosing an individual state/region and variable and seeing data for all levels of that variable, here you choose a specific level of a variable (e.g. 'Individual' as one level of 'Ownership type') and a chloropleth map is generated. This map is interactive; you can zoom, pan, and mouse over states to see exact values and error terms. Variables, levels, populations, and domains are restricted to those that are available at the level of individual states.</p>
                      <p lang='en'>The <b>More/About</b> tab contains more information on the NWOS, including links and citations. You can also download the complete dataset for a survey year."),
                      HTML(paste("<p lang='en'><b>Suggested citation: Caputo, J. and B. Butler. National Woodland Owner Survey Dashboard (NWOS-DASH) version 1.0. Accessed ", Sys.Date(), ".</b></p>",sep=""))
                           ))
                      )

server <- function(input, output, session) {
  
  #make sure LEVEL always corresponds with TABLE (mapping tab)
  observeEvent(tm(),{
    updateSelectInput(session,"l","Level",unique(et$LABEL[et$TABLE==tm()]))
  })
  
  #make sure DOMAIN always corresponds with STATE
  observeEvent(s(),{
    updateSelectInput(session,"d","Domain",unique(et$DOMAIN[et$STATE==s()]), selected="10+")
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
      paste('thousand',tolower(input$um))
    }
  })
  
  #selects desired states
  s <- reactive({
    input$s
  })
  
  #function to subset et based on choices, main tab
  subets <- function(){
    ets <- subset(et,TABLE==input$t & STATE==input$s
                  & DOMAIN==input$d & STRATUM==input$p & YEAR==input$y)
	ets$N <- ets$VALUE[ets$STATISTIC=='N'][match(ets$VARIABLE,ets$VARIABLE)[ets$STATISTIC=='N']] #add N
    #select data based on selected units
    if (input$u=='Acres'){
	  ets <- subset(ets, STATISTIC=='TOTAL' & UNITS=='ACRES')
      ets$VALUE <- round(ets$VALUE/1000,2)
      ets$VARIANCE <- round(sqrt(ets$VARIANCE)/1000,2)
    } else if (input$u=='Ownerships'){
	  ets <- subset(ets, STATISTIC=='TOTAL' & UNITS=='OWNERSHIPS')
      ets$VALUE <- round(ets$VALUE/1000,2)
      ets$VARIANCE <- round(sqrt(ets$VARIANCE)/1000,2)
    } else if (input$u=='Percent of Acres'){
      ets <- subset(ets, STATISTIC=='PROPORTION' & UNITS=='ACRES')
      ets$VALUE <- round(ets$VALUE*100,2)
      ets$VARIANCE <- round(sqrt(ets$VARIANCE)*100,2)
    } else if (input$u=='Percent of Ownerships'){
      ets <- subset(ets, STATISTIC=='PROPORTION' & UNITS=='OWNERSHIPS')
      ets$VALUE <- round(ets$VALUE*100,2)
      ets$VARIANCE <- round(sqrt(ets$VARIANCE)*100,2)
    }
	names(ets)[names(ets)=='VARIANCE'] <- 'SE'
	lv <- ets$LABEL[order(ets$ORDER)] #levels in order
  ets$LABEL <- ordered(ets$LABEL,levels=lv)
  return(ets)
  }
  
  #function to subset et based on choices, map
  subets.map <- function(){
    ets <- subset(et,TABLE==input$tm & LABEL==input$l
                  & DOMAIN==input$dm & STRATUM==input$pm & YEAR==input$ym)
    #select data based on selected units
    if (input$um=='Acres'){
	  ets <- subset(ets, STATISTIC=='TOTAL' & UNITS=='ACRES')
      ets$VALUE <- round(ets$VALUE/1000,2)
      ets$VARIANCE <- round(sqrt(ets$VARIANCE)/1000,2)
    } else if (input$um=='Ownerships'){
	  ets <- subset(ets, STATISTIC=='TOTAL' & UNITS=='OWNERSHIPS')
      ets$VALUE <- round(ets$VALUE/1000,2)
      ets$VARIANCE <- round(sqrt(ets$VARIANCE)/1000,2)
    } else if (input$um=='Percent of Acres'){
      ets <- subset(ets, STATISTIC=='PROPORTION' & UNITS=='ACRES')
      ets$VALUE <- round(ets$VALUE*100,2)
      ets$VARIANCE <- round(sqrt(ets$VARIANCE)*100,2)
    } else if (input$um=='Percent of Ownerships'){
      ets <- subset(ets, STATISTIC=='PROPORTION' & UNITS=='OWNERSHIPS')
      ets$VALUE <- round(ets$VALUE*100,2)
      ets$VARIANCE <- round(sqrt(ets$VARIANCE)*100,2)
    }
    names(ets)[names(ets)=='VARIANCE'] <- 'SE'
    return(ets)
  }
  
  #subset et by year and state (for export)
  subets.year <- function() {
    ets <- subset(et,YEAR==input$yall & STATE==input$sall)
    for (i in 1:nrow(ets)) {
      if (ets$UNITS[i]=='ACRES' & ets$STATISTIC[i]=='TOTAL'){
        ets$VALUE[i] <- round(ets$VALUE/1000,2)
        ets$VARIANCE[i] <- round(sqrt(ets$VARIANCE)/1000,2)
      } else if (ets$UNITS[i]=='OWNERSHIPS' & ets$STATISTIC[i]=='TOTAL'){
        ets$VALUE[i] <- round(ets$VALUE/1000,2)
        ets$VARIANCE[i] <- round(sqrt(ets$VARIANCE)/1000,2)
      } else if (ets$UNITS[i]=='ACRES' & ets$STATISTIC[i]=='PROPORTION'){
        ets$VALUE[i] <- round(ets$VALUE*100,2)
        ets$VARIANCE[i] <- round(sqrt(ets$VARIANCE)*100,2)
      } else if (ets$UNITS[i]=='OWNERSHIPS' & ets$STATISTIC[i]=='PROPORTION'){
        ets$VALUE[i] <- round(ets$VALUE*100,2)
        ets$VARIANCE[i] <- round(sqrt(ets$VARIANCE)*100,2)
      }
    }
    names(ets)[names(ets)=='VARIANCE'] <- 'SE'
    return(ets)
  }
  
  #creates function to find caption for main tab
  maketitle <- function() {
    
    ets <- subets() #subset et
    
    tunits <- ifelse(grepl("Percent",u()),'Proportion of','Number of') #title units
    tunits2 <- ifelse(grepl("Acres",u()),'acres by','ownerships by') #title units 2
    des <- et$DESCRIPTION[match(t(),et$TABLE)] #description
    title <- paste(tunits,tunits2,tolower(des)) #create title
    #title <- paste(title," (Table: ",ets$TNUM[1],")") #add table number
    
    if (!is.na(title)){
      title #prints question text
    }
    
  }
  
  #creates function to identify table for cross-referencing
  cref <- function(){
    
    ets <- subets() #subset et
    
    if (!grepl("NOT PUBLISHED",ets$TNUM[1])){
      paste("This data corresponds to Table ",ets$TNUM[1]," (",input$y,"; ",input$p," ",input$d,") in: Butler Brett J.; Butler, Sarah M.; Caputo, Jesse; Dias, Jaqueline; Robillard, Amanda; Sass, Emma M. 2020. Family Forest Ownerships of the United States, 2018: Results from the USDA Forest Service, National Woodland Owner Survey. General Technical Report. NRS-GTR-199. Madison, WI: U.S. Department of Agriculture, Forest Service, Northern Research Station. DOI:10.2737/NRS-GTR-199.",sep="")
    }
    
  }
  
  #creates function to generate caption for mapping tab
  maketitle.map <- function() {
    
    ets <- subets.map() #subset et

    tunits <- ifelse(grepl("Percent",um()),'Proportion of','Number of') #title units
    tunits2 <- ifelse(grepl("Acres",um()),'acres by','ownerships by') #title units 2
    des <- et$DESCRIPTION[match(tm(),et$TABLE)] #description
    title <- paste(tunits,tunits2,tolower(des)) #create title
    title <- paste(title, ": ",ets$LABEL[1],sep="")
    
    if (!is.na(title)){
      title #prints question text
    }
    
  }
  
  #creates function to make plot
  makeplot <- function() {
    
  #plotting element for use in plotting error bars
  limits <- aes(ymax=(VALUE)+2*(SE),
                ymin=(VALUE)-2*(SE))

  ets <- subets() #subset et
    
  #prints plot
    if (nrow(ets)>0){
      pl<-ggplot(data=ets,aes(x=LABEL,y=VALUE))+
        geom_bar(stat="identity",fill='darkgoldenrod2')+
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
      ets <- ets[,c("LABEL","VALUE","SE","N")]
      names(ets) <- c('Category','Value','StdErr','N')
      ets$Value <- paste(comma(ets$Value,accuracy=0.01), unit())
      ets$StdErr <- paste(comma(ets$StdErr,accuracy=0.01), unit())
      ets$N <- comma(ets$N)
      ets
    }
    
  }
  
  #creates function to make map
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
      setView(-105, 45, 3) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief,
                       options = providerTileOptions(minZoom = 3, maxZoom = 5)) %>%
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
  
  #RENDERS TABLE CROSS-REFERENCE ON SCREEN
  output$tabref <- renderText({
    
    cref()
    
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
  output$downloadCSV <- downloadHandler(filename = "NWOSdashboard_EXPORT.csv",
                                          content = function(file) { 
                                            write.csv(subets()[!names(subets()) %in% c('N',"VAR")],file,row.names=F)
                                            
                                          }
  )
  
  #SAVES TABLE TO JSON FILE
  output$downloadJSON <- downloadHandler(filename = "NWOSdashboard_EXPORT.json",
                                            content = function(file) { 
                                              write_json(subets()[!names(subets()) %in% c('N',"VAR")],file,pretty=T)
                                              
                                            }
                                            
  )
  
  #SAVES TABLE TO CSV FILE (map)
  output$downloadCSVMap <- downloadHandler(filename = "NWOSdashboard_EXPORT.csv",
                                        content = function(file) { 
                                          write.csv(subets.map()[names(subets.map())!='VAR'],file,row.names=F)
                                          
                                        }
  )
  
  #SAVES TABLE TO JSON FILE (map)
  output$downloadJSONMap <- downloadHandler(filename = "NWOSdashboard_EXPORT.json",
                                         content = function(file) { 
                                           write_json(subets.map()[names(subets.map())!='VAR'],file,pretty=T)
                                           
                                         }
                                         
  )
  
  #SAVES TABLE TO CSV FILE (by year)
  output$downloadCSVAll <- downloadHandler(filename = "NWOSdashboard_EXPORT.csv",
                                          content = function(file) { 
                                            write.csv(subets.year(),file,row.names=F)
                                            
                                          }
                                      
  )
  
  #SAVES TABLE TO JSON FILE (by year)
  output$downloadJSONAll <- downloadHandler(filename = "NWOSdashboard_EXPORT.json",
                                             content = function(file) { 
                                               write_json(subets.year(),file,pretty=T)
                                               
                                             }
                                             
  )
  
}

shinyApp(ui=ui,server=server)