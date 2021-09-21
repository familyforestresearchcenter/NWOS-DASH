library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(DT)
library(plotly)
library(leaflet)
library(sp)

#changing global settings
options(stringsAsFactors = FALSE)

#loads formatted estimates and reference tables
load("NWOS_dashboard_DATA.RData")

ui <- navbarPage("National Woodland Owner Survey (NWOS) Dashboard (BETA)",
                 windowTitle="NWOS Dashboard",
                 theme = shinytheme("cosmo"),
  
  
  tabPanel("Univariate Results",
  
    tags$head(
       tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }")
    ),       
           
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput("t", "Table", unique(et$TABLE), selected="Ownership type"),
        
        selectInput("u", "Unit", c('Acres','Ownerships','Percent of Acres','Percent of Ownerships')),
        
        selectInput("s", "State", unique(et$STATE), selected="United States"),
        
        selectInput("p", "Population", unique(et$STRATUM)),
        
        selectInput("d", "Domain", unique(et$DOMAIN), selected="10+"),
        
        selectInput("y", "NWOS Cycle", unique(et$YEAR)),
        
        br(),
        downloadButton('downloadTable', 'Download Data', class = "btn btn-primary")
      ),
      
      
      mainPanel(
        h4(textOutput("caption"),align="center"),
        br(),
        tabsetPanel(type = "tabs",
                    tabPanel("Plot",plotlyOutput("plot", height=500)),
                    tabPanel("Table",br(),dataTableOutput("table"))
        ),
        br(),
        HTML("<p><center><h4>For review purposes only: <b>Do Not Cite</b>. Please send all questions, concerns, and suggestions to <a href='mailto:jessecaputo@eco.umass.edu'>jessecaputo@eco.umass.edu</a>.</h4></center></p>")
        
      )
    )
  ),
  
  tabPanel("Mapping Results",
           
             tags$head(
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }")
             ),       
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput("tm", "Table", unique(et$TABLE), selected="Ownership type"),
                 
                 selectInput("l", "Level", unique(et$LABEL)),
                 
                 selectInput("um", "Unit", c('Percent of Acres','Percent of Ownerships','Acres','Ownerships')),

                 selectInput("pm", "Population", unique(et$STRATUM)),
                 
                 selectInput("dm", "Domain", c('10+','100+')),
                 
                 selectInput("ym", "NWOS Cycle", unique(et$YEAR)),
                 
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
                      HTML("<p><h4>U.S. Forest Service, National Woodland Owner Survey (NWOS)</h4></p>
<p>There are over 800 million acres of forestland in the U.S. and most of this land is privately owned. Approximately 36% of the forestland is owned by families and individuals, 31% by the federal government, 19% by corporations, 9% by state governments, 2% by tribal groups, 2% by other private groups, and 2% by local governments.</p>
<p>The National Woodland Owner Survey (NWOS) is aimed at increasing our understanding of private forest owners, including:</p>
<ul><li>How many private forest owners there are?</li>
<li>Why they own land?</li>
<li>What they have done with their forests in the past?</li>
<li>What they plan to do with their forests in the future?</li></ul>
<p>Summary information from the NWOS is used by people who provide, design, and implement services and policies that impact private forest owners, including government agencies, landowner organizations and other non-governmental organizations, private service providers, business analysts, forest industry companies, and academic researchers.</p>
<p>The National Woodland Owner Survey is implemented by the <a href='http://www.familyforestresearchcenter.org/' target='_blank'>Family Forest Research Center</a>, a joint venture between the U.S. Forest Service and University of Massachusetts Amherst. More information on the NWOS, including summary tables and survey instruments can be found <a href='https://www.fia.fs.fed.us/nwos/' target='_blank'>here</a>.</p>")
                      ),
             tabPanel("Help",
                      HTML("<p><h4>COMING SOON</h4></p>")))
)

server <- function(input, output, session) {
  
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
    
  #plotting element for use in plotting error bars
  limits <- aes(ymax=(VALUE)+2*(VAR),
                ymin=(VALUE)-2*(VAR))

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
  
}

shinyApp(ui=ui,server=server)