library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(DT)
library(plotly)

#changing global settings
options(stringsAsFactors = FALSE)

#loads formatted estimates and reference tables
load("C:/Users/jessecaputo/Dropbox (FFRC)/NWOS/PRODUCTS/NWOS_2018_FFO/NWOS 2018 dashboard/NWOS_dashboard_data.RData")

#plotting element for use in plotting error bars
limits <- aes(ymax=(VALUE)+2*(VAR),
              ymin=(VALUE)-2*(VAR))

ui <- navbarPage("National Woodland Owner Survey (NWOS) Dashboard",
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
        
        selectInput("t", "Table", unique(et$TABLE)),
        
        selectInput("u", "Unit", c('Acres','Owners','Percent of Acres','Percent of Owners')),
        
        selectInput("s", "State", unique(et$STATE)),
        
        selectInput("p", "Population", unique(et$STRATUM)),
        
        selectInput("d", "Domain", unique(et$DOMAIN)),
        
        selectInput("y", "NWOS Cycle", unique(et$YEAR)),
        
        br(),
        downloadButton('downloadTable', 'Download Data', class = "btn btn-primary")
      ),
      
      
      mainPanel(
        h4(textOutput("caption"),align="center"),
        br(),
        tabsetPanel(type = "tabs",
                    tabPanel("Plot",plotlyOutput("plot", height=600)),
                    tabPanel("Table",br(),dataTableOutput("table"))
        )
        
      )
    )
  ),
  
  #tabPanel("Crosstabs"),
  
  navbarMenu("More",
             tabPanel("About",
                      HTML("Some text about the NWOS, including 
                           links to the <a href='https://www.fia.fs.fed.us/nwos/'>NWOS</a>
                           and <a href='http://www.familyforestresearchcenter.org/'>FFRC</a> pages.")
                      ),
             tabPanel("Help"))
)

server <- function(input, output, session) {
  
  #observeEvent(t(),{
  #  updateSelectInput(session,"u","Unit",unique(et$UNITS[et$tt==t()]))
  #})
  
  #selects desired table
  t <- reactive({
    input$t
  })
  
  #selects desired units for plots and tables
  unit <- reactive({
    if (grepl('Percent',input$u)){
      'percent'
    } else {
      paste('thousand',tolower(input$u))
    }
  })
  
  #function to subset et based on choices
  subets <- function(){
    ets <- subset(et,TABLE==input$t & STATE==input$s
                  & DOMAIN==input$d & STRATUM==input$p & YEAR==input$y)
    #add labels to ets
    UK <- paste(ets$VARIABLE,ets$LEVEL,sep="_")
    UK2 <- paste(rl$ï..VARIABLE,rl$LEVEL,sep="_")
    ets$LABEL <- rl$LABEL[match(UK,UK2)] #add labels
    lv <- ets$LABEL[order(rl$ORDER[match(UK,UK2)])] #levels in order
    ets$LABEL <- ordered(ets$LABEL,levels=lv)
    #select data based on selected units
    if (input$u=='Acres'){
      ets$VALUE <- ets$ACRES/1000
      ets$VAR <- sqrt(ets$ACRES_VARIANCE)/1000
    } else if (input$u=='Owners'){
      ets$VALUE <- ets$OWNERSHIPS/1000
      ets$VAR <- sqrt(ets$OWNERSHIPS_VARIANCE)/1000
    } else if (input$u=='Percent of Acres'){
      ets$VALUE <- ets$ACRES_PROPORTION*100
      ets$VAR <- sqrt(ets$ACRES_PROPORTION_VARIANCE)*100
    } else if (input$u=='Percent of Owners'){
      ets$VALUE <- ets$OWNERSHIPS_PROPORTION*100
      ets$VAR <- sqrt(ets$OWNERSHIPS_PROPORTION_VARIANCE)*100
    }
    return(ets)
  }
  
  #creates function to find caption
  maketitle <- function() {
    
    hd <- rt$VIZ_HEADER[match(t(),rt$VIZTABLE)]
    nm <- rt$ï..TABLE_NUMBER[match(t(),rt$VIZTABLE)] #identify title
    title <- paste(hd,' (Table #',nm,')',sep='') #create title
    
    if (!is.na(title)){
      title #prints question text
    }
    
  }
  
  #creates function to make plot
  makeplot <- function() {

  ets <- subets() #subset et  
    
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
      ets <- ets[,c("LABEL","VALUE","VAR")]
      names(ets) <- c('Category','Value','StdErr')
      ets$Value <- paste(comma(ets$Value,accuracy=0.01), unit())
      ets$StdErr <- paste(comma(ets$StdErr,accuracy=0.01), unit())
      ets
    }
    
  }
  
  #RENDERS TITLE ON SCREEN
  output$caption <- renderText({
    
    maketitle()
    
  })
  
  #RENDERS PLOT ON SCREEN
  output$plot <- renderPlotly({
    
    makeplot()
    
  })
  
  #RENDERS TABLE ON SCREEN
  output$table <- renderDataTable({
    
    maketable()
    
  })
  
  #SAVES TABLE TO CSV FILE
  output$downloadTable <- downloadHandler(filename = function() {paste('NWOS2018', Sys.time(),'.csv', sep="")},
                                          content = function(file) { 
                                            write.csv(maketable(),file)
                                            
                                          }
  )
  
}

shinyApp(ui=ui,server=server)