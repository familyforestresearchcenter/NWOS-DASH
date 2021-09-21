library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(DT)
library(plotly)

#changing global settings
options(stringsAsFactors = FALSE)

#upload metadata
load("C:/Users/jessecaputo/Dropbox (FFRC)/NWOS/PRODUCTS/NWOS_2018_FFO/NWOS 2018 dashboard/QUEST_METADATA.RData")

#upload estimates table
et <- read.csv("C:/Users/jessecaputo/Dropbox (FFRC)/NWOS/PRODUCTS/NWOS_2018_FFO/NWOS 2018 dashboard/estimates_table_DEMO.csv")
et <- merge(et,QUEST_METADATA,by.x='TABLE',by.y='CN')
cat <- which(et$DATA_TYPE=='FACTOR') #index of categorical variables

#function for parsing UNITS_FACTORS columns into a matrix
par.m <- function(x){matrix(eval(parse(text=x)),ncol=2,byrow=T)}
#function for looking up label based on factor level
add.l <- function(f,fm){par.m(fm)[par.m(fm)[,1]==f,2]}

#add labels for categorical variables
et$LABELS[cat] <- mapply(add.l,et$ROW[cat],et$UNITS_FACTORS[cat])
#add table title
et$tt <- paste(et$TABLE,et$QUESTION_NAME,sep=":")

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
        
        selectInput("t", "Table",unique(et$tt)),
        
        selectInput("u", "Unit",unique(et$UNITS)),
        
        selectInput("s", "State",unique(et$STATE)),
        
        selectInput("p", "Population",unique(et$POP)),
        
        selectInput("d", "Domain",unique(et$DOMAIN)),
        
        selectInput("y", "NWOS Cycle",unique(et$YEAR)),
        
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
  
  observeEvent(t(),{
    updateSelectInput(session,"u","Unit",unique(et$UNITS[et$tt==t()]))
  })
  
  #selects desired table
  t <- reactive({
    input$t
  })
  
  #selects desired units
  u <- reactive({
    input$u
  })
  
  #creates function to find caption
  maketitle <- function() {
    
    #subsets et to chosen variable
    ets <- subset(et,tt==t() & UNITS==u()) #!!!!ADD OTHER VARIABLES
    title <- ets$QUESTION_TEXT[1]
    title <- gsub("<STATE>","your state",title,fixed=T)
    title <- paste("Original question text:",title)
    
    if (nrow(ets)>0){
      title #prints question text
    }
    
  }
  
  #creates function to make plot
  makeplot <- function() {
    
    #subsets et to chosen variable
    ets <- subset(et,tt==t() & UNITS==u()) #!!!!ADD OTHER VARIABLES
    ets <- ets[order(ets$ROW),]
    ets$LABELS <- ordered(ets$LABELS,levels=ets$LABELS)
    
    #prints plot
    if (ets$DATA_TYPE[1] == 'FACTOR' & nrow(ets)>0){
      pl<-ggplot(data=ets,aes(x=LABELS,y=VALUE))+
        geom_bar(stat="identity")+
        geom_errorbar(limits,width=0,size=0.5,position=position_dodge(width=0.90))+
        labs(x="ROW",y=ets$UNITS[1])+
        scale_x_discrete(labels=wrap_format(10))+
        #coord_flip()+
        theme(text=element_text(size=12),
              legend.position="none",
              axis.title.x=element_blank(),
              panel.background=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_rect(fill="transparent"),
              axis.ticks=element_line(color="black"),
              axis.text=element_text(color="black"))
    } else if (ets$DATA_TYPE[1] == 'INTEGER' & nrow(ets)>0) {
      pl<-ggplot(data=ets,aes(x=TABLE,y=VALUE))+
        geom_bar(stat="identity",width=0.3)+
        geom_errorbar(limits,width=0,size=0.5,position=position_dodge(width=0.90))+
        labs(x=ets$UNITS[1],y=ets$UNITS_FACTORS[1])+
        theme(text=element_text(size=12),
              legend.position="none",
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
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
  
  #creates function to make plot
  maketable <- function() {
    
    #subsets et to chosen variable
    ets <- subset(et,tt==t() & UNITS==u()) #!!!!ADD OTHER VARIABLES
    ets <- ets[order(ets$ROW),]
    ets$LABELS <- ordered(ets$LABELS,levels=ets$LABELS)
    
    #prints table
    if (ets$DATA_TYPE[1] == 'FACTOR' & nrow(ets)>0){
      ets[,c("LABELS","VALUE","VAR")]
    } else if (ets$DATA_TYPE[1] == 'INTEGER' & nrow(ets)>0) {
      ets[,c("VALUE","VAR")]
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