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
et <- et[et$INC==1,] #constrain to those that should be included (n=50+)
et$STRATUM <- ordered(et$STRATUM,levels=c('FFO','SCORP','LCORP'),labels=c('Family','Small corporate','Large corporate'))

#changes in terminology due to administrative requirements
et$VARIABLE <- gsub("GENDER","SEX",et$VARIABLE) #rename gender to sex
et$TABLE <- gsub("gender","sex",et$TABLE)
et$DESCRIPTION <- gsub("gender","sex",et$DESCRIPTION)
# et <- et[et$VARIABLE!="CNC_CLIM",] #remove climate change question

ui <- navbarPage(title=div(img(src="usdalogo.svg",alt="United States Department of Agriculture logo",height=42),"National Woodland Owner Survey Dashboard (NWOS-DASH)"),
                 windowTitle="NWOS-DASH",
                 collapsible = FALSE,
                 theme = shinytheme("cosmo"),
  
  tabPanel("Univariate Results",
  
    tags$head(
       includeHTML(("google-analytics.html")),
       tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }",
                #".navbar-nav > li > a, .navbar-brand {padding-top:4px !important; padding-bottom:0 !important; height: 100px; }",
                #".navbar-brand { height: 100px; line-height: 70px; }",
                #".navbar { min-height: 100px !important; }",
                ".navbar-brand { padding: 2px 5px; line-height: 46px; }",
                ".btn-primary, .btn-primary:hover, .btn-primary:active, .btn-primary:visited, .btn-primary:focus, .btn-primary:active:focus { background-color: #1d76d9; border-color: #1d76d9; }")
    ),
           
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput("t", "Variable", unique(et$TABLE[et$STRATUM=='FFO'&et$YEAR=='2018']), selected="Size of forest holdings"),
        
        selectInput("u", "Unit", c('Acres','Ownerships','Percent of Acres','Percent of Ownerships')),
        
        selectInput("s", "State", unique(et$STATE), selected="United States"),
        
        selectInput("p", "Population", unique(et$STRATUM)),
        
        selectInput("d", "Domain (acres)", unique(et$DOMAIN)),
        
        selectInput("y", "NWOS Cycle", unique(et$YEAR)),
        
        downloadButton('downloadCSV', 'CSV', class = "btn btn-primary"),
        downloadButton('downloadJSON', 'JSON', class = "btn btn-primary"),
        br(),
        HTML("<p style='font-size:90%; line-height:98%; padding-top:3px;' lang='en'><i>Note: Clicking download buttons will initiate the download of external file(s).</i></p>"),
        br(),
        img(src="usfslogo.svg",alt="United States Forest Service logo",height=42,style="padding-right:5px; float:left;"),
        HTML("<p style='line-height:98%; padding-top:10px;' lang='en'><b>USDA Forest Service</br>Forest Inventory and Analysis</b></p>")
        
      ),
      
      
      mainPanel(
        h4(textOutput("caption"),align="center"),
        br(),
        tabsetPanel(type = "tabs",
                    tabPanel("Plot",plotlyOutput("plot", height=450),htmlOutput("tabref")),
                    tabPanel("Table",br(),dataTableOutput("table"))
        )
        
      )
    )
  ),
  
  tabPanel("Bivariate Results",
           mainPanel(
             HTML("<p lang='en'><h4>Bivariate Results</h4></p>
                        <p lang='en'>NWOS-DASH will eventually include full capability for bivariate cross-tabulation.</p>")
           )
  ),
  
  tabPanel("Mapped Results",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput("tm", "Variable", unique(et$TABLE[et$STRATUM=='FFO'&et$YEAR=='2018']), selected="Size of forest holdings"),
                 
                 selectInput("l", "Level", unique(et$LABEL)),
                 
                 selectInput("um", "Unit", c('Percent of Acres','Percent of Ownerships','Acres','Ownerships')),

                 selectInput("pm", "Population", unique(et$STRATUM[et$STATE%in%states@data$STATE_NWOS_ALPHA])),
                 
                 selectInput("dm", "Domain (acres)", unique(et$DOMAIN)),
                 
                 selectInput("ym", "NWOS Cycle", unique(et$YEAR)),
                 
                 downloadButton('downloadCSVMap', 'CSV', class = "btn btn-primary"),
                 downloadButton('downloadJSONMap', 'JSON', class = "btn btn-primary"),
                 br(),
                 HTML("<p style='font-size:90%; line-height:98%; padding-top:3px;' lang='en'><i>Note: Clicking download buttons will initiate the download of external file(s).</i></p>"),
                 br(),
                 img(src="usfslogo.svg",alt="United States Forest Service logo",height=42,style="padding-right:5px; float:left;"),
                 HTML("<p style='line-height:98%; padding-top:10px;' lang='en'><b>USDA Forest Service</br>Forest Inventory and Analysis</b></p>")
                 
               ),
               
               
               mainPanel(
                 h4(textOutput("caption.map"),align="center"),
                 br(),
                 leafletOutput("map", height=600)
                 
               )
             )
           ),
  
  navbarMenu("Help",
             tabPanel("Getting Started",
                      HTML("<p lang='en'><h4>Getting started with NWOS-DASH</h4></p>
                      <p lang='en'>NWOS-DASH is the official data visualization and retrieval tool for the USDA Forest Service's National Woodland Owner Survey (NWOS). Here you can obtain population-level estimates of landowners' objectives, activities, demographics and more &mdash; summarized by state or region. For all variables, you can view output in terms of the number or percentage of either acres or ownerships. For example, you can view the total number of ownerships that have a management plan or the percentage of acres owned by people who have harvested timber in the past five years. In addition, you can filter by domain, which is defined as the minimum size of forest holdings owned by an ownership for inclusion in a population-level estimate. For example, you can view the number of ownerships owning 10 or more acres who have a management plan, or the number of ownerships owning 100 or more acres who have harvested timber. The smallest domain that meets the Forest Service definition of 'Forest' is 1+ acres.</p>
                      <p lang='en'>The <b>Univariate Results</b> tab on the navigation bar allows a number of options for summarizing individual variables. You begin by selecting the variable, statistical unit, state/region, population, domain, and survey year from the selection menu. An interactive plot based on your custom selection will be generated. By mousing over the bars, you will be able to see exact values. Buttons on the top allow you to zoom, change axes and other plot attributes, and finally to export the plot as a PNG file. Within this pane, the TABLE and PLOT tabs allow you to toggle between the plot view and an interactive table view. You can also download the underlying data as CSV or JSON using the buttons on the bottom of the selection menu.</p>
                      <p lang='en'>The <b>Mapped Results</b> tab provides the same data as the Univariate Results tab, but displayed on an interactive map of the United States. Instead of choosing an individual state/region and variable and seeing data for all levels of that variable, here you choose a specific level of a variable (e.g. 'Individual' as one level of 'Ownership type') and a chloropleth map is generated. This map is interactive; you can zoom, pan, and mouse over states to see exact values and error terms. Variables, levels, populations, and domains are restricted to those that are available at the level of individual states.</p>
					            <p lang='en'>The <b>Help/Glossary</b> tab contains definitions for a short list of technical terms.</p>
                      <p lang='en'>The <b>About</b> tab contains more information on the NWOS, including links and citations. You can also download the complete dataset for a survey year and/or geography.</p>
                      <p lang='en'>On most tabs, you will have the option of downloading data as either or CSV or JSON files. These machine-readable files contain the same summary-level data displayed in the dashboard &mdash; as opposed to raw data (i.e. individual survey response), which are not available without proper credentials. These summary files may be useful for those interested in constructing plots and tables or running custom analyses offline.</p>"), 
                      HTML(paste("<p lang='en'><b>Suggested citation: Caputo, J. and B. Butler. National Woodland Owner Survey Dashboard (NWOS-DASH) version 1.0. Accessed ", Sys.Date(), ".</b></p>",sep=""))
                           ),
			tabPanel("Glossary",
					HTML("<p lang=''><h4>Glossary</h4></p>
					<ul lang='en'>
					<li><b>Domain</b>: The minimum size of forest holdings that an ownership must own in order to be included in a population-level estimate.</li>
					<li><b>Forest</b>: Forest or woodland, defined as \"land that has at least 10 percent crown cover by live tally trees of any size or has had at least 10 percent canopy cover of live tally species in the past, based on the presence of stumps, snags, or other evidence. To qualify, the area must be at least 1.0 acre in size and 120.0 feet wide\".</li>
					<li><b>FFO/Family</b>: Family forest ownership, an ownership composed of individuals, families, and trusts that owns 1+ acres of forested land.</li>
					<li><b>Small corporate</b>: Small corporate ownership, which may include clubs, associations, conservation groups, other NGOs, and corporations or businesses owning <i>less</i> than 45 thousand acres nationwide.</li>
					<li><b>Large corporate</b>: Large corporate ownership, corporations or businesses owning <i>more</i> than 45 thousand acres nationwide. An early analysis of large corporate ownerships is included in <a href='https://research.fs.usda.gov/treesearch/53826' target='_blank'>this article (opens in new window)</a>.</li>
					<li><b>LLC</b>: Limited Liability Company.</li>
					<li><b>LLP</b>: Limited Liability Partnership.</li>
					</ul>"))),
   
  tabPanel("About", 
                      sidebarLayout(
                        
                        sidebarPanel(
                          HTML("<p lang='en'><b>Download complete NWOS summary dataset(s):</b></p>"),
                          
                          selectInput("yall", "NWOS Cycle", unique(et$YEAR)),
                          
                          selectInput("sall", "State", c('All Geographies',unique(et$STATE)), selected="All Geographies"),
                          
                          downloadButton('downloadCSVAll', 'CSV', class = "btn btn-primary"),
                          downloadButton('downloadJSONAll', 'JSON', class = "btn btn-primary"),
                          br(),
                          HTML("<p style='font-size:90%; line-height:98%; padding-top:3px;' lang='en'><i>Note: Clicking download buttons will initiate the download of external file(s).</i></p>"),
                          br(),
                          img(src="usfslogo.svg",alt="United States Forest Service logo",height=42,style="padding-right:5px; float:left;"),
                          HTML("<p style='line-height:98%; padding-top:10px;' lang='en'><b>USDA Forest Service</br>Forest Inventory and Analysis</b></p>")
                        ),
                        
                      mainPanel(
                        HTML("<p lang='en'><h4>About the National Woodland Owner Survey (NWOS)</h4></p>
                        <p lang='en'>There are over 800 million acres of forestland in the U.S. and most of this land is privately owned. Approximately 36% of the forestland is owned by families and individuals, 31% by the federal government, 19% by corporations, 9% by state governments, 2% by tribal groups, 2% by other private groups, and 2% by local governments.</p>
                        <p lang='en'>The National Woodland Owner Survey (NWOS) is aimed at increasing our understanding of private forest owners, including:</p>
                        <ul><li>How many private forest owners there are?</li>
                        <li>Why they own land?</li>
                        <li>What they have done with their forests in the past?</li>
                        <li>What they plan to do with their forests in the future?</li></ul>
                        <p lang='en'>Summary information from the NWOS is used by people who provide, design, and implement services and policies that impact private forest owners, including government agencies, landowner organizations and other non-governmental organizations, private service providers, business analysts, forest industry companies, and academic researchers.</p>
                        <p lang='en'>The National Woodland Owner Survey is implemented by the <a href='http://www.familyforestresearchcenter.org/' target='_blank'>Family Forest Research Center (opens in new window)</a>, a joint venture between the USDA Forest Service, Forest Inventory and Analysis Program and the University of Massachusetts Amherst. More information on the NWOS, including official reports, summary tables, and survey instruments can be found on the <a href='https://research.fs.usda.gov/programs/nwos' target='_blank'>NWOS website (opens in new window)</a>.</p>"),
                        HTML(paste("<p lang='en'><b>Suggested citation: Caputo, J. and B. Butler. National Woodland Owner Survey Dashboard (NWOS-DASH) version 1.0. Accessed ", Sys.Date(), ".</b></p>",sep=""))
                      )
                    )
                  )
    )

server <- function(input, output, session) {
  
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
    } else if (input$u=='Ownerships'&input$p=='Large corporate'){
      tolower(input$u)
    } else {
      paste('thousand',tolower(input$u))
    }
  })
  unit.map <- reactive({ #for mapping
    if (grepl('Percent',input$um)){
      'percent'
    } else if (input$um=='Ownerships'&input$pm=='Large corporate'){
      tolower(input$um)  
    } else {
      paste('thousand',tolower(input$um))
    }
  })
  
  #selects desired states
  s <- reactive({
    input$s
  })
  
  #selects desired domain
  d <- reactive({
    input$d
  })
  
  #selects desired year
  y <- reactive({
    input$y
  })
  ym <- reactive({
    input$ym
  })
  
  #selects desired population
  p <- reactive({
    input$p
  })
  pm <- reactive({
    input$pm
  })
  
  #year on ABOUT tab
  yall <- reactive({
    input$yall
  })
  
  #update dropdown choices dynamically
  
  observeEvent(y(),{ #when year is changed
    updateSelectInput(session,"t","Variable",unique(et$TABLE[et$YEAR==y()&et$STRATUM==p()]), selected="Size of forest holdings")
    updateSelectInput(session,"p","Population",unique(et$STRATUM[et$YEAR==y()]))
  })
  observeEvent(p(),{ #when stratum is changed
    updateSelectInput(session,"d","Domain (acres)",unique(et$DOMAIN[et$STRATUM==p()]))
    updateSelectInput(session,"t","Variable",unique(et$TABLE[et$STRATUM==p()&et$YEAR==y()]), selected="Size of forest holdings")
    updateSelectInput(session,"s","State",unique(et$STATE[et$STRATUM==p()]), selected="United States")
  })
  observeEvent(s(),{ #when state is changed
    updateSelectInput(session,"d","Domain (acres)",unique(et$DOMAIN[et$STATE==s()]))
  })
  
  observeEvent(ym(),{ #when year is changed (map tab)
    updateSelectInput(session,"tm","Variable",unique(et$TABLE[et$YEAR==y()&et$STRATUM==pm()]), selected="Size of forest holdings")
    updateSelectInput(session,"pm","Population",unique(et$STRATUM[et$YEAR==ym()&et$STATE%in%states@data$STATE_NWOS_ALPHA]))
    updateSelectInput(session,"l","Level",unique(et$LABEL[et$TABLE==tm()&et$YEAR==ym()&et$STRATUM==pm()]))
  })
  observeEvent(pm(),{ #when stratum is changed (map tab)
    updateSelectInput(session,"dm","Domain (acres)",unique(et$DOMAIN[et$STRATUM==pm()]))
    updateSelectInput(session,"tm","Variable",unique(et$TABLE[et$STRATUM==pm()&et$YEAR==ym()]), selected="Size of forest holdings")
    updateSelectInput(session,"l","Level",unique(et$LABEL[et$TABLE==tm()&et$YEAR==ym()&et$STRATUM==pm()]))
  })
  observeEvent(tm(),{ #when variable is changed (map tab)
    updateSelectInput(session,"l","Level",unique(et$LABEL[et$TABLE==tm()&et$YEAR==ym()&et$STRATUM==pm()]))
  })
  
  observeEvent(yall(),{ #when year is selected (download tab)
    updateSelectInput(session,"sall","State",c('All Geographies',unique(et$STATE[et$YEAR==yall()])), selected="All Geographies")
  })
  
  #function to subset et based on choices, main tab
  subets <- function(){
    ets <- subset(et,TABLE==input$t & STATE==input$s
                  & DOMAIN==input$d & STRATUM==input$p & YEAR==input$y)
	  ets$N <- ets$VALUE[ets$STATISTIC=='N'][match(paste(ets$VARIABLE,ets$LEVEL),paste(ets$VARIABLE,ets$LEVEL)[ets$STATISTIC=='N'])] #add N
    #select data based on selected units
    if (input$u=='Acres'){
	    ets <- subset(ets, STATISTIC=='TOTAL' & UNITS=='THOUSAND ACRES')
    } else if (input$u=='Ownerships'){
	    ets <- subset(ets, STATISTIC=='TOTAL' & UNITS %in% c('THOUSAND OWNERSHIPS','OWNERSHIPS'))
    } else if (input$u=='Percent of Acres'){
      ets <- subset(ets, STATISTIC=='PERCENTAGE' & UNITS=='THOUSAND ACRES')
    } else if (input$u=='Percent of Ownerships'){
      ets <- subset(ets, STATISTIC=='PERCENTAGE' & UNITS %in% c('THOUSAND OWNERSHIPS','OWNERSHIPS'))
    }
	lv <- ets$LABEL[order(ets$ORDER)] #levels in order
  ets$LABEL <- ordered(ets$LABEL,levels=lv,labels=lv)
  return(ets)
  }
  
  #function to subset et based on choices, map
  subets.map <- function(){
    ets <- subset(et,TABLE==input$tm & LABEL==input$l
                  & DOMAIN==input$dm & STRATUM==input$pm & YEAR==input$ym)
    #select data based on selected units
    if (input$um=='Acres'){
	    ets <- subset(ets, STATISTIC=='TOTAL' & UNITS=='THOUSAND ACRES')
    } else if (input$um=='Ownerships'){
	    ets <- subset(ets, STATISTIC=='TOTAL' & UNITS %in% c('THOUSAND OWNERSHIPS','OWNERSHIPS'))
    } else if (input$um=='Percent of Acres'){
      ets <- subset(ets, STATISTIC=='PERCENTAGE' & UNITS=='THOUSAND ACRES')
    } else if (input$um=='Percent of Ownerships'){
      ets <- subset(ets, STATISTIC=='PERCENTAGE' & UNITS %in% c('THOUSAND OWNERSHIPS','OWNERSHIPS'))
    }
    return(ets)
  }
  
  #subset et by year and state (for export)
  subets.year <- function() {
    if (input$sall=='All Geographies'){
      ets <-  subset(et,YEAR==input$yall)
    } else {
      ets <- subset(et,YEAR==input$yall & STATE==input$sall)
    }
    return(ets)
  }
  
  #creates function to find caption for main tab
  maketitle <- function() {
    
    ets <- subets() #subset et
    
    tunits <- ifelse(grepl("Percent",u()),'Percentage of','Number of') #title units
    tunits2 <- ifelse(grepl("Acres",u()),'acres by','ownerships by') #title units 2
    des <- et$DESCRIPTION[match(t(),et$TABLE)] #description
    title <- paste(tunits,tunits2,des) #create title

    if (!is.na(title)){
      title #prints question text
    }
    
  }
  
  #creates function to identify table for cross-referencing
  cref <- function(){
    
    ets <- subets() #subset et
    
    if (!grepl("NOT PUBLISHED",ets$TNUM[1])){
      #HTML(paste("<h5 align='justify' lang='en'>These data correspond to Table ",ets$TNUM[1]," (",input$y,"; ",input$p," ",input$d,") in: Butler Brett J.; Butler, Sarah M.; Caputo, Jesse; Dias, Jaqueline; Robillard, Amanda; Sass, Emma M. 2020. Family Forest Ownerships of the United States, 2018: Results from the USDA Forest Service, National Woodland Owner Survey. General Technical Report. NRS-GTR-199. Madison, WI: U.S. Department of Agriculture, Forest Service, Northern Research Station. <a href='https://doi.org/10.2737/NRS-GTR-199' target='_blank'>DOI:10.2737/NRS-GTR-199 (opens in new window)</a>.</h5>",sep=""))
    }
    HTML("<h5 align='justify' lang='en'>NOTE: plotted error bars are equal to two standard errors.</h5>")
    
  }
  
  #creates function to generate caption for mapping tab
  maketitle.map <- function() {
    
    ets <- subets.map() #subset et

    tunits <- ifelse(grepl("Percent",um()),'Percentage of','Number of') #title units
    tunits2 <- ifelse(grepl("Acres",um()),'acres by','ownerships by') #title units 2
    des <- et$DESCRIPTION[match(tm(),et$TABLE)] #description
    title <- paste(tunits,tunits2,des) #create title
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
  ets$Value <- ifelse(ets$VALUE==0,'<1',comma(ets$VALUE))
  ets$StdErr <- ifelse(ets$SE==0,'<1',comma(ets$SE))
    
  #prints plot
    if (nrow(ets)>0){
      pl<-ggplot(data=ets,aes(x=LABEL,y=VALUE,label=Value,label2=StdErr))+
        geom_bar(stat="identity",fill='darkgoldenrod2')+
        geom_errorbar(limits,width=0,linewidth=0.5,position=position_dodge(width=0.90))+
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
      ggplotly(pl,tooltip=c("label","label2"))
    }
    
  }
  
  #creates function to make table
  maketable <- function() {
    
    ets <- subets() #subset et
    ets
    
    #prints table
    if (nrow(ets)>0){
      ets <- ets[,c("LABEL","VALUE","SE","N")]
      names(ets) <- c('Category','Value','StdErr','N')
      ets$Value <- ifelse(ets$Value==0,paste('<1',unit()),
          paste(comma(ets$Value), unit()))
      ets$StdErr <- ifelse(ets$StdErr==0,paste('<1',unit()),
        paste(comma(ets$StdErr), unit()))
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
    states@data$SE <- ets$SE[match(states@data$STATE_NWOS_ALPHA,
                                         ets$STATE)]
    
    #create bins
    nonull <- na.omit(states@data$VALUE)
    if (length(nonull)>0){
      bins <- c(floor(min(nonull)),
                1:8*((max(nonull)-min(nonull))/9)+
                  min(nonull),
                ceiling(max(nonull)))
      bins <- round(bins)
      bins <- unique(bins) #unique bins
    } else {
      bins <- 0 #if there are no values in bin, create one
    }
    if (all(bins==0)){bins <- c(0:1)} #1 bin for zeros
    if (all(bins==100)){bins <- c(100:101)} #1 bin for hundreds 
    pal <- colorBin("YlOrRd", domain = states$VALUE, bins = bins)
    
    #create labels
    labform <- "<strong>%s</strong><br/>%s UNIT<br/>SE = %s UNIT"
    labform <- gsub("UNIT",unit.map(),labform)
    labels <- sprintf(
      labform,
      states$STATE_NWOS_ALPHA, comma(states$VALUE), comma(states$SE))
    labels <- ifelse(is.na(states$VALUE),
                     paste("<strong>",states$STATE_NWOS_ALPHA,"</strong><br/>no data",sep=""),
                     labels
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
                values = ~!is.na(VALUE),
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
  output$tabref <- renderUI({
    
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
                                            exp <- subets()[!names(subets()) %in% c('N')]
                                            exp$VALUE <- ifelse(exp$VALUE==0,'<1',exp$VALUE)
                                            exp$SE <- ifelse(exp$SE==0,'<1',exp$SE)
                                            write.csv(exp,file,row.names=F)
                                            
                                          }
  )
  
  #SAVES TABLE TO JSON FILE
  output$downloadJSON <- downloadHandler(filename = "NWOSdashboard_EXPORT.json",
                                            content = function(file) {
                                              exp <- subets()[!names(subets()) %in% c('N')]
                                              exp$VALUE <- ifelse(exp$VALUE==0,'<1',exp$VALUE)
                                              exp$SE <- ifelse(exp$SE==0,'<1',exp$SE)
                                              write_json(exp,file,pretty=T)
                                              
                                            }
                                            
  )
  
  #SAVES TABLE TO CSV FILE (map)
  output$downloadCSVMap <- downloadHandler(filename = "NWOSdashboard_map_EXPORT.csv",
                                        content = function(file) {
                                          exp <- subets.map()
                                          exp$VALUE <- ifelse(exp$VALUE==0,'<1',exp$VALUE)
                                          exp$SE <- ifelse(exp$SE==0,'<1',exp$SE)
                                          write.csv(exp,file,row.names=F)
                                          
                                        }
  )
  
  #SAVES TABLE TO JSON FILE (map)
  output$downloadJSONMap <- downloadHandler(filename = "NWOSdashboard_map_EXPORT.json",
                                         content = function(file) {
                                           exp <- subets.map()
                                           exp$VALUE <- ifelse(exp$VALUE==0,'<1',exp$VALUE)
                                           exp$SE <- ifelse(exp$SE==0,'<1',exp$SE)
                                           write_json(exp,file,pretty=T)
                                           
                                         }
                                         
  )
  
  #SAVES TABLE TO CSV FILE (full)
  output$downloadCSVAll <- downloadHandler(filename = "NWOSdashboard_full_EXPORT.csv",
                                          content = function(file) {
                                            exp <- subets.year()
                                            exp$VALUE <- ifelse(exp$VALUE==0,'<1',exp$VALUE)
                                            exp$SE <- ifelse(exp$SE==0,'<1',exp$SE)
                                            write.csv(exp,file,row.names=F)
                                            
                                          }
                                      
  )
  
  #SAVES TABLE TO JSON FILE (full)
  output$downloadJSONAll <- downloadHandler(filename = "NWOSdashboard_full_EXPORT.json",
                                             content = function(file) {
                                               exp <- subets.year()
                                               exp$VALUE <- ifelse(exp$VALUE==0,'<1',exp$VALUE)
                                               exp$SE <- ifelse(exp$SE==0,'<1',exp$SE)
                                               write_json(exp,file,pretty=T)
                                               
                                             }
                                             
  )
  
}

shinyApp(ui=ui,server=server)