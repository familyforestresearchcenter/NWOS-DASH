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
load("../NWOS_dashboard_DATA.RData")

shinyUI(navbarPage("National Woodland Owner Survey (NWOS) Dashboard",
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
                    tabPanel("Plot",plotlyOutput("plot", height=600)),
                    tabPanel("Table",br(),dataTableOutput("table"))
        )
        
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
))