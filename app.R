#Author: Pauline Cairns   Date: 8/8/19
#Runner file to create dashboard


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


#inspiration for dashboard items and code structure -  https://shiny.rstudio.com/gallery/
#all sourced from the packages install facility within RStudio
#in turn these are sourced from the cran repository 
#https://cran.r-project.org/ 
#Load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(zoo)
library(xts)
library(dygraphs)

#Load data
source('filter.R')

#set up messages, notifications, and tasks - currently list of items fixed
messages <- dropdownMenu(type = "messages",
                         
                         messageItem(
                           from = "Tug",
                           message = "Maintencance request - check fuel",
                           icon = icon("ship")
                         ),
                         messageItem(
                           from = "New User",
                           message = "Looking to berth on 29th December",
                           icon = icon("question"),
                           time = "2017-12-05 13:45"
                         ),
                         messageItem(
                           from = "Alert",
                           message = "Port oversubscribed",
                           icon = icon("bell"),
                           time = "2017-12-01"
                         )
)

notifications<-dropdownMenu(type = "notifications",
                            notificationItem(
                              text = "5 new users today",
                              icon("users")
                            ),
                            notificationItem(
                              text = "12 cars delivered ready for load onto vessel",
                              icon("truck"),
                              status = "success"
                            ),
                            notificationItem(
                              text = "Speed of vessel approaching - too fast",
                              icon = icon("exclamation-triangle"),
                              status = "warning"
                            )
)
tasks<-dropdownMenu(type = "tasks", badgeStatus = "success",
                    taskItem(value = 95, color = "green",
                             "All visitors safety briefing instructions updated"
                    ),
                    taskItem(value = 50, color = "aqua",
                             "Idle time project"
                    ),
                    
                    taskItem(value = 40, color = "red",
                             "Dashboard project"
                    ))


# Create UI of dashboard
ui <- dashboardPage(
  
   
  #Set up header and header contents
  dashboardHeader(
    
  # Application title
  title="AIS - NE Scotland",
  #info in header bar
  messages,notifications, tasks
  ),
  
  #Set up sidebar with tab options
  dashboardSidebar(
      
    sidebarMenu(
      id="tabs",
      #list of tabs
      #icons from https://fontawesome.com/icons?d=gallery
      menuItem("Dashboard - Home", tabName="dashboard", icon=icon("dashboard")),
      menuItem("Ports", tabName= "ports", icon = icon("anchor")),
      menuItem("Routes", tabName="routes", icon = icon("map-pin")),
      menuItemOutput("menuitem")
      
      #end of dashboardsidebar - closing bracket 
    )
      
   #end of sidebarmenu -  closing bracket  
    ),
   
   
  
  #Set up body of dashboard
  dashboardBody(
    
   # set up bookmark facility
  #bookmarkButton()
    
    #link body content to tab selected from sidebar
    tabItems(
       #set up body of dashboard-home tab
       tabItem(tabName="dashboard",
             #set up date range
              fluidRow(dateRangeInput("inDateRange4", "Date range input: dd-mm-yyyy", 
                                                    format = "dd/mm/yy", start= "2016-07-01", end="2017-12-04",
                                                    min= "2016-07-01", max="2017-12-04")),
              
               #set up three port boxes with dropdown area selection
              fluidRow(
               column(width=4,
                box(
                  title = "Aberdeen", width=NULL, status = "primary",solidHeader = TRUE,
                  collapsible = TRUE, collapsed=TRUE,
                  selectInput(inputId="aberdeen_pic_port", 
                                       label= "Choose area:",
                                       choices = c("Aberdeen", 
                                                   "Aberdeen_extended")),
                  "Unique vessels",
                  infoBoxOutput("uniquevesselsaberdeen")
                )),
               
                column(width=4,
                  box(title = "Montrose", width=NULL, status = "primary",solidHeader = TRUE,
                      collapsible = TRUE, collapsed=TRUE,
                      selectInput(inputId="montrose_pic_port", 
                      label= "Choose a port:",
                      choices = c("Montrose", 
                      "Montrose_extended")),
                      "Unique vessels",
                       infoBoxOutput("uniquevesselsmontrose")
                 )),
    
               column(width=4, 
                 box(title = "Peterhead", width=NULL, status = "primary",solidHeader = TRUE,
                     collapsible = TRUE, collapsed=TRUE,
                     selectInput(inputId="peterhead_pic_port", 
                         label= "Choose a port:",
                         choices = c("Peterhead", 
                                     "Peterhead_extended")),
                     "Unique vessels",
                     infoBoxOutput("uniquevesselspeterhead"))  
                )),
             
             
           #set up graph 
           fluidRow(
              box(title = "Unique vessels within port boundaries", width=12, status = "primary",solidHeader = TRUE,
              collapsible = TRUE, 
              dygraphOutput("ves_graph2", height=400),
              h5("      Slider: shows total data period with highlighted section showing
                 section of data currently on display in the window"))
              )

        #dashboard-home tabitem closing bracket
        ),

        #ports tab -  body setup
        tabItem(
          tabName = "ports",
        #dropdown for port selection & dropdown for date selection
         fluidRow(
            box( width=6,
                 selectInput(inputId="pick_ves_port", 
                 label= "Choose a port:",
                 choices = c("Montrose","Montrose_extended", "Aberdeen", 
                           "Aberdeen_extended", "Peterhead", "Peterhead_extended"
                             )
                             )
                ),
            box(width=6,
                dateRangeInput("inDateRange", "Date range input: dd-mm-yyyy", 
                             format = "dd/mm/yy", start= "2016-07-01", end="2017-12-04",
                             min= "2016-07-01", max="2017-12-04")
               )
            ),
        #set up three info boxes - actual, target, progress
        fluidRow(
           infoBoxOutput("uniquevessels"), infoBoxOutput("target"),
           infoBoxOutput("progressBox")
          ),
        #set up graph
        fluidRow(
           box(title = "Unique vessels in period", width=12, status = "primary",solidHeader = TRUE,
               collapsible = TRUE, collapsed=TRUE,
               dygraphOutput("ves_graph", height=300)
               )
          ),
       #set up table
        fluidRow(  
          box(collapsible = TRUE,collapsed=TRUE, width=12,status = "primary", solidHeader = TRUE,title = "Duration in port", 
              DT::dataTableOutput("port_table")
          )),
      #set up map
        fluidRow(
         box(width=12, solidHeader = TRUE, collapsible = TRUE, status="primary", 
              title="Map of port area", collapsed=TRUE,
              dateInput("inDateRange3", "Select date:", 
                        format = "dd/mm/yy", value= "2016-07-01",
                        min= "2016-07-01", max="2017-12-04"),
              selectInput(inputId="pick_ves_port2", 
                      label= "Choose an area:",
                      choices = c("Montrose", "Aberdeen", 
                                  "Peterhead")
                      ),
              leafletOutput("ves_harbour")
           )
        )

    #ports tab -closing bracket
    ),
      # routes tab setup
       tabItem(
         tabName="routes",
          
                    h2("Routes"), 
       #setup date range
         dateRangeInput("inDateRange2", "Select period between 1 June to 14 June 2017", 
                        format = "dd/mm/yy", start= "2017-06-01", end="2017-06-14",
                        min= "2017-06-01", max="2017-06-14")
         ,
       #set up call to vessel type and vessel dropdown lists
       fluidRow(column(width=6,
         
        box(width=NULL,
          
          uiOutput("type_selected"),
            uiOutput("ves_selected"),
          br()
         )
       ),
       #reset map button
       column(width=3,
              box(width = NULL,
                actionButton("reset", "Reset map")
              ))),
       #show map
       fluidRow(
         column( width = 12,  leafletOutput("scotmap", width='750px', height='650px'))
       )
    
      #routes tab closing bracket
       )  
    
    #tabitems - closing bracket
    )
    
  #closing dashboardbody - closing bracket  
  )
  #dashboardpage - closing bracket
)
  
    

server <- function(input, output, session) {
  
#map -create and populate vessel type dropdown list
  output$type_selected <- renderUI({
    selectInput("pick_type", "Choose a vessel type:",
                choices= c(Choose="", sort(unique(all_vessels$description))))
  
    
  })
  
#map -create and populate vessel name dropdown list
  output$ves_selected <- renderUI({
    vessel_list<-all_vessels[all_vessels$description == input$pick_type, "Name"]
    selectInput("pick_ves", "Choose a vessel:",
                choices= c(Choose = "", sort(unique(vessel_list))
                                            )
    )
  })
   
#map- create map and add markers based on selection above  
   output$scotmap <- renderLeaflet({
     # inspiration taken from https://rstudio.github.io/leaflet/map_widget.html
     req(input$inDateRange2)
     uniq_name_ves<-filter(all_vessels, date(all_vessels$RecvTime) >=input$inDateRange2[1], date(all_vessels$RecvTime)<= input$inDateRange2[2],
                           all_vessels$Name==input$pick_ves)
     fullmap<- leaflet()
     fullmap <- setView(fullmap, lng = -2, lat = 57, zoom = 8)
  #   fullmap<- fitBounds(fullmap,lng1 =-8.1, lat1=55.1, lng2=1.1, lat2=61)
     fullmap<- addTiles(fullmap, group = "OSM (default)")
     fullmap<-addRectangles(fullmap, group ="Harbour boundaries", lng1=-2.096, lat1=57.176, lng2=-1.978 , lat2=57.101, weight=1, label="Aberdeen Harbour limits")
     fullmap<-addRectangles(fullmap, group="Harbour boundaries", lng1=-2.476, lat1=56.708,
                           lng2=-2.413, lat2=56.701,weight=1,
                           label="Montrose Port limits")
     fullmap<-addRectangles(fullmap, group="Harbour boundaries", lng1=-1.795, lat1=57.512,
                          lng2=-1.735, lat2=57.472,weight=1,
                         label="Peterhead harbour limits")
   #Layers control
     fullmap<-addLayersControl(fullmap,
                         baseGroups = c("OSM (default)"),
                       overlayGroups = c("Harbour boundaries"),
                       options = layersControlOptions(collapsed = FALSE)
                      
    )
    #to allow colour of markers to change
    a<- 1/length(uniq_name_ves$Latitude)
    b<- 0
    #draw markers
     for(i in 1:length(uniq_name_ves$Latitude)){
       b<- b+a
       fullmap <- addCircles(fullmap, lng=uniq_name_ves$Longitude[i], lat= uniq_name_ves$Latitude[i], radius=15,
                             label = input$pick_ves, fillColor="red", fillOpacity = b, color="red",opacity = b )
                             
     }
     fullmap

   })

   #map - reset button
   observeEvent(input$reset, 
                {leafletProxy("scotmap") %>%
                    setView(lng = -2, lat = 57, zoom = 8)
                })
   
   #ports - set up ability for datasetInput function to change based on port selected
   datasetInput <- reactive ({
     switch(input$pick_ves_port,

            "Montrose" = montrose,
            "Montrose_extended" = montrose_extended,
            "Aberdeen" = aberdeen,
           "Aberdeen_extended" = aberdeen_extended,
            "Peterhead" = peterhead,
           "Peterhead_extended" = peterhead_extended
     )
   })
   
   #ports map section - set up ability for dataset input function to change based on port selected
   datasetInput2 <- reactive ({
     switch(input$pick_ves_port2,
            
            "Montrose" =  montrose_extended,
            "Aberdeen"  = aberdeen_extended,
            "Peterhead" = peterhead_extended
     )
   })
   
  #homepage - set up ability for dataset input function to change based on port selected
   montrosesetInput <- reactive ({
     switch(input$montrose_pic_port,
            
            "Montrose" = montrose,
            "Montrose_extended" = montrose_extended
            
     )
   })
   
   aberdeensetInput <- reactive ({
     switch(input$aberdeen_pic_port,
            
            "Aberdeen" = aberdeen,
            "Aberdeen_extended" = aberdeen_extended
            
     )
   })
   
   peterheadsetInput <- reactive ({
     switch(input$peterhead_pic_port,
            
            "Peterhead" = peterhead,
          "Peterhead_extended" = peterhead_extended
            
     )
   })
  
    
  #ports - set up info box counting number of unique vessels
   output$uniquevessels <- renderInfoBox({
     req(input$inDateRange)
     count_in_port <- datasetInput() %>%
       filter(date(RecvTime) >=input$inDateRange[1], date(RecvTime)<= input$inDateRange[2])
     count_in_port<- length(unique(count_in_port$MMSI))
     
     infoBox(
       "Unique vessels",
        count_in_port,
       "in period",
       icon = icon("ship"),
       color = "blue"
     )
   })
   #ports - progress box
   output$target <- renderInfoBox({
     req(input$inDateRange)
     count_in_port <- datasetInput() %>%
       filter(date(RecvTime) >=input$inDateRange[1], date(RecvTime)<= input$inDateRange[2])
     count_in_port<- length(unique(count_in_port$MMSI))
     infoBox(
       "Target", paste0(as.integer(count_in_port*1.2)), icon = icon("list"),
       "unique vessels",
       color = "teal"
     )
   })
   
   
   #homepage - set up info box for aberdeen counting number of unique vessels
   output$uniquevesselsaberdeen <- renderInfoBox({
     req(input$inDateRange4)
     count_in_port_ab <- aberdeensetInput() %>%
       #mutate(Date = str_sub(RecvTime, 1,10)) %>% # convert thtr_rel_date to Date format
       filter(date(RecvTime) >=input$inDateRange4[1], date(RecvTime)<= input$inDateRange4[2])
     count_in_port_ab<- length(unique(count_in_port_ab$MMSI))
     infoBox(
       "Unique vessels",
       count_in_port_ab,
       "in period",
       icon = icon("ship"),
       color = "blue", 
       width=NULL
     )
   })
   #homepage - set up info box for montrose counting number of unique vessels
   output$uniquevesselsmontrose <- renderInfoBox({
     req(input$inDateRange4)
     count_in_port_mn <- montrosesetInput() %>%
       filter(date(RecvTime) >=input$inDateRange4[1], date(RecvTime)<= input$inDateRange4[2])
     count_in_port_mn<- length(unique(count_in_port_mn$MMSI))
     infoBox(
       "Unique vessels",
       count_in_port_mn,
       "in period",
       icon = icon("ship"),
       color = "blue"
     )
   })
  # homepage - set up info box for peterhead counting number of unique vessels
   output$uniquevesselspeterhead <- renderInfoBox({
     req(input$inDateRange4)
     count_in_port_ph <- peterheadsetInput() %>%
       filter(date(RecvTime) >=input$inDateRange4[1], date(RecvTime)<= input$inDateRange4[2])
     count_in_port_ph<- length(unique(count_in_port_ph$MMSI))
     infoBox(
       "Unique",
       count_in_port_ph,
       "in period",
       icon = icon("ship"),
       color = "blue"
     )
   })
  
  #ports - create graph 
  output$ves_graph<-renderDygraph({
    # inspiration taken from https://rstudio.github.io/dygraphs/index.html
    req(input$inDateRange)
    date1<- input$inDateRange[1]
    date2<- input$inDateRange[2]
    #Works to calculate the number of distinct vessel
    no_of_vessels <- datasetInput() %>% group_by(date(RecvTime)) %>%
      summarize(distinct_vessels= n_distinct(MMSI))
    #Make into a time series
    no_of_vessels.ts <- xts(no_of_vessels[,2, drop=FALSE], order.by=as.Date(no_of_vessels$`date(RecvTime)`))
    #Draw on graph
    dygraph(no_of_vessels.ts, main="Number of unique vessels each day") %>% 
      dyRangeSelector(dateWindow =c(date1, date2)) %>%
      dyOptions(drawPoints = TRUE, pointSize = 2)
    
  })
  
  #ports- createmap 
    output$ves_harbour<- renderLeaflet({
      # inspiration taken from https://rstudio.github.io/leaflet/map_widget.html
     req(input$inDateRange3)
     port_selected <- datasetInput2() %>%
     filter(date(RecvTime) ==input$inDateRange3)
     m <- leaflet()
     m <- setView(m, lng =mean(port_selected$Longitude), lat = mean(port_selected$Latitude), zoom = 10)
     m <- addTiles(m, group = "OSM (default)")
     m<-addRectangles(m, group="Harbour boundaries", lng1=-2.476, lat1=56.708,
                     lng2=-2.413, lat2=56.701,weight=1,
                     label="Montrose Port limits")
     m<-addRectangles(m, group="Harbour boundaries", lng1=-1.795, lat1=57.512,
                      lng2=-1.735, lat2=57.472,weight=1,
    label="Peterhead harbour limits")
    m<-addRectangles(m, group ="Harbour boundaries", lng1=-2.096, lat1=57.176, 
                    lng2=-1.978 , lat2=57.101, weight=1, 
                    label="Aberdeen Harbour limits")
    m <-addRectangles(m, group="Extended boundaries", lng1=-2.476, lat1=56.758,
                          lng2=-2.367, lat2=56.651,weight=1
                          #,label="Montrose extended"
                     )
    m<-addRectangles(m, group="Extended boundaries", lng1=-1.795, lat1=57.542,
                          lng2=-1.695, lat2=57.442,weight=1
                          #,label="Peterhead extended"
                    )
   m<-addRectangles(m, group ="Extended boundaries", lng1=-2.096, lat1=57.189, 
                          lng2=-1.947, lat2=57.089, weight=1
                          #,label="Aberdeen extended"
                    )
   m<-addLayersControl(m,
                       baseGroups = c("OSM (default)"),
                       overlayGroups = c("Harbour boundaries", "Extended boundaries"),
                        options = layersControlOptions(collapsed = FALSE), 
                       position="bottomright")
   for(i in 1:length(port_selected$Latitude)){
     m <- addCircles(m, group = "OSM (default)", lng=port_selected$Longitude[i], lat= port_selected$Latitude[i],
                     label= port_selected$Name[i])
        }
      m
  
  }
  )
  
   #ports -table
   output$port_table <- DT::renderDataTable(DT::datatable({
     req(input$inDateRange)
     names_in_port<- datasetInput()%>%
     filter(date(RecvTime)>=input$inDateRange[1], date(RecvTime)<= input$inDateRange[2])
     #remove excess row and arrange order
     names_in_port<- names_in_port[, -c(3:8,11)]
     names_in_port<- names_in_port %>% arrange(MMSI, RecvTime, NavigationalStatus)
     #set up new column changed - all cells given same initial value
     names_in_port$changed<-1
     #change status of changed if nav status goes from stopped to moving (99 will mean starting to move)
     for(c in 1:(length(names_in_port$NavigationalStatus)-1)){
     if((names_in_port$NavigationalStatus[c]==1 | 
         names_in_port$NavigationalStatus[c]==5) & (names_in_port$NavigationalStatus [c+1]==0))
     {names_in_port$changed[c+1]<-99}
     }
     #change status of changed if nav status goes from moving to stopped (89 will mean stopping)
     for(d in 1:(length(names_in_port$NavigationalStatus)-1)){
     if((names_in_port$NavigationalStatus[d+1]==1 | 
         names_in_port$NavigationalStatus[d+1]==5) & (names_in_port$NavigationalStatus [d]==0))
     {names_in_port$changed[d]<-89}
     }
     #remove excess rows where nav status moving and not about to change
     names_in_port<-names_in_port%>% filter((NavigationalStatus==0 & changed==89)|
                                            NavigationalStatus==5 | NavigationalStatus==1 |
                                            (NavigationalStatus==0 & changed==99))
     #new column set up called Date to take only date element of RecvTime
     names_in_port$Date<- as.Date(names_in_port$RecvTime)
     #remove RecvTime column as currently not required
     names_in_port$RecvTime<- NULL
     #set up new columns and populate all cells with the same value
     names_in_port$Stopped_moving<- as.Date("2000-01-01")
     names_in_port$Started_moving<- as.Date("2000-01-01")
     #identify dates stopped moving 
     for(p in 1:(length(names_in_port$changed)-1)){
     if(names_in_port$changed[p+1]==1 & names_in_port$changed[p]==89){
       names_in_port$Stopped_moving[p+1]<-names_in_port$Date[p+1]
       }
     }
     #idenfify dates stareted to move
     for(q in 1:(length(names_in_port$changed)-1)){
     if(names_in_port$changed[q]==1 & names_in_port$changed[q+1]==99){
       names_in_port$Started_moving[q]<-names_in_port$Date[q]
     }
     }
     #as dataset starts at 1 July 2016, may be some vessels stopped before that date and not yet moving
     #set default stopped date to date data starts
     for(r in 1:length(names_in_port$Date)){
     if(names_in_port$Date[r]=="2016-07-01" & (names_in_port$NavigationalStatus[r]==5 &
                                               names_in_port$changed[r]==1)){
       names_in_port$Stopped_moving[r]<- names_in_port$Date[r]
     }
    }
    #if both stopping and starting show no change in date - remove
    names_in_port<- names_in_port%>% filter((Stopped_moving!="2000-01-01" | 
                                             Started_moving != "2000-01-01"))
    #move stopping date up a row
    for(u in 1:(length(names_in_port$Date)-1)){
     if(names_in_port$Stopped_moving[u]== "2000-01-01"){
       names_in_port$Started_moving[u-1]<- names_in_port$Started_moving[u]
     }
    }
    #remove rows excess rows
   names_in_port<- names_in_port %>% filter(Stopped_moving != "2000-01-01")
   #data set ends at 4 Dec 2017 - for those vessels stopped at 4 Dec, show them moving on 5 Dec
   #only required as can't currently get the column to show blank fields - needs to be fix
   for(v in 1:length(names_in_port$Date)){
     if(names_in_port$Started_moving[v]=="2000-01-01"){
       names_in_port$Started_moving[v]<-"2017-12-05"
    }
   }
   #remove excess columns
   names_in_port$changed<- NULL
   names_in_port$NavigationalStatus<- NULL
   #duration
   names_in_port$Duration_in_days<- names_in_port$Started_moving - names_in_port$Stopped_moving
   #remove excess columns
   names_in_port$Date<-NULL
   names_in_port
  

   }))
   
   #port - target box - made up target of 120% of current amount of unique vessels - as with most targets it can never be met!
   output$progressBox <- renderInfoBox({
    req(input$inDateRange)
    count_in_port <- datasetInput() %>%
      filter(date(RecvTime) >=input$inDateRange[1], date(RecvTime)<= input$inDateRange[2])
    count_in_port<- length(unique(count_in_port$MMSI))
    infoBox(
      "Progress", paste0(as.integer(count_in_port/(count_in_port*1.2)*100), "%"), icon = icon("list"),
      color = "purple"
    )
  })
   
 
  #date rangeinputs - set up to render a change if date changed 
  reactive({
  input$inDateRange
  })
  
  reactive({
    input$inDateRange4
  })
  
  #homepage - draw graph
  output$ves_graph2<-renderDygraph({

   # inspiration taken from https://rstudio.github.io/dygraphs/index.html
    req(input$inDateRange4)
    date1<- input$inDateRange4[1]
    date2<- input$inDateRange4[2]
    #Works to calculate the number of distinct vessel
   
    
    no_of_vessels2 <-montrose %>% group_by(date(RecvTime)) %>%
      summarize(montrose= n_distinct(MMSI))
    #Make into a time series
    no_of_vessels2.ts <- xts(no_of_vessels2[,2, drop=FALSE], order.by=as.Date(no_of_vessels2$`date(RecvTime)`))
    no_of_vessels3 <-peterhead %>% group_by(date(RecvTime)) %>%
      summarize(peterhead= n_distinct(MMSI))
    #Make into a time series
    no_of_vessels3.ts <- xts(no_of_vessels3[,2, drop=FALSE], order.by=as.Date(no_of_vessels3$`date(RecvTime)`))
    no_of_vessels4<-aberdeen %>% group_by(date(RecvTime)) %>%
      summarize(aberdeen= n_distinct(MMSI))
    #Make into a time series
    no_of_vessels4.ts <- xts(no_of_vessels4[,2, drop=FALSE], order.by=as.Date(no_of_vessels4$`date(RecvTime)`))
    allports<- cbind(no_of_vessels2.ts,no_of_vessels4.ts, no_of_vessels3.ts)
    
    #Draw on graph
    dygraph(allports, main="Number of unique vessels each day") %>% 
      dyRangeSelector(dateWindow =c(date1, date2)) %>%
      dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2, 
                  highlightSeriesOpts = list(strokeWidth = 1.5)
                  )%>% 
      dyOptions(colors =c("steelblue", "seagreen", "blueviolet") )
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")





  
