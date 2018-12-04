#
# This is a Shiny web application brought t by. 
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(xlsx)

# To read all the data
co2waste <- read.xlsx("GHG_Emissions_by_Sector.xlsx","GHG2015_cleaned",startRow = 2,endRow = 179,colIndex = c(2,4,9,10,11,12,14),header = T)
mswGeneration <- read.xlsx("World bank income and msw per capita.xlsx","Sheet1",startRow = 2,endRow = 163,colIndex = c(1:7),header = T)
recyclePercentage <- read.xlsx("PercentageMunicipalWaste_recycled.xlsx","Cleaned",startRow = 1, endRow = 58, colIndex = c(2,24,25),header = T)
wasteComposition <- read.xlsx("Waste Composition.xlsx","Sheet1",startRow = 1,endRow = 32,colIndex = c(1:6),header = T)

co2waste$range  <- cut(co2waste$Total.GHG.Emissions.in.MMTCDE,  #categorised the emission values
                       breaks = c(0,5,25,100,500,7500),right = FALSE, 
                       labels = c("0-5","5-25","25-100","100-500","500-7500"))

pal <- colorFactor(c("blue","green","yellow","orange","red"),co2waste$range) #create a color pallette for the category created 

co2waste$percentageWaste  <- co2waste$GHGfromWaste / co2waste$Total.GHG.Emissions.in.MMTCDE * 100 #calculate the percentage of CO2 emission by waste

top20CO2waste <- co2waste %>% select(Country,percentageWaste) %>%  arrange(desc(percentageWaste)) %>% top_n(20,percentageWaste)

co2waste$percentagerange  <- cut(co2waste$percentageWaste,  #categorised the percentage values and create a color pallete
                       breaks = c(0,5,10,20,30,100),right = FALSE, 
                       labels = c("[0-5)","[5-10)","[10-20)","[20-30)","[30-100)"))

pal2 <- colorFactor(c("blue","green","yellow","orange","red"),co2waste$percentagerange)

#calculate the average waste generation based on income level
mswEconomy <- mswGeneration %>% group_by(Income.Level) %>% summarise(Average.MSW.Generation.Per.Capita.kg.day = mean(MSW.Generation.Per.Capita..kg.capita.day.)) %>% arrange(desc(Average.MSW.Generation.Per.Capita.kg.day))


# Define User Interface for the application
ui <- fluidPage(
  
  title = "How Malaysia is doing in municipal waste management? A global comparison.",              
  tags$head(
    
    tags$style(  #css
      "
      h1 {
      text-align:center;
      }

      .centered {
      position: absolute;;
      top: 10%;
      left: 50%;
      transform: translate(-50%, -50%);
      text-shadow:
      -1px -1px 0 #000,
      1px -1px 0 #000,
      -1px 1px 0 #000,
      1px 1px 0 #000;
      color: white;
      } 
      
     
     "
    )
  ),
  
  tags$div(img(src='global-environmental-law-and-governance-1600x600.jpg',height="300px",width="100%"),
           tags$div(h1(class="centered","How Malaysia is doing in municipal waste management? A global comparison.")),
  
  tags$div(
  
           tabsetPanel(
              tabPanel("CO2 generation from waste",column(8,leafletOutput("mymapCO2",height = 500),fluidRow(verbatimTextOutput("map_marker_click"))),
                       column(4,dataTableOutput("CO2emission")),
                       column(8,plotOutput("CO2waste")),
                       column(4,h3("Malaysia is among the top 20 countries in CO2 emission by waste!"))
                       ),
              tabPanel("Economy vs Amount of Municipal Waste Generation",
                       h2("Based on data from World Bank, the richer the country, the higher the amount of waste generated."),
                       column(6,plotOutput("MSW")),
                       column(6,dataTableOutput("MSWrank"))
                       ),
              tabPanel("% of Municipal waste Recycled",
                       column(6,plotOutput("Recycle")),
                       column(6,dataTableOutput("Recyclerank"))
                       ),
              tabPanel("Malaysia Waste Composition",
                       column(6,plotOutput("Composition")),
                       column(6,dataTableOutput("Compositionrank"))
                       ),
              tabPanel("What can we do as a solution?",
                       column(6,h3("1. First of all, do not waste food!"),
                       tags$ul(tags$li("When we waste food, remember that we contribute to global warming!"),
                               tags$li("We are also wasting all the energy and water it takes to grow, harvest, transport, and package it.")),
                       img(src="foodwastegraph.JPG"),
                       h3("2. Plan ahead and buy only what you need."),
                       tags$ul(tags$li("Going to the store without a plan or on an empty stomach can lead to buying more than we need."),
                               tags$li("Avoid unnecessary purchases by planning your grocery list ahead of time.")),
                       h3("3. Be creative with left over"),
                       tags$ul(tags$li("Before you shop, use food you already have."),
                               tags$li("Websites like Big Oven, Supercook, and MyFridgeFood allow you to search for recipes based on ingredients already in your kitchen.")),
                       h3("4. Make DIY Compost!"),
                       tags$ul(tags$li("Home composting is a process that uses natural decomposition to transform landscape and kitchen waste into a rich soil amendment that does wonders for a garden."),
                               tags$li("Turning garbage into green cabbage!"),
                               tags$li(tags$a(href="https://www.diynetwork.com/how-to/outdoors/gardening/how-to-make-compost", "A simple guide to DIY compost!")))),
                       column(4,offset = 2,img(src="antifoodwaste2.jpg", height = "50%" ,width="70%"),
                                img(src="antifoodwaste.jpg"),
                              tags$br(),
                              tags$br(),
                              img(src="fromgarbagetogarden.jpg",width="70%")
                       )
              ),
              tabPanel("Video",tags$video(id="video", type = "video/mp4",src = "awareness.mp4", controls = "controls"),height=500)
              
          )
  )
  )
  
)
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #plot the map
  output$mymapCO2 <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(group = "OSM",
               options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
      
      setMaxBounds( lng1 = -165
                    , lat1 = -90
                    , lng2 = 180
                    , lat2 = 90 ) %>%
      
      addCircleMarkers(lng = co2waste$Long, 
                       lat = co2waste$Lat, weight = 3, 
                       radius = (co2waste$Total.GHG.Emissions.in.MMTCDE)/50, 
                       popup = co2waste$Label,
                       color = pal(co2waste$range),
                       group = "co2 emission"
                       ) %>%
      
      addCircleMarkers(lng = co2waste$Long, 
                       lat = co2waste$Lat, weight = 3, 
                       radius = (co2waste$percentageWaste), 
                       popup = co2waste$Label,
                       color = pal2(co2waste$percentagerange),
                       group = "co2 percentage by waste"
      ) %>%
      
      addLegend("bottomleft", pal = pal, values = co2waste$range,
                title = "million metric tonnes of carbon dioxide equivalents",
                labFormat = labelFormat(suffix  = " mmtcde"),
                opacity = 0.5,
                group = "co2 emission"
                ) %>%
    
      addLegend("bottomleft", pal = pal2, values = co2waste$percentagerange,
              title = "percentage of CO2 emission by waste",
              labFormat = labelFormat(suffix  = "%"),
              group = "co2 percentage by waste",
              opacity = 0.5
      ) %>%
      
      addLayersControl(
        overlayGroups = c("co2 emission", "co2 percentage by waste"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    m
    
  })
  
  #to print the label of the markers on the map when clicked
  observeEvent(input$mymap_marker_click, { 
    p <- input$map_marker_click
    print(p)
  })
  
  #output table
  output$CO2emission <- renderDataTable(co2waste %>% select(Country,Total.GHG.Emissions.in.MMTCDE,Rank) %>%  arrange(desc(Total.GHG.Emissions.in.MMTCDE)),options = list(lengthMenu = c(5,10), pageLength = 10))
  
  #output bar chart    
  output$CO2waste <- renderPlot({
    par(mar=c(10, 4.1, 4.1, 2.1))
    barplot(top20CO2waste$percentageWaste, 
            main="Top 20 CO2 emission by waste",
            ylab="Percentage",
            names.arg = top20CO2waste$Country,
            las=2)
    
  }
    
  )
  
  #output bar chart  
  output$MSW <- renderPlot({
    barplot(mswEconomy$Average.MSW.Generation.Per.Capita.kg.day, 
            main="Average MSW Generation (kg/capita/day)",
            ylab="kg/capita/day",
            ylim= c(0,3),
            names.arg = mswEconomy$Income.Level)
    
  }
  
  )
  
  #output data table
  output$MSWrank <- renderDataTable(mswGeneration %>% select(Country,Income.Level,MSW.Generation.Per.Capita..kg.capita.day.,Rank)%>%  arrange(desc(MSW.Generation.Per.Capita..kg.capita.day.)),options = list(lengthMenu = c(5,10), pageLength = 10))
  
  #output bar chart  
  output$Recycle <- renderPlot({
    
    barplot(recyclePercentage$Percentage.Recycled.in.2015, 
            main="Percentage of Municipal Waste Recycled in 2015",
            ylab="percentage",
            names.arg = recyclePercentage$Country,
            las =2)
    
  }
  
  )
  
  #output data table
  output$Recyclerank <- renderDataTable(recyclePercentage %>% select(Country,Percentage.Recycled.in.2015,Rank),options = list(lengthMenu = c(5,10), pageLength = 10))
  
  #output bar chart
  output$Composition <- renderPlot({
    par(mar=c(14, 4.1, 4.1, 2.1))
    barplot(wasteComposition$Percentage, 
            main="Malaysia Municipal Waste Composition",
            ylim=c(0,50),
            names.arg = wasteComposition$Waste.Components,
            las =2
            )
            
    
  }
  
  )
  
  #output data table
  output$Compositionrank <- renderDataTable(wasteComposition %>% select(Type,Waste.Components,Percentage),options = list(lengthMenu = c(5,10), pageLength = 10))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

