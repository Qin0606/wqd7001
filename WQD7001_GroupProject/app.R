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
library(ggplot2)
library(waffle)
library(RColorBrewer)

# To read all the data
ghgEmission <- read.xlsx("GHG_Emissions_by_Sector.xlsx","GHG2015_cleaned",startRow = 2,endRow = 179,colIndex = c(2,4,9,10,11,12,14),header = T)
mswGeneration <- read.xlsx("World bank income and msw per capita.xlsx","Sheet1",startRow = 2,endRow = 163,colIndex = c(1:7),header = T)
recyclePercentage <- read.xlsx("PercentageMunicipalWaste_recycled.xlsx","Cleaned",startRow = 1, endRow = 58, colIndex = c(2,24,25),header = T)
wasteComposition <- read.xlsx("Waste Composition.xlsx","Sheet1",startRow = 1,endRow = 32,colIndex = c(1:6),header = T)

ghgEmission$range  <- cut(ghgEmission$Total.GHG.Emissions.in.MMTCDE,  #categorised the emission values
                       breaks = c(0,5,25,100,500,7500),right = FALSE, 
                       labels = c("0-5","5-25","25-100","100-500","500-7500"))

pal <- colorFactor(c("blue","green","yellow","orange","red"),ghgEmission$range) #create a color pallette for the category created 

ghgEmission$percentageWaste  <- ghgEmission$GHGfromWaste / ghgEmission$Total.GHG.Emissions.in.MMTCDE * 100 #calculate the percentage of GHG emission by waste

top20GHGwaste <- ghgEmission %>% select(Country,percentageWaste) %>%  arrange(desc(percentageWaste)) %>% top_n(20,percentageWaste)

ghgEmission$percentagerange  <- cut(ghgEmission$percentageWaste,  #categorised the percentage values and create a color pallete
                       breaks = c(0,5,10,20,30,100),right = FALSE, 
                       labels = c("[0-5)","[5-10)","[10-20)","[20-30)","[30-100)"))

pal2 <- colorFactor(c("blue","green","yellow","orange","red"),ghgEmission$percentagerange)

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
  
  tags$div(img(src='header11.jpg',height="300px",width="100%"),
           
  
  tags$div(
  
           tabsetPanel(
              tabPanel("Greenhouse Gas generation from waste",column(8,leafletOutput("mymapGHG",height = 500),fluidRow(verbatimTextOutput("map_marker_click"))),
                       column(4,dataTableOutput("GHGemission")),
                       column(8,plotOutput("GHGByWastePercentage")),
                       column(4,h3("Malaysia is among the top 20 countries in GHG emission by percentage of waste over total emission !!!"))
                       ),
              tabPanel("Economy vs Amount of Municipal Waste Generation",
                       h2("Based on data from World Bank, the richer the country, the higher the amount of waste generated."),
                       column(6,plotOutput("MSW")),
                       column(6,dataTableOutput("MSWrank"))
                       ),
              tabPanel("% of Municipal waste Recycled",
                       column(6,plotOutput("Recycle"), style="overflow-y: scroll;overflow-x: scroll;"),
                       column(6,dataTableOutput("Recyclerank"))
                       ),
              tabPanel("Malaysia Waste Composition",
                       column(8,plotOutput("Composition")),
                       column(4,dataTableOutput("Compositionrank"))
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
              tabPanel("Video",tags$video(id="video", type = "video/mp4",src = "awareness.mp4", controls = "controls"),height=500),
              tabPanel("Documentation") #Jiunn Jye to add code inside the tabPanel bracket
              
          )
  )
  )
  
)
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #plot the map
  output$mymapGHG <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(group = "OSM",
               options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
      
      setMaxBounds( lng1 = -165
                    , lat1 = -90
                    , lng2 = 180
                    , lat2 = 90 ) %>%
      
      addCircleMarkers(lng = ghgEmission$Long, 
                       lat = ghgEmission$Lat, weight = 3, 
                       radius = (ghgEmission$Total.GHG.Emissions.in.MMTCDE)/50, 
                       popup = ghgEmission$Label,
                       color = pal(ghgEmission$range),
                       group = "GHG emission"
                       ) %>%
      
      addCircleMarkers(lng = ghgEmission$Long, 
                       lat = ghgEmission$Lat, weight = 3, 
                       radius = (ghgEmission$percentageWaste), 
                       popup = ghgEmission$Label,
                       color = pal2(ghgEmission$percentagerange),
                       group = "GHG percentage by waste"
      ) %>%
      
      addLegend("bottomleft", pal = pal, values = ghgEmission$range,
                title = "million metric tonnes of carbon dioxide equivalents",
                labFormat = labelFormat(suffix  = " mmtcde"),
                opacity = 0.5,
                group = "GHG emission"
                ) %>%
    
      addLegend("bottomleft", pal = pal2, values = ghgEmission$percentagerange,
              title = "percentage of GHG emission by waste",
              labFormat = labelFormat(suffix  = "%"),
              group = "GHG percentage by waste",
              opacity = 0.5
      ) %>%
      
      addLayersControl(
        overlayGroups = c("GHG emission", "GHG percentage by waste"),
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
  output$GHGemission <- renderDataTable(ghgEmission %>% select(Country,Total.GHG.Emissions.in.MMTCDE,Rank) %>%  arrange(desc(Total.GHG.Emissions.in.MMTCDE)),options = list(lengthMenu = c(5,10), pageLength = 10))
  
  #output bar chart    
  output$GHGwaste <- renderPlot({
    ggplot(top20GHGwaste[1:20,], aes(x=reorder(Country, -percentageWaste),y=percentageWaste)) + geom_text (label= round(top20GHGwaste$percentageWaste,1),position=position_dodge(width=0.9), vjust=-0.25) +geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5, size=22),axis.text=element_text(size=12))+labs(title="Top 20 Country (Percentage of GHG Emission by Waste",x = "Top 20 Country", y="Percentage of GHG Emission by Waste")
    
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
    
    ggplot(recyclePercentage, aes(x=reorder(Country, -Percentage.Recycled.in.2015),y=Percentage.Recycled.in.2015)) + geom_text (label= recyclePercentage$Rank,position=position_dodge(width=0.9), vjust=-0.25) +geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),plot.title = element_text(hjust = 0.5, size=22),axis.text=element_text(size=12))+labs(title="Percentage or Waste Recycled in 2015",x = "Country", y="Percentage of Waste Recycled")
    
    
    
  },
  height=500,
  width=1000
  )
  
  #output data table
  output$Recyclerank <- renderDataTable(recyclePercentage %>% select(Country,Percentage.Recycled.in.2015,Rank),options = list(lengthMenu = c(5,10), pageLength = 10))
  
  #output bar chart
  output$Composition <- renderPlot({
    component <- wasteComposition$Percentage
    names(component) <- wasteComposition$Waste.Components
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    waffle(component,colors = sample(col_vector,length(component)),xlab = "Types of Waste", title = "Malaysia Municipal Solid Waste Composition") + theme(plot.title = element_text(hjust = 0.5,size = 27, face = "bold", colour = "darkred"), legend.text = element_text(size = 15),axis.title.x = element_text(size=18))
  }
  
  )
  
  #output data table
  output$Compositionrank <- renderDataTable(wasteComposition %>% select(Type,Waste.Components,Percentage),options = list(lengthMenu = c(5,10), pageLength = 10)) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

