#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Data and application developed by J Blanford, 2019

library(shiny)
library(leaflet)
library(ggplot2)
library(rgdal)
library(maptools)
library(spatstat)

library(DT)
library(raster)

#---------------change directory to where you saved the data----------------
#LatLong
#used in leaflet map
Pumpbnd <-readOGR("C:/Pumpbndll.shp", layer = "Pumpbndll", GDAL1_integer64_policy = FALSE)
pumpsllcsv <- read.csv("C:/pumpsll.csv", header=TRUE,sep=",")
attach(pumpsllcsv)
choleradeath <- read.csv("C:/choleradeathspumps_ll.csv", header=TRUE,sep=",")
attach(choleradeath)

#projected 
choleradeathprj2 <- read.csv("C:/Choleradeaths2prj.csv", header=TRUE,sep=",")
attach(choleradeathprj2)

#used for distancebased analysis
studybndprj <-readOGR("C:/StudyareaBnd.shp", layer = "StudyareaBnd", GDAL1_integer64_policy = FALSE)
Pumpbndprj <-readOGR("C:/Pumpsbnd_prj.shp", layer = "Pumpsbnd_prj", GDAL1_integer64_policy = FALSE)
choleraroadshpprj <-readOGR("C:/cholera_roadsprj.shp", layer = "cholera_roadsprj", GDAL1_integer64_policy = FALSE)

#------------------------------------------------------------------------

bnd_df <- fortify(studybndprj)
Pumpbndprj_df <-fortify(Pumpbndprj)
bndrd_df <- fortify(choleraroadshpprj)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Cholera Deaths: Interactive Map"),
  
   
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textOutput("crimetypetval"),
      textOutput("crimetotalval"),
      sliderInput("pnt_radius", label = "Size of points on map:",
                  min = 1, max = 30, value = 1, step = 1)
    ),
    
    # Show maps and various plots
    mainPanel(
      leafletOutput("crimeMapz3"),
      plotOutput("distPlot2"),
      plotOutput("distPlot3"),
      plotOutput("PlotPPA3"),
      plotOutput("PlotPPA2"),
      plotOutput("PlotPPA")

    )
    
  )
)


# Define server logic 
server <- function(input, output) {
  
  #textlabel to say number of records in table
    output$crimetotalval <- renderText({ 
        crime_data_stats <- paste("There are", nrow(choleradeath), "records.")
    })

    #calculate mean center of cholera deaths
    xmeanL<-mean(choleradeath$xL)
    ymeanL<-mean(choleradeath$yL)

#Leaflet crime map
  output$crimeMapz3 <-renderLeaflet({
      leaflet() %>% 
      addTiles() %>%  # Add default OpenStreetMap map tiles
      clearShapes() %>%
      
      #calculate the mean center of the outbreak
      addPolygons(data=Pumpbnd, color = "#444444", weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, fill= FALSE,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addCircles(data=pumpsllcsv, lng = ~xL, lat = ~yL, popup=pumpsllcsv$pumpname, weight = 10,  radius = 2, color = "red", fill = TRUE, fillColor = "red")%>%
      addCircles(data=choleradeath, lng = ~xL, lat = ~yL, popup=choleradeath$streetname, weight = input$pnt_radius,  radius = choleradeath$count)%>%
      addCircleMarkers(lng = xmeanL, lat= ymeanL, color="black")
  })
  
  
  output$distPlot2 <- renderPlot({
    # draw the histogram with the specified number of bins
    par(mfrow=c(1, 1))
    par(mar=c(8,8,4,2)+0.1,mgp=c(6,1,0))
      counts <- table(choleradeath$pumpname)
      barplot(counts, main="Deaths",xlab="PumpName",las=2) 
 
  })
  
  
  output$distPlot3 <- renderPlot({
    # draw a boxplot for all deaths
    par(mar=c(8,7,4,2)+0.1,mgp=c(6,1,0))
    boxplot(count~pumpname, data=choleradeath,main="Boxplot of deaths by Pump",
            xlab="PumpName", ylab="No of Deaths",
            col="orange", border="brown", las=2
    )
  })
  
  
  #-----PPA
  
  #PPA
  output$PlotPPA <- renderPlot({
    par(mfrow=c(1, 1))
    Sbnd    <- as(studybndprj, "owin")
    Pumpbnd2 <-as(Pumpbndprj,"owin")
    scholdeathppp <- as(choleradeathshpprj, "ppp")
    marks(scholdeathppp)<-NULL
    Window(scholdeathppp)<-Sbnd


    #Density
    K1 <- density(scholdeathppp) # Using the default bandwidth
    plot(K1, main=NULL, las=1)
    contour(K1, add=TRUE)
    
    K2 <- density(scholdeathppp, sigma=20) # Using a 50ft bandwidth
    plot(K2, main=NULL)
    contour(K2, add=TRUE)

  })
  
  #PPA3
  output$PlotPPA3 <- renderPlot({
    par(mfrow=c(1, 1))
    
    gg <- ggplot()
    gg <- gg + stat_density2d(data=choleradeathprj2, show.legend=F, aes(x=Xcoord,y=Ycoord, fill=..level.., alpha=..level..), geom="polygon", size=2, bins=10)
    gg<-gg + geom_line(data = bndrd_df, aes(x=bndrd_df$long, y=bndrd_df$lat, group=group), color = "black", size = 1)
    gg <- gg + geom_polygon(data=Pumpbndprj_df, aes(x=Pumpbndprj_df$long, y=Pumpbndprj_df$lat,group=group),color='blue',size = .2, fill=NA)
    gg <- gg + geom_polygon(data=bnd_df, aes(x=bnd_df$long, y=bnd_df$lat,group=group),color='black',size = .2, fill=NA)
    gg <- gg + scale_fill_gradient(low="deepskyblue2", high="firebrick1", name="Distribution")
    gg
    
  })
  
  
  #PPA
  output$PlotPPA2 <- renderPlot({
    par(mfrow=c(1, 3))
  
    Sbnd    <- as(studybndprj, "owin")
    Pumpbnd2 <-as(Pumpbndprj,"owin")
    scholdeathppp <- as(choleradeathshpprj, "ppp")
    
    marks(scholdeathppp)<-NULL
    Window(scholdeathppp)<-Sbnd
    
    #NN
    nnd <- nndist.ppp(scholdeathppp)
    hist(nnd)
    summary(nnd)
    
    #Ripley
    k_env <- envelope(scholdeathppp, Kest, nsim=5, nrank=1)
    plot(k_env)
    
        g_env <- envelope(scholdeathppp, Gest, nsim=5, nrank=1)
        plot(g_env)
    
  })
  
  
  output$info <- renderText({
  })
  
  #server.r   
}

# Run the application 
shinyApp(ui = ui, server = server)
