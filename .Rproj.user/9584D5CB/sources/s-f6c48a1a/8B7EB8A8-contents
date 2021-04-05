#Sydney's shiny app
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library (shiny)
library (ggplot2)
library (maps)
library (tidyr)
library (shinythemes)
library (plotly)
library (dplyr)
library (lubridate)

#now read in data
#make sure to use url for RAW data
#Confirmed:
covid.us1 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
#need to convert to long format
covid.us1 <- gather (covid.us1, Date, Confirmed, 12:ncol(covid.us1))

#deaths"
covid.us2 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
#and convert to long form
covid.us2 <- gather (covid.us2, Date, Deaths, 13:ncol(covid.us2))

#join together:
#covid.us3 <- full_join (covid.us1, covid.us2)
#this code automatically joins by all cloumns that match
#so everything but the confirmed and deaths column
#essentially the same as adding the deaths column to the confirmed cases

if (sum (covid.us1$Date!=covid.us2$Date & covid.us1$Admin2!=covid.us2$Admin2)==0){
  covid.us1$Population <- covid.us2$Population
  covid.us1$Deaths <- covid.us2$Deaths
}
covid.us3 <- covid.us1

#Fix the date column
#get rid of Xs
covid.us3$Date <- gsub ("X", "", covid.us3$Date)

#convert to date format
covid.us3$Date <- mdy (covid.us3$Date)

#adjust column names
#because github used _ instead of .
#this way you don't have to change things later on
colnames (covid.us3) <- gsub ("_", ".", colnames (covid.us3))


#the github data has additional columns not present in it
#you may not need to delete columns (who knows, that info might be useful)
#but if you refer to columns by number in you app, the data frames will need to match exactly
covid.us <- covid.us3[,c(12, 6, 7, 13, 15, 8)]
#this should be the exact same as the "covid.us" we used before



#subset wyoming counties
covid.wy <- covid.us [covid.us$Province.State=="Wyoming",]
covid.wy <- covid.wy[covid.wy$Admin2!="Unassigned" & covid.wy$Admin2!="Out of WY",]
#adding population data
#read in the counties labels point data
point_data <- read.csv ("County_labels.csv")
colnames (covid.wy)[2] <- "County"
covid.wy.pop <- left_join(covid.wy[,1:6], point_data, by="County")
#add per 100,000 data for confirmed and deaths
covid.wy.pop$Confirmed_per_pop <- covid.wy.pop$Confirmed/covid.wy.pop$Population*100000
covid.wy.pop$Deaths_per_pop <- covid.wy.pop$Deaths/covid.wy.pop$Population*100000

#Create function to make a rolling average time series from covid.wy.pop data
make.covid.rolling.average <- function (data=covid.wy.pop, colnumber=4, n=7){
  #first convert data to wide form
  wide_covid <- spread (data[,c(1,2,colnumber)], Date, 3)
  rownames (wide_covid) <- wide_covid[,1]
  wide_covid <- wide_covid [,-1]
  
  #convert to new cases per day
  new.covid <- data.frame (wide_covid[,1])
  for (i in 2:(ncol (wide_covid))){
    for (j in 1:nrow (wide_covid)){
      new.covid [j,i] <- as.numeric (wide_covid[j,i]) - as.numeric (wide_covid[j,i-1])
    }
  }
  
  #calculate rolling average
  rolling.average <- data.frame ()
  for (i in 1:(ncol (new.covid) - n)){
    for (j in 1:nrow (new.covid)){
      rolling.average[j,i] <- mean (as.numeric(new.covid [j,i:(i+n)]))
    }
  }
  
  #adjusting column names
  rownames (rolling.average) <- rownames (wide_covid)
  colnames (rolling.average) <- colnames (wide_covid)[1:(ncol (wide_covid)-n)]
  
  #Start at date of first wy case  (3-12-2020) (column 51)
  #colSums (rolling.average.covid2)[44]
  rolling.average <- rolling.average[,-(1:50)]
  colnames (rolling.average) [1]
  #transforming data
  rolling.average <- data.frame (t (rolling.average))
  return (rolling.average)
}

rolling.average.covid <- make.covid.rolling.average (covid.wy.pop, 4, n=7)
rolling.average.deaths <- make.covid.rolling.average (covid.wy.pop, 5, n=7)
rolling.average.covid.per.pop <- make.covid.rolling.average (covid.wy.pop, 12, n=7)
rolling.average.deaths.per.pop <- make.covid.rolling.average (covid.wy.pop, 13, n=7)

#read in air polution data
NO2<- read.csv ("NO2_Updated_December.csv")
pm2.5 <- read.csv("PM2.5.csv")
pm10 <- read.csv("PM10.csv")
ozone <- read.csv("Ozone.csv")

#aggregate each air pollution data.frame
aggregated_NO2 <- aggregate (DAILY_AQI_VALUE ~ COUNTY + Date, data=NO2, FUN=mean, na.rm=TRUE)
aggregated_NO2$Date <- lubridate::mdy (aggregated_NO2$Date)

aggregated_pm2.5 <- aggregate (DAILY_AQI_VALUE ~ COUNTY + Date, data=pm2.5, FUN=mean, na.rm=TRUE)
aggregated_pm2.5$Date <- lubridate::mdy (aggregated_pm2.5$Date)

aggregated_pm10 <- aggregate (DAILY_AQI_VALUE ~ COUNTY + Date, data=pm10, FUN=mean, na.rm=TRUE)
aggregated_pm10$Date <- lubridate::mdy (aggregated_pm10$Date)

aggregated_ozone <- aggregate (DAILY_AQI_VALUE ~ COUNTY + Date, data=ozone, FUN=mean, na.rm=TRUE)
aggregated_ozone$Date <- lubridate::mdy (aggregated_ozone$Date)

#Creating rolling averages for air data
#defining a function to do that
#data is the input data frame
#n is the umber of days in the average
#march12th is the coulmn in the data representing march 12th
rolling.average.air <- function (data, n=7, march12) {
  wide.data <- tidyr::spread(data, Date, DAILY_AQI_VALUE)
  rownames (wide.data) <- wide.data[,1]
  wide.data <- wide.data[,-1]
  
  rolling.air <- data.frame ()
  for (i in 1:(ncol (wide.data) - n)){
    for (j in 1:nrow (wide.data)){
      rolling.air[j,i] <- mean (as.numeric(wide.data [j,i:(i+n)]))
    }
  }
  rownames (rolling.air) <- rownames (wide.data)
  colnames (rolling.air) <- colnames (wide.data)[1:ncol (rolling.air)]
  rolling.air <- rolling.air[,-(1:march12)]
  colnames (rolling.air)[1]
  #transforming data
  rolling.air <- data.frame (t (rolling.air))
  
}

rolling.no2 <- rolling.average.air (aggregated_NO2, march12=71)
rolling.ozone <- rolling.average.air (aggregated_ozone, march12=71)
rolling.pm2.5 <- rolling.average.air (aggregated_pm2.5, march12=71)
rolling.pm10 <- rolling.average.air (aggregated_pm10, march12=71)

#map data
#pulling data from maps package:
counties.wy <- map_data("county", region="wyoming")
#adding total confirmed COVID as of most recent date
covid.wy$subregion <- tolower(covid.wy$County)
covid.wy.recent <- covid.wy [covid.wy$Date==max (covid.wy$Date),]

counties.covid.wy <- left_join(counties.wy, covid.wy.recent, by="subregion")

#read in elevation data:
US_Elevation <- read.csv("US_Elevation.csv")

#subsetting covid data for counties with and without national parks
counties_w_NPS_covidDF <- data.frame("Date"= covid.wy.pop$Date, "County" = covid.wy.pop$County,
                                     "Confirmed"=covid.wy.pop$Confirmed, "Deaths"=covid.wy.pop$Deaths,
                                     "Confirmed_by_pop"=covid.wy.pop$Confirmed/covid.wy.pop$Population*100000,
                                     "Deaths_by_pop"=covid.wy.pop$Deaths/covid.wy.pop$Population*100000)
counties_no_NPS_covidDF <- counties_w_NPS_covidDF [!(counties_w_NPS_covidDF$County %in% c("Crook", "Goshen", "Lincoln", "Park", "Teton")),]
counties_w_NPS_covidDF <- counties_w_NPS_covidDF [(counties_w_NPS_covidDF$County %in% c("Crook", "Goshen", "Lincoln", "Park", "Teton")),]

#aggregate by county
averaged_NPS_counties_cases <- aggregate(counties_w_NPS_covidDF[,-2], by=list(counties_w_NPS_covidDF$Date), FUN = mean)
averaged_no_NPS_counties_cases <- aggregate(counties_no_NPS_covidDF[,-2], by=list(counties_no_NPS_covidDF$Date), FUN = mean)
#put those data frames back into one
NPScounty_comparisons <- full_join (averaged_NPS_counties_cases, averaged_no_NPS_counties_cases, by="Group.1")
#keeping only relevant columns and renaming them
NPScounty_comparisons <- NPScounty_comparisons [,c (2, 3:6, 8:11)]
colnames (NPScounty_comparisons) <- c("Date", "Confirmed_NPS", "Deaths_NPS",
                                      "Confirmed_by_pop_NPS", "Deaths_by_pop_NPS",
                                      "Confirmed_no_NPS", "Deaths_no_NPS",
                                      "Confirmed_by_pop_no_NPS", "Deaths_by_pop_no_NPS")





# Reading in CSV file with National Park Service data (visits and traffic counts)
NPS_Rready <- read.csv("NPS_Rready.csv") # Reading in CSV file with National Park Service data (visits and traffic counts)
#change month from numerical to factor
NPS_Rready$Month <- factor (NPS_Rready$Month, levels=c(1:12),
                            labels=month.abb)
NPS_Rready$Unit <- factor (NPS_Rready$Unit, levels=c ("DETO", "FOBU", "FOLA", "GRTE", "JODR", "YELL"), 
                           labels= c("Devils Tower", "Fossil Butte", "Ft. Laramie", "Grand Teton", "John D. Rockefeller", "Yellowstone"))
# A new data from to clean up Rec_visits for plotting
new.data.Rec_visits <- aggregate (Rec_visits ~ Unit + Month, FUN=sum, na.rm=T, data=NPS_Rready)




#define custon colors for Colin's plot:
custom_col <- c("grey", "grey", "grey", "grey", "grey", "#F8766D", "grey", "#36B600", "grey", "grey",
                "grey", "#BB9D00", "grey", "grey", "#FC61D5", "grey", "grey", "grey", "grey", "#00ABFD", 
                "grey", "grey", "grey")

# Define UI for application
# UI (user interface) defines what the user sees
#first line defines tabbed page and theme (check out https://rstudio.github.io/shinythemes/ for more)
ui <- fluidPage(navbarPage("Environmental Impacts on COVID-19 Outcomes", theme = shinytheme("darkly"), collapsible = TRUE, id="nav",
                           #first tab (show air quality data)
                           tabPanel("Air Quality", #or whatever you want to call it
                                    #using layout with side bar and main panel
                                    sidebarLayout(
                                      #defining what's in the side bar
                                      sidebarPanel(width=5,
                                                   #defining drop down menu with Air quality options
                                                   selectInput("air_var", #this tells us what to refer to this variable as later on
                                                               label = "Select Air Quality Variable",
                                                               choices = c ("PM2.5", "PM10", "NO2", "Ozone")),
                                                   #defining drop down menu to select county for time series
                                                   selectInput ("county_var",
                                                                label="Select County to display time series",
                                                                choices= c("Albany", "Big.Horn", "Campbell", "Carbon",
                                                                           "Converse","Fremont", "Johnson", "Laramie",
                                                                           "Lincoln", "Natrona", "Park", "Platte",
                                                                           "Sheridan", "Sublette", "Sweetwater", 
                                                                           "Teton", "Uinta", "Weston")),
                                                   #defining slider to show dates for both air and covid data
                                                   sliderInput("air_date",
                                                               label="Select date to show air quality data",
                                                               min=as.Date ("2020-3-12", "%Y-%m-%d"),
                                                               max=as.Date (max (covid.wy$Date)),
                                                               value=as.Date (max (covid.wy$Date))),
                                                   sliderInput("covid_date",
                                                               label="Select date to show COVID data",
                                                               min=as.Date ("2020-3-12", "%Y-%m-%d"), #starting at date of first covid case in WY
                                                               max=as.Date (max (covid.wy$Date)),
                                                               value=as.Date (max (covid.wy$Date))),
                                                   #telling it we want to show plot 1 (defined in server) in side bar
                                                   plotOutput("plot1", width="100%", height = "300px"),
                                                   #insert text if you want:
                                                   em ("Daily AQI data vs Daily COVID data. Not all counties have each air poullutants AQI data. An error message will show if a county is missing air quality data. ")
                                      ),
                                      
                                      #Defining what we want to show in the main pannel
                                      mainPanel(width=7,
                                                #first show map
                                                plotlyOutput("map"),
                                                em ("Wyoming county map that displays the COVID data along a color gradient. The size of the bubble's in each county indicates the average air quality index (AQI)"),
                                                #then show time series plot
                                                plotOutput ("plot2"),
                                                #and add text if you want
                                                em ("COVID cases and AQI over time. Black line shows COVID cases, blue line shows AQI")
                                      )
                                    )
                           ),
                           #creating another tab
                           tabPanel("Elevation", #or whatever you want to call this
                                    plotlyOutput("plot3"),
                                    em ("Displays all counties in the U.S. Each counties average elevation is plotted against the rate of COVID cases per 100,000."),
                                    plotlyOutput("plot4"),
                                    em ("Displays all counties in the U.S. Each counties average elevation is plotted against number of deaths due to COVID.")
                           ),
                           tabPanel("National Park Service Visitation",
                                    sidebarLayout(
                                      sidebarPanel(width=5, #out of 12 units
                                                   selectInput("covid_var", #this tells us what to refer to this variable as later on
                                                               label = "Select COVID-19 Variable",
                                                               choices = c ("Cases per 100,000", "Deaths per 100,000",
                                                                            "Confirmed Cases", "Deaths")
                                                               #honestly copied this from another app...you don't have to use all 4
                                                   ),
                                                   plotOutput("plot6", width="100%", height = "300px",),
                                                   plotOutput("plot7", width="100%", height = "300px",)
                                      ),
                                      mainPanel(width=7,
                                                plotlyOutput("plot5"),
                                                plotlyOutput("plot8")
                                      )
                                    ))
                           
)
)

# Define server 
#the server is the code behind all the things the use sees
server <- function(input, output) {
  
  #telling the app what to do with each selected air variable
  #first time series data
  #selects a different data frame based on drop down input
  airDataTime <- reactive ({
    air.data <- switch (input$air_var,
                        "PM2.5" = rolling.pm2.5[,colnames (rolling.pm2.5) == input$county_var],
                        "PM10" = rolling.pm10[,colnames (rolling.pm10) == input$county_var],
                        "NO2" = rolling.no2[,colnames (rolling.no2) == input$county_var],
                        "Ozone" = rolling.ozone[,colnames (rolling.ozone) == input$county_var])
  })
  
  airDataAverage <- reactive ({
    air.data <- switch (input$air_var,
                        "PM2.5" = aggregated_pm2.5,
                        "PM10" = aggregated_pm10,
                        "NO2" = aggregated_NO2,
                        "Ozone" = aggregated_ozone)
    air.averages <- aggregate (DAILY_AQI_VALUE ~ COUNTY, air.data, FUN=mean, na.rm=T)
  })
  
  #tell the app how to deal with covid slider dates
  #subset covid data to only include that data
  covidData <- reactive ({
    covid.data <- covid.wy[covid.wy$Date == input$covid_date,]
  })
  
  #selecting air data by date
  airData <- reactive ({
    air.data <- switch (input$air_var,
                        "PM2.5" = aggregated_pm2.5,
                        "PM10" = aggregated_pm10,
                        "NO2" = aggregated_NO2,
                        "Ozone" = aggregated_ozone)
    air.data <- air.data[air.data$Date==input$air_date,]
  })
  
  #and finally, selecting rolling covid data by county
  
  covidDataTime <- reactive ({
    covid.data <- rolling.average.covid [,colnames (rolling.average.covid)==input$county_var]
  })
  
  #for colins data, we need to be able to switch between confirmed, deaths, and scaled by pop
  #first NPS comparison
  covidDataNPS <- reactive({
    NPS <- switch (input$covid_var,
                   "Confirmed Cases" = NPScounty_comparisons$Confirmed_NPS,
                   "Deaths" = NPScounty_comparisons$Deaths_NPS, 
                   "Cases per 100,000" = NPScounty_comparisons$Confirmed_by_pop_NPS,
                   "Deaths per 100,000" = NPScounty_comparisons$Deaths_by_pop_NPS)
    no_NPS <- switch (input$covid_var,
                      "Confirmed Cases" = NPScounty_comparisons$Confirmed_no_NPS,
                      "Deaths" = NPScounty_comparisons$Deaths_no_NPS, 
                      "Cases per 100,000" = NPScounty_comparisons$Confirmed_by_pop_no_NPS,
                      "Deaths per 100,000" = NPScounty_comparisons$Deaths_by_pop_no_NPS)
    covid.data <- data.frame (NPS=NPS, no_NPS=no_NPS, Date=NPScounty_comparisons$Date)
  })
  
  #and for rolling average
  covidDataRolling <- reactive({
    covid.data <- switch (input$covid_var,
                          "Confirmed Cases" = rolling.average.covid,
                          "Deaths" = rolling.average.deaths, 
                          "Cases per 100,000" = rolling.average.covid.per.pop,
                          "Deaths per 100,000" = rolling.average.deaths.per.pop)
  })
  
  
  output$plot1 <- renderPlot ({
    covid.data <- covidData()[(covidData()[,2]) %in% (airData()[,1]),]
    combined_data <- data.frame ((covid=covid.data[,4]), (AQI=airData()[,3]))
    #mod <- lm (covid ~ AQI, data=combined_data)
    #creating plot of covid ~ AQI
    plot1 <- ggplot2::ggplot (combined_data, aes (x=AQI, y=covid)) + 
      geom_point (size=3) +
      theme_classic () +
      labs (title="Confirmed COVID Cases vs AQI", x="AQI", y="Confirmed COVID Cases")
    #if (summary (mod)$coefficients[2,4]<0.05) geom_smooth (method="lm", se=FALSE)
    #that last line adds a trend line only if the association is significant :)
    plot1
  })
  
  output$plot2 <- renderPlot ({
    
    #defining air variable switch here
    #this is not the best way of doing it but it doesn't work the other way so...
    air.data <- switch (input$air_var,
                        "PM2.5" = c (rolling.pm2.5[,colnames (rolling.pm2.5) == input$county_var],
                                     rep (NA, (nrow (rolling.average.covid) - nrow (rolling.pm2.5)))),
                        "PM10" = c (rolling.pm10[,colnames (rolling.pm10) == input$county_var],
                                    rep (NA, (nrow (rolling.average.covid) - nrow (rolling.pm2.5)))),
                        "NO2" = c (rolling.no2[,colnames (rolling.no2) == input$county_var],
                                   rep (NA, (nrow (rolling.average.covid) - nrow (rolling.pm2.5)))),
                        "Ozone" = c (rolling.ozone[,colnames (rolling.ozone) == input$county_var],
                                     rep (NA, (nrow (rolling.average.covid) - nrow (rolling.pm2.5)))))
    
    combined_data <- data.frame (AQI=air.data, covid=covidDataTime(), 
                                 Date=seq (as.Date("2020-3-12"), by="days", 
                                           length.out = nrow (rolling.average.covid)))
    #plot of covid and air quality over time
    plot2 <- ggplot2::ggplot(data=combined_data, aes (x=Date, y=covid)) +
      geom_line() +
      labs (y="Confirmed Cases/AQI", title="Timeseries") +
      theme_classic () +
      if (exists ("AQI", where=combined_data)==TRUE) geom_line (aes (x=Date, y=AQI), color="blue")
    
    plot2
  })
  
  output$plot3 <- renderPlotly ({
    #INSERT CODE FOR ELEVATION PLOT HERE
    Elevation_Plot <-ggplot2::ggplot(US_Elevation, aes(x=Elevation,y=rate, 
                                                       text=Combined_Key)) +
      geom_point(alpha=0.4, color="darkblue") +
      labs (x="Elevation (m)", y="COVID Cases per 100,000", title="Elevation vs COVID Cases") +
      theme_classic()
    plotly::ggplotly (Elevation_Plot)
  })
  
  output$plot4 <- renderPlotly ({
    #INSERT CODE FOR ELEVATION PLOT HERE
    Elevation_Plot_Deaths <-ggplot2::ggplot(US_Elevation, aes(x=Elevation,y=Deaths, 
                                                              text=Combined_Key)) +
      geom_point(alpha=0.4, color="darkblue") +
      labs (x="Elevation (m)", y="COVID Deaths", title="Elevation vs COVID Deaths") +
      theme_classic ()
    plotly::ggplotly (Elevation_Plot_Deaths)
  })   
  
  output$plot5 <- renderPlotly ({
    NPS_visits_plot <- ggplot2::ggplot(data=new.data.Rec_visits, aes (x=Month, y=Rec_visits, color=Unit, group=Unit,
                                                                      text=paste ("Park: ", Unit,
                                                                                  "<br>", "Rec visits in ", Month, ": ",
                                                                                  Rec_visits, sep=""))) +
      geom_point() + 
      geom_line () +
      theme_classic() +
      theme (legend.position = "bottom")+
      ggtitle("Recreational Visits at Wyoming's 6 National Park Service Units")+
      ylab("Rec Visits")
    
    
    plotly::ggplotly(NPS_visits_plot, tooltip="text")
  })
  
  
  output$plot6 <- renderPlot({
    covid.data <- covidDataNPS()
    covid.data$Date <- lubridate::ymd (covid.data$Date)
    plot6 <- ggplot2::ggplot (data=covid.data, aes(x=Date, y=NPS)) + 
      geom_smooth(col="green") +
      geom_smooth(aes (y=no_NPS), col="red") +
      theme_classic() +
      labs (title="Title")
    scale_color_identity(name = "Counties",
                         breaks = c("green", "red"),
                         labels = c("With NPS units", "Without NPS units"),
                         guide = "legend")
    
    plot6
  })
  
  output$plot7 <- renderPlot({
    covid.data <- covidDataNPS()
    covid.data$Date <- lubridate::ymd (covid.data$Date)
    plot7 <- ggplot2::ggplot (data=covid.data, aes(x=Date, y=NPS-no_NPS)) +
      geom_smooth(col="blue") +
      geom_hline (yintercept=0, color="black") +
      labs (y="Difference between Counties with and without NPS", title="Title") +
      theme_classic() 
    
    plot7
    
    
  })
  
  output$plot8 <- renderPlotly({
    covid.data <- covidDataRolling()
    covid.data$Date <- rownames (covid.data)
    covid.data <- gather (covid.data, County, Cases, -24)
    
    covid.data$Date <- lubridate::ymd (covid.data$Date)
    
    plot8 <- ggplot2::ggplot(covid.data, aes (x=Date, y=Cases, color=County, group=County,
                                              text=paste (County, " County",
                                                          "<br> ", Date,
                                                          "<br> ", input$covid_var, ": ", Cases,
                                                          sep=""))) +
      geom_line () + scale_color_manual(values=custom_col) + theme_classic()
    
    plotly::ggplotly (plot8, tooltip="text")
    
  })
  
  output$map <- renderPlotly({
    
    point_data <- point_data[point_data$County %in% (airDataAverage()[,1]),]
    point_data$AQI <- airDataAverage()[,2]
    
    map <- ggplot2::ggplot() +
      #add counties as polygons and color by confirmed cases
      #color=black gives black borders
      geom_polygon(data = counties.covid.wy, aes(x=long, y = lat, group = group, fill=Confirmed,
                                                 text= paste ("Confirmed Cases:", Confirmed)), color="black") +
      #Add points in each county scaled by population
      geom_point (data=point_data, aes(x=long, y = lat + 0.075, size=AQI,
                                       text= paste ("AQI:", round (AQI, digits=1)))) +
      #adding labels
      geom_text(data=point_data, aes(x=long, y = lat - 0.15, label=County, 
                                     text=paste (County, "County")), size=2.5) +
      #Changing the color scheme to obnoxiously bright colors
      scale_fill_gradient (low="#FFEE58", high="#FF0000") +
      #getting rid of axis/labels
      theme_void() +
      #creating a title
      ggtitle("Wyoming County Map COVID Cases vs AQI") +
      #centering the title
      theme(plot.title = element_text(hjust = 0.5))
    plotly::ggplotly (map, tooltip="text")
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
