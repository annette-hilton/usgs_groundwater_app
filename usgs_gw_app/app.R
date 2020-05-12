#---------------------------------------------
# USGS Well Water Levels Shiny App 
#---------------------------------------------
# Annette Hilton 
# Started: 1/30/2020 
# Completed: 
#---------------------------------------------
#---------------------------------------------

#---------------------------------------------
# Getting Started
#---------------------------------------------

# Attach packages

library(tidyverse) 
library(shiny)
library(shinydashboard)
library(janitor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# Disable scientific notation 

options(scipen=999)

#----------------------------------------------
# Read in data and all data wrangling
#----------------------------------------------

# W2 

w2 <- readr::read_tsv(here::here("w2.txt"))

# Read in dataframe that is bounding box for USA states 

w2_bb <- readr::read_csv(here::here("US_State_Bounding_Boxes.csv")) %>% 
    clean_names() %>% 
    mutate(name = str_to_title(name))

# Load W2 map of USA 

usa <- ne_countries(scale = 110, country = 'united states of america', returnclass = "sf")

# W3

w3 <- readr::read_tsv(here::here("w3.txt"))

# W4
# Create dataframe for cumulative wells over time 

w4 <- w3 %>% 
    group_by(state) %>% 
    mutate(total_wells = cumsum(n))

# W5 
# Read in dataframe for longest well record in each state (created in complimentary Rmd)

w5 <- readr::read_tsv(here::here("w5.txt"))

#----------------------------------------------
# Shiny App Build 
#----------------------------------------------

# USER INTERFACE
#----------------------------------------------------------------------------------

ui <- dashboardPage(
    skin = "blue",
    
    # HEADER ---------------------------------------------------------------------------
    
    dashboardHeader(title = "Groundwater Wells in the United States",
                    titleWidth = 400),
    
    # SIDEBAR --------------------------------------------------------------------------
    
    dashboardSidebar(
        width = 400,
        sidebarMenu(
            menuItem("Summary", tabName = "w1", icon = icon("tint")),
            menuItem("Where are the Wells?", tabName = "w2", icon = icon("map-marked-alt
")),
            menuItem("When were the Wells Made?", tabName = "w3", icon = icon("history")), 
            menuItem("Cumulative Wells over Time", tabName = "w4", icon = icon("history")),
            menuItem("What does a Well Record Look Like?", tabName = "w5", icon = icon("chart-line"))
        )),
    
    # BODY -----------------------------------------------------------------------------
    
    dashboardBody(
        tabItems(
            
            # TAB 1 (Widget 1) --------------------------------------------------------------------------
            tabItem(
                tabName = "w1", 
                fluidRow(
                    box(width = 12, 
                        h2(strong(em("Welcome to the Well Water Shiny App!"))),   
                        h2(" Information"),
                        p("Data for this project were taken from the United States Geological Survey (USGS Groundwater Levels REST Web Service; National Water Information System). This project explores USGS groundwater wells and well water levels throughout the United States. Over 10 million observations of well water levels over time are contained in this dataset, with around 890,000 unique wells located throughout the United States (50 US States and the District of Columbia)."),
                        h2("Where are the Wells?"),
                        p("Explore where USGS groundwater wells are located throughout the United States."),
                        h2("When were the Wells Made?"), 
                        p("Discover when wells were constructed in the United States throughout time."), em("Note: First water level measurement is used as a proxy for well construction date."),
                        h2("Cumulative Wells over Time"),
                        p("Explore how many wells have been built over time. How many wells existed in the past compared to present day?"), em("Note: Displays cumulative unique wells in the US throughout time; does not account for wells that are abandoned or closed."),
                        h2("What does a Well Record Look Like?"), 
                        p("Explore what a well water level record looks like. For each state, one unique well was chosen that had a minimum of 60 measurements for the longest time period in the state.")
                    ))),
            
            # TAB 2 (Widget 2) --------------------------------------------------------------------------
            tabItem(
                tabName = "w2",
                fluidRow(
                    box(width = 4, selectInput(inputId = "state_select_w2", 
                                               label = "Choose a State", 
                                               choices = list("Alaska" = "Alaska", "Arizona" = "Arizona", "Arkansas" = "Arkansas", "California" = "California", "Colorado" = "Colorado", "Connecticut" = "Connecticut", "Delaware" = "Delaware", "District of Columbia" = "Districtcolumbia", "Florida" = "Florida", "Georgia" = "Georgia", "Hawaii" = "Hawaii", "Idaho" = "Idaho", "Illinois" = "Illinois", "Indiana" = "Indiana", "Iowa" = "Iowa", "Kansas" = "Kansassecond", "Kentucky" = "Kentucky", "Louisiana" = "Louisiana", "Maine" = "Maine", "Maryland" = "Maryland", "Massachusetts" = "Massachusetts", "Michigan" = "Michigan", "Minnesota" = "Minnesota", "Mississippi" = "Mississippi", "Missouri" = "Missouri", "Montana" = "Montana", "Nebraska" = "Nebraska", "Nevada" = "Nevada", "New Hampshire" = "Newhampshire", "New Jersey" = "Newjersey", "New Mexico" = "Newmexico", "New York" = "Newyork", "North Carolina" = "Northcarolina", "North Dakota" = "Northdakota", "Ohio" = "Ohio", "Oklahoma" = "Oklahoma", "Oregon" = "Oregon", "Pennsylvania" = "Pennsylvania", "Rhode Island" = "Rhodeisland", "South Carolina" = "Southcarolina", "South Dakota" = "Southdakota", "Tennessee" = "Tennessee", "Texas" = "Texas", "Utah" = "Utah", "Vermont" = "Vermont", "Virginia" = "Virginia", "Washington" = "Washington", "West Virginia" = "Westvirginia", "Wisconsin" = "Wisconsin", "Wyoming" = "Wyoming"))), 
                    box(width = 8, title = "Where are the wells?", status = "primary", 
                        plotOutput(outputId = "w2_plot", height = 400)),
                    box(width = 12, p("Explore where USGS groundwater wells are located throughout the United States.")),
                )), 
            
            # TAB 3 (Widget 3) --------------------------------------------------------------------------
            tabItem(
                tabName = "w3", 
                fluidRow(
                    box(width = 4, selectInput(inputId = "state_select_w3",
                                               label = "Choose a State", 
                                               choices = list("Alaska" = "Alaska", "Arizona" = "Arizona", "Arkansas" = "Arkansas", "California" = "California", "Colorado" = "Colorado", "Connecticut" = "Connecticut", "Delaware" = "Delaware", "District of Columbia" = "Districtcolumbia", "Florida" = "Florida", "Georgia" = "Georgia", "Hawaii" = "Hawaii", "Idaho" = "Idaho", "Illinois" = "Illinois", "Indiana" = "Indiana", "Iowa" = "Iowa", "Kansas" = "Kansassecond", "Kentucky" = "Kentucky", "Louisiana" = "Louisiana", "Maine" = "Maine", "Maryland" = "Maryland", "Massachusetts" = "Massachusetts", "Michigan" = "Michigan", "Minnesota" = "Minnesota", "Mississippi" = "Mississippi", "Missouri" = "Missouri", "Montana" = "Montana", "Nebraska" = "Nebraska", "Nevada" = "Nevada", "New Hampshire" = "Newhampshire", "New Jersey" = "Newjersey", "New Mexico" = "Newmexico", "New York" = "Newyork", "North Carolina" = "Northcarolina", "North Dakota" = "Northdakota", "Ohio" = "Ohio", "Oklahoma" = "Oklahoma", "Oregon" = "Oregon", "Pennsylvania" = "Pennsylvania", "Rhode Island" = "Rhodeisland", "South Carolina" = "Southcarolina", "South Dakota" = "Southdakota", "Tennessee" = "Tennessee", "Texas" = "Texas", "Utah" = "Utah", "Vermont" = "Vermont", "Virginia" = "Virginia", "Washington" = "Washington", "West Virginia" = "Westvirginia", "Wisconsin" = "Wisconsin", "Wyoming" = "Wyoming"))),
                    box(width = 8, title = "When were the wells made?", status = "primary", 
                        plotOutput(outputId = "w3_plot", height = 400)),
                    box(width = 12, p("Discover when wells were constructed in the United States throughout time."), em("Note: First water level measurement is used as a proxy for well construction date."))),
            ), 
            
            # TAB 4 (Widget 4) --------------------------------------------------------------------------
            tabItem(
                tabName = "w4", 
                fluidRow(
                    box(width = 4, selectInput(inputId = "state_select_w4",
                                               label = "Choose a State", 
                                               choices = list("Alaska" = "Alaska", "Arizona" = "Arizona", "Arkansas" = "Arkansas", "California" = "California", "Colorado" = "Colorado", "Connecticut" = "Connecticut", "Delaware" = "Delaware", "District of Columbia" = "Districtcolumbia", "Florida" = "Florida", "Georgia" = "Georgia", "Hawaii" = "Hawaii", "Idaho" = "Idaho", "Illinois" = "Illinois", "Indiana" = "Indiana", "Iowa" = "Iowa", "Kansas" = "Kansassecond", "Kentucky" = "Kentucky", "Louisiana" = "Louisiana", "Maine" = "Maine", "Maryland" = "Maryland", "Massachusetts" = "Massachusetts", "Michigan" = "Michigan", "Minnesota" = "Minnesota", "Mississippi" = "Mississippi", "Missouri" = "Missouri", "Montana" = "Montana", "Nebraska" = "Nebraska", "Nevada" = "Nevada", "New Hampshire" = "Newhampshire", "New Jersey" = "Newjersey", "New Mexico" = "Newmexico", "New York" = "Newyork", "North Carolina" = "Northcarolina", "North Dakota" = "Northdakota", "Ohio" = "Ohio", "Oklahoma" = "Oklahoma", "Oregon" = "Oregon", "Pennsylvania" = "Pennsylvania", "Rhode Island" = "Rhodeisland", "South Carolina" = "Southcarolina", "South Dakota" = "Southdakota", "Tennessee" = "Tennessee", "Texas" = "Texas", "Utah" = "Utah", "Vermont" = "Vermont", "Virginia" = "Virginia", "Washington" = "Washington", "West Virginia" = "Westvirginia", "Wisconsin" = "Wisconsin", "Wyoming" = "Wyoming"))),
                    box(width = 8, title = "Cumulative Wells over Time", status = "primary", 
                        plotOutput(outputId = "w4_plot", height = 400)),
                    box(width = 12, p("Explore how many wells have been built over time. How many wells existed in the past compared to present day?"), em("Note: Displays cumulative unique wells in the US throughout time; does not account for wells that are abandoned or closed.")),
                )),
            
            # TAB 5 --------------------------------------------------------------------------
            tabItem(
                tabName = "w5", 
                fluidRow(
                    box(width = 4, selectInput(inputId = "state_select_w5", 
                                               label = "Choose a State", 
                                               choices = list("Alaska" = "Alaska", "Arizona" = "Arizona", "Arkansas" = "Arkansas", "California" = "California", "Colorado" = "Colorado", "Connecticut" = "Connecticut", "Delaware" = "Delaware", "District of Columbia" = "Districtcolumbia", "Florida" = "Florida", "Georgia" = "Georgia", "Hawaii" = "Hawaii", "Idaho" = "Idaho", "Illinois" = "Illinois", "Indiana" = "Indiana", "Iowa" = "Iowa", "Kansas" = "Kansassecond", "Kentucky" = "Kentucky", "Louisiana" = "Louisiana", "Maine" = "Maine", "Maryland" = "Maryland", "Massachusetts" = "Massachusetts", "Michigan" = "Michigan", "Minnesota" = "Minnesota", "Mississippi" = "Mississippi", "Missouri" = "Missouri", "Montana" = "Montana", "Nebraska" = "Nebraska", "Nevada" = "Nevada", "New Hampshire" = "Newhampshire", "New Jersey" = "Newjersey", "New Mexico" = "Newmexico", "New York" = "Newyork", "North Carolina" = "Northcarolina", "North Dakota" = "Northdakota", "Ohio" = "Ohio", "Oklahoma" = "Oklahoma", "Oregon" = "Oregon", "Pennsylvania" = "Pennsylvania", "Rhode Island" = "Rhodeisland", "South Carolina" = "Southcarolina", "South Dakota" = "Southdakota", "Tennessee" = "Tennessee", "Texas" = "Texas", "Utah" = "Utah", "Vermont" = "Vermont", "Virginia" = "Virginia", "Washington" = "Washington", "West Virginia" = "Westvirginia", "Wisconsin" = "Wisconsin", "Wyoming" = "Wyoming"))), 
                    box(width = 8, title = "What does a well record look like?", status = "primary", 
                        plotOutput(outputId = "w5_plot", height = 400)),
                    box(width = 12, p("Explore what a well water level record looks like. For each state, one unique well was chosen that had a minimum of 60 measurements for the longest time period in the state.", em("Note: Y-axis: Well water depth below land surface (feet); a negative measurement or measurement of zero indicates an artesian well, or a high water level. The higher number the measurement is recorded at, the lower the water was below the land surface.")),
                    )) 
            )
        )))

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# SERVER
#------------------------------------------------------------------------------------

server <- function(input, output) {
    
    # W2 reactive dataframe 
    
    w2_select <- reactive({
        
        w2 %>%
            filter(state == input$state_select_w2)
    })
    
    bb_select <- reactive({
        
        w2_bb %>% 
            filter(name == input$state_select_w2)
    }) 
    
    # W2 plot output 
    
    output$w2_plot <- renderPlot({
        
        min_lat <- bb_select()$xmin 
        max_lat <- bb_select()$xmax
        min_long <- bb_select()$ymin
        max_long <- bb_select()$ymax
        
        ggplot(data = usa) + 
            geom_sf() +
            geom_point(data = w2_select(),
                       aes(x = dec_long_va, 
                           y = dec_lat_va),
                       size = 0.1) +
            coord_sf(xlim = c(min_lat, max_lat), 
                     ylim = c(min_long, max_long), expand = FALSE) + 
            theme_light() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank()) 
    })
    
    # W3 reactive dataframe 
    
    w3_select <- reactive({
        
        w3 %>%
            filter(state == input$state_select_w3)
    })
    
    # W3 plot output 
    
    output$w3_plot <- renderPlot({
        
        ggplot(data = w3_select(), 
               aes(x = min_year, 
                   y = n)) +
            geom_point(color = "darkblue", size = 3) + 
            labs(x = "Year", 
                 y = "Number of Wells") + 
            theme_light() +
            theme(text = element_text(family = "Microsoft Tai Le", 
                                      size = 15))
    })
    
    # W4 reactive dataframe 
    
    w4_select <- reactive({
        
        w4 %>%
            filter(state == input$state_select_w4)
    })
    
    # W4 plot output 
    
    output$w4_plot <- renderPlot({
        
        ggplot(data = w4_select(), 
               aes(x = min_year, 
                   y = total_wells)) +
            geom_col(fill = "darkblue") +
            labs(x = "Year", 
                 y = "Number of Wells") + 
            theme_light() +
            theme(text = element_text(family = "Microsoft Tai Le", 
                                      size = 15))
    })
    
    # W5 reactive dataframe 
    
    w5_select <- reactive({
        
        w5 %>%
            filter(state == input$state_select_w5)
    })
    
    # W5 plot output 
    
    output$w5_plot <- renderPlot({
        
        ggplot(data = w5_select(), 
               aes(x = level_year, 
                   y = lev_va)) +
            geom_point(color = "darkblue", size = 2.5) +
            scale_y_reverse() +
            labs(x = "Year", 
                 y = "Water Level (feet below land surface)
         ") + 
            theme_light() +
            theme(text = element_text(family = "Microsoft Tai Le", 
                                      size = 15))
    })
    
}

shinyApp(ui = ui, server = server)