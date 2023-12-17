library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(DT)
library(jsonlite)
library(tidyverse)
library(plotly)
library(rsconnect)

solomon_islands <- st_read("solomon islands.geojson")
solomon_data <- data.frame(
  year = c(1568, 1886, 1893, 1942, 1978, 1998),
  event = c("First European contact by Alvaro de Mendana", 
            "British-German Agreement", 
            "British Protectorate established", 
            "World War II", 
            "Independence from UK", 
            "Ethnic tensions and civil war"),
  category = c("Exploration", "Colonialism", "Colonialism", "War", "Independence", "Conflict")
)

data <- data.frame(
  Category = rep(c("Geography", "Culture", "History", "Economy", "Demographics"), each = 5),
  Island = c(rep(c("Solomon Islands", "Fiji", "Vanuatu", "Papua New Guinea", "New Caledonia"), 5)),
  Information = c(
    # Geography
    "Six major islands and over 900 smaller ones, volcanic mountain ranges, dense rainforests, lagoons",
    "More than 330 islands, rugged landscapes, palm-lined beaches, coral reefs",
    "About 82 volcanic islands",
    "Eastern half of New Guinea, dense rainforests, active volcanoes",
    "French territory, numerous islands, palm-lined beaches, rich marine life",
    # Culture
    "Diverse ethnic groups, wood carvings, music, dance",
    "Multicultural, indigenous Fijian, Indo-Fijian, Pacific Islander cultures",
    "Strong Melanesian culture, land diving rituals",
    "Hundreds of ethnic groups and languages",
    "Mix of French and Melanesian cultures, Kanak traditions",
    # History
    "Influenced by European exploration, colonialism, and World War II",
    "Former British colony until 1970",
    "Anglo-French condominium until 1980",
    "Independent from Australia since 1975",
    "French territory, with ongoing independence discussions",
    # Economy
    "Based on agriculture, forestry, fishing, and mining",
    "Diversified economy with tourism, sugar, and garment industries",
    "Agricultural economy with significant tourism",
    "Rich in natural resources, with an economy centered on mining",
    "Large economy supported by nickel mining and French financial aid",
    # Demographics
    "Predominantly Melanesian, with Polynesian and Micronesian communities",
    "Majority indigenous Fijians and Indo-Fijians",
    "Primarily Ni-Vanuatu (Melanesian)",
    "Over 800 languages spoken",
    "Majority Kanak (indigenous Melanesians), with Europeans, Polynesians, and Southeast Asians"
  ))

solomon_islands_data <- data.frame(
  Number = 1:9,
  Province = c("Central Province", "Choiseul Province", "Guadalcanal Province", "Isabel Province", "Makira-Ulawa Province", "Malaita Province", "Rennell and Bellona Province", "Temotu Province", "Western Province"),
  Capital = c("Tulagi", "Taro Island", "Honiara", "Buala", "Kirakira", "Auki", "Tigoa", "Lata", "Gizo"),
  Premier = c("Stanely Manetiva", "Harrison Benjamin", "Anthony Veke", "Lesley Kikolo", "Julian Maka'a", "Martin Fini", "Japhet Tuhanuku", "Clay Forau", "David Gina"),
  Area = c(615, 3837, 5336, 4136, 3188, 4225, 671, 895, 5475),
  Population_1999 = c(21577, 20008, 60275, 20421, 31006, 122620, 2377, 18912, 62739),
  Population_per_km2_2009 = c(42.4, 6.9, 17.5, 6.3, 12.7, 32.6, 4.5, 23.9, 14.0),
  Population_2009 = c(26051, 26371, 93613, 26158, 40419, 137596, 3041, 21362, 76649),
  Population_2022 = c(33476, 38453, 166838, 36688, 57396, 163085, 4465, 25701, 102083)
)







ui <- fluidPage(
  titlePanel("Solomon Islands Overview"),
  
  tabsetPanel(
    tabPanel("Map and Information", 
             leafletOutput("map"),
             fluidRow(
               column(4, wellPanel(
                 h4("Location"),
                 "Southwestern Pacific Ocean"
               )),
               column(4, wellPanel(
                 h4("Major Islands"),
                 "6 major and over 900 smaller islands"
               )),
               column(4, wellPanel(
                 h4("Capital"),
                 "Honiara, located on Guadalcanal"
               )),
               column(4, wellPanel(
                 h4("Area"),
                 "Approximately 29,000 square kilometers"
               ))
             )),
    
    tabPanel("Key Facts",
             sidebarPanel(
               sliderInput("yearRange", "Select Year Range:",
                           min = 1550, max = 2020, value = c(1550, 2020), step = 10)
             ),
             mainPanel(
               DTOutput("eventTable"),
               plotOutput("timelinePlot")
             )),
    
    tabPanel("Demographics",
             DTOutput("solomon_table"),
             selectInput("year", "Select Year", choices = c("1999", "2009", "2022")),
             plotOutput("populationPlot")
    ),
    
    tabPanel("Comparative Analysis",
             sidebarPanel(
               selectInput("category", "Select Category:", 
                           choices = c("Geography", "Culture", "History", "Economy", "Demographics"))
             ),
             mainPanel(
               tableOutput("infoTable")
             )),
    
    tabPanel("Economy",
             plotlyOutput("gdpPieChart")
    )
  )
)

server <- function(input, output) {
  
  # Map rendering
  output$map <- renderLeaflet({
    leaflet(data = solomon_islands) %>%
      addTiles() %>%
      addPolygons()
  })
  
  # Key Facts rendering
  output$eventTable <- renderDT({  
    filtered_data <- solomon_data[solomon_data$year >= input$yearRange[1] & solomon_data$year <= input$yearRange[2], ]
    datatable(filtered_data, options = list(pageLength = 5))
  })
  
  output$timelinePlot <- renderPlot({
    filtered_data <- solomon_data[solomon_data$year >= input$yearRange[1] & solomon_data$year <= input$yearRange[2], ]
    
    filtered_data$period <- cut(filtered_data$year,
                                breaks = c(1550, 1886, 1978, 2020),
                                labels = c("Arrival of Europeans", "Colonial Period", "Independence Era"),
                                include.lowest = TRUE)
    
    ggplot(filtered_data, aes(x = year, y = category, label = event, color = period)) +
      geom_point(size = 4) +
      geom_text(vjust = 2, angle = 45, hjust = 1, size = 4) +
      scale_x_continuous(breaks = filtered_data$year) +
      scale_color_manual(values = c("red", "black", "blue")) +
      theme_minimal() +
      labs(title = "Key Events in Solomon Islands' History", x = "Year", y = "Category") +
      theme(axis.text.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "lightblue"),
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_line(color = "white"),
            legend.position = "bottom")
  })

  
  # Demographics rendering
  output$solomon_table <- renderDT({
    datatable(solomon_islands_data, options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '200px', targets = "_all"))
    ))
  })
  
  output$populationPlot <- renderPlot({
    year_col <- paste0("Population_", input$year)
    data_to_plot <- solomon_islands_data %>%
      select(Province, Population = all_of(year_col))
    ggplot(data_to_plot, aes(x = Province, y = Population, fill = Province)) +
      geom_bar(stat = "identity", color = "black", size = 0.5) +  # Adds a border to the bars
      theme_minimal(base_size = 14) +  # Uses a minimal theme with larger base font size
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"), # Bolder, larger x-axis labels
        axis.text.y = element_text(size = 12, face = "bold"), # Bolder y-axis labels
        axis.title = element_text(size =  14, face = "bold"), # Bolder axis titles
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5) # Bolder and centered plot title
      ) +
      labs(y = "Population", x = "Province", title = paste("Population of Solomon Islands Provinces in", input$year)) +
      scale_fill_brewer(palette = "Paired")  
    
  })
  
  # Comparative Analysis rendering
  output$infoTable <- renderTable({
    # Filter data based on the selected category
    filteredData <- data %>% filter(Category == input$category)
    return(filteredData)
  })
  
  # Economy rendering
  output$gdpPieChart <- renderPlotly({
    # Data for the pie chart
    sectors <- c("Agriculture", "Industry", "Services")
    percentages <- c(37.7, 6.4, 55.9)
    colors <- c('#FF9999', '#66B2FF', '#99CC99')  # Custom color palette
    
    # Create the pie chart
    plot_ly(labels = sectors, values = percentages, type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial', marker = list(colors = colors),
            hoverinfo = 'label+percent+name') %>%
      layout(
        legend = list(x = 1, y = 0.5),
        titlefont = list(size = 20))
  })
}

# Run the application
shinyApp(ui = ui, server = server)


