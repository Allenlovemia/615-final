library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(DT)
library(jsonlite)
library(tidyverse)
library(plotly)
library(tidyr)
library(reshape2)
library(scales)
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

gdp_data <- read.csv("GDP.csv")
gdp_long <- melt(gdp_data, id.vars = "Country.Name", variable.name = "Year", value.name = "GDP")
gdp_long$Year <- as.numeric(gdp_long$Year)+1969

swot_data <- list(
  Strengths = data.frame(
    Aspect = c("Natural Resources", "Biodiversity", "Cultural Heritage", "Agricultural Potential", "Renewable Energy Resources"),
    Description = c(
      "Rich in natural resources, including forests, minerals, and fisheries, which can be leveraged for economic growth.",
      "High biodiversity with potential for eco-tourism and conservation projects.",
      "Strong cultural heritage and diversity can be attractive to tourists.",
      "Fertile land that is suitable for various types of agriculture.",
      "Potential for the development of renewable energy sources like hydro, solar, and wind."
    )
  ),
  Weaknesses = data.frame(
    Aspect = c("Geographic Dispersion", "Economic Dependency", "Limited Industrial Base", "Infrastructure Deficits", "Challenges in Public Services"),
    Description = c(
      "The country consists of many islands, which makes infrastructure development and governance challenging.",
      "Heavy reliance on a few key sectors, such as logging and fisheries, makes the economy vulnerable to shocks.",
      "Lack of diversification in industry can limit economic opportunities and resilience.",
      "Inadequate infrastructure in terms of transportation, utilities, and communication.",
      "Education, healthcare, and other public services may be underdeveloped, affecting human capital development."
    )
  ),
  Opportunities = data.frame(
    Aspect = c("Tourism Development", "Foreign Investment", "Trade Partnerships", "Sustainable Practices", "Regional Cooperation"),
    Description = c(
      "Potential to expand the tourism industry, especially eco-tourism and cultural tourism.",
      "Attracting foreign direct investment for resource development and infrastructure.",
      "Forming trade partnerships with other nations to expand market access for Solomon Islands products.",
      "Opportunity to develop sustainable fishing and logging practices.",
      "Engaging in regional cooperation for economic development and stability."
    )
  ),
  Threats = data.frame(
    Aspect = c("Climate Change", "Over-reliance on Imports", "Political Instability", "Global Market Fluctuations", "Environmental Degradation"),
    Description = c(
      "Vulnerability to climate change and natural disasters, which can disrupt the economy and livelihoods.",
      "Dependence on imported goods for many basic necessities can be problematic if supply chains are disrupted.",
      "Risk of political instability that can deter investment and development.",
      "Prices for key export commodities can be volatile on the global market.",
      "Unsustainable exploitation of natural resources could lead to long-term environmental damage."
    )
  )
)



ui <- fluidPage(
  titlePanel("Solomon Islands Overview"),
  
  # Use tabsetPanel to create tabs for each section of the app
  tabsetPanel(
    tabPanel("Map and Information", leafletOutput("map")),
    tabPanel("Historical Timeline", sliderInput("yearRange", "Select Year Range:", min = 1550, max = 2020, value = c(1550, 2020), step = 10), DTOutput("eventTable"), plotOutput("timelinePlot")),
    tabPanel("Key Demographics", DTOutput("solomon_table")),
    tabPanel("Population Trends", selectInput("year", "Select Year", choices = c("1999", "2009", "2022")), plotOutput("populationPlot")),
    tabPanel("Comparative Analysis", selectInput("category", "Select Category:", choices = c("Geography", "Culture", "History", "Economy", "Demographics")), tableOutput("infoTable")),
    tabPanel("GDP Comparison", sliderInput("yearRangeGDP", "Select Year Range:", min = min(gdp_long$Year), max = max(gdp_long$Year), value = c(min(gdp_long$Year), max(gdp_long$Year)), step = 1, ticks = TRUE, animate = TRUE), plotOutput("gdpPlot")),
    tabPanel("SWOT Analysis", DTOutput("strengthsTable"), DTOutput("weaknessesTable"), DTOutput("opportunitiesTable"), DTOutput("threatsTable"))
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

  output$gdpPlot <- renderPlot({
    filtered_data <- subset(gdp_long, Year >= input$yearRange[1] & Year <= input$yearRange[2])
    ggplot(filtered_data, aes(x = Year, y = GDP, group = Country.Name, color = Country.Name)) +
      geom_line() +
      geom_point() +  
      scale_color_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "GDP Over Time", x = "Year", y = "GDP (USD)") +
      scale_y_continuous(labels = scales::dollar)  
  })
  
  output$strengthsTable <- renderDataTable({datatable(swot_data$Strengths)})
  output$weaknessesTable <- renderDataTable({datatable(swot_data$Weaknesses)})
  output$opportunitiesTable <- renderDataTable({datatable(swot_data$Opportunities)})
  output$threatsTable <- renderDataTable({datatable(swot_data$Threats)})
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


