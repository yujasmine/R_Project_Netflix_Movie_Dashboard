
library(tidyverse)        
library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(fontawesome)
library(lubridate)



load("clean_netflix.RData")


generate_filters <- function() {                                                
  tagList(
    sliderInput("duration", h4("Movie Duration(mins)"),                                                    # Side Bar Element 1: Movie duration
                min = 5, max = 255,
                value = c(5, 255), step = 1, sep = ""
    ),
    selectInput("country", h4("Country"),                                                            # Side Bar Element 2: country selection
                choices = c("All",netflix_df$country %>% unique()),
                selected = c("All"),
                multiple = FALSE
    ),
    selectInput("listed_in", h4("Movie Type"),
                choices = c("All", netflix_df$listed_in %>% unique()),                                         # Side Bar Element 3: movie type selection
                selected = c("All", "Comedies", "Romantic Movies", "Music & Musicals"), multiple = TRUE)
  )
}


ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Netflix Movie Dashboard",  
                                    titleWidth = 250,
                                    tags$li(class = "dropdown",
                                            tags$a(href="https://www.linkedin.com/in/yu-chieh-yu/", # Add in personal LinkedIn
                                                   target="_blank", 
                                                   icon("linkedin", "fa-1x", lib = "font-awesome")  
                                            )
                                    ),
                                    tags$li(class = "dropdown",
                                            tags$a(href="https://github.com/yujasmine",               # Add in Github
                                                   target="_blank", 
                                                   icon("github", "fa-1x", lib = "font-awesome") 
                                            )
                                    ),
                                    tags$li(class = "dropdown", class = "pull-right")
                    ),                                                                                           
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard Info", tabName = "panel1", icon = icon("film", lib = "font-awesome")), 
                        menuItem("Movie List", tabName = "table", icon = icon("list", lib = "font-awesome")),
                        menuItem("Movie Statistics", tabName = "panel2", icon = icon("chart-bar", lib = "font-awesome")),
                        generate_filters() 
                      ) 
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName ="panel1",                                                 # Content inside the first tab "Dashboard Info"
                                h4("Dashboard Introduction"),
                                tags$div("Welcome to the Netflix movie shiny app! I'm hoping to create a website for people to choose the right movie on Netflix that fits their needs. Whether you're picking the movie according to the duration, country, or type, you can find the best matching movie for your perfect movie night! The dataset includes movie releases from 1925 to 2021, giving you the opportunity to rediscover childhood favorites and explore the latest films."),
                                tags$br(),
                                h4("Behind the scene"),
                                tags$div("As a movie-lover, I grew up watching all kinds of different movies. I still remember when I was a kid, walking into the DVD stores with my family, renting a movie and buying some delicious food for the perfect movie night. Now that I've grown up and moved to another country, I still kept the old habit of watching movies. The only difference is that I now watch them on Netflix with all my friends on the other side of the world, but it can be a struggle to find the right movie for the perfect movie night sometimes. That's why I decided to make this Shiny app."),
                                tags$br(),
                                h4("How to use the dashboard"),
                                tags$div("Please use the navigation bar on the left to find your desired movie list. First, use the slider to choose the desired duration of the movie. Second, select the country of production for the movie. Third, choose the genre of the movie. Lastly, you can see all the possible movies in the top left corner under 'Movie List'. If you want to view more statistics about the movie, go to the 'Movie Statistics' tab to discover some interesting facts about Netflix movies!"),
                        ),
                        tabItem(tabName ="table",                                                   # Content inside the second tab "Movie List"
                                DT::dataTableOutput("ttl_table")
                        ),
                        tabItem(tabName = "panel2",                                                 # Content inside the third tab "Movie Statistics"
                                h4("Netflix Movie Statistics"),
                                tabsetPanel(
                                  tabPanel("Movie Duration Distribution",                           # See the movie duration distribution
                                           tags$br(),
                                           tags$div(
                                             HTML("<p>To observe the distribution of movie durations on Netflix, please use the navigation bar on the left to customize the criteria. The resulting histogram shows that the majority of movies on Netflix have a duration of approximately 90 to 120 minutes. This information could be valuable to producers, who may wish to consider creating movies with a duration in this range to cater to the taste of Netflix's audience.</p>")
                                           ),
                                           fluidRow(
                                             column(12, plotOutput("plot_1", height = 300)) 
                                           ),
                                           fluidRow(
                                             column(12, DT::dataTableOutput("table_1"))
                                           )
                                  ),
                                  tabPanel("Movie Rating Bar Chart",                                 # See the movie rating bar chart
                                           tags$br(),
                                           tags$div(
                                             HTML("<p>This bar chart displays the number of movies in each rating classification. The rating system is divided into five levels, namely G, PG, PG-13, R, and NC-17. Additionally, there is an NR rating, which stands for Not Rated. This rating is assigned to films that have not been submitted for classification or are uncut versions of films that were submitted. In such cases, the labels Not Rated (NR) or Unrated (UR) are often used.</p>")
                                           ),
                                           fluidRow(
                                             column(12, plotOutput("plot_2", height = 300)) 
                                           ),
                                           fluidRow(
                                             column(12, DT::dataTableOutput("table_2"))
                                           )
                                  ),
                                  tabPanel("Movie Added Heatmap",                                   # See the movie added heatmap 
                                           tags$br(),
                                           tags$div(
                                             HTML("<p>The heatmap shows the frequency of how often the movie is added on to Netflix's platform. From the heatmap, we can see from the 2019 NOV to 2020 JAN, Netflix added the most movie on its platform. A part of the reason could be it's related to holiday seasons, where families gather to watch movies together!</p>")
                                           ),
                                           fluidRow(
                                             column(12, plotOutput("plot_3", height = 300)) 
                                           )
                                  )
                                )
                        )
                      )
                    )
)



server <- function(input, output){
  data <- reactive({
    filtered_data <- netflix_df %>%
      filter(duration >= input$duration[1], 
             duration <= input$duration[2])
    
    if (!"All" %in% input$listed_in) {
      filtered_data <- filtered_data %>%
        filter(str_detect(listed_in, paste(input$listed_in, collapse = "|")))
    }
    
    if (!"All" %in% input$country) {
      filtered_data <- filtered_data %>%
        filter(str_detect(country, paste(input$country, collapse = "|")))
    }
    
    return(filtered_data)
  })
  
  # Output ttl_table for movie list
  output$ttl_table <- DT::renderDataTable({
    data() %>%
      datatable(
        options = list(
          lengthMenu = list(c(10, 20, 30), c('10', '20', '30')),
          pageLength = 10, scrollX = TRUE, searching = TRUE                                      
        )
      )
  })
  
  # output plot_1 - histogram of movie duration
  output$plot_1 <- renderPlot({
    data() %>%
      group_by(title) %>%
      summarise(Movie_duration = duration) %>%
      ggplot(aes(x = Movie_duration, fill = stat(count))) +
      geom_histogram(binwidth = NULL, aes(y = ..density..)) +
      scale_fill_gradient(low = "black", high = "red") +
      theme_minimal() +
      ylab("movie_counts")
  }, height = 300)
  
  # Output plot_1 table for duration
  output$table_1 <- DT::renderDataTable({
    data() %>%
      datatable(
        options = list(
          lengthMenu = list(c(5, 10, 20, 30), c('5', '10', '20', '30')),
          pageLength = 5, scrollX = TRUE                                         
        )
      )
  })
  
  # Output plot_2 - Bar chart of movie rating 
  output$plot_2 <- renderPlot({
    data() %>%
      group_by(rating) %>%
      summarise(movie_counts = n()) %>%
      ggplot(aes(x = fct_reorder(rating, movie_counts), y = movie_counts, fill = movie_counts)) +
      geom_col() +
      scale_fill_gradient(low = "black", high = "red") +
      theme_minimal() +
      xlab("movie_rating")
  }, height = 300)
  
  # Output plot_2 table for rating
  output$table_2 <- DT::renderDataTable({
    data() %>%
      datatable(
        options = list(
          lengthMenu = list(c(5, 10, 20, 30), c('5', '10', '20', '30')),
          pageLength = 5, scrollX = TRUE                                      
        )
      )
  })
  
  # Output plot_3 - Heat Map of movie Added 
  output$plot_3 <- renderPlot({
    netflix_df$date_added <- ymd(netflix_df$date_added) # convert date_added to date format
    
    df_counts <- netflix_df %>%
      mutate(year = year(date_added), month = month(date_added, label = TRUE)) %>%
      group_by(year, month) %>%
      summarise(movie_counts = n())
    
    ggplot(df_counts, aes(x = year, y = month, fill = movie_counts)) +
      geom_tile() +
      scale_fill_gradient(low = "#EC9696", high = "darkred") +
      theme_minimal() +
      xlab("Year") +
      ylab("Month") +
      ggtitle("Number of Movies Added by Netflix per Month/Year") +
      scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022))
  }, height = 500)
  
  
  output$pt <- DT::renderDataTable({data()})
  
  
  output$text <- renderText("Choose parameters")
}


shinyApp(ui = ui, server = server)



