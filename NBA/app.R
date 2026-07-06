library(shiny)
library(bslib)


# Read the CSV file into an R object
NBA_data <- read_csv("~/ST558_Project2/NBA_PBP_2019-20.csv")

# Save the object as an RData file
save(NBA_data, file = "NBA_PBP_2019-20.RData")

load("NBA_PBP_2019-20.RData")

# Define UI for application that draws a histogram
ui <- page_sidebar(
  
  # Application title
  title = "NBA PBP Analytics Dashboard",
  
  # Sidebar with a slider input for number of bins 
  sidebar = sidebar(
    title = "Filter Controls",
    width = 340, 
    
    hr(),
    
    h6("Categorical Filtering"),
    
    checkboxGroupInput("game_type", "Game Type:",
                       choices = c("playoff", "regular"),
                       selected = c("playoff", "regular")),
    checkboxGroupInput("game_type_all", "Select All Game Types"),
    
    checkboxGroupInput("shot_outcome", "Shot Outcome:",
                       choices = c("miss", "make"),
                       selected = c("miss", "make")),
    checkboxGroupInput("shot_outcome_all", "Select All Shot Outcomes"),
    
    checkboxGroupInput("shot_type_group", "Shot Type Group:",
                       choices = c("3-Pointers", "Inside Shots", "Mid-Range"),
                       selected = c("3-Pointers", "Inside Shots", "Mid-Range")),
    checkboxGroupInput("shot_type_group_all", "Select All Shot Type Group"),
    
    
    hr(),
    
    h6("Numerical Filtering"),
    
    selectInput("num_var1", "Numeric Variable 1:",
                choices = c("None", "ShotDist", "SecLeft", "AwayScore", "HomeScore"),
                selected = "ShotDist"),
    
    
    selectInput("num_var2", "Numeric Variable 2:",
                choices = c("None", "ShotDist", "SecLeft", "AwayScore", "HomeScore"),
                selected = "ShotDist"),
    hr(),
    
    actionButton("apply", "Apply Filters"),
  ),  
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("About",
                 h3("Purpose of the App"),
                 p("This Shiny application help users explore and filter NBA play-by-play shooting data. Use the sidebar to filter the dataset, and click the 'Apply Filters' button to run the subset."),
                 h4("Database Information"),
                 p("The data representing single events from the 2019-20 regular and playoff session."),
                 h4("Court Visualization")
                 ),
      
        tabPanel("Data Download",
                 h3("Subsetted Data Table"),
                 downloadButton("download", "Download Filtered Dataset"),
                 br(),
                 DT::dataTableOutput("data_table")
                 ),
        tabPanel("Data Exploration",
                 h3("Contingency Tables"),
                 fluidRow(
                   column(6,
                   h4("One-way Table (Counts)"),
                   selectInput("one-way","Categorical Column:",
                               choices = c("GameType","ShotOutcome", "Quarter", "ShotTypeGroup")),
                   tableOutput("One-way-table")
                 ),
                  column(6, 
                         h4("Two-way Table (Cross Counts)"),
                         selectInput("two-way_row","Row Variable:",
                                     choices = c("GameType","ShotOutcome", "Quarter")),
                         selectInput("two-way_col","Column Variable:",
                                     choices = c("GameType","ShotOutcome", "Quarter")),
                         tableOutput("Two-way-table")
                         )
                  ),
                 hr(),
                 h3("Exploration Graphs"),
                 fluidRow(
                   column(4, 
                          radioButtons("plot_choice","Choose Visulization Plot:",
                                       choices = c(
                                         "Plot 1: Shot Type Counts (Bar)" = "plot1",
                                         "Plot 2: Density Plot (Shot Distance)" = "plot2",
                                         "Plot 3: Side-by-side Bar Chart (Multivariate - Shot Outcomes by Game Type)" = "plot3",
                                         "Plot 4: Box Plot (Multivariate - Shot Distance by Shot Outcomes)" ="plot4",
                                         "Plot 5: Faceted Histogram (Multivariate & Faceted - Shot Distance by Quarter)" = "plot5",
                                         "Plot 6: Scatter Plot (Multivariate - Shot Distance vs. Seconds Left)" = "plot6",
                                         "Plot 7: Bivariate Highest Density Region (HDR) Plot" = "plot7"
                                       ))
                          ),
                   column(8,
                          plotOutput("graph_out"))
                 )
               )
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveValues(
    filter()
  )
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
