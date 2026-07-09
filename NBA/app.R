library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(ggstatsplot)

library(DT)
library(plotly)

# Read the CSV file into an R object
NBA_data <- read_csv("~/ST558_Project2/NBA_PBP_2019-20.csv")

# Save the object as an RData file
# save(NBA_data, file = "NBA_PBP_2019-20.RData")

load("NBA_PBP_2019-20.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("NBA Play-by-Play Analysis App"),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h4("1. Categorical Filters"),
        p("Select levels to subset the data:"),
        checkboxGroupInput("game_type_all", "Game Type:",
                         choices = c("playoff", "regular"),
                         selected = character(0)),
        br(),
        checkboxGroupInput("game_type", "Select All Game Types"),
        br(),
        checkboxGroupInput("shot_outcome", "Shot Outcome:",
                         choices = c("miss", "make"),
                         selected = character(0)),
        checkboxGroupInput("shot_outcome", "Select All Shot Outcomes"),
        br(),
        checkboxGroupInput("shot_type_group", "Shot Type Group:",
                         choices = c("3-Pointers", "Inside Shots", "Mid-Range"),
                         selected = character(0)),
        checkboxGroupInput("shot_type_group_all", "Select All Shot Type Groups"),
        br(),
        checkboxGroupInput("quarter", "Quarter:",
                         choices = c("1","2","3","4","5","6"),
                         selected = character(0)),
        checkboxGroupInput("quarter_all", "Select All Quarters"),
        br(),
        checkboxGroupInput("foul_type", "Foul Types:",
                           choices = c("shooting","offensive","personal","loose ball",
                                       "techinical","away from play","personal take",
                                       "flagrant","clear path"),
                           selected = character(0)),
        checkboxGroupInput("foul_type_all", "Select All Foul Types"),
        hr(),
        
        h4("2. Numerical Filters"),
        p("Dynamic range sliders for continuous variables:"),
        selectInput("num_var1", "Numeric Variable 1:",
                    choices = c("None", "ShotDist", "SecLeft", "AwayScore", "HomeScore"),
                    selected = "ShotDist"),
        uiOutput("slider1_ui"),
        selectInput("num_var2", "Numeric Variable 2:",
                    choices = c("None", "ShotDist", "SecLeft", "AwayScore", "HomeScore"),
                    selected = "ShotDist"),
        uiOutput("slider2_ui"),
        hr(),
        actionButton("apply", "Apply Filters")
      ),  

      mainPanel(
        tabsetPanel(
          id = "main_tabs",
          
          # --- TAB 1: ABOUT ---
          tabPanel("About",
              h3("Purpose of the App"),
              p("This Shiny application help users to explore, filter, analyze, and visualize NBA play-by-play shooting dataset from the  2019-20 regular seasons and playoffs.
                Use the sidebar to filter the dataset, and click the 'Apply Filters' button to run the subset."),
              
              h4("Data & Souce Information"),
              p("The data captures event-level play-by-play shooting records, including games type, shot distances, outcomes, quarter progression, and foul types. 
               For more detail documentation on NBA statistics and play-by-play tracking methodology, please visit the",
               tags$a(href = "https://www.kaggle.com/datasets/schmadam97/nba-playbyplay-data-20182019?select=NBA_PBP_2019-20.csv",
                      target = "_blank",
                      "NBA Play-by-Play Data 2015-2021"),
                      "created by SCHMADAMCO."
               ),
              
              h4("NBA Logo"),
              tags$img(src = "https://storage.googleapis.com/kaggle-datasets-images/229525/490833/81a9b62c4e207da2202af79ec2813f51/dataset-cover.png?t=2019-06-12-22-56-14", height = "150px"),
              
              h4("Tab Guide"),
                tags$ul(
                tags$li(strong("Sidebar Panel: "), "Allows subsetting on categorcial variabels and numeric sliders. Click", strong("Apply Filters"), "to unpdate."),
                tags$li(strong("Data Download Tab: "), "Interactive table of subsetted records"),
                tags$li(strong("Data Exploration Tab: "), "Contains Subtabs for One/Two-way contingency tables, numeric summary stats (n, mean, median, sd, min, max), and assigned Plots 1 through 7.")
                )
        ),
        
        # --- TAB 2: DATA DOWNLOAD ---
        tabPanel("Data Download",
                h3("Subsetted Data Table"),
                downloadButton("download", "Download Filtered Dataset"),
                br(), br(),
                DT::dataTableOutput("data_table")
       ), 
        
        # --- TAB 3: DATA EXPLORATION ---
        tabPanel(title = "Data Exploration",
                 tabsetPanel(
                   id = "explore_subtabs",
                   
                   tabPanel( "1. Categorical Summaries",
                             br(),
                             h4("1. One-Way Contingency Tables"),
                             selectInput("cat_oneway_var","Select Variable:",
                                         choices = c("GameType", 
                                                     "ShotType", 
                                                     "ShotOutcome",
                                                     "FoulType", 
                                                     "Quarter", 
                                                     "TurnoverType",
                                                     "ShotTypeGroup",
                                                     "ViolationType",
                                                     "WinningTeam",
                                                     "ReboundType")),
                             tableOutput("one_way_out"),
                             br(),
                             hr(),
                             h4("2. Two-Way Contingency Tables (Cross Counts)"),
                             fluidRow(
                               column(4, selectInput("cat_twoway_row","Row Variable:", 
                                                     choices = c("GameType","ShotType", "Quarter"),
                                                     selected = "GameType"
                               )
                               ),
                               column(4, selectInput("cat_twoway_col","Column Variable:",
                                                     choices = c("ShotOutcome", "FoulType", "TurnoverType", "Quarter"),
                                                     selected = "ShotOutcome"
                               )
                               )
                             ),
                             tableOutput("two_way_out"),
                   ),
                   
                   tabPanel("2. Numerical Summaries ",
                            br(),
                            h4("Summary Statistics across Categorical Groups (n, mean, median, min, max)"),
                            fluidRow(
                              column(6,
                                     selectInput("num_stat_var", "Numeric Variable:", 
                                                 choices = c("ShotDist", "AwayScore", "SecLeft", "HomeScore"),
                                                 selected = "ShotDist")
                              ),
                              column(6,
                                     selectInput("num_stat_group", "Group by Categorical Variable:",
                                                 choices = c("ShotOutcome", "GameType", "Quarter", "FoulType"),
                                                 selected = "ShotOutcome")
                              )
                            ),
                            br(),
                            tableOutput("num_summary_table"),
                            br(),
                   ),
                   
                   tabPanel("Interactive Visualizations",
                            br(),
                            fluidRow(
                              column(4,
                                     radioButtons("assigned_plot_choice","Choose Visualization Plot:",
                                                  choices = c("Plot 1: Shot Type Counts (Bar)" = "plot1",
                                                              "Plot 2: Density Plot (Shot Distance)" = "plot2",
                                                              "Plot 3: Side-by-side Bar Chart (Multivariate - Shot Outcomes by Game Type)" = "plot3",
                                                              "Plot 4: Box Plot (Multivariate - Shot Distance by Shot Outcomes)" ="plot4",
                                                              "Plot 5: Faceted Histogram (Multivariate & Faceted - Shot Distance by Quarter)" = "plot5",
                                                              "Plot 6: Scatter Plot (Multivariate - Shot Distance vs. Seconds Left)" = "plot6",
                                                              "Plot 7: Bivariate Highest Density Region (HDR) Plot" = "plot7"
                                                  ))
                                     
                              ),
                              column(8,
                                     plotlyOutput("assigned_plot_out")
                              )
                              
                            )
                        )
                    )
                )
            )
        )
    )
)





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv <- reactiveValues(filtered = NBA_data)
  
  # Select All for categorical checkboxes
  observeEvent(input$game_type_all, {
    if (isTRUE(input$game_type_all)) updateCheckboxGroupInput(session, "game_type",selected = c("playoff","regular"))
    else updateCheckboxGroupInput(session, "game_type", selected = character(0))
    }, ignoreInit = TRUE)
  
  observeEvent(input$shot_outcome_all, {
    if (isTRUE(input$shot_outcome_all)) updateCheckboxGroupInput(session, "shot_outcome",selected = c("miss","make"))
    else updateCheckboxGroupInput(session, "shot_outcome", selected = character(0))
  }, ignoreInit = TRUE)
  
  observeEvent(input$shot_group_all, {
    if (isTRUE(input$shot_group_all)) updateCheckboxGroupInput(session, "shot_group",selected = c("3-Pointers","Inside Shots","Mid-Range"))
    else updateCheckboxGroupInput(session, "shot_group", selected = character(0))
  }, ignoreInit = TRUE)
  
  observeEvent(input$quarter_all, {
    
    if (isTRUE(input$quarter_all)) updateCheckboxGroupInput(session, "quarter",selected = c("1","2","3","4","5","6"))
    else updateCheckboxGroupInput(session, "quarter", selected = character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$foul_type_all, {
   if (isTRUE(input$foul_type_all)) updateCheckboxGroupInput(session, "foul_type",selected = c("shooting","offensive","personal","loose ball","techinical","away from play","personal take","flagrant","clear path"))
  else updateCheckboxGroupInput(session, "foul_type", selected = character(0))
  }, ignoreInit = TRUE)
  
  # Dynamic numeric slider UIs
  output$slider1_ui <- renderUI({
    req(input$num_var1); if (input$num_var1 == "None") return(NULL)
    v <- input$num_var1; 
    lo <- min(NBA_data[[v]], na.rm = TRUE);
    hi <- max(NBA_data[[v]], na.rm = TRUE)
    sliderInput("slider1", paste("Range for", v), min = lo, max = hi, value = c(lo, hi))
  })
  
  output$slider2_ui <- renderUI({
    req(input$num_var2); if (input$num_var2 == "None") return(NULL)
    v <- input$num_var2; 
    lo <- min(NBA_data[[v]], na.rm = TRUE);
    hi <- max(NBA_data[[v]], na.rm = TRUE)
    sliderInput("slider2", paste("Range for", v), min = lo, max = hi, value = c(lo, hi))
  })
  
  observeEvent(input$apply, {
      req(NBA_data)
      df <- NBA_data
      
      if (length(input$game_type) > 0) df <- df %>% filter(GameType %in% input$game_type) 
      if (length(input$shot_outcome) > 0) df <- df %>% filter(ShotOutcome %in% input$shot_outcome) 
      if (length(input$shot_group) > 0) df <- df %>% filter(ShotTypeGroup %in% input$shot_group)
      if (length(input$quarter) > 0) df <- df %>% filter(Quarter %in% input$quarter) 
      if (length(input$foul_type) > 0) df <- df %>% filter(FoulType %in% input$foul_type) 
    
      if (!is.null(input$num_var1) && input$num_var1 != "None" && is.null(input$slider1)){
        v1 <- input$num_var1; df <- df %>% filter(.data[[v1]] >= input$slider1[1] & .data[[v1]] <= input$slider1[2])
      }
    
      if (!is.null(input$num_var2) && input$num_var2 != "None" && is.null(input$slider2)){
      v2 <- input$num_var2; df <- df %>% filter(.data[[v2]] >= input$slider1[1] & .data[[v2]] <= input$slider2[2])
      }
    
      rv$filtered <- df
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  output$data_table <- DT::renderDataTable({
    df <- rv$filtered ; validate(need(nrow(df) > 0, "No data available."))
    DT::datatable(df, options = list(pageLength = 10 , scrollX = TRUE),rownames = FALSE)
  })
  
  output$download <- downloadHandler( 
    filename = function() paste0("NBA_data_clean", Sys.Date(), ".csv"),
    content = function(file) write_csv(rv$filtered , file)
  )
  
  output$one_way_out <- renderTable({
    df <- rv$filtered ; validate(need(nrow(df) > 0, "No records."))
    v <- input$cat_oneway_var
    as.data.frame(table(df[[v]])) %>% rename(Category_Level = Var1, Frequency_Count = Freq)
  })
  
  output$two_way_out <- renderTable({
    df <- rv$filtered ; validate(need(nrow(df) > 0,"No records."))
    v1 <- input$cat_twoway_row; v2 <- input$cat_twoway_col
    as.data.frame.matrix(table(df[[v1]],df[[v2]])) %>% rownames_to_column("Row / Col")
  })
  
  output$num_summary_table <- renderTable({
    df <- rv$filtered ; validate(need(nrow(df) > 0, "No records."))
    n_var <- input$num_stat_var; g_var <- input$num_stat_group
    df %>%
      filter(!is.na(.data[[n_var]]), !is.na(.data[[g_var]])) %>%
      group_by(Level = .data[[g_var]]) %>%
      summarise(
        n = n(),
        mean = round(mean(.data[[n_var]], na.rm = TRUE), 2),
        median = round(median(.data[[n_var]], na.rm = TRUE), 2),
        sd = round(sd(.data[[n_var]], na.rm = TRUE), 2),
        min = round(min(.data[[n_var]], na.rm = TRUE), 2),
        max = round(max(.data[[n_var]], na.rm = TRUE), 2),
        .groups = "drop"
      )
  })
  
  output$assigned_plot_out <- renderPlotly({
    df <- rv$filtered ; validate(need(nrow(df) > 0, "No data."))
    NBA_data_clean <- df %>% filter(!is.na(ShotOutcome), !is.na(ShotDist), !is.na(GameType))
    validate(need(!is.null(NBA_data_clean) && nrow(NBA_data_clean) > 0, "All entries are NA in subset."))
    
    req(input$assigned_plot_choice)
    choice <- input$assigned_plot_choice
    
    if (choice == "plot1") {
      g <- ggplot(NBA_data_clean, aes(x = ShotType)) + geom_bar() +
        labs(
          title = "Counts of Shot Types",
          subtitle = "2019-20 NBA Play-by-Play Data",
          x = "Shot Outcome",
          y = "Count"
        ) + 
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
      ggplotly(g)
    } else if (choice == "plot2") {
      g <- ggplot(NBA_data_clean, aes(x = ShotDist)) +
        geom_density(fill = "#1e3d59", alpha = 0.8 ) +
        labs(title = "Density of Shot Distance",
             x = "Shot Outcome", 
             y = "Density") + theme_minimal()
      ggplotly(g)
    } else if (choice == "plot3") {
      g <- ggplot(NBA_data_clean, aes(x = ShotOutcome, fill = GameType)) +
          geom_bar(position = "dodge", alpha = 0.9) +
          scale_fill_manual(values = c("playoff" = "#ff6e40", "regular" = "#1e3d59")) +
        labs(
          title = "Counts of Shot Outcomes by Game Type",
          x = "Shot Outcome",
          y = "Count",
          fill = "Game Type"
       ) 
      ggplotly(g)
    } else if (choice == "plot4") {
      g <- ggplot(NBA_data_clean , aes(x = ShotOutcome, y = ShotDist, fill = ShotOutcome)) +
          geom_boxplot()+
          scale_fill_manual(values = c("miss" = "#e85a4f", "make" = "#45b6fe"))
          labs(
          title = "Shot Distance by Shot Outcome",
            x = "Shot Outcome",
            y = "Shot Distance"
          )
      ggplotly(g)
    } else if (choice == "plot5") {
      g<-  ggplot(NBA_data_clean, aes(x = ShotDist, fill = ShotOutcome)) +
          geom_histogram(bins = 20, position = "identity", alpha = 0.6) +
          facet_wrap(~ Quarter, labeller = label_both) +
          scale_fill_manual(values = c("miss" = "#e85a4f", "make" = "#45b6fe"))
         labs(
            title = "Density of Shot Distance by Quarter and Outcome",
            x = "Shot Outcome",
            y = "Density",
            fill = "Shot Outcome"
          )
      ggplotly(g)
    } else if (choice == "plot6") {
      g <- ggplot(NBA_data_clean, aes(x = SecLeft, y = ShotDist, color = ShotOutcome)) +
            geom_point(alpha = 0.6) +
            scale_color_manual(values = c("miss" = "#e85a4f", "make" = "#45b6fe"))
            labs(
              title = "Shot Distance vs. Seconds Left in Quarter",
                x = "Seconds Remaining in Quarter",
                y = "Shot Distance(feet)",
                color = "Shot Outcome"
              ) 
      ggplotly(g)
    } else if (choice == "plot7") {
      top3_shot_types <- NBA_data_clean %>% 
        dplyr::count(ShotType, sort = TRUE) %>%
        slice_head(n = 3) %>%
        pull(ShotType)
      
      NBA_top3 <- NBA_data_clean %>% 
        filter(ShotType %in% top3_shot_types)
      
      g <- ggbetweenstats(
            data  = NBA_top3,
            x     = ShotType, 
            y     = ShotDist,
            title = "Shot Distance by Shot Type(Top3)",
            xlab  = "Shot Type",
            ylab  = "Shot Distance(feet)",
            type  = "nonparametric",
            pairwise.comparisons = TRUE, 
            p.adjust.method = "BH"
          )
      ggplotly(g)
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
