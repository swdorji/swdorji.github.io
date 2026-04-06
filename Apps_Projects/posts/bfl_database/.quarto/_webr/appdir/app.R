library(shiny)
library(dplyr)
library(DT)
library(shinyjs)
library(googlesheets4)
library(janitor)
library(DT)
library(tidyr)
library(plotly)
library(bslib)


# Load datasets
T1 <- read.csv("Goals_1.csv")
T2 <- read.csv("Milestone_2.csv")
T3 <- read.csv("Activity_3.csv")
T4 <- read.csv("Subactivity_4.csv")
AWP <- read.csv("AWP.csv")
T4$Activity <- as.character(T4$Activity)
T3$Activity <- as.character(T3$Activity)


# Google Sheets authentication
#gs4_auth(path = "service-account.json")
##keep if you want to pull directly from sheets (I was using sample data)
#sheet_id_awp <- "1n2xMiUyMSWCMRQBVVbiaRNnJ_idNs4ILSrTBV9Rd8RQ"


##import gender data
gender<- read.csv("gender_data.csv")
gender<- gender |> drop_na()

completion<- read.csv("per_completion.csv")
completion<- completion |> clean_names() |> drop_na()
names(completion)<- c("Milestone", "2019 Completion Percentage","2020 Completion Percentage","2021 Completion Percentage",
                      "2022 Completion Percentage", "2023 Completion Percentage", "2024 Completion Percentage")

completion_long<- completion |> pivot_longer(c("2019 Completion Percentage" : "2024 Completion Percentage"), names_to = "year", values_to = "percentages" )  

gender_long<- gender |> pivot_longer(c("Total", "Male", "Female"), names_to = "gender", values_to = "numbers_reached")

# UI

ui <- page_navbar(
  title = "Bhutan For Life Dashboard",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c7a4b",
    secondary = "#d73",
    base_font = font_google("Inter")
  ),
  
  # Sidebar shown on all tabs
  nav_panel("Gender Impact Dashboard",
            layout_sidebar(
              sidebar = sidebar(
                img(src = "rgob.png", height = "80px", style = "display:block; margin:auto;"),
                hr(),
                p("Bhutan For Life", style = "text-align:center; font-weight:bold;"),
                img(src = "bfl.jpg", height = "120px", style = "display:block; margin:auto;")
              ),
              fluidRow(
                card(
                  card_header("Milestone Percent Completion by Year"),
                  plotlyOutput("completion_plot", height = "300px")
                )
              ),
              fluidRow(
                card(
                  card_header("People Reached by Gender and Milestone"),
                  plotlyOutput("gender_plot", height = "300px")
                )
              )
            )
  ),
  
  nav_panel("Milestone Percent Completion",
            layout_columns(
              card(
                card_header("Milestone Completion Tracker"),
                card_body(
                  DTOutput("completion_table")
                ),
                card_footer(
                  downloadButton("downloadCompletionData", "Download Completion Data")
                )
              )
            )
  ),
  
  nav_panel("Gender Data",
            layout_columns(
              card(
                card_header("Gender Data Table"),
                card_body(
                  DTOutput("awp_table")
                ),
                card_footer(
                  downloadButton("downloadGenderData", "Download Gender Data")
                )
              )
            )
  ),
  
  nav_panel("Annual Report Entry",
            layout_columns(
              col_widths = c(6, 6),
              
              card(
                card_header("Activity Details"),
                selectInput("Component", "Component", choices = T3$Component),
                selectInput("Milestone", "Milestone", choices = T3$Milestone),
                selectInput("Activity", "Activity", choices = T3$Activity),
                textInput("subactivity", "Subactivity"),
                textInput("subactivity_description", "Subactivity Description"),
                selectInput("Implementing_Agency", "Implementing Agency",
                            choices = c("JDNP", "JKSNR", "WCNP", "BWS", "SWS", "PNP", "JSWNP",
                                        "RMNP", "JWS", "PWS", "BC-1", "BC-2", "BC-9", "BC-3",
                                        "BC-4", "BC-5", "BC-6", "BC-7", "BC-8", "RBP",
                                        "Tarayana Foundation"), multiple = TRUE),
                selectInput("Location", "Location",
                            choices = c("JDNP", "JKSNR", "WCNP", "BWS", "SWS", "PNP", "JSWNP",
                                        "RMNP", "JWS", "PWS", "BC-1", "BC-2", "BC-9", "BC-3",
                                        "BC-4", "BC-5", "BC-6", "BC-7", "BC-8", "RBP"), multiple = TRUE)
              ),
              
              card(
                card_header("Targets & Funding"),
                numericInput("Fund_Used", "Fund Used (Nu.)", value = 0, min = 0),
                selectInput("Quarter", "Quarter", choices = c("Q1", "Q2", "Q3", "Q4")),
                dateInput("date", "Date", value = Sys.Date()),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("Target_Amount", "Target Amount", value = 0, min = 0),
                  selectInput("Units1", "Units", choices = T3$Units1)
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("Cumulative_Target_Achieved", "Cumulative Target Achieved", value = 0),
                  selectInput("Units2", "Units", choices = T3$Units2)
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("Final_Target", "Final Target", value = 0),
                  selectInput("Units3", "Units", choices = T3$Units3)
                ),
                actionButton("add", "Submit Entry",
                             class = "btn-success", style = "width:100%; margin-top:10px;"),
                downloadButton("downloadAnnualData", "Download Annual Data",
                               style = "width:100%; margin-top:10px;")
              )
            )
  ),
  
  nav_panel("Annual Workplan Entry",
            layout_columns(
              col_widths = c(6, 6),
              
              card(
                card_header("Activity Details"),
                selectInput("awp_Component", "Component", choices = T3$Component),
                selectInput("awp_Milestone", "Milestone", choices = T3$Milestone),
                selectInput("awp_Activity", "Activity", choices = T3$Activity),
                textInput("awp_subactivity", "Subactivity"),
                textInput("awp_description", "Subactivity Description"),
                selectInput("awp_Implementing_Agency", "Implementing Agency",
                            choices = c("JDNP", "JKSNR", "WCNP", "BWS", "SWS", "PNP", "JSWNP",
                                        "RMNP", "JWS", "PWS", "BC-1", "BC-2", "BC-9", "BC-3",
                                        "BC-4", "BC-5", "BC-6", "BC-7", "BC-8", "RBP",
                                        "Tarayana Foundation"), multiple = TRUE),
                selectInput("awp_Location", "Location",
                            choices = c("JDNP", "JKSNR", "WCNP", "BWS", "SWS", "PNP", "JSWNP",
                                        "RMNP", "JWS", "PWS", "BC-1", "BC-2", "BC-9", "BC-3",
                                        "BC-4", "BC-5", "BC-6", "BC-7", "BC-8", "RBP"), multiple = TRUE)
              ),
              
              card(
                card_header("Planned Funding"),
                numericInput("Total_Fund", "Total Fund (USD)", value = 0, min = 0),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("Q1_Planned_Funds", "Q1 Funds (Nu.)", value = 0, min = 0),
                  numericInput("Q2_Planned_Funds", "Q2 Funds (Nu.)", value = 0, min = 0)
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("Q3_Planned_Funds", "Q3 Funds (Nu.)", value = 0, min = 0),
                  numericInput("Q4_Planned_Funds", "Q4 Funds (Nu.)", value = 0, min = 0)
                ),
                actionButton("add_awp", "Submit Entry",
                             class = "btn-success", style = "width:100%; margin-top:10px;")
              )
            )
  )
  )


# Server
server <- function(input, output, session) {
  # Autofill logic for Annual Report Entry
  observeEvent(input$Activity, {
    selected_row <- T3 %>% filter(Activity == input$Activity)
    if (nrow(selected_row) > 0) {
      updateSelectInput(session, "Milestone", selected = selected_row$Milestone)
      updateSelectInput(session, "Component", selected = selected_row$Component)
      updateSelectInput(session, "Units1", selected = selected_row$Units1)
      updateSelectInput(session, "Units2", selected = selected_row$Units2)
      updateSelectInput(session, "Units3", selected = selected_row$Units3)
      updateNumericInput(session, "Final_Target", value = selected_row$Final_Target)
      updateNumericInput(session, "Cumulative_Target_Achieved", value = selected_row$Cumulative_Target_Achieved)
      shinyjs::disable("Milestone")
      shinyjs::disable("Component")
      shinyjs::disable("Units1")
      shinyjs::disable("Units2")
      shinyjs::disable("Units3")
      shinyjs::disable("Cumulative_Target_Achieved")
      shinyjs::disable("Final_Target")
    }
  })
  
  output$completion_plot <- renderPlotly({
    req(completion_long)
    
    completion_long %>%
      mutate(Milestone = factor(substr(Milestone, 1, 12))) %>%
      plot_ly(x = ~Milestone, y = ~percentages, color = ~year,
              colors = c(
                "2019 Completion Percentage" = "#80808050",
                "2020 Completion Percentage" = "#80808050",
                "2021 Completion Percentage" = "#80808050",
                "2022 Completion Percentage" = "#80808050",
                "2023 Completion Percentage" = "#80808050",
                "2024 Completion Percentage" = "#dd7733"
              ),
              type = "bar") %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Milestone"),
        yaxis = list(title = "Percentage Completion (%)")
      )
  })
  
  ##Gender Plot
  output$gender_plot <- renderPlotly({
    req(gender_long)
    
    gender_long %>%
      filter(gender != "Total") %>%
      mutate(Milestone = factor(substr(Milestone, 1, 12))) %>%
      plot_ly(
        x = ~Milestone,
        y = ~numbers_reached,
        color = ~gender,
        colors = c("Male" = "#4575b4", "Female" = "#d73"),
        type = "bar",
        text = ~paste("Gender:", gender, "<br>People Reached:", numbers_reached),
        hoverinfo = "text"
      ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Milestone", tickangle = -45),
        yaxis = list(title = "People Reached"),
        legend = list(title = list(text = "Gender"))
      )
  })
  
  # Autofill logic for AWP Entry
  observeEvent(input$awp_Activity, {
    selected_row <- T3 %>% filter(Activity == input$awp_Activity)
    if (nrow(selected_row) > 0) {
      updateSelectInput(session, "awp_Milestone", selected = selected_row$Milestone)
      updateSelectInput(session, "awp_Component", selected = selected_row$Component)
      shinyjs::disable("awp_Milestone")
      shinyjs::disable("awp_Component")
    }
  })
  #awp table
  awp_data <- reactiveVal(gender)
  
  
  output$awp_table <- DT::renderDataTable({
    DT::datatable(
      gender,
      options = list(
        pageLength = 20,
        autoWidth = TRUE
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("GenderData_", Sys.Date(), ".csv"),
    content = function(file) write.csv(awp_data(), file, row.names = FALSE)
  )
  #awp table
  completion_table <- reactiveVal(completion)
  
  
  output$completion_table <- DT::renderDataTable({
    DT::datatable(
      completion,
      options = list(
        pageLength = 20,
        autoWidth = TRUE
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("CompletionData_", Sys.Date(), ".csv"),
    content = function(file) write.csv(completion_table(), file, row.names = FALSE)
  )
  
  
  observeEvent(input$add, {
    showNotification("Annual report entry added!", type = "message")
  })
  
  observeEvent(input$add_awp, {
    new_awp_entry <- data.frame(
      Component = input$awp_Component,
      Milestone = input$awp_Milestone,
      Activity = input$awp_Activity,
      Subactivity = input$awp_subactivity,
      Subactivity_Description = input$awp_description,
      Implementing_Agency = paste(input$awp_Implementing_Agency, collapse = "; "),
      Location = paste(input$awp_Location, collapse = "; "),
      Total_Fund = input$Total_Fund,
      Q1 = input$Q1_Planned_Funds,
      Q2 = input$Q2_Planned_Funds,
      Q3 = input$Q3_Planned_Funds,
      Q4 = input$Q4_Planned_Funds,
      stringsAsFactors = FALSE
    )
  })
}

shinyApp(ui, server)
