library(rsconnect)

rsconnect::setAccountInfo(name='markabboud', token='7E6E2743DCA441F75CDE5024B803EB3D', secret='EtkzgKSA4ixfiLD3qyA1DtNtw4xDffdpjZCr1ulx')

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  install.packages("shinydashboard")
}
if (!requireNamespace("rhandsontable", quietly = TRUE)) {
  install.packages("rhandsontable")
}
if (!requireNamespace("RAR", quietly = TRUE)) {
  install_github("JessLGraves/RAR")
}
if (!requireNamespace("cosinor", quietly = TRUE)) {
  install.packages("cosinor")
}
if (!requireNamespace("GGIR", quietly = TRUE)) {
  install.packages("GGIR")
}
if (!requireNamespace("accelerometry", quietly = TRUE)) {
  install.packages("accelerometry")
}
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("spectral", quietly = TRUE)) {
  install.packages("spectral")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(RAR)
library(cosinor)
library(GGIR)
library(accelerometry)
library(shinyjs)
library(ggplot2)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Actigraphy Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "input_tab"),
      menuItem("Numeric Output", tabName = "numeric_output_tab"),
      menuItem("Visual Output", tabName = "visual_output_tab"),
      menuItem("References", tabName = "References_tab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "input_tab",
        fluidPage(
          titlePanel("Data Input"),
          numericInput("num_rows_input", "Number of Epochs", value = 1440, min = 0, max = 1000),
          fluidRow(
            column(width = 6,
                   rHandsontableOutput("table_page1"),
                   br(),
                   selectInput("epoch_duration", "Epoch Duration (s)", choices = c(15, 30, 60), selected = 60),
                   numericInput("sleep_bout_duration", "'Sleep' Bout Duration (m)", value = 4),
                   actionButton("clear_button", "Clear Table", class = "btn-danger", style = "margin-top: 10px;"),
                   actionButton("calculate_button", "Calculate", class = "btn-primary", style = "margin-top: 10px;")
            )
          )
        )
      ),
      tabItem(
        tabName = "numeric_output_tab",
        fluidPage(
          titlePanel("Numeric Output"),
          fluidRow(
            column(width = 6,
                   # Subsection 1 with 6 numeric outputs
                   box(
                     title = "Extended Cosinor Analysis",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                     uiOutput("numeric_output_subsection1")
                   ),
                   # Subsection 4 with 2 numeric outputs
                   box(
                     title = "Non-Parametric Analysis",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                     uiOutput("numeric_output_subsection3")
                   )
            ),
            column(width = 6,
                   # Subsection 2 with 3 numeric outputs
                   box(
                     title = "Standard Cosinor Analysis",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                     uiOutput("numeric_output_subsection2")
                   ),
                   # Subsection 5 with 2 numeric outputs
                   box(
                     title = "'Sleep' Analysis",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                     uiOutput("numeric_output_subsection4")
                   )
            )
          )
        )
      ),
      tabItem(
        tabName = "visual_output_tab",
        fluidPage(
          titlePanel("Visual Output"),
          
          fluidRow(
            column(width = 12,
                   box(
                     title = "Activity Plot",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                     plotlyOutput("activity_plot")
                   )
            )
          ),
          
          fluidRow(
            column(width = 12,
                   box(
                     title = "Spectral Density Plot",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                     plotlyOutput("spectral_density")
                   )
            )
          ),
          
          fluidRow(
            column(width = 12,
                   box(
                     title = "'Sleep' Bout Frequent Plot",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                     plotlyOutput("sleep_bout")
                   )
            )
          )
        )
      ),
      tabItem(
        tabName = "References_tab",
        fluidPage(
          titlePanel("References Page"),
          # Add content for the References tab/page here
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Create a reactive data frame for Analysis
  data_page1 <- reactiveVal(data.frame(
    Activity = rep("", 100)
  ))
  
  # Initialize numeric outputs with default values
  output$numeric_output_subsection1 <- renderUI({
    do.call(tagList, lapply(1:6, function(i) {
      tagList(
        verbatimTextOutput(paste0("numeric_output1_text_", i))
      )
    }))
  })
  
  observe({
    lapply(1:6, function(i) {
      output[[paste0("numeric_output1_text_", i)]] <- renderText({
        label <- c("Alpha", "Beta", "F", "Amplitude", "Mesor", "Acrophase")[i]
        value <- NA  # Placeholder value; replace with your actual logic
        paste0(label, ": ", value)
      })
    })
  })
  
  output$numeric_output_subsection2 <- renderUI({
    do.call(tagList, lapply(4:6, function(i) {
      tagList(
        verbatimTextOutput(paste0("numeric_output2_text_", i))
      )
    }))
  })
  
  observe({
    lapply(4:6, function(i) {
      output[[paste0("numeric_output2_text_", i)]] <- renderText({
        label <- c("Amplitude", "Mesor", "Acrophase")[i - 3]
        value <- NA  # Set default value to 1
        paste0(label, ": ", value)
      })
    })
  })
  
  output$numeric_output_subsection3 <- renderUI({
    do.call(tagList, lapply(1:2, function(i) {
      tagList(
        verbatimTextOutput(paste0("numeric_output3_text_", i))
      )
    }))
  })
  
  observe({
    lapply(1:2, function(i) {
      output[[paste0("numeric_output3_text_", i)]] <- renderText({
        label <- c("IV", "IS")[i]
        value <- NA  # Set default value to 1
        paste0(label, ": ", value)
      })
    })
  })
  
  output$numeric_output_subsection4 <- renderUI({
    do.call(tagList, lapply(1:2, function(i) {
      tagList(
        verbatimTextOutput(paste0("numeric_output4_text_", i))
      )
    }))
  })
  
  observe({
    lapply(1:2, function(i) {
      output[[paste0("numeric_output4_text_", i)]] <- renderText({
        label <- c("Mean 'Sleep' Bout Duration (s)", "Mean 'Sleep' Bouts/day")[i]
        value <- NA  # Set default value to NA
        paste0(label, ": ", value)
      })
    })
  })
  
  observeEvent(input$calculate_button, {
    
    # Check if the table is filled
    if (any(data_page1()$Activity == "")) {
      session$reload()
      showNotification("Please fill in all activity values in the table.", type = "warning")
      return()
    }
    
    # Perform calculations or analysis here
    start_timestamp <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
    interval <- as.numeric(input$epoch_duration)
    FirstColumn <- seq(from = start_timestamp, by = interval, length.out = input$num_rows_input)
    SecondColumn <- as.numeric(data_page1()$Activity)
    RARProgramInput <- data.frame(FirstColumn, SecondColumn)
    
    tryCatch({
      RARProgramOutput = RAR(RARProgramInput, SecondColumn, FirstColumn, transform = "antilogit")
      RARProgramParameters_output= c(RARProgramOutput$parameters$alpha,RARProgramOutput$parameters$beta,
                                     RARProgramOutput$parameters$F_stat,RARProgramOutput$parameters$amp,
                                     RARProgramOutput$parameters$mesor,RARProgramOutput$parameters$acrophase)
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    
    TimeCount <- as.data.frame(1:input$num_rows_input)
    CosinorInputInitial = cbind(SecondColumn, TimeCount)
    CosinorColumnNames = c("x", "y")
    colnames(CosinorInputInitial) = CosinorColumnNames
    CosinorInputFinal = CosinorInputInitial[c("y", "x")]
    
    periodlength <- 86400/as.numeric(input$epoch_duration)
    
    X <- cosinor.lm(x ~ time(y), period = periodlength, data = CosinorInputFinal)
    Y <- summary(X)
    Mesor <- Y$transformed.table$estimate[1]
    Amp <- Y$transformed.table$estimate[2]
    Acr <- (Y$transformed.table$estimate[3] * periodlength) / (2 * pi) + periodlength/2
    CosinorOutput = c(Amp,Mesor,Acr)
    
    IVISData <- pmax(SecondColumn, 0)
    IVIS <- sapply(seq(1, length(IVISData), 60), function(n) sum(IVISData[seq(n, length = 60, by = 1)]))
    IVISOutput <- g.IVIS(Xi = as.numeric(IVIS), epochsizesecondsXi = 3600 * (60 / as.numeric(input$epoch_duration)))
    IVISFinal <- as.data.frame(cbind(IVISOutput$IntradailyVariability, IVISOutput$InterdailyStability))
    colnames(IVISFinal) <- c("IV", "IS")
    
    BinaryInactiveStates =  as.data.frame(bouts(SecondColumn, weartime = NULL, bout_length = as.numeric(input$sleep_bout_duration), thresh_upper = 0))
    
    # Initialize FirstISLabel and ISLengthLabel
    FirstISLabel <- data.frame(State = numeric(0))
    ISLengthLabel <- data.frame(State = numeric(0))
    
    # Compute FirstISLabel
    transition_indices <- which(diff(BinaryInactiveStates) == 1) + 1
    if (BinaryInactiveStates[1,] == 1) {
      transition_indices <- c(1, transition_indices)
    }
    
    # Identify the positions where the transition from 0 to 1 occurs
    transition_positions <- which(diff(c(0, as.numeric(unlist(BinaryInactiveStates)))) == 1)
    
    # Create a result vector with zeros
    FirstISLabel <- numeric(length(BinaryInactiveStates))
    
    # Assign 1 to the identified transition positions
    FirstISLabel[transition_positions] <- 1
    
    # Compute the run-length encoding of BinaryInactiveStates
    rle_result <- rle(unlist(as.vector(BinaryInactiveStates)))
    
    # Extract the lengths of consecutive 1s
    lengths_of_ones <- rle_result$lengths[rle_result$values == 1]
    
    # Filter out lengths of 0 (corresponding to 0s)
    lengths_of_ones <- lengths_of_ones[lengths_of_ones > 0]
    ISLengthLabelOnly = as.vector(lengths_of_ones)
    
    NumberDays = input$num_rows_input/periodlength
    
    MeanISTotalTimePerDay = sum(ISLengthLabelOnly)/(NumberDays*60)
    MeanISBoutDuration = sum(ISLengthLabelOnly)/length(ISLengthLabelOnly)
    MeanNumberIS = length(ISLengthLabelOnly)/NumberDays
    ISOutput = c(MeanISBoutDuration, MeanNumberIS)
    
    # Update the text output with the calculated value
    lapply(1:6, function(i) {
      output[[paste0("numeric_output1_text_", i)]] <- renderText({
        label <- c("Alpha", "Beta", "F", "Amplitude", "Mesor", "Acrophase")[i]
        value <- round(RARProgramParameters_output, digits = 3)
        paste0(label, ": ", value[i])
      })
    })
    lapply(4:6, function(i) {
      output[[paste0("numeric_output2_text_", i)]] <- renderText({
        label <- c("Amplitude", "Mesor", "Acrophase")[i-3]
        value <- round(CosinorOutput, digits = 3)
        paste0(label, ": ", value[i-3])
      })
    })
    lapply(1:2, function(i) {
      output[[paste0("numeric_output3_text_", i)]] <- renderText({
        label <- c("IV", "IS")[i]
        value <- round(IVISFinal, digits = 3)  # Set default value to 1
        paste0(label, ": ", value[i])
      })
    })
    lapply(1:2, function(i) {
      output[[paste0("numeric_output4_text_", i)]] <- renderText({
        label <- c("Mean 'Sleep' Bout Duration (s)", "Mean 'Sleep' Bouts/Day")[i]
        value <- round(ISOutput, digits = 3)  # Set default value to NA
        paste0(label, ": ", value[i])
      })
    })
    
    data <- SecondColumn  # Replace this line with your actual data
    
    # Calculate the day values based on the total number of minutes
    days <- rep(1:NumberDays, each = (1440*as.numeric(input$epoch_duration)/60))
    Time = 1:(NumberDays*(1440*as.numeric(input$epoch_duration)/60))
    # Create a data frame for ggplot2
    df <- data.frame(
      Time = Time/(1440*as.numeric(input$epoch_duration)/60),
      Activity = data,
      Day = factor(days)
    )
    
    # Plot the data using ggplot2 with days on the x-axis
    output$activity_plot <- renderPlotly({
      gg <- ggplot(df, aes(x = Time, y = Activity, group = Day)) +
        geom_line(color = "darkblue") +
        labs(x = "Days", y = "Activity (Counts/Min)") +
        scale_x_continuous(breaks = seq(0, NumberDays, by = 1)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
        geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        )
      
      ggplotly(gg)
    })

    x = as.data.frame(unlist(FirstISLabel))
    rows_to_update <- which(x == 1)
    rows_to_update = as.data.frame(rows_to_update)
    # Update the values in the identified rows without using a loop
    rows_to_update <- rows_to_update %% 1440
    FirstISLabel = rows_to_update
    FirstISLabel = as.numeric(unlist(FirstISLabel))
    buckets <- cut(FirstISLabel, breaks = seq(0, max(FirstISLabel) + 30, 30), include.lowest = TRUE)
    frequency_table <- table(buckets)
    total_observations <- length(FirstISLabel)
    y1 <- as.data.frame(((frequency_table / total_observations) * 100))
    Frequency = y1$Freq
    Time = as.data.frame(seq(from = 0, to = 1410, by = 30))
    df2 = as.data.frame(cbind(Time,Frequency))
    colnames(df2) = c("Time","Frequency")

    output$sleep_bout <- renderPlotly({
      gg2 <- ggplot(df2, aes(x = Time, y = Frequency)) +
        geom_line(color = "darkred", linewidth = 1.25) +
        labs(x = "'Sleep' Bout Onset", y = "% Frequency") +
        scale_x_continuous(breaks = c(0, 360, 720, 1080, 1440), labels = c("0000", "0600", "1200", "1800", "0000")) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + # Add this line for y = 0
        geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        )
      ggplotly(gg2)
    })
    
    sampledfreq = seq(from = 30, to = 2400, by = 1.5)
    sampledfreq = as.data.frame(1/sampledfreq)
    sampledfreq <- sampledfreq %>% arrange(desc(row_number()))
    
    spec = spec.lomb(y = as.numeric(unlist(as.vector(SecondColumn))),f = as.numeric(unlist(as.vector(sampledfreq))), mode = "normal")
    specdf = as.data.frame(cbind(1/spec$f,spec$A))
    colnames(specdf) = c("Minute","Power")
    
    output$spectral_density <- renderPlotly({
      gg3 <- ggplot(specdf, aes(x = Minute, y = Power)) +
        geom_line(color = "darkred", linewidth = 1.25) +
        labs(x = "Period Length (h)", y = "Normalized Spectral Density") +
        scale_x_continuous(breaks = c(0, 360, 720, 1080, 1440, 1800, 2160), labels = c("0", "6", "12", "18", "24", "30", "36")) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + # Add this line for y = 0
        geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        )
      ggplotly(gg3)
    })
  })
  
  observeEvent(input$clear_button, {
    # Clear the table when the "Clear" button is clicked
    data_page1(data.frame(Activity = rep("", input$num_rows_input)))
  })
  
  output$table_page1 <- renderRHandsontable({
    rhandsontable(data_page1(), height = 300) %>%
      hot_table(rowHeaders = FALSE, stretchH = "all")
  })
  
  observe({
    if (!is.numeric(input$num_rows_input) || input$num_rows_input <= 0) {
      showNotification("Please enter a valid positive integer for the number of epochs.", type = "warning")
      return()
    }
    
    if (!is.numeric(input$sleep_bout_duration) || input$sleep_bout_duration <= 0) {
      showNotification("Please enter a valid positive integer for the 'sleep' bout duration.", type = "warning")
      return()
    }
    
    data_page1(data.frame(Activity = rep("", input$num_rows_input)))
  })
  
  observeEvent(input$table_page1, {
    data_page1(hot_to_r(input$table_page1))
    
    # Ensure the correct number of rows when updating data_page1
    updated_data <- hot_to_r(input$table_page1)
    if (nrow(updated_data) != input$num_rows_input) {
      showNotification("The number of rows in the table does not match the specified number of epochs.", type = "warning")
      return()
    }
  })
}

# Run the application
shinyApp(ui, server)