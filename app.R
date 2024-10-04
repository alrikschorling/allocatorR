# Set up
#load packages
pacman::p_load(shiny, shinyFeedback, shinythemes, shinyjs, #shiny-related
               DT, #modifiable tables
               tidyverse, dplyr, #data manipulation
               ggplot2, ggridges, #plotting 
               anticlust, #anticlustering (allocation)
               ggpubr, rstatix, ggprism) #statistics

#set themes and palettes
basic_theme <- theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(linewidth = 0.5),
        axis.ticks.length = unit(.2, "cm"), 
        axis.text.x = element_text(color = "black", family = "sans", size = 12),
        axis.text.y = element_text(color = "black", family = "sans", size = 12),
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12), 
        strip.background = element_rect(color = NA, fill = NA), 
        plot.title = element_text(color = "black", family = "sans", size = 12))

theme_1 <- basic_theme + theme(axis.line = element_line(linewidth = 0.5))

theme_2 <- basic_theme + 
  theme(axis.line = element_line(linewidth = 0.3), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#make custom color palette
pal <- c("#B5B5B5", "#80607E", "#64D1C5", "#D1B856", "#8F845B", "#5C7C78", "#524410", "#470C43")
pal2 <- c("#5EBF92", "#F2E205", "#F2B705", "#BF7E04", "#F23838")
expanded_pal <- colorRampPalette(pal)(50)
expanded_pal2 <- colorRampPalette(pal2)(50)


#create function to subsample colors based on the number of levels of factors of interest
get_subsampled_palette <- function(n) {
  expanded_pal[round(seq(1, length(expanded_pal), length.out = n))]
}

#create function to subsample colors based on the number of levels of factors of interest
get_subsampled_palette <- function(n) {
  expanded_pal2[round(seq(1, length(expanded_pal), length.out = n))]
}

#generate example data that matches the format users should upload
example_table <- data.frame(
  rat_id = c("1", "2", "3"),
  behavioral_test1 = c(250, 245, 260),
  behavioral_test2 = c(248, 244, 258)
)



# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .btn-spacing {
        margin-bottom: 5px; margin-right: 5px;
      }
      .container-fluid {
        max-width: 1600px;  /* Set maximum width for the window */
      }
    "))
  ),
  useShinyFeedback(),
  useShinyjs(),
  
  titlePanel("allocatoR - animal group allocator"),
  
  fluidRow(
    column(4,
           div(
             
             #user input csv files 
             fileInput("file1", "Choose CSV file", 
                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
             
             #help text
             helpText(HTML("Please upload a CSV file with the first column being 'rat_id' given as a number (see below).",
                           "The remaining columns should contain the behavioral data (see below).<br><br>",
                           
                           "The allocation aims to minimize the variance and standard deviation between groups",
                           "using the anticlust package with the following parameters: <br>",
                           
                           "  -objective = kplus <br>",
                           "  -standardize = TRUE <br>",
                           "  -method = local-maximum <br><br>",
                           
                           "Example table:<br>")),
             tableOutput("exampleTable"),
             
             #output number of rats (to simplify for group size determination)
             verbatimTextOutput("num_rats"),  # Display unique number of rat_id
             
             #user input
             numericInput("num_groups", "Number of groups for anticlustering", 2),
             textInput("group_names_allocation", "Group names (comma-separated)", 
                       placeholder = "Eg. Lesion, Control"),  # Comma-separated input
             textInput("group_sizes", "Number of rats in each group (comma-separated)", 
                       placeholder = "E.g. 5,5"),  # Comma-separated input for sizes
             selectInput("set_seed", "Set seed (for reproducibility)", 
                         choices = c(123, 69, 111), selected = 123),
             
             #action buttons
             actionButton("process_data", "Process Data"),
             downloadButton("downloadData1", "Download Fig. 1", class = "btn-spacing"),  
             downloadButton("downloadData2", "Download Fig. 2", class = "btn-spacing"),
             downloadButton("downloadData3", "Download Fig. 3", class = "btn-spacing"),
             downloadButton("downloadAllocation", "Download allocation")
           )
    ),
    
    column(8,
           verbatimTextOutput("shapiro_result"),  # Shapiro-Wilk test result
           verbatimTextOutput("levene_result"),  # Levene's test result
           plotOutput("plot1"),
           plotOutput("plot2"),
           plotOutput("plot3"),
           DTOutput("allocation_table")
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  #display the example table
  output$exampleTable <- renderTable({
    example_table
  })
  
  #initially disable download buttons
  shinyjs::disable("downloadData1")
  shinyjs::disable("downloadData2")
  shinyjs::disable("downloadData3")
  shinyjs::disable("downloadAllocation")
  
  #function to read and process CSV files
  df <- reactive({
    req(input$file1)
    files <- input$file1
    df <- read.csv(files$datapath) |> 
      rename_with(tolower) |>
      mutate(rat_id = factor(rat_id)) 
  })
  
  #display unique number of rat_id
  output$num_rats <- renderText({
    paste("Number of unique rats IDs:", length(unique(df()$rat_id)))
  })
  
  #reactive to process anticlustering after user input
  processed_data <- eventReactive(input$process_data, {
    
    #validate user inputs
    group_names <- strsplit(input$group_names_allocation, ",\\s*")[[1]]
    group_sizes <- as.numeric(strsplit(input$group_sizes, ",\\s*")[[1]])
    
    #set the seed (for reproducibility)
    set.seed(input$set_seed)
    
    #validate that group sizes and number of names matches the input number of groups
    validate(
      need(length(group_names) == input$num_groups, "Number of group names doesn't match the number of groups."),
      need(length(group_sizes) == input$num_groups, "Group sizes don't match the number of groups."),
      need(sum(group_sizes) == nrow(df()), "The total size of all groups doesn't match the number of animals.")
    )
    
    
    df_with_groups <- df()
    
    #perform anticlustering
    df_with_groups$group <- factor(anticlustering(
      df_with_groups[, -1], #exclude the rat_id column
      K = group_sizes,  # User-defined group sizes
      objective = "kplus", #anticlust algorithm (minimize variance and standard deviation between groups)
      standardize = TRUE, #scale the variables
      method = "local-maximum" #algorithm endpoint: local maximum method 
    ), labels = group_names)  #apply user-defined group names
    
    shinyjs::enable("downloadAllocation")
    df_with_groups
  })
  
  #convert to long format
  df_long <- reactive({
    processed_data() |> pivot_longer(!c(rat_id, group), names_to = 'test', values_to = 'vals') |> 
      mutate(test = factor(test)) |> ungroup()
  })
  
  # perform statistical tests
  shapiro <- reactive({
    result <- shapiro_test(df_long()$vals)
    result
  })
  
  output$shapiro_result <- renderText({
    shapiro_result <- shapiro()
    
    paste(
      "Shapiro-Wilk Test for Normality\n",
      "Statistic:", round(shapiro_result$statistic, 3), "\n",
      "p-value:", ifelse(!is.null(shapiro_result$p.value), round(shapiro_result$p.value, 3), "NA")
    )
  })
  
  
  levene <- reactive({
    levene_test(vals ~ group, data = df_long())
  })
  
  output$levene_result <- renderText({
    levene_result <- levene()
    
    paste(
      "Levene's Test for Homogeneity of Variance\n",
      "Statistic:", round(levene_result$statistic, 3), "\n",
      "Degrees of freedom:", levene_result$df1, ",", levene_result$df2, "\n",
      "p-value:", ifelse(!is.null(levene_result$p), round(levene_result$p, 3), "NA")
    )
  })
  
  #conditional statistics
  sts <- reactive({
    
    #case when non-normal heteroskedastic data (p < 0.05 for both test) and more than 2 groups
    if (!is.null(shapiro()$p.value) && !is.null(levene()$p) &&
        shapiro()$p.value < 0.05 && levene()$p < 0.05 && 
        length(unique(df_long()$group)) > 2) {
      df_long() |> group_by(test) |>
        dunn_test(vals ~ group, p.adjust.method = "BH") |>
        add_xy_position() 
      #print('Using Dunns non-parametric, multiple comparison test')
      
      #case when non-normal heteroskedastic data (p < 0.05 for both test) and 2 groups
    } else if (!is.null(shapiro()$p.value) && !is.null(levene()$p) &&
               shapiro()$p.value < 0.05 && levene()$p < 0.05 && 
               length(unique(df_long()$group)) == 2) {
      df_long() |> group_by(test) |>
        wilcox_test(vals ~ group, paired = FALSE) |>
        add_xy_position()
      #print('Using Wilcoxon (signed rank) non-parametric test')
      
      #case when normal but heteroskedastic data (levene()$p < 0.05)
    } else if (!is.null(levene()$p) && levene()$p < 0.05) {
      df_long() |> group_by(test) |>
        t_test(vals ~ group, var.equal = FALSE) |>
        add_xy_position()
      #print('Using two-sided t-test with unequal variance')
      
      #case when normal and homoskedastic data (p > 0.05 for both test)
    } else {
      #res <- df_long() |> group_by(test) |> t_test(vals ~ group) |> add_xy_position()
      #print(res)  # Check the structure of res
      #return(res)
      
      df_long() |> group_by(test) |>
        t_test(vals ~ group) |>
        add_xy_position()
      #print('Using two-sided t-test with equal variance')
    }
  })
  
  
  #plot 1
  reactivePlot1 <- reactive({
    ggplot(df_long(), aes(vals, group)) + 
      geom_density_ridges(aes(fill = group)) +
      scale_fill_manual(values = pal) +
      labs(title = "Fig 1. Behavioral data distribution", x = "Values", y = "Group") +
      theme_1
  })
  
  #display plot1
  output$plot1 <- renderPlot({
    print(reactivePlot1())
  })
  
  shinyjs::enable("downloadData1")
  
  #plot2
  reactivePlot2 <- reactive({
    
    #case when more than 2 groups
    if (length(unique(df_long()$group)) > 2) {
      ggplot(df_long(), aes(group, vals)) +
        geom_violin(aes(fill = group)) +
        geom_point(position = position_jitter(width = 0.2)) + 
        facet_wrap(~test) + 
        scale_fill_manual(values = pal) + 
        scale_y_continuous(limits = c(0, max(1.4 * df_long()$vals)), expand = c(0, 0)) +
        add_pvalue(sts(), label = 'p.adj', bracket.size = 0.4, label.size = 3) +
        labs(title = "Fig 2. Behavioral data group comparison") +
        theme_2
      
      #case when 2 groups
    } else {
      ggplot(df_long(), aes(group, vals)) +
        geom_violin(aes(fill = group)) +
        geom_point(position = position_jitter(width = 0.2)) + 
        facet_wrap(~test) + 
        scale_fill_manual(values = pal) + 
        scale_y_continuous(limits = c(0, max(1.4 * df_long()$vals)), expand = c(0, 0)) +
        add_pvalue(sts(), label = 'p', bracket.size = 0.4, label.size = 3) +
        labs(title = "Fig 2. Behavioral data group comparison") +
        theme_2
    }
  })
  
  #display plot2
  output$plot2 <- renderPlot({
    print(reactivePlot2())
  })
  
  shinyjs::enable("downloadData2")
  
  
  #plot3
  reactivePlot3 <- reactive({
    ggplot(df_long(), aes(group, vals, col = rat_id)) + 
      geom_point() + 
      scale_color_manual(values = get_subsampled_palette(length(unique(df_long()$rat_id)))) +
      facet_wrap(~test) +
      labs(title = "Fig 3. Behavioral data per animal") +
      theme_1
  })
  
  #display plot3
  output$plot3 <- renderPlot({
    print(reactivePlot3())
  })
  
  shinyjs::enable("downloadData3")
  
  #display group allocation table
  output$allocation_table <- renderDT({
    processed_data() |> relocate(rat_id, group) |> arrange(group, rat_id) |>
      datatable(editable = TRUE,
                filter = 'top',
                options = list(pageLength = 5)) 
  })
  
  #download group allocation as CSV
  output$downloadAllocation <- downloadHandler(
    filename = function() {
      paste("group_allocation_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data() %>% select(rat_id, group), file, row.names = FALSE)
    }
  )
  
  #dynamically adjust the plot download dimensions
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("Fig_1_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      p1 <- isolate(reactivePlot1())
      g <- ggplot_build(p1)
      
      plot_width <- 4
      plot_height <- length(unique(df_long()$group)) * 1.5
      
      pdf(file, width = plot_width, height = plot_height)  # Use dynamic size for the plot
      print(p1)
      dev.off()
    }
  )
  
  #dynamically adjust the plot download dimensions
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Fig_2_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      p2 <- isolate(reactivePlot2())  # Correctly isolate plot 2
      g <- ggplot_build(p2)  # Get the plot build object
      
      plot_width <- length(unique(df_long()$test)) * 1.5
      plot_height <- length(unique(df_long()$test)) * 1.5
      
      pdf(file, width = plot_width, height = plot_height)
      print(p2)
      dev.off()
    }
  )
  
  #dynamically adjust the plot download dimensions
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("Fig_3_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      p3 <- isolate(reactivePlot3())
      g <- ggplot_build(p3)
      
      # Dynamically adjust the plot size
      plot_width <- length(unique(d_long$test)) * 1.5
      plot_height <- length(unique(d_long$test)) * 1.5
      
      pdf(file, width = plot_width, height = plot_height)
      print(p3)
      dev.off()
    }
  )
  
}



# Run the application
shinyApp(ui = ui, server = server)