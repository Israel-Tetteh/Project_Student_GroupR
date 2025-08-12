
##############################################################################
#                             Shiny App
##############################################################################
library(bslib)
library(shiny)
source('utils/Project_Grouper_function.R') # source the function
ui <- page_navbar(
  id = 'key',
  title = "Project Student GroupR",
  theme = bs_theme(
    version = "5", 
    bootswatch = "flatly",
    primary = "#667eea",
    success = "#4CAF50"
  ),
  # Landing Page Content for the Welcome nav_panel
  nav_panel(
    title = "Welcome",
    icon = icon("home"),
    
    # Hero Section
    div(
      class = "hero-section",
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
           color: white;
           padding: 80px 0;
           margin: -15px -15px 30px -15px;
           text-align: center;",
      
      div(
        class = "container",
        h1(
          "Project Student GroupR",
          style = "font-size: 3.5rem;
               font-weight: 700;
               margin-bottom: 20px;
               text-shadow: 2px 2px 4px rgba(0,0,0,0.3);"
        ),
        p(
          "Intelligent Student Grouping for Academic Excellence",
          style = "font-size: 1.3rem;
               font-weight: 300;
               margin-bottom: 30px;
               opacity: 0.9;"
        ),
        actionButton(
          "get_started_hero",
          "Get Started Now",
          icon = icon("rocket"),
          class = "btn-light btn-lg",
          style = "padding: 12px 30px;
               font-size: 1.1rem;
               border-radius: 25px;
               font-weight: 600;
               box-shadow: 0 4px 15px rgba(0,0,0,0.2);"
        )
      ),
      
      tags$footer(
        style = "background-color: rgba(255, 255, 255, 0.1);
             padding: 15px;
             margin-top: 40px;
             font-size: 0.9rem;
             color: #f5f5f5;",
        HTML("&copy; 2025 Project Student GroupR | Developed by: Ephraim Ampofo, Bandoh Alex, Omari Owusu, Isaac Asiedu Kwakye, Tenkorang Godfred Samuel, Sophia Abaidoo, Israel Tawiah Tetteh"),
        tags$br(),
        tags$a(
          href = "https://github.com/Israel-Tetteh?tab=repositories",
          target = "_blank",
          icon("github"), " GitHub"
        ),
        " | ",
        tags$a(
          href = "mailto:israeltetteh715@gmail.com",
          icon("envelope"), " Contact"
        )
      )
    ),
    
    
    # Main Content
    div(
      class = "container-fluid",
      style = "max-width: 1200px; margin: 0 auto;",
      
      # What is this app section
      div(
        class = "row mb-5",
        div(
          class = "col-12 text-center mb-4",
          h2("What is Project Student GroupR?", 
             style = "color: #2c3e50; font-weight: 600; margin-bottom: 20px;"),
          p("An intelligent web application designed to create balanced student groups for academic projects. 
          Our algorithm considers student performance (CWA) and department preferences to ensure 
          optimal team composition for collaborative research.",
            style = "font-size: 1.1rem; color: #5a6c7d; max-width: 800px; margin: 0 auto; line-height: 1.6;")
        )
      ),
      
      # Key Features Section
      div(
        class = "row mb-5",
        div(
          class = "col-12 text-center mb-4",
          h2("Key Features", style = "color: #2c3e50; font-weight: 600; margin-bottom: 40px;")
        ),
        
        # Feature Cards
        div(
          class = "col-md-4 mb-4",
          div(
            class = "card h-100 shadow-sm",
            style = "border: none; border-radius: 10px; transition: transform 0.3s;",
            div(
              class = "card-body text-center p-4",
              div(
                style = "background: linear-gradient(45deg, #4CAF50, #45a049); 
                       width: 80px; height: 80px; 
                       border-radius: 50%; 
                       margin: 0 auto 20px auto; 
                       display: flex; 
                       align-items: center; 
                       justify-content: center;",
                icon("users", style = "font-size: 2rem; color: white;")
              ),
              h4("Smart Grouping", style = "color: #2c3e50; font-weight: 600;"),
              p("Automatically creates balanced groups based on student performance and department distribution.",
                style = "color: #5a6c7d; line-height: 1.5;")
            )
          )
        ),
        
        div(
          class = "col-md-4 mb-4",
          div(
            class = "card h-100 shadow-sm",
            style = "border: none; border-radius: 10px;",
            div(
              class = "card-body text-center p-4",
              div(
                style = "background: linear-gradient(45deg, #2196F3, #1976D2); 
                       width: 80px; height: 80px; 
                       border-radius: 50%; 
                       margin: 0 auto 20px auto; 
                       display: flex; 
                       align-items: center; 
                       justify-content: center;",
                icon("chart-bar", style = "font-size: 2rem; color: white;")
              ),
              h4("Performance Analytics", style = "color: #2c3e50; font-weight: 600;"),
              p("Comprehensive statistics and visualizations to analyze group composition and student distribution.",
                style = "color: #5a6c7d; line-height: 1.5;")
            )
          )
        ),
        
        div(
          class = "col-md-4 mb-4",
          div(
            class = "card h-100 shadow-sm",
            style = "border: none; border-radius: 10px;",
            div(
              class = "card-body text-center p-4",
              div(
                style = "background: linear-gradient(45deg, #FF9800, #F57C00); 
                       width: 80px; height: 80px; 
                       border-radius: 50%; 
                       margin: 0 auto 20px auto; 
                       display: flex; 
                       align-items: center; 
                       justify-content: center;",
                icon("chalkboard-teacher", style = "font-size: 2rem; color: white;")
              ),
              h4("Lecturer Distribution", style = "color: #2c3e50; font-weight: 600;"),
              p("Optional feature to distribute student groups among different lecturers for supervision.",
                style = "color: #5a6c7d; line-height: 1.5;")
            )
          )
        )
      ),
      
      # How it works section
      div(
        class = "row mb-5 py-5",
        style = "background: #f8f9fa; border-radius: 15px; margin: 0;",
        div(
          class = "col-12 text-center mb-4",
          h2("How It Works", style = "color: #2c3e50; font-weight: 600; margin-bottom: 40px;")
        ),
        
        div(
          class = "col-md-3 text-center mb-3",
          div(
            style = "background: #667eea; 
                   width: 60px; height: 60px; 
                   border-radius: 50%; 
                   margin: 0 auto 15px auto; 
                   display: flex; 
                   align-items: center; 
                   justify-content: center;",
            span("1", style = "color: white; font-size: 1.5rem; font-weight: bold;")
          ),
          h5("Upload Files", style = "color: #2c3e50; font-weight: 600;"),
          p("Upload student CWA data and department assignment files", 
            style = "color: #5a6c7d; font-size: 0.9rem;")
        ),
        
        div(
          class = "col-md-3 text-center mb-3",
          div(
            style = "background: #667eea; 
                   width: 60px; height: 60px; 
                   border-radius: 50%; 
                   margin: 0 auto 15px auto; 
                   display: flex; 
                   align-items: center; 
                   justify-content: center;",
            span("2", style = "color: white; font-size: 1.5rem; font-weight: bold;")
          ),
          h5("Configure Settings", style = "color: #2c3e50; font-weight: 600;"),
          p("Set group size, department, and lecturer preferences", 
            style = "color: #5a6c7d; font-size: 0.9rem;")
        ),
        
        div(
          class = "col-md-3 text-center mb-3",
          div(
            style = "background: #667eea; 
                   width: 60px; height: 60px; 
                   border-radius: 50%; 
                   margin: 0 auto 15px auto; 
                   display: flex; 
                   align-items: center; 
                   justify-content: center;",
            span("3", style = "color: white; font-size: 1.5rem; font-weight: bold;")
          ),
          h5("Generate Groups", style = "color: #2c3e50; font-weight: 600;"),
          p("Our algorithm creates optimally balanced student groups", 
            style = "color: #5a6c7d; font-size: 0.9rem;")
        ),
        
        div(
          class = "col-md-3 text-center mb-3",
          div(
            style = "background: #667eea; 
                   width: 60px; height: 60px; 
                   border-radius: 50%; 
                   margin: 0 auto 15px auto; 
                   display: flex; 
                   align-items: center; 
                   justify-content: center;",
            span("4", style = "color: white; font-size: 1.5rem; font-weight: bold;")
          ),
          h5("Analyze Results", style = "color: #2c3e50; font-weight: 600;"),
          p("View detailed statistics and export your group assignments", 
            style = "color: #5a6c7d; font-size: 0.9rem;")
        )
      ),
      
      # Call to Action Section
      div(
        class = "row text-center py-5",
        div(
          class = "col-12",
          h2("Ready to Create Balanced Student Groups?", 
             style = "color: #2c3e50; font-weight: 600; margin-bottom: 20px;"),
          p("Join educators worldwide who trust Project Student GroupR for their academic projects.",
            style = "font-size: 1.1rem; color: #5a6c7d; margin-bottom: 30px;"),
          actionButton(
            "get_started_bottom",
            "Start Grouping Students",
            icon = icon("play-circle"),
            class = "btn-primary btn-lg",
            style = "padding: 12px 30px; 
                   font-size: 1.1rem; 
                   border-radius: 25px; 
                   font-weight: 600;
                   background: linear-gradient(45deg, #667eea, #764ba2);
                   border: none;
                   box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);"
          )
        )
      ),
      
      # Quick Stats
      div(
        class = "row py-4",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               color: white; 
               border-radius: 15px; 
               margin: 40px 0;",
        div(
          class = "col-md-4 text-center",
          h3("∞", style = "font-size: 2.5rem; margin-bottom: 10px;"),
          p("Students Grouped", style = "font-size: 1.1rem; margin: 0;")
        ),
        div(
          class = "col-md-4 text-center",
          h3("∞", style = "font-size: 2.5rem; margin-bottom: 10px;"),
          p("Projects Completed", style = "font-size: 1.1rem; margin: 0;")
        ),
        div(
          class = "col-md-4 text-center",
          h3("100%", style = "font-size: 2.5rem; margin-bottom: 10px;"),
          p("Satisfaction Rate", style = "font-size: 1.1rem; margin: 0;")
        )
      )
    )
  ),

  # Group students
  nav_panel(
    title = "Get Started",
    icon = icon("play-circle"),
    # Input widgets for the function argumnents
    sidebarLayout(
      sidebarPanel(
        bslib::card(
          card_body(
            tagList(
              # student_and_cwa file input
              bslib::tooltip(
                span("Hint", bsicons::bs_icon("question-circle-fill")),
                "File must have CWA ,NAME, INDEX NUMBER as colum names.",
                placement = "right"
              ),
              fileInput(
                inputId = "student_and_cwa",
                label = "Upload Student and CWA File",
                accept = c(".csv", ".xlsx")
              ),
              bslib::tooltip(
                span("Hint", bsicons::bs_icon("question-circle-fill")),
                'File must have NAME, INDEX NUMBER ,DEPARTMENT as colum names',
                placement = "right"
              ),
              # student_and_department file input
              fileInput(
                inputId = "student_and_department",
                label = "Upload Student and Department File",
                accept = c(".csv", ".xlsx")
              ),

              # stud_per_grp numeric input
              numericInput(
                inputId = "stud_per_grp",
                label = "Students per Group",
                value = 7,
                min = 1
              ),
              # dist_grp_to_lecturers switch
              input_switch(
                id = "dist_grp_to_lecturers",
                label = "Distribute Groups to Lecturers?",
                value = TRUE
              ),

              # dynamically show input widget if and only if user prefer randomisation
              # to lecturers.
              conditionalPanel(
                condition = "input.dist_grp_to_lecturers == true",

                # lecturer_names text input 
                textInput(
                  inputId = "lecturer_names",
                  label = "Lecturer Names (comma separated)",
                  value = NULL
                )
              ),

              # year text input
              textInput(
                inputId = "year",
                label = "Project Year",
                value = "2025"
              ),

              # department select input
              selectInput(
                inputId = "department",
                label = "Select of Choice",
                choices = NULL
              )
            )
          ),
          card_footer(
            # Submit button
            actionButton(
              inputId = "submit",
              label = "Submit",
              width = "100%",
              icon = icon('rocket'),class = 'btn-success'
            )
          )
        )
      ),
      # outputs
      mainPanel(
        navset_card_pill(
          nav_panel(title = "Grouped Result",
                    selectInput(inputId = 'lect_name',
                                label = 'Select lecturer names to view groups',
                                choices = NULL ,
                                width = "45%"),
                    # output for dataframe rendering
                    bslib::card_body(DT::DTOutput(outputId = 'grouped_result')),
                    card(
                      bslib::card_footer(textInput(inputId = 'file_name1',
                                                   value = "Groupings_2025",
                                                   label = 'Save file'),
                                         downloadButton(outputId = 'grp_download',
                                                        class = 'btn-warning')
                                         )
                    )
                    ),
          nav_panel(title = "Statistics",
                    splitLayout(
                    card(height = '400px',
                        card_body(
                          p(tags$b("Frequency of students within Groups")),
                          DT::DTOutput("Group_and_freq",height = '400px')
                        )
                      ),
                    card(
                      card_body(
                        p(tags$b("Frequency of students within classes")),
                        DT::DTOutput("statistic_df",height = '400px')
                      )
                    )
                    )
                    ),
          nav_panel(
            title = "Plots",
            # Barplot|scatter and distribution
            card(
              card_body(
                selectInput(inputId = 'plot_choice',
                            label = "Select desired plot to view",
                            choices = NULL),
                plotOutput(outputId = "plots"),
                card(
                  bslib::card_footer(textInput(inputId = 'file_name2',
                                               value = "Plots_2025",
                                               label = 'Save file'),
                                     downloadButton(outputId = 'plot_download',
                                                    class = 'btn-warning')
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Navigation functionality for Get Started buttons
  observeEvent(input$get_started_hero, {
    updateNavbarPage(session, inputId = 'key', selected = "Get Started")
  })
  
  observeEvent(input$get_started_bottom, {
    updateNavbarPage(session, inputId = 'key', selected = "Get Started")
  })
  
  
  # server side for the grouper function.
  #- first lets find the unique groups in the excel sheet so we use that to update
  # the select input.
  brk_data  <- reactiveVal(NULL) # an empty reactive container
  observe({
    req(input$student_and_department)
    # handle error gracefully
    tryCatch({
      # read the sheet
      result <- readxl::read_excel(input$student_and_department$datapath) |> as.data.frame()
      # get colnames that tally to department.
      depa_col <- grep('depa',x = colnames(result),ignore.case = T ,value = T)[[1]] # if there are two grab
      
      # check if depa_col is null. if it is null then inform user to correct it.
      if(length(depa_col) < 1){
        stop('There is no column name Department in the file. Please provide one')
      }
      
      # get unique values in department column if name is available.
      unique_depa <- result[[depa_col]] |> unique()
      
      brk_data(unique_depa) # store results in the reactive file
      
    },error = function(e){
      
      shinyWidgets::show_toast(title = "ERROR",
                               text = paste(e$message),
                               type = 'error' )
      
    })
   
   
  })
  
  # Update the selectinput widget for the department now
  observe({
    req(brk_data())
    updateSelectInput(session , inputId = "department" ,choices = brk_data() )
  })
  
  # Now to the submission, of user has fulfilled all righteousness.
  result <- reactiveVal(NULL)
  
  observeEvent(input$submit,{
    req(input$student_and_cwa$datapath , 
        input$department ,
        input$year,
        input$stud_per_grp,
        input$student_and_department$datapath)
    # defensive programming to prevent user from submitting when lecturer names
    # have not been provided.
    if(input$dist_grp_to_lecturers) req(input$lecturer_names)
    
    shinybusy::show_modal_spinner(spin = "swapping-squares",text = "Generating groups... Please wait")
    
    # Show a processing widget.
  tryCatch({
    
   result_1 <- Group_project_students(student_and_cwa = input$student_and_cwa$datapath ,
                           student_and_department = input$student_and_department$datapath,
                           stud_per_grp = input$stud_per_grp ,
                           dist_grp_to_lecturers = input$dist_grp_to_lecturers,
                           lecturer_names = if(input$dist_grp_to_lecturers) input$lecturer_names else NULL,
                           department = input$department,
                           project_year = input$year)
   
   result(result_1)
   
   # show alert
   shinyWidgets::show_alert(title = "Results Generated Successfully",
                            text = "" ,
                            type = 'success')
    
  },error = function(e){
    shinyWidgets::show_alert(title = 'Error',
                             text = e$message,
                             type = 'error')
  },finally = {
    shinybusy::remove_modal_spinner()
  })
  })
  

  
  # Ouptputs of results.
  # Dynamically display names of lecturers if they are available or not.
  observe({
    req(result())
    if(input$dist_grp_to_lecturers){
      updateSelectInput(session , inputId = 'lect_name',choices = names(result()[['Grouped_students']]))
    }else{
      updateSelectInput(session , inputId = 'lect_name',choices = c("No lecturers Provided"),selected = )
    }
  })

  # Render outputs now.
  observe({
    req(result())
    if(!any(names(result()[['Grouped_students']]) %in% c("Group","Index_Number","Name","CWA"))){
      output$grouped_result <- DT::renderDT(
        as.data.frame(result()[['Grouped_students']][[input$lect_name]],row.names = rep('',times = nrow(result()[['Grouped_students']][[input$lect_name]])))
      )
    }else if(any(names(result()[['Grouped_students']]) %in% c("Group","Index_Number","Name","CWA"))){
      output$grouped_result <- DT::renderDT(
        as.data.frame(result()[['Grouped_students']],row.names = rep('',times = nrow(result()[['Grouped_students']])))
      )
    }

    })
  
  #------ Plots side.
  # grap names within the results from the function.
  plot_names <- reactive({
    req(result())
    grep(pattern = 'bar|sca|dis',x = names(result()),ignore.case = T,value = T)
    
  })

  # Update the plot_choice input.
  observe({
    req(plot_names())
    updateSelectInput(session , inputId = 'plot_choice',choices  = plot_names())
  })
  
  # render outputs.
  output$plots <- renderPlot({
    req(input$plot_choice, result())
    print(result()[[input$plot_choice]])
  })
  
  #------ Statistics side.
  output$Group_and_freq <- DT::renderDT({
    req(result())
    as.data.frame(result()[["Group_and_freq" ]],row.names = rep('',times = nrow(result()[["Group_and_freq" ]])))
  })
  
  output$statistic_df <- DT::renderDT({
    req(result())
    as.data.frame(result()[["statistic_df"]],row.names = rep('',times = nrow(result()[["statistic_df"]])))
  })
  
  
  #-- download section
  #- Grouping results
  output$grp_download <- downloadHandler(
    filename = function(){
      paste(input$file_name1,".xlsx")
    },
    content = function(file){
      writexl::write_xlsx(x = result()[['Grouped_students']], file)
    }
  )
  
 
  #- Pdf or plots download.
  output$plot_download <- downloadHandler(
    filename = function(){
      paste(input$file_name2 ,".pdf")
    }, 
    content = function(file){
      plots <- result()[c("Bar_plot","Scatter_plot","Distribution_plot")]
      
      # Start PDF
      grDevices::pdf(file, width = 8, height = 6, onefile = TRUE)
      
      # Print plots
      if (is.list(plots)) lapply(plots, print) else print(plots)
      
      grDevices::dev.off() # Close PDF
    }
  )
  
  
}

shinyApp(ui, server)

