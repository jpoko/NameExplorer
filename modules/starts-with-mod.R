# starts with module

starts_with_ui <- function(id){
    
    ns <- shiny::NS(id)
    
    tagList(
        card(
            card_header(
                "Information",
                bslib::popover( 
                    bs_icon("info-circle"),
                    title = "Info",
                    shiny::markdown("
                                    
              Find names that start with a certain letter or letters.  
             
              **Table:** List of names in alphabetical order
              with the number of babies given that name, and the
              percentile (i.e., popularity) of the name.    
              
              **Plot:** Shows the percent of males and females given
              each name. Hover over the plot for more information.  
                            
                            "
                    ))
            ),
            
            card_body(
                fillable = TRUE,
                fill = TRUE,
                
                layout_sidebar(
                    sidebar = sidebar(
                        bslib::popover(span(strong("Options"), bs_icon("info-circle")),
                                       shiny::markdown("
                                                       
               Enter/select:  
                              
              * the letter or letters the name(s) should start with.    
              * number of random names to return (1-100).   
              * if you want names that have been given to either males or females,
                or names definitely given to females or definitely given to males.   
                                
              Click the 'Get sample' button to get a random sample of names that
              fit your criteria; a new sample is returned every time the button is clicked.  
                                
              The total number of names fitting your criteria is printed below
              the button.

                                                       ")),
                        
                        textInput(inputId = ns("starts_with_search_text"),
                                  label = tooltip(span("Starts with...",
                                                       bs_icon("question-circle")),
                                                  "Enter letter or letters.")),
                        
                        sliderInput(inputId = ns("starts_with_random_sample_choice"),
                                    label = tooltip(span("Number of random names to return",
                                                         bs_icon("question-circle")),
                                                    "Single value, between 1 and 100, in blue"),
                                    value = 10,
                                    min = 1,
                                    max = 100,
                                    step = 1),

                        radioButtons(
                            inputId = ns("starts_with_sex_type_choice"),
                            label = bslib::tooltip(span("Names given to...", 
                                                 bs_icon("question-circle")),
                                            "If select 'Female' or 'Male', will 
                                            return names that definitely have 
                                            been given to females or males, 
                                            respectively. It does not mean that 
                                            the name is ONLY given to females or 
                                            males."),
                            choices = c("either female or male" = "both",
                                        "female" = "female",
                                        "male" = "male"),
                            selected = "both"
                        ),
                        
                        shiny::actionButton(
                            inputId = ns("run_starts_with_sample"),
                            label = "Get sample",
                            icon = icon("play")
                        ),
                        hr(),
                        
                        textOutput(ns("starts_with_input_name_total_num"))
                    ),
                    
                    layout_column_wrap(
                        width = 1/2,
                        fillable = TRUE,
                        fill = TRUE,
                        tableOutput(ns("starts_with_table")),
                        plotlyOutput(ns("starts_with_overall_prop_selected_mf_plot"))
                    )
                )
            )
        )

    )
    
}

starts_with_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        #ns <- session$ns
        
        interest_values <- reactiveValues(
            all_selected_data = NULL
        )
        
        ## data ----
        starts_with_name_input_dat <- eventReactive(input$run_starts_with_sample, {
            
            # make search text capitalized
            search_text <- str_to_title(input$starts_with_search_text)
            
            # filter data for names that start with string
            dat <- dat_all |>
                dplyr::filter(str_detect(name, paste0("^", search_text)))
            
            # filter if want only M or F names (otherwise keep all)
            if (input$starts_with_sex_type_choice == "female") {
                dat <- dat |>
                    dplyr::filter(prop_F > 0)
            } else if (input$starts_with_sex_type_choice == "male") {
                dat <- dat |>
                    dplyr::filter(prop_M > 0)
            }
            
            # Save data
            interest_values$all_selected_data <- dat 
            
            # select random sample of those names
            return_dat <- 
                random_sample_names(
                    dat,
                    n_samples = as.numeric(input$starts_with_random_sample_choice))
            
            # make data long for plotting
            dat_long <- return_dat |>
                pivot_longer(cols = c(prop_M, prop_F),
                             names_to = "sex",
                             values_to = "percentage") |>
                mutate(name = as.factor(name) |> fct_rev(),
                       sex = case_when(
                           sex |> str_detect("prop_M") ~ "M",
                           sex |> str_detect("prop_F") ~ "F"
                       ),
                       percentage = round(percentage, 2)) 
            
            # Pass long data
            return(dat_long)
            
        })
        
        ## name ----
        starts_with_name_input_name <- eventReactive(input$run_starts_with_sample, {
            
            name_input <- input$starts_with_search_text
            
            return(name_input)
        })
        
        
        ## total num names ----
        # Return total number of names that satisfy selected criteria
        output$starts_with_input_name_total_num <- renderText({
            full_dat <- interest_values$all_selected_data
            
            num_names <- nrow(full_dat)
            num_names_text <- str_glue("Total number of names in data set that meet the search criteria: {num_names}")
            return(num_names_text)
            
        })
        
        
        # Plot of ratio of males/females with given name 
        output$starts_with_overall_prop_selected_mf_plot <- renderPlotly({
            
            # Arrange names in alphabetical order
            result <- starts_with_name_input_dat() |>
                dplyr::arrange(name)
 
            # Create title for plot
            title_text <- glue::glue(
                'Percentage ',
                '<span style = "color:{set_colors["M"]}">male</span>',
                '/',
                '<span style = "color:{set_colors["F"]}">female</span>')
            
            # Create bar plot
            p <- ggplot(result, aes(name, 
                                    percentage, 
                                    color = sex, 
                                    fill = sex)) +
                geom_col(position = "stack") +
                coord_flip() +
                scale_color_manual(values = set_colors) +
                scale_fill_manual(values = set_colors) +
                labs(
                    title = title_text,
                    x = ""
                ) +
                theme_bw() +
                theme(
                    legend.position = "none",
                    plot.title = ggtext::element_markdown(),
                    plot.title.position = 'plot',
                    axis.text = element_text(size = 11)
                )
            
            # Make plotly
            ggplotly(p) |>
                plotly::config(displayModeBar = FALSE)
            
        })
        
        
        # Make table of names
        output$starts_with_table <- renderTable({
            starts_with_name_input_dat() |>
                distinct(name, .keep_all = TRUE) |>
                dplyr::arrange(desc(name)) |>
                dplyr::rename(n = total_n) |>
                dplyr::select(name, n, percentile)
        })
        
        
    }
    )
}