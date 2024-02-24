# starts with module

length_ui <- function(id){
    
    ns <- shiny::NS(id)
    
    tagList(
        card(
            card_header(
                "Information",
                bslib::popover( 
                    bs_icon("info-circle"),
                    title = "Info",
                    shiny::markdown("
              Find names that have a certain number of letters in the name.  
              
              Output:  
              
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
            Select:  
                              
            * the number of letters the name(s) should contain.    
            * number of random names to return (1-100).  
            * if you want names that have been given to either males or females,
            or names definitely given to females or definitely given to males.   
            
            Click the 'Get sample' button to get a random sample of names that
            fit your criteria; a new sample is returned every time the button is
            clicked.  
                                
            The total number of names fitting your criteria is printed below
            the button.
            
            "
                                       )),     
                                                       
                        
                        sliderInput(inputId = ns("name_length_input"),
                                     label = tooltip(span("Length of name",
                                                          bs_icon("question-circle")),
                                                     "Single value, between 2 and 15, in blue"),
                                     value = 5,
                                     min = 2,
                                     max = 15,
                                     step = 1),
                        
                        sliderInput(inputId = ns("length_random_sample_choice"),
                                    label = tooltip(span("Number of random names to return",
                                                         bs_icon("question-circle")),
                                                    "Single value, between 1 and 100, in blue"),
                                    value = 10,
                                    min = 1,
                                    max = 100,
                                    step = 1),
                        
                        radioButtons(
                            inputId = ns("length_sex_type_choice"),
                            label = bslib::tooltip(span("Names given to...", 
                                                 bs_icon("question-circle")),
                                            "If select 'Female' or 'Male', will return names that definitely have been given to females or males, respectively. It does not mean that the name is ONLY given to females or males."),
                            choices = c("either female or male" = "both",
                                        "female" = "female",
                                        "male" = "male"),
                            selected = "both"
                        ),
                        
                        shiny::actionButton(
                            inputId = ns("run_length_sample"),
                            label = "Get sample",
                            icon = icon("play")
                        ),
                        
                        hr(),
                        
                        textOutput(ns("length_input_name_total_num"))
                    ),
                    
                    layout_columns(
                        fillable = TRUE,
                        fill = TRUE,
                        tableOutput(ns("length_table")),
                        plotlyOutput(ns("length_overall_prop_selected_mf_plot"))
                    )
                )
            )
        )

    )
    
}

length_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        #ns <- session$ns
        
        interest_values <- reactiveValues(
            all_selected_data = NULL
        )
        
        
        ## data ----
        length_name_input_dat <- eventReactive(input$run_length_sample, {
            
            # filter for names with selected character length
            dat <- dat_all |>
                dplyr::filter(nchar(name) == input$name_length_input)
            
            # filter if want only M or F names (otherwise keep all)
            if (input$length_sex_type_choice == "female") {
                dat <- dat |>
                    dplyr::filter(prop_F > 0)
            } else if (input$length_sex_type_choice == "male") {
                dat <- dat |>
                    dplyr::filter(prop_M > 0)
            }
            
            # Save data
            interest_values$all_selected_data <- dat 
            
            # select random sample of those names
            return_dat <- 
                random_sample_names(
                    dat,
                    n_samples = as.numeric(input$length_random_sample_choice))
            
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
        

        
        ## total num names ----
        # Return total number of names that satisfy selected criteria
        output$length_input_name_total_num <- renderText({
            full_dat <- interest_values$all_selected_data
            
            num_names <- nrow(full_dat)
            num_names_text <- str_glue("Total number of names in data set that meet the search criteria: {num_names}")
            return(num_names_text)

        })
        
        

        output$length_overall_prop_selected_mf_plot <- renderPlotly({
            
            # Arrange names in alphabetical order
            result <- length_name_input_dat() |>
                dplyr::arrange(name)
            
            # Create title for plot
            title_text <- glue::glue(
                'Percentage ',
                '<span style = "color:{set_colors["M"]}">male</span>',
                '/',
                '<span style = "color:{set_colors["F"]}">female</span>')
            
            # Create bar plot
            p <- ggplot(result, aes(name, percentage, color = sex, fill = sex)) +
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
        output$length_table <- renderTable({
            
            length_name_input_dat() |>
                distinct(name, .keep_all = TRUE) |>
                dplyr::arrange(desc(name)) |>
                dplyr::rename(n = total_n) |>
                dplyr::select(name, n, percentile)
        })
        
        
    }
    )
}