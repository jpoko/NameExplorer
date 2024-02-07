# overall popularity page

popularity_year_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        card(
            card_header(
                "Information",
                bslib::popover( 
                    bs_icon("info-circle"),
                    title = "Info",
                    shiny::markdown("
              Find names by popularity for different years. Popularity is the
              overall proportion of that 
              name appearing (for either sex) over time. Explore names within
              a given popularity percentile for different years.  

              **Table:** The table has the list of names in alphabetical order
               along with the number of babies that were given that name in
              the year you selected, and the
              percentile (i.e., popularity) of the name, which you specified in 
              the options.  
              
              **Plot:** The plot shows the percent of males and females that were given
              each name in the selected year. Hover over the plot to see more information.  
                            
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
                        
                        * the year of interest (1880-2017).  
                        * popularity percentile of interest (1-100): 
                        higher = more popular, lower = less popular.  
                        * number of random names to return (1-100).    
                        * if you want names that have been given to either males or females, 
                        or names definitely given to females or definitely given to males.   
                                
                        Click the 'Get sample' button
                        to get a random sample of names that fit your criteria; a new sample
                        is returned every time the button is clicked.  
                                
                        The total
                        number of names fitting your criteria is printed below
                        the button.
                        "
                                       )),
                        
                        sliderInput(
                            inputId = ns("year_input"),
                            label = tooltip(span("Select year",
                                                 bs_icon("question-circle")),
                                            "Single value, in blue."),
                            min = 1880,
                            max = 2017,
                            value = 1950,
                            step = 1,
                            sep = ""
                        ),
                        
                        sliderInput(
                            inputId = ns("pop_year_percentile_input"),
                            label = tooltip(span("Select popularity percentile",
                                                 bs_icon("question-circle")),
                                            "Single value, in blue. Higher Value =>
                                            More Popular, Lower Value => Less Popular"),
                            min = 1,
                            max = 100,
                            value = 75,
                            step = 1,
                            sep = ""
                        ),
                        
                        sliderInput(inputId = ns("pop_year_random_sample_choice"),
                                    label = tooltip(span("Number of random names to return",
                                                         bs_icon("question-circle")),
                                                    "Single value, between 1 and 100, in blue"),
                                    value = 10,
                                    min = 1,
                                    max = 100,
                                    step = 1),

                        radioButtons(
                            inputId = ns("pop_year_sex_type_choice"),
                            label = bslib::tooltip(span("Names given to...", 
                                                 bs_icon("question-circle")),
                                            "If select 'Female' or 'Male',
                                            will return names that definitely 
                                            have been given to females or males, 
                                            respectively. It does not mean that 
                                            the name is ONLY given to females 
                                            or males."),
                            choices = c("either female or male" = "both",
                                        "female" = "female",
                                        "male" = "male"),
                            selected = "both"
                        ),
                        
                        shiny::actionButton(
                            inputId = ns("run_pop_year_sample"),
                            label = "Get sample",
                            icon = icon("play")
                        ),
                        
                        hr(),
                        
                        textOutput(ns("pop_year_input_name_total_num"))
                        
                    ),

                    layout_column_wrap(
                        width = 1/2,
                        fillable = TRUE,
                        fill = TRUE,
                        #verbatimTextOutput(ns("messageOutput")),
                        tableOutput(ns("pop_year_table")),
                        #DTOutput(ns("pop_year_table_dt")),
                        plotlyOutput(ns("pop_year_selected_mf_plot"))
                    )
                )
            )
        )
        

    )
    
}


popularity_year_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        #ns <- session$ns

        interest_values <- reactiveValues(
            all_selected_data = NULL,
            all_selected_data_year = NULL
        )
        
        ## data ----
        
        pop_year_dat_selected <- eventReactive(input$run_pop_year_sample, {
            
            req(input$pop_year_percentile_input, input$year_input)
            
            # filter for year
            babynames_year <- babynames |>
                dplyr::filter(year == input$year_input) 
            
            # calculate proportion and n of each name, assign percentiles
            dat_year <- babynames_year |>
                
                # group by name
                dplyr::group_by(name) |>
                
                # get sum of prop & n for each name
                dplyr::summarise(
                    sum_prop = sum(prop), 
                    count_n = sum(n)) |> 
                
                # put in descending order
                dplyr::arrange(desc(sum_prop)) |>
                
                # ungroup
                dplyr::ungroup() |>
                
                # assign percentile to each name based on sum_prop value
                mutate(percentile = ntile(sum_prop, 100))
            
            # filter by percentile
            dat_year <- dat_year |>
                dplyr::filter(percentile==as.numeric(input$pop_year_percentile_input))
            
            # merge dat_year with babynames_year (so just data from selected year)
            # calculate total n, n Male, n Female, percent M, percent F
            dat_calc <- dat_year |>
                left_join(babynames_year, by = join_by(name), 
                          relationship = "many-to-many") |>
                dplyr::group_by(name) |>
                summarise(total_n = sum(n),
                          total_n_M = sum(n[sex == "M"]),
                          total_n_F = sum(n[sex == "F"])) |>
                mutate(prop_M = total_n_M / total_n * 100,
                       prop_F = total_n_F / total_n * 100)
            
            # put dat_calc together with dat_year
            dat <- dat_year |>
                full_join(dat_calc, by = join_by(name))

            # filter if want only M or F names (otherwise keep all)
            if (input$pop_year_sex_type_choice == "female") {
                dat <- dat |>
                    dplyr::filter(prop_F > 0)
            } else if (input$pop_year_sex_type_choice == "male") {
                dat <- dat |>
                    dplyr::filter(prop_M > 0)
            }
            
            # Save all data
            interest_values$all_selected_data <- dat 
            
            # Take random sample of names
            return_dat <- random_sample_names(dat = dat,
                                            n_samples = as.numeric(input$pop_year_random_sample_choice))
            
            # Save random sample data
            interest_values$all_selected_data_year <- return_dat
            
            #return_dat <- return_dat |>
            #    select(name, percent_female, percent_male)
            
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
        output$pop_year_input_name_total_num <- renderText({
            full_dat <- interest_values$all_selected_data
            
            num_names <- nrow(full_dat)
            num_names_text <- str_glue("Total number of names in data set that meet the search criteria: {num_names}")
            return(num_names_text)
            
        })
        
        
        
        # TODO: possible option to implement: sort the table and update the order 
        # in the plot (not yet working)
        # observe({
        #     # Check if the table is sorted
        #     if (!is.null(input$table_order)) {
        #         order_col <- names(input$table_order)[1]
        #         reactive_data$table <- table_data[order(table_data[[order_col]]), ]
        #         reactive_data$plot <- plot_data[order(plot_data[[order_col]]), ]
        #         
        #         # Render the sorted bar plot
        #         output$plot <- renderPlot({
        #             ggplot(reactive_data$plot, aes(x = name, y = m_f_ratio)) +
        #                 geom_bar(stat = "identity")
        #                 # Additional plot configurations
        #         })
        #     }
        # })
        

        # Plot of ratio of males/females with given name
        output$pop_year_selected_mf_plot <- renderPlotly({
            req(input$pop_year_percentile_input, input$year_input)
            
            result <- pop_year_dat_selected() 
            
            if (nrow(result) == 0) {
                # If no data, create an empty plot with a custom message
                empty_plot <- ggplot() +
                    annotate(
                        "text", 
                        x = 0.5, 
                        y = 0.5, 
                        label = "No data fit your search criteria", 
                        vjust = 0.5, 
                        hjust = 0.5, 
                        size = 5, 
                        color = "red") +
                    theme_void()
                
                # Convert ggplot to plotly
                bar_plot <- ggplotly(empty_plot) |>
                    plotly::config(displayModeBar = FALSE)
                
            } else {
            title_text <- glue::glue(
                'Percent ',
                '<span style = "color:{set_colors["M"]}">male</span>',
                '/',
                '<span style = "color:{set_colors["F"]}">female</span>')
                
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
            
            bar_plot <- ggplotly(p) |>
                plotly::config(displayModeBar = FALSE)
            }

            return(bar_plot)
        })

        
        # Make table of names
        output$pop_year_table <- renderTable({
            #req(input$pop_year_percentile_input, input$year_input)
            req(!is.null(interest_values$all_selected_data_year))

            full_dat_year <- interest_values$all_selected_data_year

            if (nrow(full_dat_year) == 0) {
                cat("No data fit your search criteria")
            } else {
                full_dat_year |>
                    # dplyr::select(name, total_n, percentile) |>
                    dplyr::rename(
                        n = total_n
                    ) |>
                    dplyr::arrange(name) |>
                    dplyr::select(name, n)
            }

        })
        

       
       
       
    }
    )   
}