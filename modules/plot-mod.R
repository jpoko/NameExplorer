# plot module

plot_name_trend_ui <- function(id){
    
    ns <- shiny::NS(id)
    
    tagList(
        h4("Plot"),

        uiOutput(ns("name_selected_trend"))
    )
    
}

plot_name_trend_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
       
        interest_values <- reactiveValues(
            name_selected = NULL,
        )
        
        name_input_dat <- eventReactive(input$run_name_trend_plot, {
            
            input_name <- input$overall_prop_name_trend
            
            ## clean search text 
            # remove punctuation and spaces
            cleaned_name <- str_replace_all(input_name, "[[:punct:][:space:]]", "")
            
            # make text capitalized
            search_text <- str_to_title(cleaned_name)
            
            interest_values$name_selected <- search_text
            
            # find that name in the babynames df
            dat <- babynames |>
                dplyr::filter(name == search_text)
                #dplyr::filter(str_detect(name, paste0("^", search_text)))
            
            return(dat)
        })

        # Proportion plot
        # output$name_trend_plot <- renderPlotly({
        #     
        #     req(input$overall_prop_name_trend, name_input_dat())
        #     
        #     if (nrow(name_input_dat()) < 2) {
        #         
        #         empty_plotly_msg("Name does not\nexist in the data")
        #         
        #     } else if (nrow(name_input_dat()) > 1) {
        #         
        #         title_text <- glue::glue(
        #             "Proportion ",
        #             "<span style = 'color:{set_colors['M']}'>male</span>",
        #             "/",
        #             "<span style = 'color:{set_colors['F']}'>female </span>",
        #             "{interest_values$name_selected}'s")
        #         
        #         
        #         p <- line_plot_by_time_by_sex(name_input_dat(), name_input_dat()$prop, title_text, "Proportion")
        #         
        #         ggplotly(p) |>
        #             plotly::config(displayModeBar = FALSE)
        #     }
        #     
        #     
        #     
        # })
        
        # Number plot
        output$name_trend_num_plot <- renderPlotly({
            
            req(input$overall_prop_name_trend, name_input_dat())
            
            if (nrow(name_input_dat()) < 2) {
                
                empty_plotly_msg("Name does not\nexist in the data")
                
            } else if (nrow(name_input_dat()) > 1) {
                
                title_text <- glue::glue(
                    "Number of ",
                    "<span style = 'color:{set_colors['M']}'>male</span>",
                    "/",
                    "<span style = 'color:{set_colors['F']}'>female </span>",
                    "{interest_values$name_selected}'s")
                
                p <- line_plot_by_time_by_sex(name_input_dat(), name_input_dat()$n, title_text, "Number")
                
                p <- p + aes_(y = as.name("n"))
                
                ggplotly(p, tooltip = c("x", "y", "color")) |>
                    plotly::config(displayModeBar = FALSE)
  
            }
            
        })
        
        
        trend_plot_info <- bslib::popover(
            bs_icon("info-circle"),
            title = "Trend info",
            p("Enter a name of interest and
             click the botton to view a plot showing the number
             of males and females with that name over time.")
        )
        
        
        # name inputted 
        output$text_plot_input_name <- eventReactive(input$run_name_trend_plot, {
            
            input_name <- input$overall_prop_name_trend
            
            ## clean search text 
            # remove punctuation and spaces
            cleaned_name <- str_replace_all(input_name, "[[:punct:][:space:]]", "")
            
            # make text capitalized
            search_text <- str_to_title(cleaned_name)
            
            interest_values$name_selected <- search_text
            
            name_entered_text <- glue::glue("
                                            Name matched in dataset: {search_text}
                                            ")
            return(name_entered_text)
            
        })
        
        name_input_dat <- eventReactive(input$run_name_trend_plot, {
            req(interest_values$name_selected)
 
            # find inputted name in the babynames df
            dat <- babynames |>
                dplyr::filter(name == interest_values$name_selected)
            
            return(dat)
        })
        
        output$name_selected_trend <- renderUI({
            
            tagList(
                
                card(
                    card_header(
                        "Trend info", trend_plot_info
                    ),
                    card_body(
                        layout_sidebar(
                            sidebar = sidebar(
                                textInput(inputId = ns("overall_prop_name_trend"),
                                          label = "Name"),
                                shiny::actionButton(
                                    inputId = ns("run_name_trend_plot"),
                                    label = "View trend",
                                    icon = icon("play")
                                ),
                                hr(),
                                textOutput(ns("text_plot_input_name"))
                            ),
                            plotlyOutput(ns("name_trend_num_plot"))
                            
                            # if include proportion and number plots
                            # layout_column_wrap(
                            #     width = 1/2,
                            #     plotlyOutput(ns("name_trend_num_plot")),
                            #     plotlyOutput(ns("name_trend_plot"))
                            #     
                            # )
                            
                        )
                    )
                )
                
                
            )
        })
        
        
    }
    )
}