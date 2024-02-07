# overall/general trends page

overall_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        h3(class = 'text-center', strong("General trends over time")),
        p(class = "text-center", "More trends coming soon..."),
        
        div(
            style = "margin: auto; width: 75%;",
            h4("Uniqueness"),
            p("Plot showing the number of unique names given to male and female babies from 1880 to 2017."),
            uiOutput(ns("unique_trend"))
        )

    )
    
}


overall_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns

        
        output$title_text_text <- renderText(
            title_text <- glue::glue(
                'Both ',
                '<span style = "color:{set_colors["M"]}">male </span>',
                'and ',
                '<span style = "color:{set_colors["F"]}">female </span>',
                'babies have been given more unique names over time')
        )
        
        output$num_names_year_plot <- renderPlotly({
            
            # calculate number of unique names by sex by year
            num_names_year <- babynames |>
                group_by(year, sex) |>
                summarize(num = n())
            
            title_text <- ""

            p <- line_plot_by_time_by_sex(num_names_year, 
                                          num_names_year$num, 
                                          title_text, 
                                          "Number of unique names")
            
            #p <- p + aes_(y = as.name("n"))
            
            ggplotly(p, tooltip = c("x", "y", "color")) |>
                plotly::config(displayModeBar = FALSE)
            
            
            # ggplotly(p) %>% 
            #     layout(
            #         #plot_bgcolor = "#bababa",
            #         #paper_bgcolor= "#bababa",
            #            #xaxis = list(
            #             #   color = '#ffffff'),
            #            #yaxis = list(
            #             #   color = '#ffffff')
            #            )
            
        })
        
        
        
        output$unique_trend <- renderUI({
            card(
                card_body(
                    tags$p(
                        HTML(
                            glue::glue(
                                'Both ',
                                '<span style="color:{set_colors["M"]}">male </span>',
                                'and ',
                                '<span style="color:{set_colors["F"]}">female </span>',
                                'babies have been given more unique names over time'
                            )
                        )
                    ),
                    plotlyOutput(ns("num_names_year_plot"))
                )
            )
        })
        

        
    })
      
}