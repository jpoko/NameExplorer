# explore page

explore_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        # About -----
        accordion(
            open = FALSE,
            accordion_panel(
                "About",
                
                uiOutput(ns("about_description"))
            )
        ),
        
        # Heading ----

        
        # Explore tabs ----
        navset_tab(
            id = ns("explore_tabs"),
            #height = "1500px",
            #full_screen = TRUE,
            #fillable = TRUE,
            
            #### Overall popularity ----
            nav_panel(
                title = "Overall popularity",
                overall_popularity_ui(NS(id, "pop"))
            ),
            
            #### Popularity by year ----
            nav_panel(
                title = "Popularity by year",
                popularity_year_ui(NS(id, "year"))
            ),
            
            
            ## Starts with ----
            nav_panel(
                title = "Starts with",
                starts_with_ui(NS(id, "start"))
            ),
            
            ## Contains ----
            nav_panel(
                title = "Contains",
                contains_ui(NS(id, "contains"))
            ),
            
            ## Length ----
            nav_panel(
                title = "Length",
                length_ui(NS(id, "length"))
            )

        )
    )
    
}


explore_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns

        
        output$about_description <- renderUI({
            includeMarkdown("./txt/about_description.md")
        })
        
        
        starts_with_server("start")
        length_server("length")
        contains_server("contains")
        overall_popularity_server("pop")
        popularity_year_server("year")
        

    }
    )   
}