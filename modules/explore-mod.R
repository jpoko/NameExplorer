# explore page

explore_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        # About -----
        accordion(
            open = FALSE,
            accordion_panel(
                "About",
                p("Explore names various ways: overall popularity, popularity by year,
                  names that start with a certain letter or letters, names that contain 
                  certain letter(s), and names by length."),
                p(strong("Popularity")),
                p("Popularity is the overall proportion of that name
                  appearing (for either sex) over time. Explore more or less
                  popular names by getting a random sampling of names within
                  a given popularity percentile."),
                p("Higher percentile = More popular"),
                p("Lower percentile = Less popular"),
                p("You can also explore popularity by year - select the year of
                  interest, then the percentile to explore."),
                p(strong("Starts with")),
                p("Find names that start with a certain letter or string of
                  letters by entering the desired starting letter(s)."),
                p(strong("Contains")),
                p("Find names that contain a certain string/pattern of letters
                  somewhere in the name by entering the desired string."),
                p(strong("Length")),
                p("Find names that have a certain length (number of characters) by
                  entering the desired length.")

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
            
            nav_panel(
                title = "Contains",
                contains_ui(NS(id, "contains"))
            ),
            
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

        starts_with_server("start")
        length_server("length")
        contains_server("contains")
        overall_popularity_server("pop")
        popularity_year_server("year")
        

    }
    )   
}