# home page

home_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(

        # center heading and subheading
        h3(class = 'text-center', strong("NameExplorer")),
        div(
            style = "margin: auto; width: 75%;",
            class = 'text-center', 
            h5(style = "font-style: italic;", "Find and explore first names given to babies born in the U.S. 
               between 1880 and 2017"),
        ),

        # Images with use cases
        tags$style(
            HTML(".custom-row { display: flex; align-items: center; margin-top: 10px; }")
        ),
        
        div(
            style = "margin: auto; width: 75%;",
        
        # Image 1
        div(
            class = "custom-row",
            div(
                style = "flex: 1;",
                tags$img(src = "slide_1.jpg", class = "img-fluid rounded", alt = "Cartoon image of baby in crib.")
            ),
            div(
                style = "flex: 2; padding-left: 20px;text-align: center;",  # Adjust padding or other styles as needed
                h5("Expecting a new bundle of joy?"),
                h5("Find possible names here!")
            )
        ),
        
        
        # Image 2
        div(
            class = "custom-row",
            div(
                style = "flex: 2; padding-left: 20px; text-align: center;",  # Adjust padding or other styles as needed
                h5("Writing a novel and need names for 3 sisters that all contain 'ina'?"),
                h5("Find their names here!")
            ),
            div(
                style = "flex: 1;",
                tags$img(src = "slide_2.jpg", class = "img-fluid rounded", alt = "Cartoon image of 3 females holding baskets in an apple orchard.")
            )
        ),
        
        # Image 3
        div(
            class = "custom-row",
            div(
                style = "flex: 1;",
                tags$img(src = "slide_5.jpg", class = "img-fluid rounded", alt = "The name 'Ava' with different years around the name."
                )
            ),
            div(
                style = "flex: 2; padding-left: 20px; text-align: center;",  # Adjust padding or other styles as needed
                h5("Curious what year your name was the most popular?"),
                h5("Explore naming trends here!")
            )
        ),

        hr(),

        # Accordion - how to use the app
        accordion(
            open = TRUE, 
            multiple = TRUE,
            accordion_panel(
                title = "How to use this app",
                p("Main navigation is along the top. Below are descriptions about what
          is found on each page."),
          
          p(strong("Explore")),
          p("Explore names in various ways: overall popularity, popularity by year,
          starting letter(s), contains letter(s), number of letters."),
          
          p(strong("Plot")),
          p("Enter a name of interest and view a plot showing the number of babies born each year who were given that name."),
          
          p(strong("General trends")),
          p("View general U.S. naming trends over time."),

        ),
        
        accordion_panel(
            title = "Recommendations",
            p("If you are interested in finding names, start with 'Explore'."),
            p("If you have a name and want to see how popular, or not, the name has been over time, start with 'Plot'."),
            p("If you are curious about naming trends in the U.S., start with 'General trends'.")
            
        ),
        
          accordion_panel(
              title = "Caveats",
              p("It appears that all names have been 'cleaned' and standarized,
                but this is speculation on my behalf based on observations of
                the names in the dataset."),
              p("Names that originally had
                spaces or punctuation have had spaces and/or punctuation removed.
                For example, the name D'Andre appears as Dandre, and
                Hannah Elizabeth appears as Hannahelizabeth."),
              p("All names are 2-15 letters long - there are no names longer than
                15 letters. Names were likely limited to 15 letters and any
                letters beyond 15 were removed. For example, the name Matthew Alexander
                appears as Matthewalexande."),
              
              p("All names follow the pattern of capitalized first letter followed
                by all lowercase letter(s) - there are no names that have capitalization
                within the name. For example, the name MaryAnn appears as
                as Maryann.")
        
          )
        ),
  br()

        ),
  
    )
    
}


home_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
  
    }
    )   
}