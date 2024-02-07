# about module

about_ui <- function(id){
    
    ns <- shiny::NS(id)
    
    tagList(
        div(
            class = "d-flex justify-content-center",
        accordion(
            open = TRUE,
            multiple = TRUE,
            width = "75%",
            margin = "0 auto",
            
            accordion_panel(
                "About this dashboard app",
                value = "credits_about",
                uiOutput(ns("credits_about"))
            ),
            
            accordion_panel(
                title = "Data",
                uiOutput(ns("package_info"))
            )
        ),
        
        
       
        )

    )
}

about_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        output$credits_about <- renderUI({
            tagList(
                markdown("This app was created by Jennifer Pokorny, PhD: <https://www.linkedin.com/in/jenpokorny/>."), 
                markdown("For questions pertaining to the app, please write to Jen at [jenpokorny@gmail.com](mailto:jenpokorny@gmail.com)."),
                markdown("Code can be found here: <https://github.com/jpoko/NameExplorer>")
            ) 
        })
        
        output$package_info <- renderUI({
            tagList(
                markdown("Data from {babynames} R package: N Wickham H (2021). babynames: US Baby Names 1880-2017. R package version 1.0.1,
  <https://CRAN.R-project.org/package=babynames>."), 
                markdown("Details (from {babynames} package): 'For each year from 1880 to 2017, the number of children of each 
          sex given each name. All names with more than 5 uses are given. (Source: http://www.ssa.gov/oact/babynames/limits.html).'")
            ) 
        })
        
       
    }
    )
}