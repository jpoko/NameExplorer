#app.R

# LOAD PACKAGES ----
source("./modules/load-packages.R")

# SOURCE MODULES ----
dir(path = "modules", full.names = TRUE) |> map(~ source(.))

# thematic_on(bg = "auto",
#             fg = "auto",
#             accent = "auto",
#             font = "auto")

# Define UI 
ui <- page_navbar(
    #tags$head(includeHTML("google-analytics.html")),
    
    title = "NameExplorer",
    
    theme = bs_theme(
        version = 5,
        bootswatch = "yeti"
        ), 

    nav_panel("Home", home_ui("home")),
    nav_panel("Explore", explore_ui("explore")),
    nav_panel("Plot", plot_name_trend_ui("plot_trend")),
    nav_panel("General trends", overall_ui("overall")),
    nav_panel("About", about_ui("about")),
    nav_spacer(),
    nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
   
)

# Define server logic 
server <- function(input, output, session) {

    home_server("home")
    overall_server("overall")
    explore_server("explore")
    about_server("about")
    plot_name_trend_server("plot_trend")
}

# Run the application 
shinyApp(ui = ui, server = server)
