

require(shiny)
require(tidyverse)
require(shinyjs)


ui=navbarPage("NBA features",
              tabPanel("First Page",
                       fluidPage( 
                         sidebarLayout(
                           sidebarPanel("Side Bar panel"),
                           mainPanel("Main panel, Output")
                         )
                       )
              ),
              tabPanel("Second Page",
                       fluidPage( 
                         sidebarLayout(
                           sidebarPanel("Side Bar panel"),
                           mainPanel("Main panel, Output")
                         )
                       )
              )
)


server <- function(input, output, session) {

}


# Run the application 
shinyApp(ui = ui, server = server)
