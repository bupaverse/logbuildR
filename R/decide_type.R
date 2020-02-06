
#' Title
#'
#' @param construction_object
#'
#' @export
#'
decide_type <- function(construction_object) {

    ui <- miniPage(
        gadgetTitleBar("Activities or events"),
        miniContentPanel(
            radioButtons(width = "100%", "choice", "Is each row in the data an event, or an activity instance?", choices = c("Event", "Activity")),
            tableOutput("data")
        )
     )


    server <- function(input, output, session){


        output$data <- renderTable(construction_object$data)

        observeEvent(input$done, {
            construction_object$type <- input$choice
            .construction_object <<- construction_object

            if(input$choice == "Event") {
                rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = T)"))
            } else {
                rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = F)"))
            }
            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))

}

