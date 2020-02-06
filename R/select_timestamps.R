

#' Title
#'
#' @param construction_object
#' @param single

#' @export
#'
select_timestamps <- function(construction_object, single) {

    ui <- miniPage(
        gadgetTitleBar(ifelse(single, "Select timestamp", "Select timestamps")),
        miniContentPanel(
            uiOutput("selection"),
            tableOutput("data")
        )
    )


    server <- function(input, output, session){

        timestamps <- names(construction_object$data)[unlist(map(map(construction_object$data, class), ~any(.x %in% c("POSIXct","Date"))))]

        output$selection <- renderUI({
            if(single) {
                selectInput("timestamp", "The following columns are timestamps:",
                               choices = names(construction_object$data),
                               multiple = F,
                               selected = ifelse(length(timestamps)>0, timestamps[[1]], NA))

            } else {
                selectInput("timestamp", "The following columns are timestamps:",
                               choices = names(construction_object$data),
                               multiple = T,
                               selected = ifelse(length(timestamps)>0, timestamps, NA))
            }
        })

        output$data <- renderTable(construction_object$data)

        observeEvent(input$done, {
            construction_object$timestamps <- input$timestamp
            .construction_object <<- construction_object
            rstudioapi::sendToConsole(glue::glue("check_timestamps(.construction_object)"))
            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))

}
