
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
            fluidRow(column(width = 6,
            radioButtons(width = "100%", "choice", "Is each row in the data an event, or an activity instance?",
                         choices = c( "Event - one relevant timestamp for each row" = "Event",
                                      "Activity - multiple relevant timestamps for each row" = "Activity"))),
            column(width = 6,
            HTML("<b>Possible timestamps</b><br/>"),
            htmlOutput("possible_timestamps"))),
            verbatimTextOutput("data")
        )
     )


    server <- function(input, output, session){

        output$possible_timestamps <- renderUI({
            timestamps <- names(construction_object$data)[unlist(map(map(construction_object$data, class),
                                                                     ~any(.x %in% c("POSIXct","Date"))))]
            if(length(timestamps) > 0) {
                HTML(paste(c(timestamps,"<br/>"), collapse = "<br/>"))
            } else {
                HTML("No timestamp variables found.<br/>")
            }
        })

        output$data <- renderPrint(construction_object$data %>% glimpse())

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

