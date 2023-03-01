
#' Title
#'
#' @param construction_object
#'
#' @export
#'
decide_type <- function(construction_object) {

    ui <- miniPage(
        gadgetTitleBar("Activities or events",right = miniTitleBarButton("done","Next", TRUE)),



        miniContentPanel(
            fluidRow(column(width = 6,
            radioButtons(width = "100%", "choice", "Is each row in the data an event, or an activity instance?",
                         choices = c( "Event - one relevant timestamp for each row" = "Event",
                                      "Activity - multiple relevant timestamps for each row" = "Activity"),
                         selected = ifelse(length(names(construction_object$data)[unlist(map(map(construction_object$data, class),
                                                                                                                                                                                ~any(.x %in% c("POSIXct","Date"))))]) > 1, "Activity", "Event"))),
            column(width = 6,
            HTML("<b>Possible timestamps</b><br/>"),
            htmlOutput("possible_timestamps"))),
            verbatimTextOutput("data"),
            actionButton("previous", "Previous")
        )
     )


    server <- function(input, output, session){

        output$possible_timestamps <- renderUI({
            timestamps <- names(construction_object$data)[unlist(map(map(construction_object$data, class),
                                                                     ~any(.x %in% c("POSIXct","Date"))))]
            timestamps2 <- names(construction_object$data[stringr::str_detect(names(construction_object$data), "time")])
            timestamps <- append(timestamps, timestamps2)

            if(length(timestamps) > 0) {
                HTML(paste(c(timestamps,"<br/>"), collapse = "<br/>"))
            } else {
                HTML("No timestamp variables found.<br/>")
            }
        })

        output$data <- renderPrint(construction_object$data %>% glimpse())

        observeEvent(input$previous, {
            construction_object$page = "Previous"
            .construction_object <<- construction_object
            stopApp()
        })

        observeEvent(input$done, {
            construction_object$type <- input$choice
            construction_object$page = "Next"
            .construction_object <<- construction_object

            # if(input$choice == "Event") {
            #     rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = T)"))
            # } else {
            #     rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = F)"))
            # }
            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))

    if(.construction_object$page == "Next") {
        if(.construction_object$type == "Event") {
            rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = T)"))
        }
        else {
            rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = F)"))
        }
    }
    else {
        rstudioapi::sendToConsole(glue::glue("select_ids(.construction_object)"))
    }
}

