

#' Title
#'
#' @inheritParams select_log_identifiers
#' @param single [lgl]: Single timestamp per row if `TRUE` for event log, multiple timestamps per row if `FALSE` for activity log.

#' @export
#'
select_timestamps <- function(construction_object, single) {

    ui <- miniPage(
        gadgetTitleBar(ifelse(single, "Select timestamp", "Select timestamps"), right = miniTitleBarButton("done","Next", TRUE)),
        miniContentPanel(
            uiOutput("selection"),
            verbatimTextOutput("data"),
            actionButton("previous", "Previous")
        )
    )


    server <- function(input, output, session){

        output$data <- renderPrint(construction_object$data %>% str())

        timestamps <- names(construction_object$data)[unlist(map(map(construction_object$data, class), ~any(.x %in% c("POSIXct","Date"))))]
        timestamps2 <- names(construction_object$data[stringr::str_detect(names(construction_object$data), "time")])
        timestamps <- append(timestamps, timestamps2)

        output$selection <- renderUI({
            if(single) {
                selectizeInput("timestamp", "The following column is a timestamp:",
                               choices = names(construction_object$data),
                               multiple = F,
                               selected = ifelse(length(timestamps)>0, timestamps[[1]], NA))

            } else {
                selectizeInput("timestamp", "The following columns are timestamps:",
                               choices = names(construction_object$data),
                               multiple = T,
                               selected = timestamps)
            }
        })

        observeEvent(input$previous, {
            construction_object$page = "Previous"
            .construction_object <<- construction_object
            stopApp()
        })

        observeEvent(input$done, {
            construction_object$timestamps <- input$timestamp
            construction_object$page = "Next"

            .construction_object <<- construction_object
            # rstudioapi::sendToConsole(glue::glue("check_timestamps(.construction_object)"))
            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))
    # rstudioapi::sendToConsole(glue::glue("check_timestamps(.construction_object)"))

    if(.construction_object$page == "Next") {
        rstudioapi::sendToConsole(glue::glue("check_timestamps(.construction_object)"))
    }
    else {
        rstudioapi::sendToConsole(glue::glue("decide_type(.construction_object)"))
    }

}
