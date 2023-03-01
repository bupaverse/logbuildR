


#' Title
#'
#' @inheritParams select_ids
#'
#' @importFrom bupaR convert_timestamps
#' @export
#'
prepare_timestamps <- function(construction_object) {

    timestamps <- construction_object$timestamps

    timestamp_data <- select(construction_object$data, timestamps)

    are_timestamps <- unlist(map(map(timestamp_data, class), ~any(.x %in% c("POSIXct","Date"))))

    not_timestamps <- timestamps[!are_timestamps]

    timestamp_data <- select(timestamp_data, not_timestamps) %>% slice(1:20)

    ui <- miniPage(
        gadgetTitleBar("Prepare timestamps", right = miniTitleBarButton("done","Next", TRUE)),
        miniContentPanel(
            selectizeInput("format", "Timestamps have the following format:",
                           choices = c("ymd_hms", "ymd_hm", "ymd_h","ymd",
                                       "dmy_hms", "dmy_hm", "dmy_h", "dmy",
                                       "mdy_hms", "mdy_hm", "mdy_h", "mdy"),
                           multiple = F),
            tableOutput("data"),
            actionButton("previous", "Previous")
        )
    )


    server <- function(input, output, session){

        output$data <- renderTable(timestamp_data)

        observeEvent(input$previous, {
            construction_object$page = "Previous"
            .construction_object <<- construction_object
            stopApp()
        })

        observeEvent(input$done, {
            construction_object$data <- mutate_at(construction_object$data, not_timestamps, match.fun(input$format))
            construction_object$timestamps_to_prepare <- not_timestamps
            construction_object$timestamps_format <- input$format

            construction_object$page = "Next"

            .construction_object <<- construction_object

            # if(construction_object$type == "Activity") {
            #     rstudioapi::sendToConsole("check_lifecycle_activities(.construction_object)")
            # } else {
            #     rstudioapi::sendToConsole("select_lifecycle(.construction_object)")
            # }

            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))

    if (.construction_object$page == "Next") {
        if(.construction_object$type == "Activity") {
            rstudioapi::sendToConsole("check_lifecycle_activities(.construction_object)")
        } else {
            rstudioapi::sendToConsole("select_lifecycle(.construction_object)")
        }
    }

    else {
        rstudioapi::sendToConsole("select_timestamps(.construction_object, single = TRUE)")
    }
}
