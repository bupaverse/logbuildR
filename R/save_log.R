



#' Title
#'
#' @param construction_object
#'
#' @export
#'
save_log <- function(construction_object) {
    ui <- miniPage(
        gadgetTitleBar("Save log"),
        miniContentPanel(
            radioButtons("type", "Save log as:", choices = c("Eventlog" = "eventlog", "Activitylog" = "activitylog")),
            textInput("objectname", "Name:")
        )
    )

    server <- function(input, output, session){


        observeEvent(input$done, {
            if(input$type == "activitylog" & construction_object$type == "Activity") {
                log <- activitylog(construction_object$data,
                                   case_id = construction_object$case_id,
                                   activity_id = construction_object$activity_id,
                                   resource_id = construction_object$resource_id,
                                   lifecycle_ids = construction_object$timestamps)
            } else if(input$type == "eventlog" & construction_object$type == "Event") {
                log <- eventlog(construction_object$data,
                                case_id = construction_object$case_id,
                                activity_id = construction_object$activity_id,
                                timestamp = construction_object$timestamp,
                                resource_id = construction_object$resource_id,
                                lifecycle_id = construction_object$lifecycle_id,
                                activity_instance_id = construction_object$activity_instance_id)
            } else if(input$type == "eventlog" & construction_object$type == "Activity") {
                log <- activities_to_eventlog(construction_object$data,
                                              case_id = construction_object$case_id,
                                              activity_id = construction_object$activity_id,
                                              resource_id = construction_object$resource_id,
                                              timestamps = construction_object$timestamps)
            }

            assign(input$objectname, log, envir = .GlobalEnv)
            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 800))
}
