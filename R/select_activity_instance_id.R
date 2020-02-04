



#' Title
#'
#' @param construction_object
#'
#' @export
#'
select_activity_instance <- function(construction_object) {
    ui <- miniPage(
        gadgetTitleBar("Select activity instance id"),
        miniContentPanel(
            radioButtons("is_available", "Activity instance id available?", choices = c("No, guess activity instance id" = "no",
                                                                                    "Yes, activity instance id column is available" = "yes"), selected = "no"),
            uiOutput("selection"),
            dataTableOutput("data")
        )
    )


    server <- function(input, output, session){

        output$data <- renderDataTable(construction_object$data)

        output$selection <- renderUI({
            if(input$is_available == "yes"){
                selectInput("selected_column", "Select activity instance id column", choices = names(construction_object$data))
            }
        })


        observeEvent(input$done, {

            if(input$is_available == "yes") {
                construction_object$activity_instance_id <- input$selected_column
                rstudioapi::sendToConsole(glue::glue("save_log(.construction_object)"))
            } else {
                construction_object$data <- assign_instance_id(construction_object$data,
                                                               construction_object$case_id,
                                                               construction_object$activity_id,
                                                               construction_object$timestamps,
                                                               construction_object$lifecycle_id)

                construction_object$activity_instance_id <- "activity_instance_logbuildR"
                rstudioapi::sendToConsole(glue::glue("save_log(.construction_object)"))
            }

            .construction_object <<- construction_object

            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))


}
