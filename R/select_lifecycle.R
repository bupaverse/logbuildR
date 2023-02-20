



#' Title
#'
#' @param construction_object
#'
#' @export
#'
select_lifecycle <- function(construction_object) {
    ui <- miniPage(
        gadgetTitleBar("Select lifecycle"),
        miniContentPanel(
            radioButtons("is_available", "Lifecycle column available?", choices = c("No, set lifecycle to 'complete' for all events" = "no",
                                                                                    "Yes, lifecycle column is available" = "yes"), selected = "no"),
            uiOutput("selection"),
            verbatimTextOutput("data")
        )
    )


    server <- function(input, output, session){

        output$data <- renderPrint(construction_object$data %>% glimpse())

        output$selection <- renderUI({
            if(input$is_available == "yes"){
                selectInput("selected_column", "Select lifecycle column", choices = names(construction_object$data))
            }
        })


        observeEvent(input$done, {

            if(input$is_available == "yes") {
                construction_object$lifecycle_id <- input$selected_column
                construction_object$complete_lifecycle_added <- FALSE
                rstudioapi::sendToConsole(glue::glue("check_lifecycle_events(.construction_object)"))
            } else {
                construction_object$data <- construction_object$data %>%
                    mutate(lifecycle_logbuildR = "complete")
                construction_object$complete_lifecycle_added <- TRUE
                construction_object$lifecycle_id <- "lifecycle_logbuildR"
                rstudioapi::sendToConsole(glue::glue("select_activity_instance(.construction_object)"))
            }

            .construction_object <<- construction_object

            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))


}
