



#' Title
#'
#' @param construction_object
#'
#' @export
#'
recode_lifecycles_events <- function(construction_object) {


    lifecycles <- unique(construction_object$data[[construction_object$lifecycle_id]])

    allowed_lifecycle <- c("schedule","assign","reassign","start","suspend","resume","abort_activity","abort_case","complete","manualskip","autoskip")

    incorrect_lifecycles <- lifecycles[!(lifecycles %in% allowed_lifecycle)]

    ui <- miniPage(
        gadgetTitleBar("Recode lifecycle column", right = miniTitleBarButton("done","Next", TRUE)),
        miniContentPanel(
            uiOutput("recode"),
            tableOutput("data"),
            actionButton("previous", "Previous")
        )
    )


    server <- function(input, output, session){

        output$recode <- renderUI({
            map(1:length(incorrect_lifecycles), function(i) {
                selectInput(inputId = paste0("recode", i), label = paste0("Lifecycle for ", incorrect_lifecycles[i]),
                            choices = allowed_lifecycle)
            })
        })

        observeEvent(input$previous, {
            construction_object$page = "Previous"
            .construction_object <<- construction_object
            stopApp()
        })

        observeEvent(input$done, {

            construction_object$page = "Next"

            for(i in 1:length(incorrect_lifecycles)) {

                construction_object$data[[construction_object$lifecycle_id]] <- as.character(construction_object$data[[construction_object$lifecycle_id]])
                construction_object$data[[construction_object$lifecycle_id]] [ construction_object$data[[construction_object$lifecycle_id]] == incorrect_lifecycles[i] ] <- input[[paste0("recode", i)]]

            }

            .construction_object <<- construction_object

            # rstudioapi::sendToConsole("select_activity_instance(.construction_object)")

            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 800))

    if(.construction_object$page == "Next") {
        rstudioapi::sendToConsole("select_activity_instance(.construction_object)")
    }
    else {
        rstudioapi::sendToConsole(glue::glue("select_lifecycle(.construction_object)"))
    }

}
