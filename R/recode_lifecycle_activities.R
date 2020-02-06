



#' Title
#'
#' @param construction_object
#'
#' @export
#'
recode_lifecycles_activities <- function(construction_object) {


    lifecycles <- construction_object$timestamps

    allowed_lifecycle <- c("schedule","assign","reassign","start","suspend","resume","abort_activity","abort_case","complete","manualskip","autoskip")

    incorrect_lifecycles <- lifecycles[!(lifecycles %in% allowed_lifecycle)]

    ui <- miniPage(
        gadgetTitleBar("Recode timestamp columns"),
        miniContentPanel(
            uiOutput("recode")
            )
    )


    server <- function(input, output, session){

        output$recode <- renderUI({
            map(1:length(incorrect_lifecycles), function(i) {
                selectInput(inputId = paste0("recode", i), label = paste0("Lifecycle for ", incorrect_lifecycles[i]),
                            choices = allowed_lifecycle)
            })
        })

        observeEvent(input$done, {

            for(i in 1:length(incorrect_lifecycles)) {
                colnames(construction_object$data)[colnames(construction_object$data) == incorrect_lifecycles[i]] <- input[[paste0("recode", i)]]
                construction_object$timestamps[construction_object$timestamps == incorrect_lifecycles[i]] <- input[[paste0("recode", i)]]
            }

            .construction_object <<- construction_object

            rstudioapi::sendToConsole("save_log(.construction_object)")

            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 800))

}
