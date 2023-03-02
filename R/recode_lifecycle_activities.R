



#' Title
#'
#' @inheritParams select_log_identifiers
#'
#' @export
#'
recode_lifecycles_activities <- function(construction_object) {


    lifecycles <- construction_object$timestamps

    allowed_lifecycle <- c("schedule","assign","reassign","start","suspend","resume","abort_activity","abort_case","complete","manualskip","autoskip")

    incorrect_lifecycles <- lifecycles[!(lifecycles %in% allowed_lifecycle)]

    ui <- miniPage(
        gadgetTitleBar("Recode timestamp columns", right = miniTitleBarButton("done","Next", TRUE)),
        miniContentPanel(
            uiOutput("recode"),
            actionButton("previous", "Previous")
            )
    )


    server <- function(input, output, session){

        output$recode <- renderUI({
            map(1:length(incorrect_lifecycles), function(i) {
                selectizeInput(inputId = paste0("recode", i), label = paste0("Lifecycle for ", incorrect_lifecycles[i]),
                            choices = allowed_lifecycle)
            })
        })

        observeEvent(input$previous, {
            construction_object$page = "Previous"
            .construction_object <<- construction_object
            stopApp()
        })

        observeEvent(input$done, {

            for(i in 1:length(incorrect_lifecycles)) {
                colnames(construction_object$data)[colnames(construction_object$data) == incorrect_lifecycles[i]] <- input[[paste0("recode", i)]]
                construction_object$timestamps[construction_object$timestamps == incorrect_lifecycles[i]] <- input[[paste0("recode", i)]]
                construction_object$lifecycle_to_recode[i] <- incorrect_lifecycles[i]
                construction_object$lifecycle_recode_to[i] <- input[[paste0("recode",i)]]

            }



            .construction_object <<- construction_object
            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 800))

    if (.construction_object$page == "Next") {
        rstudioapi::sendToConsole("save_log(.construction_object)")
    }
    else {
        rstudioapi::sendToConsole("select_timestamps(.construction_object, single = F)")

    }
}
