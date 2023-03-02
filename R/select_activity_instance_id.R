



#' Title
#'
#' @inheritParams select_log_identifiers
#'
#' @export
#'
select_activity_instance <- function(construction_object) {
    ui <- miniPage(
        gadgetTitleBar("Select activity instance id", right = miniTitleBarButton("done","Next", TRUE)),
        miniContentPanel(
            radioButtons("is_available", "Activity instance id available?", choices = c("No, guess activity instance id" = "no",
                                                                                    "Yes, activity instance id column is available" = "yes"), selected = "no"),
            uiOutput("selection"),
            textOutput("checks"),
            verbatimTextOutput("data"),
            actionButton("previous", "Previous")
        )
    )

    server <- function(input, output, session){

        output$data <- renderPrint(construction_object$data %>% glimpse())

        output$selection <- renderUI({
            if(input$is_available == "yes"){
                selectizeInput("selected_column", "Select activity instance id column", choices = names(construction_object$data))
            }
        })

        output$checks <- reactive({
            if(input$is_available == "yes") {
                validate(
                    need(!input$selected_column %in% c(construction_object$case_id,
                                                       construction_object$activity_id,
                                                       construction_object$timestamps,
                                                       construction_object$resource_id,
                                                       construction_object$lifecycle_id),
                         message = paste0(input$selected_column, " has already been assigned")
                    )
                )
            }
        })

        observeEvent(input$previous, {
            construction_object$page = "Previous"
            .construction_object <<- construction_object
            stopApp()
        })

        observeEvent(input$done, {

            shinybusy::show_modal_spinner(spin = "circle")
            # showModal(modalDialog("Assigning instance id\'s", footer=NULL))

            construction_object$page = "Next"

            if(input$is_available == "yes") {
                construction_object$activity_instance_id <- input$selected_column
                construction_object$guess_activity_instance_id <- F

                # rstudioapi::sendToConsole(glue::glue("save_log(.construction_object)"))
             } else {

                construction_object$data <- assign_instance_id(construction_object$data,
                                                               construction_object$case_id,
                                                               construction_object$activity_id,
                                                               construction_object$timestamps,
                                                               construction_object$lifecycle_id)
                construction_object$guess_activity_instance_id <- T
                construction_object$activity_instance_id <- "activity_instance_logbuildR"

                # rstudioapi::sendToConsole(glue::glue("save_log(.construction_object)"))
            }

            .construction_object <<- construction_object

            shinybusy::remove_modal_spinner()
            # removeModal()

            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))

    if(.construction_object$page == "Next") {
        rstudioapi::sendToConsole(glue::glue("save_log(.construction_object)"))
    }
    else {
        rstudioapi::sendToConsole(glue::glue("select_lifecycle(.construction_object)"))
    }



}
