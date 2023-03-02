



#' Title
#'
#' @inheritParams select_log_identifiers
#'
#' @export
#'
select_lifecycle <- function(construction_object) {
    ui <- miniPage(
        gadgetTitleBar("Select lifecycle", right = miniTitleBarButton("done","Next", TRUE)),
        miniContentPanel(
            radioButtons("is_available", "Lifecycle column available?", choices = c("No, set lifecycle to 'complete' for all events" = "no",
                                                                                    "Yes, lifecycle column is available" = "yes"), selected = "no"),
            uiOutput("selection"),
            textOutput("checks"),
            verbatimTextOutput("data"),
            actionButton("previous", "Previous")
        )
    )


    server <- function(input, output, session){

        output$data <- renderPrint(construction_object$data %>% glimpse())

        # available_columns <- reactive({
        #     choices <- names(construction_object$data)
        #
        #     validate(
        #         need(try(!choices %in% c(construction_object$case_id,
        #                                            construction_object$activity_id,
        #                                            construction_object$timestamps,
        #                                            construction_object$resource_id,
        #                                            construction_object$activity_instance_id)),
        #              message = paste0(choices, " has already been assigned")
        #         )
        #     )
        #     choices
        # })

        output$selection <- renderUI({
            if(input$is_available == "yes") {
                selectizeInput("selected_column", "Select lifecycle column", choices = names(construction_object$data), selected = NULL)
            }
        })

        output$checks <- reactive({
            if(input$is_available == "yes") {
                validate(
                    need(!input$selected_column %in% c(construction_object$case_id,
                                                       construction_object$activity_id,
                                                       construction_object$timestamps,
                                                       construction_object$resource_id,
                                                       construction_object$activity_instance_id),
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

            construction_object$page = "Next"

            if(input$is_available == "yes") {
                construction_object$lifecycle_id <- input$selected_column
                construction_object$complete_lifecycle_added <- FALSE

            } else {
                construction_object$data <- construction_object$data %>%
                    mutate(lifecycle_logbuildR = "complete")
                construction_object$complete_lifecycle_added <- TRUE
                construction_object$lifecycle_id <- "lifecycle_logbuildR"
            }

            .construction_object <<- construction_object

            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))

    if(.construction_object$page == "Next") {
        if(!.construction_object$complete_lifecycle_added) {
            rstudioapi::sendToConsole(glue::glue("check_lifecycle_events(.construction_object)"))
        }

        else {
            rstudioapi::sendToConsole(glue::glue("select_activity_instance(.construction_object)"))
        }
    }

    else {
        if (.construction_object$type == "Event") {
            rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = TRUE)"))
        }
        else {
            rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = FALSE)"))

        }
    }

}
