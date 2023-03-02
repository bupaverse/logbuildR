



#' Title
#'
#' @inheritParams select_log_identifiers
#'
#' @export
#'
save_log <- function(construction_object) {

    ui <- miniPage(
        # shinyjs::useShinyjs(),
        gadgetTitleBar("Save log"),
        miniContentPanel(
            # radioButtons("logtype", "Saving log as: ", choices = c("Eventlog" = "eventlog", "Activitylog" = "activitylog"),
            #              selected = ifelse(construction_object$type == "Event", "eventlog", "activitylog")),
            # selectInput("logtype", paste0("Save log as"), #, tolower(construction_object$type), " log"),
            #             choices = c("event log", "activity log"), multiple = T,
            #             selected = ifelse(construction_object$type == "Activity", "activity log", "event log")),

            textInput("objectname", paste("Save", tolower(construction_object$type), "log as:")),
            verbatimTextOutput("script"),
            actionButton("previous", "Previous")
        )
    )

    server <- function(input, output, session){

        output$script <- renderText({

            validate(need(!is.null(input$objectname), "Provide a name"))

                    if (construction_object$type == "Activity") {
                        script <- glue::glue("{input$objectname} <- activitylog(.construction_object,
                                                  case_id = '{construction_object$case_id}',
                                                  activity_id = '{construction_object$activity_id}',
                                                  resource_id = '{construction_object$resource_id}',
                                                  timestamps = c({toString(paste0('\"',construction_object$timestamps, '\"'))})
                                     )
                                     ")
                    }
                    else {
                        script <- glue::glue("{input$objectname} <- eventlog(.construction_object,
                                               case_id = '{construction_object$case_id}',
                                               activity_id = '{construction_object$activity_id}',
                                               activity_instance_id = '{construction_object$activity_instance_id}',
                                               lifecycle_id = '{construction_object$lifecycle_id}',
                                               timestamp = '{construction_object$timestamps}',
                                               resource_id = '{construction_object$resource_id}'
                                     )
                                     ")
                    }

        # if(input$type == "activitylog" & construction_object$type == "Activity") {
        #       script <- glue::glue("activitylog(
                                   # case_id = '{construction_object$case_id}',
                                   # activity_id = '{construction_object$activity_id}',
                                   # resource_id = '{construction_object$resource_id}',
                                   # lifecycle_ids = '{construction_object$timestamps}')")

        # } else if(input$type == "eventlog" & construction_object$type == "Event") {
        #            script <- glue::glue("eventlog(
        #                         case_id = '{construction_object$case_id}',
        #                         activity_id = '{construction_object$activity_id}',
        #                         timestamp = '{construction_object$timestamps}',
        #                         resource_id = '{construction_object$resource_id}',
        #                         lifecycle_id = '{construction_object$lifecycle_id}',
        #                         activity_instance_id = '{construction_object$activity_instance_id}')")
        #
        # } else if(input$type == "eventlog" & construction_object$type == "Activity") {
        #     script <- glue::glue("activities_to_eventlog(
        #                                       case_id = '{construction_object$case_id}',
        #                                       activity_id = '{construction_object$activity_id}',
        #                                       resource_id = '{construction_object$resource_id}',
        #                                       timestamps = '{construction_object$timestamps}')")

        # } else if(input$type == "activitylog" & construction_object$type == "Event") {
        #     script <- glue::glue("events_to_activitylog(
        #                                      case_id = '{construction_object$case_id}',
        #                                      activity_id = '{construction_object$activity_id}',
        #                                      timestamp = '{construction_object$timestamps}',
        #                                      resource_id = '{construction_object$resource_id}',
        #                                      lifecycle_id = '{construction_object$lifecycle_id}',
        #                                      activity_instance_id = '{construction_object$activity_instance_id}')")
        # }


            return(script)
            # return(compile_script(construction_object, script))
        })

        observeEvent(input$previous, {
            construction_object$page = "Previous"
            .construction_object <<- construction_object
            stopApp()
        })

        observeEvent(input$done, {
            .construction_object$page <<- "Next"
            stopApp()
        })

        # observeEvent(input$done, {
        #     .construction_object$page <<- "Next"
        #     # .construction_object <<- construction_object
        #
        #
        #     if (construction_object$type == "Activity") {
        #         log <- activitylog(construction_object$data,
        #                            case_id = construction_object$case_id,
        #                            activity_id = construction_object$activity_id,
        #                            resource_id = construction_object$resource_id,
        #                            timestamps = construction_object$timestamps)
        #
        #         if (length(input$logtype) > 1) {
        #             log %>% to_eventlog() -> log2
        #         }
        #
        #         else if (input$logtype == "Event") {
        #             log %>% to_eventlog() -> log
        #         }
        #
        #     } else if(construction_object$type == "Event") {
        #
        #         log <- eventlog(construction_object$data,
        #                         case_id = construction_object$case_id,
        #                         activity_id = construction_object$activity_id,
        #                         lifecycle_id = construction_object$lifecycle_id,
        #                         activity_instance_id = construction_object$activity_instance_id,
        #                         timestamp = construction_object$timestamps,
        #                         resource_id = construction_object$resource_id)
        #
        #         if (length(input$logtype) > 1) {
        #             log %>% to_activitylog() -> log2
        #         }
        #
        #         else if (input$logtype == "Activity") {
        #             log %>% to_activitylog() -> log
        #         }
        #     }


        # observeEvent(input$done, {
        #     if(input$type == "activitylog" & construction_object$type == "Activity") {
              #   log <- activitylog(construction_object$data,
              #                      case_id = construction_object$case_id,
              #                      activity_id = construction_object$activity_id,
              #                      resource_id = construction_object$resource_id,
              #                      lifecycle_ids = construction_object$timestamps)
              # } else if(input$type == "eventlog" & construction_object$type == "Event") {
              #   log <- eventlog(construction_object$data,
              #                   case_id = construction_object$case_id,
              #                   activity_id = construction_object$activity_id,
              #                   timestamp = construction_object$timestamps,
              #                   resource_id = construction_object$resource_id,
              #                   lifecycle_id = construction_object$lifecycle_id,
              #                   activity_instance_id = construction_object$activity_instance_id)
        #
        #     } else if(input$type == "eventlog" & construction_object$type == "Activity") {
        #         log <- activities_to_eventlog(construction_object$data,
        #                                       case_id = construction_object$case_id,
        #                                       activity_id = construction_object$activity_id,
        #                                       resource_id = construction_object$resource_id,
        #                                       timestamps = construction_object$timestamps)
        #
        #     } else if(input$type == "activitylog" & construction_object$type == "Event") {
        #         log <- events_to_activitylog(construction_object$data,
        #                                      case_id = construction_object$case_id,
        #                                      activity_id = construction_object$activity_id,
        #                                      timestamp = construction_object$timestamps,
        #                                      resource_id = construction_object$resource_id,
        #                                      lifecycle_id = construction_object$lifecycle_id,
        #                                      activity_instance_id = construction_object$activity_instance_id)
        #
        #
        # }

        #     if (length(input$logtype) > 1) {
        #         assign(input$objectname, list(log, log2), envir = .GlobalEnv)
        #         stopApp()
        #     }
        #     else {
                # assign(input$objectname, log, envir = .GlobalEnv)
                # stopApp()
        #     }
        # })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 800))

    if(.construction_object$page == "Previous") {
        if (.construction_object$type == "Event") {
            rstudioapi::sendToConsole(glue::glue("select_activity_instance(.construction_object)"))
        }
        else {
            rstudioapi::sendToConsole(glue::glue("select_timestamps(.construction_object, single = F)"))
        }
    }
}
