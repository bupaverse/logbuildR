
#' Title
#'
#' @param data
#'
#' @export
#'
select_ids <- function(construction_object) {

    ui <- miniPage(
        gadgetTitleBar("Select case, activity and resource identifier", right = miniTitleBarButton("done","Next", TRUE)),
        miniContentPanel(
            h4("Select"),
            (fluidRow(
                column(width = 4,
                       selectizeInput("case_id", label = "Case identifier(s):", choices = names(construction_object$data), multiple = T,
                                      options = list(placeholder = "case id(s)"))                       ),
                column(width = 4,
                       selectizeInput("activity_id", label = "Activity identifier(s):", choices = names(construction_object$data), multiple = T,
                                      options = list(placeholder = "activity id(s)"))
                      ),
                column(width = 4,
                       selectizeInput("resource_id", label = "Resource identifier(s):", choices = names(construction_object$data), multiple = T,
                                      options = list(placeholder = "resource id(s)"))
                       )
            )),
            wellPanel(fluidRow(
                column(width = 4,
                       textOutput("case_id_info"),
                       textOutput("case_id_info2")
                       ),
                column(width = 4,
                       textOutput("activity_id_info"),
                       textOutput("activity_id_info2")
                       ),
                column(width = 4,
                       textOutput("resource_id_info"),
                       textOutput("resource_id_info2")
                       )
            )),
            textOutput("checks"),
            h4("Data preview"),
            verbatimTextOutput("data"),
            actionButton("previous", "Previous")

        )
    )


    server <- function(input, output, session){

        output$data <- renderPrint(construction_object$data %>% glimpse())

        output$case_id_info <- renderText({
            if(is.null(input$case_id)) {
                ""
            } else {
            case_ids <- construction_object$data[input$case_id] %>%
                as.data.frame() %>%
                unique() %>%
                unite(case_id, 1:ncol(.))


            glue::glue("Number of unique values: {nrow(unique(case_ids))}")
            }
        }, sep = "\n")

        output$case_id_info2 <- renderText({
            if(is.null(input$case_id)) {
                ""
            } else {
                case_ids <- construction_object$data[input$case_id] %>%
                    as.data.frame() %>%
                    unique() %>%
                    unite(case_id, 1:ncol(.))

                case_ids %>%
                    slice(1:3) %>%
                    pull(case_id) %>%
                    str_c(collapse = ", ") %>%
                    str_sub(1, 20) -> case_id_preview

                   glue::glue("{case_id_preview}...")
            }
        }, sep = "\n")




        output$activity_id_info <- renderText({
            if(is.null(input$activity_id)) {
                ""
            } else {
                activity_ids <- construction_object$data[input$activity_id] %>%
                    as.data.frame() %>%
                    unique() %>%
                    unite(activity_id, 1:ncol(.))



                glue::glue("Number of unique values: {nrow(unique(activity_ids))}")
            }
        }, sep = "\n")

        output$activity_id_info2 <- renderText({
            if(is.null(input$activity_id)) {
                ""
            } else {
                activity_ids <- construction_object$data[input$activity_id] %>%
                    as.data.frame() %>%
                    unique() %>%
                    unite(activity_id, 1:ncol(.))

                activity_ids %>%
                    slice(1:3) %>%
                    pull(activity_id) %>%
                    str_c(collapse = ", ") %>%
                    str_sub(1, 20) -> activity_id_preview

                   glue::glue("{activity_id_preview}...")
            }
        }, sep = "\n")


        output$resource_id_info <- renderText({
            if(is.null(input$resource_id)) {
                ""
            } else {
                resource_ids <- construction_object$data[input$resource_id] %>%
                    as.data.frame() %>%
                    unique() %>%
                    unite(resource_id, 1:ncol(.))

            glue::glue("Number of unique values: {nrow(unique(resource_ids))}")
            }
        }, sep = "\n")


        output$resource_id_info2 <- renderText({
            if(is.null(input$resource_id)) {
                ""
            } else {
                resource_ids <- construction_object$data[input$resource_id] %>%
                    as.data.frame() %>%
                    unique() %>%
                    unite(resource_id, 1:ncol(.))

                resource_ids %>%
                    slice(1:3) %>%
                    pull(resource_id) %>%
                    str_c(collapse = ", ") %>%
                    str_sub(1, 20) -> resource_id_preview

                   glue::glue("{resource_id_preview}...")
            }
        }, sep = "\n")



        output$checks <- reactive({
            validate(
                need(!is.null(input$case_id), "No case identifier(s) selected"),
                need(!is.null(input$activity_id), "No activity identifier(s) selected"),
                need(!is.null(input$resource_id), "No resource identifier(s) selected"), #,
                # need(!input$activity_id %in% input$case_id, "Case identifier should be different from activity identifier"),
                need(length(intersect(input$activity_id, input$case_id)) == 0, "Case identifier should be different from activity identifier"),
                need(length(intersect(input$activity_id, input$resource_id)) == 0, "Resource identifier should be different from activity identifier"),
                need(length(intersect(input$resource_id, input$case_id)) == 0, "Case identifier should be different from resource identifier")
            )
        })

        # output$checks <- renderText({
        #     if(is.null(input$case_id)) {
        #         stop("No case identifier selected")
        #     } else if(is.null(input$activity_id)) {
        #         stop("No activity identifier selected")
        #     } else if(is.null(input$resource_id)) {
        #         stop("No resource_id identifier selected")
        #     } else if(input$activity_id == input$case_id) {
        #         stop("Case identifier should be different from activity identifier")
        #     } else if(input$resource_id == input$case_id) {
        #         stop("Case identifier should be different from resource identifier")
        #     } else if(input$activity_id == input$resource_id) {
        #         stop("Resource identifier should be different from activity identifier")
        #     }
        # })

        observeEvent(input$previous, {
            construction_object$page = "Previous"
            .construction_object <<- construction_object
            stopApp()
        })

        observeEvent(input$done, {

            construction_object$case_id = input$case_id
            construction_object$activity_id = input$activity_id
            construction_object$resource_id = input$resource_id

            construction_object$page = "Next"

            .construction_object <<- construction_object

            # rstudioapi::sendToConsole(glue::glue("decide_type(.construction_object)"))
            stopApp()
        })
    }

    # suppressWarnings(suppressMessages(runGadget(ui, server, viewer = dialogViewer("Event log construction",  height = 600, width = 1000))))
    # rstudioapi::sendToConsole(glue::glue("decide_type(.construction_object)"))

    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 800, width = 1000))

    if(.construction_object$page == "Next") {
        rstudioapi::sendToConsole(glue::glue("decide_type(.construction_object)"))
    }
    else {
        rstudioapi::sendToConsole(glue::glue("build_log()"))
    }
}
