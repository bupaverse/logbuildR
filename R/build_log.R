
#' Title
#'

#' @importFrom purrr map
#' @importFrom rlang inherits_any
#' @export
build_log <- function() {

    # ui <- miniPage(
    #     gadgetTitleBar("Select data",right = miniTitleBarButton("done","Next", TRUE)),
    #     miniContentPanel(
    #         uiOutput("datasets"),
    #         verbatimTextOutput("dataPrint"),
    #         DT::DTOutput("dataTable")
    #     )
    # )

    ui <- miniPage(
        gadgetTitleBar("Select data",right = miniTitleBarButton("done","Next", TRUE)),
        miniContentPanel(
            tabsetPanel(
                tabPanel("Dataset",
                         uiOutput("datasets"),
                         verbatimTextOutput("dataPrint")),
                tabPanel("View",
                         DT::DTOutput("dataView"))
            )
        )
    )




    # map(ls(envir = .GlobalEnv), function(x){class(get(x))}) %>%
        # map(str_detect, "data.frame") %>%
        # map(any) %>%
        # unlist-> is_data_frame

    map(ls(envir = .GlobalEnv), ~rlang::inherits_any(get(.x), class = c("data.frame", "tbl_df", "tbl"))) %>%
        unlist -> is_data_frame


    ls(envir = .GlobalEnv)[is_data_frame] -> datasets

    server <- function(input, output, session){

        output$datasets <- renderUI({
            selectizeInput("dataset", label = "Select data:",
                           choices = datasets,
                           multiple = FALSE)
        })

        output$dataPrint <- renderPrint({
            if(is.null(input$dataset)) {
                tibble()
            } else {
                glimpse(get(input$dataset))
            }
        })

         output$dataView <- DT::renderDataTable({
             if(is.null(input$dataset)) {
                 tibble()
             } else {
                 DT::datatable(get(input$dataset))
             }
         })

        observeEvent(input$done, {
            .construction_object <<- list(data = get(input$dataset), data_name = input$dataset)
            stopApp()
        })
    }
    suppressWarnings(suppressMessages(runGadget(ui, server, viewer = dialogViewer("Event log construction",  height = 600, width = 850))))
    rstudioapi::sendToConsole(glue::glue("select_ids(.construction_object)"))
}


