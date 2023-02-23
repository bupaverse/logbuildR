
#' Title
#'

#' @importFrom purrr map
#' @importFrom rlang inherits_any
#' @export
build_log2 <- function() {

    ui <- fluidPage(

        titlePanel("logbuildR"),

        sidebarLayout(

            sidebarPanel(
                uiOutput("datasets")
            ),

            mainPanel(
                tabsetPanel(
                    tabPanel("Dataset", verbatimTextOutput("data")),
                    tabPanel("tab2", verbatimTextOutput("")),
                    tabPanel("tab3", tableOutput(""))
                )
            )
        )
    )

    # ui <- miniPage(
    #     gadgetTitleBar("Select data",right = miniTitleBarButton("done","Select data", TRUE)),
    #     miniContentPanel(
    #         uiOutput("datasets"),
    #         verbatimTextOutput("data")
    #
    #     )
    # )
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

        output$data <- renderPrint({
            if(is.null(input$dataset)) {
                tibble()
            } else {
                glimpse(get(input$dataset))
            }
        })

        observeEvent(input$done, {
            .construction_object <<- list(data = get(input$dataset), data_name = input$dataset)
            stopApp()
            rstudioapi::sendToConsole(glue::glue("select_ids(.construction_object)"))
        })
    }
    suppressWarnings(suppressMessages(runGadget(ui, server, viewer = dialogViewer("Event log construction",  height = 600, width = 850))))

}
