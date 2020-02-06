
#' Title
#'

#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @export
build_log <- function() {

    ui <- miniPage(
        gadgetTitleBar("Select data"),
        miniContentPanel(
            uiOutput("datasets"),
            tableOutput("data")

        )
    )
    map(ls(envir = .GlobalEnv), function(x){class(get(x))}) %>%
        map(str_detect, "data.frame") %>%
        map(any) %>%
        unlist-> is_data_frame

    ls(envir = .GlobalEnv)[is_data_frame] -> datasets

    server <- function(input, output, session){

        output$data <- renderTable({
            get(input$dataset)
        })

        output$datasets <- renderUI({
            selectizeInput("dataset", label = "Select data:",
                           choices = datasets,
                           multiple = FALSE)
        })

        observeEvent(input$done, {

            stopApp()
            rstudioapi::sendToConsole(glue::glue("select_ids(({input$dataset}))"))
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction",  height = 600, width = 850))

}


