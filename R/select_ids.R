
#' Title
#'
#' @param data
#'
#' @export
#'
select_ids <- function(data) {

    ui <- miniPage(
        gadgetTitleBar("Select case id"),
        miniContentPanel(
            fluidRow(
                column(width = 4, selectInput("case_id", label = "Case identifier(s):", choices = names(data), multiple = T)),
                column(width = 4, selectInput("activity_id", label = "Activity identifier(s):", choices = names(data), multiple = T)),
                column(width = 4, selectInput("resource_id", label = "Resource identifier(s):", choices = names(data), multiple = T))
            ),
            dataTableOutput("data")
        )
    )


    server <- function(input, output, session){

        output$data <- renderDataTable(data)

        observeEvent(input$done, {
            .construction_object <<- list(data = data, case_id = input$case_id,
                                          activity_id = input$activity_id,
                                          resource_id = input$resource_id)
            rstudioapi::sendToConsole(glue::glue("decide_type(.construction_object)"))
            stopApp()
        })
    }
    runGadget(ui, server, viewer = dialogViewer("Event log construction", height = 600, width = 850))

}
