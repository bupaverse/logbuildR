

#' Title
#'
#' @param construction_object
#'

#' @export

check_timestamps <- function(construction_object) {

    timestamps <- construction_object$timestamps

    timestamp_data <- select(construction_object$data, timestamps)

    are_timestamps <- unlist(map(map(timestamp_data, class), ~any(.x %in% c("POSIXct","Date"))))

    if(!all(are_timestamps)) {
        prepare_timestamps(construction_object)
    } else {
        if(construction_object$type == "Activity") {
            check_lifecycle_activities(construction_object)
        } else {
            select_lifecycle(construction_object)
        }
    }


}
