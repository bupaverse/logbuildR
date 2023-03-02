

#' Title
#'
#' @inheritParams select_log_identifiers
#'
#' @importFrom bupaR activitylog
#' @export
#'
check_lifecycle_events <- function(construction_object) {

    lifecycles <- unique(construction_object$data[[construction_object$lifecycle_id]])

    allowed_lifecycle <- c("schedule","assign","reassign","start","suspend","resume","abort_activity","abort_case","complete","manualskip","autoskip")

    if(all(lifecycles %in% allowed_lifecycle)) {
        select_activity_instance(construction_object)
    } else {
        recode_lifecycles_events(construction_object)
    }



}
