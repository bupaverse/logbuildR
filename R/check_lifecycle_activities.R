

#' Title
#'
#' @inheritParams select_log_identifiers
#'
#' @importFrom bupaR activitylog
#' @export
#'
check_lifecycle_activities <- function(construction_object) {

    lifecycles <- construction_object$timestamps

    allowed_lifecycle <- c("schedule","assign","reassign","start","suspend","resume","abort_activity","abort_case","complete","manualskip","autoskip")

    if(all(lifecycles %in%allowed_lifecycle)) {
        save_log(construction_object)
    } else {
        recode_lifecycles_activities(construction_object)
    }



}
