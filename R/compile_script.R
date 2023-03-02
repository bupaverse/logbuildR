



compile_script <- function(CO, constructor) {


if(!is.null(CO$timestamps_to_prepare))  {
   timestamp_conversion <- glue::glue("convert_timestamps(columns = c(\"{paste(CO$timestamps_to_prepare, collapse = '\",\"')}\"), format = {CO$timestamps_format}) %>%\n")
} else {
    timestamp_conversion <- ""
}
if(!is.null(CO$lifecycle_to_recode)) {
    lifecycle_conversion <- paste0("rename(", paste0(paste(CO$lifecycle_recode_to, CO$lifecycle_to_recode, sep = " = "), collapse = ", "), ") %>%\n")
} else {
    lifecycle_conversion <- ""
}
    if(!is.null(CO$guess_activity_instance_id)) {
        activity_instance <- glue::glue("logbuildR::assign_instance_id(   '{CO$case_id}',\n\t          '{CO$activity_id}',
                                                     \t          '{CO$timestamps}',
                                                     \t          '{CO$lifecycle_id}') %>%\n")
    } else {
        activity_instance <- ""
    }

if(!is.null(CO$complete_lifecycle_added)) {
    lifecycle_add <- "mutate(lifecycle_logbuildR = 'complete') %>%\n"
} else {
    lifecycle_add <- ""
}

glue::glue("{CO$data_name} %>%") -> data_selection


paste(data_selection, timestamp_conversion, lifecycle_conversion, lifecycle_add, activity_instance, constructor , sep = "\t")

}





