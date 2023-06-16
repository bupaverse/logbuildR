



compile_script <- function(CO, constructor) {


if(!is.null(CO$timestamps_to_prepare))  {
    timestamp_conversion <- paste0("\n\t", glue::glue("convert_timestamps(columns = c(\"{paste(CO$timestamps_to_prepare, collapse = '\", \"')}\"), format = {CO$timestamps_format}) %>%"))
    # timestamp_conversion <- paste0("\n\t", "convert_timestamps(columns = c(\'", paste(CO$timestamps_to_prepare, sep = "\', "), "\'), format = ", CO$timestamps_format, ") %>%")

    # timestamp_conversion <- glue::glue(paste0("convert_timestamps(columns = c(\'{paste(CO$timestamps_to_prepare, collapse = '\",\"')}\'), format = \'{CO$timestamps_format}\') %>%"))
} else {
    timestamp_conversion <- NA
}
if(!is.null(CO$lifecycle_to_recode)) {
    lifecycle_conversion <- paste0("\n\t", "dplyr::rename(", paste0(paste(CO$lifecycle_recode_to, paste0("\'", CO$lifecycle_to_recode, "\'"), sep = " = "), collapse = ", "), ") %>%", "\n\t")
} else {
    lifecycle_conversion <- NA
}
    if(!is.null(CO$guess_activity_instance_id)) {
        activity_instance <- paste0("\n\t", "bupaR::assign_instance_id(\'", case_id = CO$case_id, "\', ", "\'", activity_id = CO$activity_id, "\', ", "\'", timestamp = CO$timestamps, "\', ", "\'", lifecycle_id = CO$lifecycle_id, "\') ", "%>%", "\n\t")
        # activity_instance <- glue::glue("\n\t bupaR::assign_instance_id('case_id = {CO$case_id}', 'activity_id = {CO$activity_id}','{timestamp = CO$timestamps}','{lifecycle_id = CO$lifecycle_id}') %>%\n\t")
    } else {
        activity_instance <- NA
    }

    if (!is.null(CO$complete_lifecycle_added)) {
        if(CO$complete_lifecycle_added) {
            lifecycle_add <- "\n\tdplyr::mutate(lifecycle_logbuildR = 'complete') %>%"
        } else {
            lifecycle_add <- NA
        }
    }
    else {
        lifecycle_add <- NA
    }

glue::glue("{CO$data_name} %>%") -> data_selection


items <- c(data_selection, timestamp_conversion, lifecycle_conversion, lifecycle_add, activity_instance, constructor)
items[!is.na(items)] %>% paste()

# paste(data_selection, timestamp_conversion, lifecycle_conversion, lifecycle_add, activity_instance, constructor , sep = "\n")

}





