
# ======================== > .output_prolific_study < ======================== #
.output_prolific_study <-
    function(prolific_study,
             prescreeners = NULL) {
        # Perform checks whether the study is well-formated (i.e. all required information is set) for Prolific
        if (!all((check <- prolific_study$validity_check()) == TRUE)) {
            stop(paste0("\n\r", paste0("\t", check, collapse = "\n")))
        }

        url_parameters <- if (length(prolific_study$url_parameters) > 0) {
            paste0(
                "?",
                .collapse_url_parameters(prolific_study$url_parameters)
            )
        } else {
            NULL
        }

        if (length(prolific_study$eligibility_requirements) > 0 && is.null(prescreeners)) {
            stop(paste0("length(eligibility_requirements) = ", length(prolific_study$eligibility_requirements), " - argument 'prescreeners' must be specified!."))
        }

        return(
            list(
                completion_code = prolific_study$completion_code,
                completion_option = prolific_study$completion_option,
                description = prolific_study$description,
                device_compatibility = prolific_study$device_compatibility,
                eligibility_requirements =
                    unname(lapply(prolific_study$eligibility_requirements, function(requirement) {
                        if (class(requirement) %in% c("prolific_prescreener")) {
                            requirement$.internals$methods$output(prescreeners)
                        } else {
                            stop(paste0("Trying to pass prescreeners of class '", class(requirement), "' failed.\n\tPlease use class 'prolific_prescreener' only.\n"))
                        }
                    })),
                estimated_completion_time = prolific_study$estimated_completion_time,
                external_study_url = paste0(
                    prolific_study$external_study_url,
                    url_parameters
                ),
                internal_name = prolific_study$internal_name,
                maximum_allowed_time = prolific_study$maximum_allowed_time,
                naivety_distribution_rate = prolific_study$naivety_distribution_rate,
                name = prolific_study$name,
                peripheral_requirements = prolific_study$peripheral_requirements,
                project = prolific_study$project,
                prolific_id_option = prolific_study$prolific_id_option,
                reward = prolific_study$reward,
                total_available_places = prolific_study$total_available_places
            )
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
