
#' @rdname prolific_study
#'
#' @name prolific_study
#'
#' @section Methods:
#'
#' ## \code{validity_check}
#' Check whether the study is valid in terms of the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}.\cr
#' \strong{Note:} For checking the validity of the \code{\link[=prolific_study]{eligibility_requirements}},
#' an \code{\link[=api_access]{api_access object}} that passes \code{\link[=api_access]{check_authorization()}} needs to be available.
#' It suffices if any such \code{\link[=api_access]{api_access object}} is specified, since the reference to it is determined automatically.
#'
#' ### \strong{Return Value}
#' \itemize{
#'  \item{If the study is valid: }{A \code{\link[=logical]{logical}} value indicating that the study is valid}
#'  \item{If the study is not valid: }{A \code{\link[=character]{character}} vector that lists the studie's issues.}
#' }
#'
#' ### \strong{Usage}
#' \preformatted{
#'      prolific_study$validity_check()
#' }

prolific_study$methods(
    show = function() {
        # "
        #     Show the main contents of a prolific_study to the user.
        # "

        # Define the fields that are printed
        output_fields <- c(
            "name",
            "internal_name",
            "id",
            "project",
            "external_study_url",
            "total_available_places",
            "reward"
        )

        nchars <- max(nchar(output_fields))

        sep <- paste0(rep("=", getOption("width")), collapse = "")

        cat(paste0(sep, "\n", "Prolific study summary:", "\n", sep, "\n"))

        for (output_field in output_fields) {
            cat(
                paste0(
                    output_field,
                    ":",
                    paste0(
                        rep(
                            " ",
                            max(nchars + 4 - nchar(output_field))
                        ),
                        collapse = ""
                    ),
                    get(output_field, envir = .self),
                    "\n"
                )
            )
        }
        cat(paste0(sep, "\n"))
    },
    validity_check = function() {
        # "
        #     Check whether the study is valid in terms of the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}
        # "

        check_msg <- NULL
        # Check URL parameters that are required when identifying respondents via URL parameters
        if (prolific_id_option == "url_parameters") {
            if (!any(vapply(url_parameters, function(x) any(grepl("\\{%PROLIFIC_PID%\\}", x)), TRUE))) {
                check_msg <- c(check_msg, "Prolific_id_option = \"url_parameters\" requires that \"{%PROLIFIC_PID%}\" is passed as an URL parameter.")
            }
        }

        # Check that required fields are neither empty nor null
        for (i in c("completion_code", "completion_option", "description", "device_compatibility", "external_study_url", "name", "prolific_id_option")) {
            if (all(is.null(.self[[i]]))) {
                check_msg <- c(check_msg, paste0("Field '", i, "' should not be empty."))
            } else if (all(.self[[i]] == "")) {
                check_msg <- c(check_msg, paste0("Field '", i, "' should not be empty."))
            }
        }

        # Check that required fields do only have allowed values
        if (!all(device_compatibility %in% (allowed_vals <- eval(formals(.self$initialize)$device_compatibility)))) {
            check_msg <- c(check_msg, paste0("Field 'device_compatibility' should contain only values from c(", paste0("\"", allowed_vals, "\"", collapse = ", "), ")."))
        }

        if (!all(is.null(peripheral_requirements))) {
            if (!all(peripheral_requirements %in% (allowed_vals <- c("audio", "camera", "download", "microphone")))) {
                check_msg <- c(check_msg, paste0("Field 'peripheral_requirements' should contain only values from c(", paste0("\"", allowed_vals, "\"", collapse = ", "), ")."))
            }
        }

        if (!all(prolific_id_option %in% (allowed_vals <- c("url_parameters", "question", "not_required")))) {
            check_msg <- c(check_msg, paste0("Field 'prolific_id_option' should be one of c(", paste0("\"", allowed_vals, "\"", collapse = ", "), ")."))
        }

        if (total_available_places < 1) {
            check_msg <- c(check_msg, paste0("Field 'total_available_places' should be >= 1."))
        }

        if (!(naivety_distribution_rate <= 1 && naivety_distribution_rate >= 0)) {
            check_msg <- c(check_msg, paste0("Field 'naivety_distribution_rate' should be between 0 and 1."))
        }

        # Check eligibility_requirements
        if (length(eligibility_requirements) > 0) {
            eligibility_checks <- lapply(eligibility_requirements, function(x) {
                return(x$validity_check())
            })
            add_pos <- which(!vapply(eligibility_checks, function(x) {
                if (length(x) == 1) {
                    if (Reduce(c, x) == TRUE) {
                        return(TRUE)
                    } else {
                        return(FALSE)
                    }
                } else {
                    return(FALSE)
                }
            }, TRUE))

            if (length(add_pos) > 0) {
                add_msgs <- lapply(eligibility_checks[add_pos], function(x) {
                    gsub("\\t", "", gsub("\\s\\s+", " ", Reduce(c, strsplit(x$message, "\n"))))
                })

                check_msg <- c(check_msg, Reduce(c, add_msgs))
            }
        }

        if (!all(is.null(check_msg))) {
            return(c(
                "Found the following issues:",
                paste0("  ", check_msg)
            ))
        } else {
            return(TRUE)
        }
    }
)
