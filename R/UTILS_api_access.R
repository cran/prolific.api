#' Function to obtain the list of available prescreeners from the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}
#'
#' @param .self
#' \strong{api_access object}:\cr
#' An object that allows to access the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}
#' @param update
#' \code{\link[=logical]{logical}} value:\cr
#' Whether the \href{https://docs.prolific.co/docs/api-docs/public/#tag/Requirements/paths/~1api~1v1~1eligibility-requirements~1/get}{list of prescreeners}
#' should be updated from the \href{https://docs.prolific.co/docs/api-docs/public/#tag/Requirements/paths/~1api~1v1~1eligibility-requirements~1/get}{Prolific API}.
#'
#' @return The list of available prescreeners as \code{\link[=data.table:data.table]{data.table}}
#'
#' @noRd
.api_access.prescreeners <-
    function(.self,
             update = FALSE) {
        current_val <- tryCatch(
            get("prolific_prescreener_list", envir = .self$.internals),
            error = function(e) {
                NULL
            }
        )

        # Download the list of possible prescreeners from prolific
        # if this was not yet done
        if (is.null(current_val) | update) {
            prescreeners_list <-
                .self$access(
                    endpoint = "eligibility-requirements"
                )

            invisible(lapply(prescreeners_list$results$attributes, function(x) {
                data.table::setDT(x)
                return(invisible(NULL))
            }))
            # Add list of studies as attribute for the corresponding prescreeners
            list_of_studies <- tryCatch(getOption(".prolific.api.latest.working.access")$access(endpoint = "studies", method = "get"), error = function(e) NULL)
            if (!is.null(list_of_studies)) {
                previous_studies_requirements <- grep("previousstudies(allowlist)*", tolower(prescreeners_list$results$`_cls`))
                prescreeners_list$results$attributes[previous_studies_requirements] <- lapply(previous_studies_requirements, function(i) list_of_studies)
            }
            # Store list of prescreeners as data.table
            assign(
                "prolific_prescreener_list",
                data.table::data.table(
                    title = prescreeners_list$results$query$title,
                    category = prescreeners_list$results$category,
                    subcategory = prescreeners_list$results$subcategory,
                    id = prescreeners_list$results$query$id,
                    cls = prescreeners_list$results$`_cls`,
                    attributes = prescreeners_list$results$attributes
                ),
                envir = .self$.internals
            )

            current_val <- tryCatch(
                get("prolific_prescreener_list", envir = .self$.internals),
                error = function(e) {
                    NULL
                }
            )
        }

        return(current_val)
    }

#' Function to set the API authorization string
#'
#' @param .self
#' \strong{api_access object}:\cr
#' The object for which the authorization string is to be set
#'
#' @noRd
.api_access.set_authorization <-
    function(.self) {
        # Update the API authentication string
        assign(
            "api_authorization",
            paste0("-H 'Authorization: Token ", .self$api_token, "'"),
            envir = .self$.internals
        )
    }
