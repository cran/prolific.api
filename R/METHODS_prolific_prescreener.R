#' @rdname prolific_prescreener
#'
#' @name prolific_prescreener
#'
#' @section Methods:
#'
#' ## \code{validity_check}
#' Check whether the prescreener is valid in terms of the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}.\cr
#' \strong{Note:} For checking a prescreener's validity, an \code{\link[=api_access]{api_access object}} that passes \code{\link[=api_access]{check_authorization()}} needs to be available.
#' It suffices if any such \code{\link[=api_access]{api_access object}} is specified, since the reference to it is determined automatically.
#'
#' ### \strong{Return Value}
#' \itemize{
#'  \item{If the prescreener is valid: }{A \code{\link[=logical]{logical}} value indicating that the study is valid}
#'  \item{If the prescreener is not valid: }{A \code{\link[=character]{character}} vector that lists the prescreener's issues.}
#' }
#'
#' ### \strong{Usage}
#' \preformatted{
#'      prescreener$validity_check()
#' }
prolific_prescreener$methods(
    # ================================= > show < ================================= #

    # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
    # || Show the main contents of a prolific_prescreener object to the user    || #
    # └└────────────────────────────────────────────────────────────────────────┘┘ #
    show =
        function() {
            sep <- paste0(rep("=", getOption("width")), collapse = "")
            indent <- "    "

            cat(paste0(sep, "\n", "Prolific prescreener:", "\n", sep, "\n"))

            cat("Title:\n")
            cat(paste0(indent, title, "\n\n"))
            if (!all(grepl("^(black|white)_list$", tolower(names(constraints))))) {
                cat("Constraints:\n")
                cat(paste0(indent, do.call(.pass_named, constraints), collapse = "\n"))
            } else {
                list_type <- gsub("^(black|white)_list$", "\\1", tolower(names(constraints)))
                cat(paste0(list_type, "list:\n"))
                cat(paste0(indent, unlist(constraints), collapse = ",\n"))
            }
            cat("\n\n")
        },
    # ────────────────────────────────── <end> ─────────────────────────────────── #

    # ============================ > validity_check < ============================ #

    # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
    # || Check whether the prescreener is valid in terms of the                 || #
    # || \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}    || #
    # └└────────────────────────────────────────────────────────────────────────┘┘ #

    validity_check =
        function() {
            prolific_prescreener_list <- tryCatch(getOption(".prolific.api.latest.working.access")$.internals$methods$prescreeners(), error = function(e) NULL)

            if (all(is.null(prolific_prescreener_list))) {
                return(paste0("validity_check for prolific_prescreener \"", .self$title, "\" failed, because any api_access object with passed check_authorization() is required."))
            }
            check_msg <- tryCatch(
                {
                    x <- .self$.internals$methods$output(prolific_prescreener_list)
                    invisible(NULL)
                },
                error = function(e) e
            )

            if (!all(is.null(check_msg))) {
                return(check_msg)
            } else {
                return(TRUE)
            }
        }
    # ────────────────────────────────── <end> ─────────────────────────────────── #
)
