#' @rdname api_access
#'
#' @name api_access
# @aliases token access get post place patch delete
#'
#' @section Methods:
#'
#' ## \code{access}
#' Main method for accessing the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}
#'
#' ### \strong{Parameters}
#' \describe{
#'      \item{\code{endpoint}}{(\code{\link[=character]{character}}):}\cr
#'          The endpoint to access. If this is a vector, its elements are collapsed by \code{'/'}.
#'      \item{\code{method}}{(\code{\link[=character]{character}}):}\cr
#'          The method to use. One of \code{get, post, place, patch and delete}.
#'          The commands associated with each method are defined in the \code{\link[=api_access]{accessors}} field of the \code{\link[=api_access]{api_access}} object.
#'      \item{\code{data}}{(\code{\link[jsonlite:toJSON]{json string}}, \code{\link[jsonlite:toJSON]{json file}}, \code{\link[=list]{list}}, \code{\link[=prolific_study]{prolific_study object}} or \code{\link[=NULL]{NULL}})}\cr
#'          The data to be transfered in the body of the \href{https://docs.prolific.co/docs/api-docs/public/}{API} call.
#'          R-objects are converted to a \code{\link[jsonlite:toJSON]{json string}} using \code{\link[jsonlite:toJSON]{jsonlite:toJSON}} .
#'          \code{\link[=NULL]{NULL}} means that no data is transfered.
#'      \item{\code{as_list}}{(\code{\link[=logical]{logical}}):}\cr
#'          Whether the return of the \href{https://docs.prolific.co/docs/api-docs/public/}{API} call should be converted to a list or (if applicable) \link[=prolific_study]{prolific_study} object,
#'          rather than returned as the raw \code{\link[jsonlite:toJSON]{json string}}.
#' }
#'
#' ### \strong{Return Value}
#' A \code{\link[=list]{list}} or \code{\link[jsonlite:toJSON]{json string}}, depending on argument \code{as_list}.
#'
#' ### \strong{Usage}
#' \preformatted{
#'      api_access$access(
#'           endpoint,
#'           method,
#'           data,
#'           as_list
#'      )
#' }
#'
# ### \strong{Shortcut-methods}
# Instead of using \code{method="get"}, \code{method="post"}, \code{method="place"}, \code{method="patch"} and \code{method="delete"}},
# the shortcut-methods
# \code{api_access$get(...)},
# \code{api_access$post(...)},
# \code{api_access$place(...)},
# \code{api_access$patch(...)} and
# \code{api_access$delete(...)}
# can be used, respectively.
# The remaining arguments
# (\code{endpoint},\code{data},\code{as_list})
# are the same.
#'
#'
#' ## \code{check_authorization}
#' Check whether the \href{https://docs.prolific.co/docs/api-docs/public/#section/Authentication}{API authorization} works
#'
#' ### \strong{Return Value}
#' A \code{\link[=logical]{logical}} value that indicates whether the \href{https://docs.prolific.co/docs/api-docs/public/#section/Authentication}{API authorization} works.
#'
#' ### \strong{Usage}
#' \preformatted{
#'      api_access$check_authorization()
#' }
#'
#'
#' @export
NULL

api_access$methods(
    show =
        function(hide_token = TRUE) {
            # "
            #     Show the main contents of an api_access object to the user.
            #     \\subsection{Parameters}{
            #         \\itemize{
            #             \\item{
            #                 \\code{hide_token}:
            #                 Whether the object's \\code{hapi_token} should be shown.\\cr
            #                 The default is \\bold{FALSE}, since this token is private information.
            #             }
            #         }
            #     }
            # "
            sep <- paste0(rep("=", getOption("width")), collapse = "")

            cat(paste0(sep, "\n", "API access summary:", "\n"))

            indent <- "    "
            nchars_sub <- 10 # max(nchar(names(accessors)))

            # Output token
            cat("\nAPI token:\n")
            cat(paste0(
                indent,
                c(
                    "value:",
                    if (hide_token) {
                        paste0(
                            rep(
                                " ",
                                nchar("value:")
                            ),
                            collapse = ""
                        )
                    }
                ),
                paste0(
                    rep(
                        " ",
                        max(nchars_sub - nchar("value"))
                    ),
                    collapse = ""
                ),
                if (!hide_token) {
                    api_token
                } else {
                    if (is.null(api_token)) {
                        "<not yet set>"
                    } else {
                        c("<hidden>", "(Use api_access$show(hide_token = FALSE) to show the token.)")
                    }
                },
                collapse = "\n"
            ), "\n")

            cat(paste0(
                indent,
                c(
                    "status:",
                    paste0(
                        rep(
                            " ",
                            nchar("status:")
                        ),
                        collapse = ""
                    )
                ),
                paste0(
                    rep(
                        " ",
                        max(nchars_sub - nchar("status"))
                    ),
                    collapse = ""
                ),
                if (get(".valid_authorization", .self$.internals$fields)) {
                    c("valid", "(API access successful!)")
                } else {
                    c("invalid", "(API access failed!)")
                },
                collapse = "\n"
            ), "\n")

            cat("\n")

            # Output entrypoint
            cat("\nEntrypoint:\n")
            cat(
                paste0(
                    indent,
                    entrypoint
                ),
                "\n\n"
            )
            # Output accessors

            cat("\nAccessors:\n")
            for (accessor in names(accessors)) {
                cat(
                    paste0(
                        indent,
                        accessor,
                        ":",
                        paste0(
                            rep(
                                " ",
                                max(nchars_sub - nchar(accessor))
                            ),
                            collapse = ""
                        ),
                        accessors[accessor],
                        "\n"
                    )
                )
            }
            cat("\n")
            cat(sep, "\n")
        },
    access =
        function(endpoint,
                 method = c("get", "post", "patch", "put", "delete"),
                 data = NULL,
                 as_list = TRUE,
                 silent = TRUE) {
            # "
            #     Method for accessing the API.
            #     \\subsection{Parameters}{
            #         \\itemize{
            #             \\item{
            #                 \\code{endpoint}:
            #                 The API endpoint which is accessed, as \\bold{string}.
            #             }
            #             \\item{
            #                 \\code{method}:
            #                 The method that is used, as \\bold{string}.
            #                 One of 'get', 'post', 'place', 'patch', 'delete'.
            #             }
            #             \\item{
            #                 \\code{data}:
            #                 The data to be transfered in the body of the API call.
            #                 A \\bold{JSON string}, \\bold{JSON file},
            #                 \\bold{list}, \\bold{prolific_study object} or \\bold{NULL}.
            #                 R-objects are converted to a JSON string.
            #                 NULL means that no data is transfered.
            #             }
            #             \\item{
            #                 \\code{as_list}:
            #                 A \code{\link[=logical]{logical}} value that indicates whether the return of the API call should be converted to a list.
            #             }
            #         }
            #     }
            #     \\subsection{Return Value}{
            #          A \\code{\link[=list]{list}} or \\strong{json-string}, depending on argument \\code{as_list}.
            #     }
            # "

            method <- tryCatch(match.arg(method),
                error = function(e) stop(gsub("'arg'", "'method'", e))
            )

            # Run API access command,
            # consisting of acces method, header (data and authorization) and endpoint
            output <- .format_output(
                .execute(
                    paste0(
                        accessors[[tolower(method)]],
                        ifelse(silent, " -s", "")
                    ),
                    " ",             
                        .format_input(data, list_of_prescreeners = if (any(class(data) %in% c("prolific_study", "eligibility_requirements", "prolific_prescreener"))) {
                            .self$.internals$methods$prescreeners()
                        } else {
                            NULL
                        }),
                    " ",
                    .self$.internals$api_authorization,
                    " ",
                    .self$.internals$fields$`.referer`,
                    " ",
                    "\"", .make_url(c(entrypoint, endpoint)), "\""
                ),
                as_list = as_list || (class(data) %in% c("prolific_study"))
            )

            # Convert the entpoint link to check the endpoint

            link_split <- strsplit(
                .make_url(c(endpoint)), "/"
            )[[1]]

            if (method == "get" && link_split[length(link_split)] == "studies") {
                output <- output$results
                output[(c(
                    "publish_at", "is_pilot", "is_underpaying", "reward_level",
                    "quota_requirements", "is_reallocated"
                ))] <- NULL

                output$date_created <-
                    as.POSIXct(output$date_created, format = "%Y-%m-%dT%H:%M:%S")

                date_created <-
                    data.table::IDateTime(output$date_created)
                data.table::setnames(date_created, 1:2, c("creation_day", "creation_time"))
                output$date_created <-
                    NULL
                output <-
                    c(output, date_created)

                data.table::setDT(output)

                if (nrow(output) == 0) {
                    output <-
                        data.table::data.table(
                            "creation_day" = integer(1),
                            "creation_time" = integer(1),
                            "internal_name" = character(1),
                            "name" = character(1),
                            "id" = character(1),
                            "study_type" = character(1),
                            "total_available_places" = integer(1),
                            "places_taken" = integer(1),
                            "reward" = integer(1),
                            "status" = character(1),
                            "number_of_submissions" = integer(1),
                            "total_cost" = double(1),
                            "privacy_notice" = character(1)
                        )[0, ]
                }

                data.table::setcolorder(output, c("creation_day", "creation_time", "internal_name", "name"))
                data.table::setkeyv(output, c("creation_day", "creation_time"))
            }

            # Check if output is a Prolific study, and return it as the respective class

            study_fields <- names(formals(prolific_study$methods("initialize")))

            study_fields <- study_fields[!study_fields %in% c("url_parameters", "...")]
            if (all(study_fields %in% names(output))) {
                if (class(data) %in% c("prolific_study")) {

                    # If the input is a prolific_study, e.g. when submitting / updating a study:
                    # update the input and return a reference instead of a new object
                    if ((length(data$eligibility_requirements) > 0) | (length(output$eligibility_requirements) > 0)) {
                        output$eligibility_requirements <- data$eligibility_requirements
                    }
                    do.call(data$initialize, output)
                    output <- data
                } else {
                    if ("eligibility_requirements" %in% names(output)) {
                        if (length(output$eligibility_requirements) > 0) {
                            output$eligibility_requirements <-
                                .to_prolific_prescreeners(output$eligibility_requirements, .self$.internals$methods$prescreeners())
                        }
                    }

                    if ("naivety_distribution_rate" %in% names(output)) {
                        if (all(is.null(output$naivety_distribution_rate))) {
                            output$naivety_distribution_rate <- NULL
                        }
                    }
                    output <- do.call(prolific_study, output)
                }
            }
            return(output)
        },
    # get =
    #     function(endpoint,
    #              data = NULL,
    #              as_list = TRUE) {
    #         .self$access(
    #             endpoint = endpoint,
    #             method = "get",
    #             data = data,
    #             as_list = as_list
    #         )
    #     },
    # post =
    #     function(endpoint,
    #              data = NULL,
    #              as_list = TRUE) {
    #         .self$access(
    #             endpoint = endpoint,
    #             method = "post",
    #             data = data,
    #             as_list = as_list
    #         )
    #     },
    # patch =
    #     function(endpoint,
    #              data = NULL,
    #              as_list = TRUE) {
    #         .self$access(
    #             endpoint = endpoint,
    #             method = "patch",
    #             data = data,
    #             as_list = as_list
    #         )
    #     },
    # put =
    #     function(endpoint,
    #              data = NULL,
    #              as_list = TRUE) {
    #         .self$access(
    #             endpoint = endpoint,
    #             method = "put",
    #             data = data,
    #             as_list = as_list
    #         )
    #     },
    # delete =
    #     function(endpoint,
    #              data = NULL,
    #              as_list = TRUE) {
    #         .self$access(
    #             endpoint = endpoint,
    #             method = "delete",
    #             data = data,
    #             as_list = as_list
    #         )
    #     },
    check_authorization =
        function() {
            # "
            #     Check whether the API authorization using \\code{api_tokenz} works.
            #     \\subsection{Return Value}{
            #         A logical value that indicates whether API access works.
            #     }
            # "

            output <- !grepl("error", tolower(access(endpoint = "users/me", method = "get", as_list = FALSE)))

            if (output) {
                options(".prolific.api.latest.working.access" = .self)
            }

            assign(".valid_authorization", output, .self$.internals$fields)

            return(output)
        },
    prescreeners =
        function(filter = NULL,
                 show_full = FALSE) {
            output <- data.table::as.data.table(.self$.internals$methods$prescreeners())
            if (!is.null(filter)) {
                output <- output[with(output, eval(filter)), ]
            }
            if (show_full) {
                titles <- output$title
                output <- lapply(1:nrow(output), function(i) {
                    result <- list(
                        prescreener = output[i, ],
                        available_constraints = output[i, ]$attributes[[1]]
                    )
                    result$prescreener$attributes <- NULL
                    return(result)
                })

                names(output) <- gsub("\\W", "_", titles)
            } else {
                output[c("attributes", "cls", "id")] <- NULL
            }
            return(output)
        }
)
