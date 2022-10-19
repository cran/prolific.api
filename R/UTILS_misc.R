#' Function for enclosing arguments in quotation marks
#'
#' @param ...
#' Arbitrary arguments, each of which are put in quotation marks.
#'
#' @return
#' A character string of quoted arguments
#'
#' @noRd
.quote <-
    function(...) {
        return(paste0("\"", c(...), "\""))
    }





#' Function to paste and run commands in system shell
#'
#' @param ...
#' \strong{Character vector}:\cr
#' Arbitrary components of the command, which are and collapsed.
#'
#' @return The output of the shell command
#'
#' @noRd
.execute <-
    function(...) {
        cmd <- paste0(
            ...,
            collapse = ""
        )
        return(
            tryCatch(
                {
                    system(
                        cmd,
                        intern = TRUE # ,
                        # ignore.stdout = getOption("prolific.api_ignore.stdout", default = TRUE)
                    )
                },
                error = function(e) {
                    stop(paste0("\tCommand: ", paste0(..., collapse = "", " failed.\n\t", e, "\n")))
                }
            )
        )
    }

#' Function to format data (list, file or prolific study) as JSON
#'
#' @param data
#' \strong{JSON string or file connection, list, prolific_study object or \code{\link[=NULL]{NULL}}}:\cr
#' The data to be transferred in the body of an API access, or the respective JSON file connection.
#' R-objects are converted to a JSON string.
#'
#' @param data_file
#' \code{\link[=logical]{logical}} value:\cr
#' Whether the data should be trated and transferred as a file.
#'
#' @param content_type
#' \strong{String value}:\cr
#' The content type of the body.
#' Typically `application/json`.
#'
#' @param list_of_prescreeners
#' \strong{data.table}:\cr
#' The list of prescreeners, as returned by prescreeners(api_access)
#'
#' @return The string that can be added when accessing the API
#'
#' @noRd
.format_input <-
    function(data,
             data_file = FALSE,
             content_type = "application/json",
             list_of_prescreeners = NULL) {
        if (all(is.null(data))) {
            return(invisible(NULL))
        }

        data_file <- data_file || ("file" %in% attr(data, "class"))

        # If data is a prolific study object:
        # format it as list

        if (any(class(data) %in% "prolific_study")) {
            data <-
                data$.internals$methods$output(list_of_prescreeners)
        }

        if (any(class(data) %in% "eligibility_requirements")) {
            data <-
                list(
                    eligibility_requirements =
                        lapply(data, function(x) x$.internals$methods$output(list_of_prescreeners))
                )
        }

        return(
            paste0(
                "-d '",
                if (!data_file) {
                    # Convert data to JSON format if it is not a file
                    gsub(
                        "'",
                        "\\'",
                        jsonlite::toJSON(
                            data,
                            auto_unbox = TRUE
                        )
                    )
                } else {
                    # Add data file path
                    paste0(
                        "@",
                        # "'",
                        summary(data)$description
                        # "'"
                    )
                },
                "'",
                # Writen header information
                " -H 'Content-Type: ", content_type, "' "
            )
        )
    }

..tryCatchfromJSON <- function(x) {
    tryCatch(jsonlite::fromJSON(x), error = function(e) x)
}

#' Function to format JSON as list if requested
#'
#' @param output
#' \strong{String value}:\cr
#' The output (typically JSON) obtained from an API access
#' @param as_list
#' \code{\link[=logical]{logical}} value:\cr
#' Whether the output should be converted to a list.
#'
#' @return The output as raw or list
#'
#' @noRd
.format_output <-
    function(output,
             as_list = TRUE) {
        # Convert output to list if requested
        switch(as_list + 1,
            I,
            ..tryCatchfromJSON
        )(output)
    }

#' Function to combine URL components to a single URL
#'
#' @param ...
#' \strong{Character vector}:\cr
#' Arbitrary components of the URL, which are pasted and collapsed by slashes.
#' @param add_protocol
#' \code{\link[=logical]{logical}} or \code{\link[=character]{character}} value:\cr
#' Whether `https` should be added as protocol, or the protocol to be added.
#'
#' @noRd
.make_url <-
    function(...,
             add_protocol = TRUE) {
        url_components <- c(...)

        # Add protocol (default: https) if not yet contained
        if (!any(grepl("http", url_components)) & (add_protocol != FALSE)) {
            if (add_protocol != TRUE) {
                url_components <- c(paste0(add_protocol, "://"), url_components)
            } else {
                url_components <- c("https://", url_components)
            }
        }
        return(
            # Collapse arbitrary number of arguments in given order
            # Replace double slashes everywhere except after the protocol
            gsub(
                paste0("(?<![http|https|", add_protocol, "]:)(/){2,}"), "/",
                paste0(paste0(url_components, collapse = "/"), "/"),
                perl = TRUE
            )
        )
    }

#' Function to collapse URL parameters
#'
#' @param url_parameters
#' \code{\link[=list]{list}}:
#' The parameters to be collapsed
#'
#' @return A string where parameters are collapsed by '?'
#'
#' @noRd
.collapse_url_parameters <-
    function(url_parameters) {
        return(paste0(
            names(url_parameters), "=",
            ifelse(!tolower(names(url_parameters)) %in% "q_eed", "{", "{"),
            url_parameters,
            ifelse(!tolower(names(url_parameters)) %in% "q_eed", "}", "}"),
            collapse = "&"
        ))
    }
# collapsed_url_parameters <- gsub("(.*?)\\?(.*)", "\\2", external_study_url)
#' Function to de-collapse URL parameters
#'
#' @param collapsed_url_parameters
#' The string of collapsed parameters
#'
#' @return A list of decollappsed parameters
#'
#' @noRd
.decollapse_url_parameters <-
    function(collapsed_url_parameters) {
        x <- strsplit(collapsed_url_parameters, "&")[[1]]
        output <- as.list(gsub("(.+)=\\{(.*?)\\}", "\\2", x))
        names(output) <- gsub("(.*)=\\{(.*?)\\}", "\\1", x)
        return(output)
    }

#' Pass arbitrary arguments with names
#'
#' @param ... arbitrary named arguments
#' @noRd
.pass_named <- function(...) {
    return(
        paste0(
            ...names(),
            " = ",
            c(...)
        )
    )
}
