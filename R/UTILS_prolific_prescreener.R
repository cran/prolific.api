#' Function to convert prolific_prescreener objects
#' to lists for passing them to the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}
#'
#' @param prolific_prescreener_list
#' \strong{data.table}:
#' The possible prescreeners, as obtained from an \code{api_access} object
#'
#' @return
#' A list
#'
#' @noRd
.output_prolific_prescreener <-
    function(prolific_prescreener,
             prolific_prescreener_list) {
        requirement <- prolific_prescreener_list[prolific_prescreener_list$title %in% prolific_prescreener$title, ]

        if (nrow(requirement) > 1) {
            stop(paste0(
                "Non-unique prescreener title: ", prolific_prescreener$title, "!\n",
                "\tUse the help '?prescreeners' to identifiy proper prescreening variables."
            ))
        }

        if (nrow(requirement) < 1) {
            stop(paste0(
                "Invalid prescreener title: ", prolific_prescreener$title, "!\n",
                "\tUse the help '?prescreeners' to identifiy proper prescreening variables."
            ))
        }

        # if ((is_custom_study_list <- grepl("previousstudies(allowlist)*", tolower(requirement$cls)))) {
        #     selection <- getOption(".prolific.api.latest.working.access")$access(endpoint = "studies", method = "get")
        # } else {
        selection <- requirement$attributes[[1]]
        # }

        if ("value" %in% names(selection)) {
            selection$value <- NULL
        }

        if ((is_custom_list <- grepl("custom(black|white)list", tolower(requirement$cls)))) {
            constraints <- list(as.list(names(unlist(prolific_prescreener$constraints))))
            names(constraints) <- selection[1, ]$name
            constraints[[1]] <- constraints[[1]][vapply(prolific_prescreener$constraints, function(x) x != "FALSE", TRUE)]
        } else {
            constraints <- prolific_prescreener$constraints
        }

        # if ((is_custom_list <- grepl("custom(black|white)list", tolower(requirement$cls)))) {
        #     names(constraints) <- gsub("_", "", names(constraints))
        #     selection$name <- gsub("_", "", selection$name)
        # }

        target_column_identifier <- vapply(selection, function(x) {
            mean((tolower(names(constraints)) %in% tolower(x)))
        }, 1.0)

        if (!"name" %in% names(target_column_identifier)) {
            target_column_identifier <- c(target_column_identifier, name = 0)
        }



        if (target_column_identifier["name"] == 1) {
            target_column_identifier <- "name"
        } else {
            if (!(any(target_column_identifier == 1))) {
                guess_col <- names(target_column_identifier)[which.max(target_column_identifier)]

                freq_fmt <- paste0(
                    "\t",
                    format(names(target_column_identifier), width = 4 + max(nchar(names(target_column_identifier)))),
                    paste0(format(round(target_column_identifier * 100, 1), width = 5, nsmall = 1), " %"),
                    collapse = "\n"
                )

                tbl_fmt <- utils::capture.output(print(selection, row.names = FALSE, trunc.cols = TRUE))
                missing_labs <- paste0("'", names(constraints)[!tolower(names(constraints)) %in% tolower(selection[[guess_col]])], "'")
                tbl_fmt <<- tbl_fmt

                missing_labs_fmt <- paste0(lapply(split(
                    missing_labs,
                    c(0, cumsum(diff(cumsum(nchar(missing_labs)) %% getOption("width") * 0.7) < 0))
                ), function(x) paste0(x, collapse = ",")), collapse = ",\n\t")

                stop(paste0(
                    "\nAmbigious constraints for '", prolific_prescreener$title, "'.",
                    "\nNo single column represents all constraints.\n\n",
                    "All names of the constraints should come from a single column of\n",
                    paste0("\t", tbl_fmt, collapse = "\n"),
                    "\n\n",
                    "The percentages of names contained in each of these columns are:\n",
                    freq_fmt,
                    "\n\n",
                    "--> The most likely column seems to be '", guess_col, "', ",
                    "but it does not contain ",
                    "\n\t",
                    missing_labs_fmt,
                    ".\n",
                    "\tUse the help '?prescreeners' to identifiy proper prescreening constraints."
                ))
            }
            target_column_identifier <- names(selection)[which(target_column_identifier == 1)[1]]
        }

        selection$target <- tolower(selection[[target_column_identifier]])
        if ("label" %in% names(selection)) {
            selection$label <- NULL
        }
        selection <- data.table::as.data.table(selection)

        constraint_dt <- data.table::data.table(
            target = tolower(names(constraints)),
            value = constraints
        )

        if (is.logical(constraint_dt$value)) {
            constraint_dt$value <- tolower(constraint_dt$value)
        }

        selection <- merge(selection, constraint_dt, by = "target", all.x = FALSE, all.y = TRUE)

        col_order <- unique(c(target_column_identifier, "name", "label", "id", "value", "index"))

        selection <- selection[col_order[col_order %in% names(selection)]]

        if ("index" %in% names(selection)) {
            data.table::setkeyv(selection, "index")
        }

        return(
            list(
                "type" = "select",
                "attributes" = selection,
                "query" = list(
                    "id" = requirement$id
                ),
                "_cls" = requirement$cls
            )
        )
    }

#' Function to convert prescreeners
#' as returned by the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}
#' to prolific_prescreener objects
#'
#' @inheritParams .output_prolific_prescreener
#'
#' @param prescreeners_input
#' \code{\link[=list]{list}}:\cr
#' The list for of prescreeners for a Prolific study obtained from the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}
#'
#' @param prolific_prescreener_list
#'
#' @return
#' A list of \code{prolific_prescreener} objects
#'
#' @noRd
.to_prolific_prescreeners <-
    function(prescreeners_input,
             prolific_prescreener_list) {
        active_requirements_indices <- which(!is.na(prescreeners_input$id))

        xxx <- lapply(prescreeners_input$attributes[active_requirements_indices], function(x) {
            data.table::setDT(x)
            return(invisible(NULL))
        })

        result <- lapply(active_requirements_indices, function(i) {
            constraints <- as.list(prescreeners_input$attributes[[i]]$value)
            names(constraints) <- if (sum(duplicated(prescreeners_input$attributes[[i]]$name)) == 0) prescreeners_input$attributes[[i]]$name else prescreeners_input$attributes[[i]]$id
            constraints <- constraints[vapply(constraints, function(x) any(x != FALSE), TRUE)]

            return(
                do.call(
                    "prolific_prescreener",
                    c(
                        list(title = prolific_prescreener_list[i, ]$title),
                        constraints
                    )
                )
            )
        })

        return(result)
    }

# Function to convert dots to a named list
#'
#' @return
#' A named list to be used as constraints for \code{prolific_prescreener} objects
#'
#' @noRd
.make_prescreener_constraints <- function(...) {
    # Construct output list
    output <- vector("list", ...length())

    # Consider named dots as the name = values pairs
    names(output) <- ...names()
    named_pos <- which(names(output) != "")
    output[named_pos] <- lapply(named_pos, function(i) ...elt(i))

    # Consider unnamed dots as the names of boolean constraints
    if (is.null(names(output))) names(output) <- rep("", ...length())
    unnamed_pos <- which(names(output) == "")
    names(output)[unnamed_pos] <- as.character(substitute(list(...))[-1])[unnamed_pos]
    output[unnamed_pos] <- TRUE

    # Evaluate names which are enclosed in `eval()`
    eval_pos <- grep("eval\\(.*\\)", names(output))
    names(output)[eval_pos] <-
        Reduce(c, lapply(eval_pos, function(sfdakljhsdadasfcxysfadnlk) {
            eval(parse(text = names(output)[sfdakljhsdadasfcxysfadnlk]))
        }))
    return(output)
}
