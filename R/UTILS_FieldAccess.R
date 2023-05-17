# ============================= > ..checkField < ============================= #

# ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
# || Worker Function for checking fields assignments                        || #
# └└────────────────────────────────────────────────────────────────────────┘┘ #

..checkField <-
    function(field_name,
             value,
             envir,
             attribute,
             update = FALSE) {
        # Obtain attribute restriction from RefClass
        field_attribute <- unlist(envir[[".field_restrictions"]][[field_name]][[attribute]], recursive = TRUE)

        # If there is no restriction to be set or checked, we are done here
        if (!update & is.null(field_attribute)) {
            return("TRUE")
        }

        # Get attribute function
        attr_fun <- get(attribute, envir = globalenv())
        # ... and check whether it's actually a function
        if (!is.function(attr_fun)) {
            stop(paste0("attribute ", attribute, "should be a function but is ", typeof(attr_fun), "!\n"))
        }

        # Obtain attribute from value.
        # If the attribute cannot be applied (e.g. 'sum' for characters): return NULL
        value_attribute <-
            tryCatch(
                do.call(
                    attr_fun,
                    list(x = value)
                ),
                error = function(e) {
                    return(NULL)
                }
            )

        # The attribute restriction can be updated to the current value
        if (update) {
            field_attribute <-
                envir[[".field_restrictions"]][[field_name]][[attribute]] <-
                value_attribute
        }

        # Collapse attributes that have more than one element (e.g. for 'dim')
        # if (length(value_attribute) > 1) {
        value_attribute <- paste0(value_attribute, collapse = ",")
        # }
        # if (length(field_attribute) > 1) {
        field_attribute <- paste0(field_attribute, collapse = ",")
        # }

        # Make sure comparison and output do not contain NULL (empty strings) for attribute
        if (length(field_attribute) == 0) {
            field_attribute <- "NULL"
        }
        if (value_attribute == "") {
            value_attribute <- "NULL"
        }

        if (field_attribute == "") {
            field_attribute <- "NULL"
        }

        # Check whether restriction is met, and return corresponding error message if it is not
        if (!all(all.equal(value_attribute, field_attribute) == TRUE)) {
            paste0(
                attribute,
                "(",
                field_name,
                ") would become ",
                value_attribute,
                " but should be ",
                field_attribute,
                ".\n"
            )
        } else {
            return("TRUE")
        }
    }

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================= > .checkField < ============================== #

# ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
# || Function for checking fields assignments                               || #
# └└────────────────────────────────────────────────────────────────────────┘┘ #

.checkField <-
    function(name,
             value,
             envir,
             attributes,
             update = FALSE) {
        # Check whether attribute restrictions are met by 'value'
        field_checks <- vapply(attributes, function(attr) {
            ..checkField(
                field_name = name,
                value = value,
                envir = envir,
                attribute = attr,
                update = update
            )
        }, "x")

        # Throw an error if 'value' violates one of the restrictions
        if (any(field_checks != "TRUE")) {
            return(
                paste0(
                    "Assignment to field '", name, "' failed:\n",
                    paste0("\t- ", field_checks[which(field_checks != "TRUE")], collapse = "")
                )
            )
        } else {
            return(TRUE)
        }
    }

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================== > .getField  < ============================== #

# ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
# || General field getter                                                   || #
# └└────────────────────────────────────────────────────────────────────────┘┘ #

.getField <-
    function(name,
             envir) {
        tryCatch(
            get(name, envir = envir),
            error = function(e) {
                if (grepl(paste0("object '", name, "' not found"), e)) {
                    return(NULL)
                } else {
                    stop(paste0("Access for field ", name, " failed:\n\t", e, "\n"))
                }
            }
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================== > .setField  < ============================== #

# ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
# || General field setter                                                   || #
# └└────────────────────────────────────────────────────────────────────────┘┘ #
.setField <-
    function(name,
             value,
             envir) {
        assign(name, value, envir = envir)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================= > .accessField < ============================= #

# ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
# || General field accessor                                                 || #
# || (dispatches getter / setter)                                           || #
# └└────────────────────────────────────────────────────────────────────────┘┘ #

.accessField <-
    function(name,
             value,
             envir,
             check = TRUE) {
        val_name <- paste0("..", name, "_value")
        if (missing(value)) {
            # If no value is provided, apply getter
            .getField(val_name, envir)
        } else {
            # If a value is provided, apply setter
            if (check & !is.null(value)) {
                # Check whether attribute restriction
                restricted_attributes <-
                    unique(unlist(lapply(get(".field_restrictions", envir = envir), names), recursive = TRUE))
                field_checks <- .checkField(name, value, envir = envir, attributes = restricted_attributes)
                if (!all(field_checks == TRUE)) {
                    stop(field_checks)
                }
            }
            .setField(val_name, value, envir)
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
