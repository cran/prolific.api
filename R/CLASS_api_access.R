#' @importFrom methods new
#' @importFrom methods getGeneric
#' @importFrom methods setGeneric
NULL

#' Prolific API access
#'
#' @name api_access
#' @aliases api_access-class API_ACCESS api_accessor
#'
#' @description
#' This class provides functionalities for accessing the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}.
#' The core method for this purpose is \code{\link[=api_access]{access}}, which can be used
#' to create, review, change, manage and delete studies on the Prolific platform.\cr
#' \emph{The fields and methods are available as in RefClass or S4 objects (see examples).}
#'
#' @field accessors (\code{\link[=character]{character}}):\cr
#' The commands for accessing the API.
#' The command for each type of access method can be altered using this field.
#' The default is
#' \preformatted{
#'      accessors = c(
#'         get    = "curl",
#'         post   = "curl -X POST",
#'         put    = "curl -X PUT",
#'         patch  = "curl -X PATCH",
#'         delete = "curl -X DELETE"
#'      )
#' }
#' \strong{Note:} \emph{A value for each of the names \code{(get, post, put, patch and delete)} is required, as these represent the methods that can be used when accessing the API.}
#'
#' @field api_token (\code{\link[=character]{character}}):\cr
#' The \href{https://docs.prolific.co/docs/api-docs/public/#section/Authentication}{Prolific API token}.
#'
#' @field entrypoint (\code{\link[=character]{character}}):\cr
#' The \href{https://docs.prolific.co/docs/api-docs/public/#section/Get-started/Conventions}{API's entrypoint URL}.
#'
#' @examples
#' library(prolific.api)
#'
#' # Create API access
#' prolific_api_access <- api_access(api_token = "<api_token>")
#'
#' # View fields
#' ## RefClass Methods
#' prolific_api_access$accessors
#' prolific_api_access$api_token
#' prolific_api_access$entrypoint
#'
#' ## S4 Methods
#' accessors(prolific_api_access)
#' api_token(prolific_api_access)
#' entrypoint(prolific_api_access)
#'
#' # Change fields
#' # (this is usually only required for the api_token)
#' # replace <new_token> in the by the actual API token
#' # before running these lines
#' \dontrun{
#' ## RefClass Method
#' prolific_api_access$api_token <- "<new_token>"
#' ## S4 Method
#' api_token(prolific_api_access) <- "<new_token>"
#' }
#'
#'
#' # Note: For the following code to work,
#' # you have to replace <new_token> in the lines above by the actual API token
#' \dontrun{
#' # Check wheter Authorization is working
#' ## RefClass Method
#' prolific_api_access$check_authorization()
#' ## S4 Method
#' check_authorization(prolific_api_access)
#'
#' # Obtain list of existing studies
#' ## RefClass Method
#' list_of_studies <-
#'     prolific_api_access$access(
#'         endpoint = "studies",
#'         method = "get",
#'         as_list = TRUE
#'     )
#' ## S4 Method
#' list_of_studies2 <-
#'     access(
#'         prolific_api_access,
#'         endpoint = "studies",
#'         method = "get",
#'         as_list = TRUE
#'     )
#' }
#'
#' @exportClass api_access
#' @export api_access
#'
api_access <- setRefClass(
    Class = "api_access",
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Fields Block (setter/getter functions)
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    fields = list(
        accessors = function(value) .accessField("accessors", value, .self$.internals$fields, TRUE),
        api_token = function(value) {
            retval <- .accessField("api_token", value, .self$.internals$fields, TRUE)
            # Update the authorization string if a new token was provided
            if (!missing(value)) {
                .api_access.set_authorization(.self)
                (access_check <- .self$check_authorization())

                if (!access_check) {
                    message("API token status: invalid\n\t API access failed!\n")
                } else {
                    message("API token status: valid\n\t API access successful!\n")
                }
            }
            return(retval)
        },
        entrypoint = function(value) .accessField("entrypoint", value, .self$.internals$fields, TRUE)
    ),
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Methods Block
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    methods = list(
        # Initializer
        initialize =
            function(accessors =
                         c(
                             get = "curl",
                             post = "curl -X POST",
                             patch = "curl -X PATCH",
                             put = "curl -X PUT",
                             delete = "curl -X DELETE"
                         ),
                     api_token = NULL,
                     entrypoint = "https://api.prolific.co/api/v1/") {
                # Create Internals
                assign(
                    ".internals",
                    methods::new(
                        "..internals"
                    ),
                    .self
                )

                assign(
                    ".field_restrictions",
                    list(
                        accessors = list(typeof = "character", length = 5, names = c("get", "post", "patch", "put", "delete")),
                        api_token = list(typeof = "character", length = 1),
                        entrypoint = list(typeof = "character", length = 1)
                    ),
                    .self$.internals$fields
                )

                # Assign internal fields / methods
                assign("prescreeners", function(update = FALSE) .api_access.prescreeners(.self, update), .self$.internals$methods)
                assign("set_authorization", function() .api_access.set_authorization(.self), .self$.internals$methods)
                assign(".valid_authorization", FALSE, .self$.internals$fields)

                assign(
                    ".referer",
                    paste0(
                        "-H 'Referer: ",
                        "R_package_prolific_api/v", gsub("\\.", "_", sessionInfo()$otherPkgs$`prolific.api`$Version), "/",
                        "'"
                    ),
                    envir = .self$.internals$fields
                )

                # Assign fields
                entrypoint <<- entrypoint
                accessors <<- accessors
                api_token <<- api_token
            }
    )
)
