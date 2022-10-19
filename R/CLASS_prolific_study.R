#' @importFrom methods new
NULL

#' Prolific study
#'
#' @name prolific_study
#' @aliases prolific_study-class
#'
#' @description
#' Class that represents Prolific studies, such that they can be transferred to or from the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API}.
#' This allows to create, review and update studies.\cr
#' \emph{The fields and methods are available as in RefClass or S4 objects (see examples and the \href{../doc/prolificapi-package.html}{prolific.api package vignette}).}
#'
#' API access to interact with the Prolific platform is done by using objects from the \code{\link[=api_access]{api_access class}}, i.e.
#' \code{\link[=prolific_study]{prolific_studies}} are intended to be transferred as bodies in calls to the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API} (see examples).
#'
#' @field id (\code{\link[=character]{character}}):\cr
#' The study's ID on Prolific.\cr
#' \strong{Note: This ID is set by Prolific and can not be changed by the user}\cr (see the '\emph{Further (read-only) fields}' section below).
#'
#' @field name (\code{\link[=character]{character}}):\cr
#' Public name or title of the study \emph{(will be publicly visible when publishing the study).}
#'
#' @field internal_name (\code{\link[=character]{character}}):\cr
#' Internal name of the study \emph{(not shown to participants)}.
#'
#' @field description (\code{\link[=character]{character}}):\cr
#' Description of the study \emph{(will be publicly visible when publishing the study).}
#'
#' @field external_study_url (\code{\link[=character]{character}}):\cr
#' URL of the survey or experiment the participants will be redirected to \emph{(will be publicly visible when publishing the study).}\cr
#' \strong{Note:}
#' \itemize{
#'      \item{The URL must be valid at the time the study is created on the Prolific platform.}
#'      \item{For the use of URL parameters, see field \code{url_parameters}.}
#' }
#'
#' @field url_parameters (\code{\link[=list]{list}}):\cr
#' A named list of URL parameters that is appended to \code{external_study_url}.
#' The default
#' \preformatted{list(
#'      prolific_id = "{\%PROLIFIC_PID\%}",
#'      study_id = "{\%STUDY_ID\%}",
#'      session_id = "{\%SESSION_ID\%}"
#' )
#' }
#' is used for passing the participant's, study's and session's ID from Prolific to the data collection website.
#'
#' @field prolific_id_option (\code{\link[=character]{character}}):\cr
#' This determines the method of passing the respondent's Prolific ID.\cr
#' \strong{Valid options are:}
#' \itemize{
#'      \item{\code{"url_parameters"} }{for passing the ID as URL parameter \code{{\%PROLIFIC_PID\%}}}
#'      \item{\code{"question"} }{for letting the respondents enter their ID (e.g. via copy & paste), or}
#'      \item{\code{"not_required"} }{if the Prolific ID is not to be passed.}
#' }
#'
#' @field completion_code (\code{\link[=character]{character}}):\cr
#' The completion code that is provided to participants after completing the study.
#' This code is used to prove that a participant completed the study.
#' It is therefore \emph{visible for participants after completing the study.}
#'
#' @field completion_option (\code{\link[=character]{character}}):\cr
#' This determines the method for passing the \code{completion_code}.\cr
#' \strong{Valid options are:}
#' \itemize{
#'      \item{\code{"url"}}{for passing the code as URL parameter when redirecting participants back to Prolific after completing the study, or}
#'      \item{\code{"code"}}{for providing a code for copy and paste.}
#' }
#'
#' @field total_available_places (\code{\link[=integer]{integer}}):\cr
#' The number of participant you would like to recruit
#' in the study \emph{(will be publicly visible when publishing the study).}
#'
#' @field estimated_completion_time (\code{\link[=integer]{integer}}):\cr
#' The estimated time it takes to complete the study, \emph{in minutes} \emph{(will be publicly visible when publishing the study).}
#'
#' @field maximum_allowed_time (\code{\link[=integer]{integer}}):\cr
#' The maximum allowed time for participants to complete the study, \emph{in minutes}.
#'
#' @field reward (\code{\link[=integer]{integer}}):\cr
#' The amount of money (in pence)
#' you pay for completing the study \emph{(will be publicly visible when publishing the study).}\cr
#' \strong{Note:} Compensation...
#'
#' @field eligibility_requirements (\code{\link[=list]{list}}):\cr
#' A list containing \code{\link[=prolific_prescreener]{prolific_prescreener objects}} that characterize the participants to be recruited.
#' \strong{Note:}
#' \itemize{
#'  \item{\code{\link[=NULL]{NULL}} means that every participant can see and complete the study.}{}
#'  \item{\strong{Only persons fulfiling these requirements will be able to participate in the study.}}{}
#' }
#'
#' @field device_compatibility (\code{\link[=character]{character}}):\cr
#' \strong{Note:} \code{\link[=NULL]{NULL}} means that all options are available.
#'
#' @field peripheral_requirements (\code{\link[=character]{character}}):\cr
#' A vector of technical requirements that participants have to fulfill to complete the study.
#' One or multiple values from
#' \preformatted{   c("audio", "camera", "download", "microphone")}
#' \strong{Note:} \code{\link[=NULL]{NULL}} means that none of the requirements is needed.
#'
#'
#' @field naivety_distribution_rate (\code{\link[=numeric]{numeric}}):\cr
#' A value between \code{1} and \code{0} that controls the balance between speed of your study and the naivety of the participants.\cr
#' Prolific's description of this field is rather vague, but it seems to imply that
#' \itemize{
#'      \item{\code{1} }{means that less trained or 'professional' participants will have access to the study.}
#'      \item{\code{0} }{means that all eligible participants will have access to the study at the same time.}
#'      \item{\code{values between 0 and 1} }{represent a tradeoff between both options.}
#' }
#'
#' @field further_fields (\code{\link[=list]{list}}):\cr
#' Prolific studies can have various further fields, which (if used) are stored in \code{\link[=prolific_study]{further_fields}}.
#' These fields are read-only,  and determined by Prolific.
#' See the '\emph{Further (read-only) fields}' section below for a list of these read-only fields.
#'
#' @field \dots (\link[=dots]{further arguments}):\cr
#' Will be added to the \code{\link[=prolific_study]{further_fields}} field of the \code{\link[=prolific_study]{prolific_study}}
#' (see above).
#'
#' @description
#' # Types of fields
#' \describe{
#'       \item{Required fields}{
#'           are required for creating a study on Prolific.\cr
#'           \strong{The values for all of these except \code{completion_option} and \code{prolific_id_option} should be specified before publishing a study. Default values are only placeholders.}
#'      }
#' }
#' \describe{
#'      \item{Optional fields}{
#'           are writable, but optional for Prolific.\cr
#'           The user can but does not have to set these fields when creating a study.\cr
#'           The required and optional fields are:
#'      }
#' }
#'           \tabular{lll}{
#'      		    ```           ```\tab \strong{Required fields}		\tab	\strong{Optional fields}		\cr
#'           		\tab \code{completion_code}		\tab	\code{device_compatibility}		\cr
#'           		\tab \code{completion_option}		\tab	\code{internal_name}		\cr
#'           		\tab \code{description}		\tab	\code{maximum_allowed_time}		\cr
#'           		\tab \code{eligibility_requirements}		\tab	\code{naivety_distribution_rate}		\cr
#'           		\tab \code{estimated_completion_time}		\tab	\code{peripheral_requirements}		\cr
#'           		\tab \code{external_study_url}		\tab	\code{url_parameters}		\cr
#'           		\tab \code{name}				\cr
#'           		\tab \code{prolific_id_option}				\cr
#'           		\tab \code{reward}				\cr
#'           		\tab \code{total_available_places}				\cr
#'           }
#'
#' \describe{
#'      \item{Further (read-only) fields}{
#'            contain information that is determined internally by Prolific and read-only.\cr
#'            The \code{id}-field
#'            is of particular relevance.
#'            Once a study is created via \href{https://docs.prolific.co/docs/api-docs/public/}{API} access, it is \strong{obtained from the \href{https://docs.prolific.co/docs/api-docs/public/}{API} and
#'            stored in the \code{prolific_study} object}, since it can be used to update, manage or delete a study.\cr
#'            To fully represent the information that is obtainable from the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API},
#'            the \code{further_fields} list can contain some or all of the entries listed below.
#'            The corresponding overview provided in the
#'            \href{https://docs.prolific.co/docs/api-docs/public/#tag/Studies/paths/~1api~1v1~1studies~1%7Bid%7D~1/get}{Prolific API documentation}
#'            currently seems to be work in progress.
#'      }
#' }
#'      \tabular{lll}{
#'      		```           ``` \tab \code{_links }	\tab	\code{average_reward_per_hour}			\cr
#'      		\tab \code{average_reward_per_hour_without_adjustment}	\tab	\code{average_time_taken}			\cr
#'      		\tab \code{currency_code}	\tab	\code{date_created}			\cr
#'      		\tab \code{device_compatibility}	\tab	\code{discount_from_coupons}			\cr
#'      		\tab \code{eligible_participant_count}	\tab	\code{estimated_reward_per_hour}			\cr
#'      		\tab \code{fees_per_submission}	\tab	\code{fees_percentage}			\cr
#'      		\tab \code{has_had_adjustment}	\tab	\code{internal_name}			\cr
#'      		\tab \code{is_pilot}	\tab	\code{is_underpaying}			\cr
#'      		\tab \code{last_email_update_sent_datetime}	\tab	\code{maximum_allowed_time}			\cr
#'      		\tab \code{minimum_reward_per_hour}	\tab	\code{naivety_distribution_rate}			\cr
#'      		\tab \code{number_of_submissions}	\tab	\code{peripheral_requirements}			\cr
#'      		\tab \code{pilot_test_steps_state}	\tab	\code{places_taken}			\cr
#'      		\tab \code{project}	\tab	\code{publish_at}			\cr
#'      		\tab \code{published_at}	\tab	\code{publisher}			\cr
#'      		\tab \code{quota_requirements}	\tab	\code{receipt}			\cr
#'      		\tab \code{representative_sample}	\tab	\code{representative_sample_fee}			\cr
#'      		\tab \code{researcher}	\tab	\code{reward_level}			\cr
#'      		\tab \code{share_id}	\tab	\code{stars_remaining}			\cr
#'      		\tab \code{status}	\tab	\code{study_type}			\cr
#'      		\tab \code{total_cost}	\tab	\code{total_participant_pool}			\cr
#'      		\tab \code{vat_percentage}	\tab	\code{workspace}			\cr
#'      }
#'
#'
#' @examples
#' library(prolific.api)
#'
#' prolific_api_access <- api_access(api_token = "<api_token>")
#'
#' # Create a new study
#' fancy_new_study <- prolific_study(
#'     name = "A fancy study on Prolific",
#'     external_study_url = "https://www.my_fancy_study_url.com",
#'     completion_code = "123ab456cd78",
#'     eligibility_requirements = list(),
#'     estimated_completion_time = 1,
#'     reward = 1,
#'     total_available_places = 0
#' )
#'
#' # Check the study's validity
#' print(fancy_new_study$validity_check())
#'
#' # Whoops, better add a description and change the total_available_places,
#' # using RefClass and S4 methods for illustration
#' # both are equivalent, so only one of the two commands is required in practice
#' # RefClass variant
#' fancy_new_study$total_available_places <- 1L
#' # S4 variant
#' total_available_places(fancy_new_study) <- 1L
#'
#' # RefClass variant
#' fancy_new_study$description <- "A fancy description"
#' # S4 variant
#' description(fancy_new_study) <- "A fancy description"
#'
#' # Re-Check the study's validity
#' print(fancy_new_study$validity_check())
#'
#' # Note: For the following code to work,
#' # you have to replace <api_token> in the code above by the actual API token
#'
#' \dontrun{
#' # Post the 'fancy_new_study' to Prolific - i.e. create it as a draft study on the platform
#' output_of_post <- prolific_api_access$access(
#'     endpoint = "studies",
#'     method = "post",
#'     data = fancy_new_study
#' )
#'
#' # Success: fancy_new_study got an ID - it is now a draft study on Prolific!
#' fancy_new_study$id
#'
#' # Note: The output of the access() command with a prolific_study object as `data` argument
#' # is a pointer to this prolific_study object.
#' # The prolific_study object is updated by reference
#' print(tracemem(output_of_post) == tracemem(fancy_new_study))
#'
#' # Change the study's name
#' name(fancy_new_study) <- "A NEW name for 'fancy_new_study'"
#'
#' # Update (patch) the study on Prolific,
#' # using S4 methods for illustration
#' output_of_patch <- access(
#'     prolific_api_access,
#'     endpoint = c("studies", id(fancy_new_study)),
#'     method = "patch",
#'     data = fancy_new_study
#' )
#'
#' # Note: As above, the output of the access() command is a pointer to the prolific_study object.
#' print(tracemem(output_of_post) == tracemem(fancy_new_study))
#'
#' # Delete fancy_new_study
#' prolific_api_access$access(
#'     endpoint = c("studies", id(fancy_new_study)),
#'     method = "delete",
#'     as_list = FALSE
#' )
#' }
#' @exportClass prolific_study
#' @export prolific_study
prolific_study <- setRefClass(
    Class = "prolific_study",
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Fields Block (setter/getter functions)
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    fields = list(
        completion_code = function(value) .accessField("completion_code", value, .self$.internals$fields, TRUE),
        completion_option = function(value) .accessField("completion_option", value, .self$.internals$fields, TRUE),
        description = function(value) .accessField("description", value, .self$.internals$fields, TRUE),
        device_compatibility = function(value) .accessField("device_compatibility", value, .self$.internals$fields, TRUE),
        eligibility_requirements = function(value) {
            if (!(missing(value))) {
                if (length(class(value)) == 1 & all(class(value) == "list")) {
                    class(value) <- c(class(value), "eligibility_requirements")
                }
                if (!is.null(value)) {
                    if (length(value) > 0) {
                        names(value) <- vapply(value, function(x) x$title, "a")
                    }
                }
            }

            .accessField("eligibility_requirements", value, .self$.internals$fields, TRUE)
        },
        estimated_completion_time = function(value) .accessField("estimated_completion_time", value, .self$.internals$fields, TRUE),
        external_study_url = function(value) .accessField("external_study_url", value, .self$.internals$fields, TRUE),
        further_fields = function(value) .accessField("further_fields", value, .self$.internals$fields, TRUE),
        id = function(value) .accessField("id", value, .self$.internals$fields, TRUE),
        internal_name = function(value) .accessField("internal_name", value, .self$.internals$fields, TRUE),
        maximum_allowed_time = function(value) .accessField("maximum_allowed_time", value, .self$.internals$fields, TRUE),
        naivety_distribution_rate = function(value) .accessField("naivety_distribution_rate", value, .self$.internals$fields, TRUE),
        name = function(value) .accessField("name", value, .self$.internals$fields, TRUE),
        peripheral_requirements = function(value) .accessField("peripheral_requirements", value, .self$.internals$fields, TRUE),
        project = function(value) .accessField("project", value, .self$.internals$fields, TRUE),
        prolific_id_option = function(value) .accessField("prolific_id_option", value, .self$.internals$fields, TRUE),
        reward = function(value) .accessField("reward", value, .self$.internals$fields, TRUE),
        total_available_places = function(value) .accessField("total_available_places", value, .self$.internals$fields, TRUE),
        url_parameters = function(value) .accessField("url_parameters", value, .self$.internals$fields, TRUE)
    ),
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Methods Block
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    methods = list(
        # Initializer
        initialize =
            function(completion_code = NULL,
                     completion_option = "url",
                     description = NULL,
                     device_compatibility = c("desktop", "mobile", "tablet"),
                     eligibility_requirements = list(),
                     estimated_completion_time = 1,
                     external_study_url = NULL,
                     id = NA_character_,
                     internal_name = "",
                     maximum_allowed_time = 100L,
                     naivety_distribution_rate = 1,
                     name = NULL,
                     peripheral_requirements = NULL,
                     project = NA_character_,
                     prolific_id_option = "url_parameters",
                     reward = 1L,
                     total_available_places = 1L,
                     url_parameters = list(prolific_id = "{%PROLIFIC_PID%}", study_id = "{%STUDY_ID%}", session_id = "{%SESSION_ID%}"),
                     ...) {

                # Create Internals
                assign(
                    ".internals",
                    methods::new(
                        "..internals"
                    ),
                    .self
                )

                # Define Attribute restrictions
                assign(
                    ".field_restrictions",
                    list(
                        completion_code = list(typeof = "character", length = 1),
                        completion_option = list(typeof = "character", length = 1),
                        description = list(typeof = "character", length = 1),
                        device_compatibility = list(typeof = "character"),
                        eligibility_requirements = list(class = c("list", "eligibility_requirements")),
                        estimated_completion_time = list(typeof = "double", length = 1),
                        external_study_url = list(typeof = "character", length = 1),
                        further_fields = list(typeof = "list"),
                        id = list(typeof = "character", length = 1),
                        internal_name = list(typeof = "character", length = 1),
                        maximum_allowed_time = list(typeof = "double", length = 1),
                        naivety_distribution_rate = list(typeof = "double"),
                        name = list(typeof = "character", length = 1),
                        peripheral_requirements = list(typeof = "character"),
                        project = list(typeof = "character", length = 1),
                        prolific_id_option = list(typeof = "character", length = 1),
                        reward = list(typeof = "integer", length = 1),
                        total_available_places = list(typeof = "integer", length = 1),
                        url_parameters = list(typeof = "list")
                    ),
                    .self$.internals$fields
                )

                # Assign fields
                completion_code <<- completion_code
                completion_option <<- completion_option
                description <<- description
                device_compatibility <<- device_compatibility
                eligibility_requirements <<- eligibility_requirements
                estimated_completion_time <<- as.double(estimated_completion_time)
                external_study_url <<- external_study_url
                id <<- id
                internal_name <<- internal_name
                maximum_allowed_time <<- as.double(maximum_allowed_time)
                naivety_distribution_rate <<- naivety_distribution_rate
                name <<- name
                peripheral_requirements <<- as.character(peripheral_requirements)
                project <<- project
                prolific_id_option <<- prolific_id_option
                reward <<- as.integer(reward)
                total_available_places <<- as.integer(total_available_places)
                url_parameters <<- url_parameters

                further_fields <<- list(...)

                # Convert URL parameters that are already part of the URL
                if (!is.null(external_study_url)) {
                    # (as Prolific does not return URL and parameters separately)
                    if (grepl("\\?", external_study_url)) {
                        add_url_parameters <- .decollapse_url_parameters(gsub("(.*?)\\?(.*)", "\\2", external_study_url))
                        url_parameters <<- c(url_parameters[!names(url_parameters) %in% names(add_url_parameters)], add_url_parameters)

                        external_study_url <<- gsub("(.*?)\\?(.*)", "\\1", external_study_url)
                    }
                }

                assign("output", function(x) .output_prolific_study(.self, x), .self$.internals$methods)
            }
    )
)
