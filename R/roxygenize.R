#' Parse a Function to Construct Roxygen Headers
#'
#' @author Nathan C. Layman
#'
#' @param func The function to parse
#' @param required_tags A vector specifying the tags to include. Default is c("title", "author", "param", "return", "note", "example", "export")
#' @param provided_tags A list of tags and their associated values to include in the roxygen header. Default is list(author = "Nathan C. Layman")
#' @param model An OpenAI model to use. Default is "gpt-3.5-turbo".
#' @param prompt The prompt to guide the OpenAI model. Default is a specific instruction for roxygen header creation.
#'
#' @return The roxygen header of the given function.
#'
#' @note OpenAI API key is required to run this function. If the API key is not set, a warning message is given with instructions on where to get the API key and how to set it.
#'
#' @example
#' \dontrun{
#' function_definition <- function(x) { return(x^2) }
#' roxygen_header <- create_roxygen(function_definition)
#' print(roxygen_header)
#' }
#'
#' @export
roxygenize <- function(func,
                       required_tags = c("author",
                                         "param",
                                         "return",
                                         "note",
                                         "example",
                                         "export"),
                       provided_tags = list(author = "Nathan C. Layman"), # Named list of any provided tags as key-value pairs
                       model = "gpt-4",
                       prompt = "You parse a function definition to automatically construct roxygen headers.") {

  if(!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    warning(cli::cli_text("OpenAI API key not found! To remedy:\n
            \t 1. Visit {.url https://platform.openai.com/api-keys} to create an account and request an API key\n
            \t 2. Run `Sys.setenv(OPENAI_API_KEY = 'sk-xxxx')` where 'sk-xxx' is the API key provided by OpenAI or add your key to your .env file or .RProfile \n
            \t 3. Try `explain_last_error()` again\n"))
    return()
  }

  # Deparse the function code into a character vector
  func_code <- deparse(func)

  # Collapse the vector into a single string with newlines
  func_code_string <- paste("Here is the function definition:", func_code, sep = "\n", collapse = "\n")

  required_tags <-  c("author",
                      "param",
                      "return",
                      "note",
                      "example",
                      "export")

  context <- glue::glue("Come up with a title for the function and return the {paste(required_tags, collapse = ', ')} tags in that order. If possible fill out tag details using the provided function definition or using the details provided. Separate tags with empty roxygen lines. Only return the header not the function code or any other text.")

  messages <-
    list(
      list(
        "role" = "system",
        "content" = prompt
      ),
      list(
        "role" = "user",
        "content" = context
      )
    )

  if(!is.null(provided_tags)) {
    provided_tags_message <- paste(paste0("@",names(provided_tags)), provided_tags, sep = " ", collapse = ", ")
    provided_tags_message <- glue::glue("Provided tags: {provided_tags_message}")
    messages <- c(messages, list(list("role" = "user", "content" = provided_tags_message)))
  }

  messages <- c(messages, list(list(
    "role" = "user",
    "content" = func_code_string
  )))

  roxygen_header <- openai::create_chat_completion(
    model = model,
    messages = messages)['choices'][[1]]$message.content

  cat(roxygen_header)
}

