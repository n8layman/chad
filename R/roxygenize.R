#' 'roxygenize' function is used to automatically construct roxygen headers for a given function.
#'
#' @author Nathan C. Layman
#'
#' @param func Function for which the roxygen documentation needs to be generated.
#' @param required_tags Vector of tag names required for the documentation. Default is c("author", "param", "return", "note", "example", "export").
#' @param provided_tags List of provided tags in the format (tag_name = "tag_content"). Default is list(author = "Nathan C. Layman").
#' @param model The model that will be used for generation. Default is "gpt-4".
#' @param prompt The initial prompt that guides the documentation generation. Default is "You parse a function definition to automatically construct roxygen headers."
#'
#' @return Prints out the constructed roxygen header.
#'
#' @note Requires OpenAI API key. Warning will be issued and function will terminate if it is not set.
#'
#' @examples
#' \dontrun{
#'   roxygenize(my_func, required_tags = c("author", "param", "return"), provided_tags = list(author = "Nathan C. Layman"), model = "gpt-4", prompt = "Automatically construct roxygen headers for my_func.")
#' }
#'
#' @export
roxygenize <- function(func,
                       required_tags = c("author",
                 "param",
                 "return",
                 "note",
                 "examples",
                 "export"),
                       provided_tags = list(author = "Nathan C. Layman"), # Named list of any provided tags as key-value pairs
                       model = "gpt-4",
                       prompt = "You parse a function definition to automatically construct roxygen headers returning only the header.") {

  if(!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    warning(cli::cli_text("OpenAI API key not found! To remedy:\n
            \t 1. Visit {.url https://platform.openai.com/api-keys} to create an account and request an API key\n
            \t 2. Run `Sys.setenv(OPENAI_API_KEY = 'sk-xxxx')` where 'sk-xxx' is the API key provided by OpenAI or add your key to your .env file or .RProfile \n
            \t 3. Try `explain_last_error()` again\n"))
    return()
  }

  # Figure out name of function
  func_name <- deparse(substitute(func))

  # Deparse the function code into a character vector
  func_code <- deparse(func)

  # Collapse the vector into a single string with newlines
  func_code_string <- paste("Here is the function definition:", func_code, sep = "\n", collapse = "\n")

  # Figure out function arguments
  func_args <- formals(func)
  params <- paste(names(func_args), collapse = ", ")

  required_tags <-  c("author",
                      "param",
                      "return",
                      "note",
                      "examples",
                      "export")

  # Dynamic context message for the model prompt
  context <- glue::glue(
    "Generate a complete Roxygen documentation header for the provided R function.
    The function is named '{func_name}' and has the following parameters: {params}.
    Come up with a title and description and include them at the top of the header every time.
    The required tags include: {paste(required_tags, collapse = ', ')} in that order.
    Please use the function definition provided to fill in the details for these tags.
    Separate diffent discrete tags with an empty roxygen header line. For example @author
    and @param should be separated but not different @param tags.
    Be concise but complete in your descriptions of the parameters, return values, and examples.
    Return only the required header and nothing else."
  )

  example <- "Here is an axample of what the response should look like:\n#' Preprocess GLW Data\n#'\n#' This function performs preprocessing tasks on glw datasets.\n#'\n#' @author Nathan C. Layman\n#'\n#' @param glw_directory_dataset Directory containing GLW datasets. This directory is created if it doesn't exist.\n#' @param glw_urls URLs of the GLW datasets to download.\n#' @param continent_raster_template Template to be used for terra raster operations.\n#' @param overwrite Boolean flag indicating whether existing preprocessed files should be overwritten. Default is FALSE.\n#' @param ... Additional parameters not used by this function but included for generic function compatibility.\n#'\n#' @return A character vector of filepaths to the preprocessed GLW dataset files.\n#'\n#' @note This function creates a new directory, downloads datasets, processes data and saves results\n#'       as parquet files in the specified directory.\n#'\n#' @examples\n#' preprocess_glw_data(glw_directory_dataset = './data',\n#'                     glw_urls = c('http://example.com/dataset1', 'http://example.com/dataset2'),\n#'                     continent_raster_template = raster_template,\n#'                     overwrite = TRUE)\n#'\n#' @export\n#' "

  messages <-
    list(
      list(
        "role" = "system",
        "content" = prompt
      ),
      list(
        "role" = "user",
        "content" = context
      ),
      list(
        "role" = "user",
        "content" = example
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

