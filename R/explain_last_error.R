#' Error Explanation Using OpenAI
#'
#' @author Nathan C. Layman
#'
#' @param model (default "gpt-3.5-turbo") The OpenAI model to use for error explanation.
#' @param prompt (default instruction) Set the instructions for OpenAI model.
#' @param show_trace (logical, default F) Flag to decide whether the trace stack should be shown.
#' @param show_trace_JSON (logical, default F) Flag to decide whether the JSON representation of the trace stack should be shown.
#'
#' @return A string explaining the error returned by OpenAI model.
#'
#' @note This function requires an OPENAI_API_KEY to operate. Request one from https://platform.openai.com/api-keys and set it using Sys.setenv.
#'       Also, for the error stack trace, the function depends on rlang enriched errors.
#'
#' @examples
#' \dontrun{
#' tryCatch({
#'   # Trigger an error
#'   invalid_code
#' }, error = function(e) {
#'   # Use AI to understand error
#'   explain_last_error()
#' })
#' }
#' @export
explain_last_error <- function(model = "gpt-3.5-turbo",
                               prompt = "Give an explanation for the following error in the R programing language and use the provided stack trace to aid the user in resolving the problem. Do not ask for code snippets or any further information.",
                               show_trace = F,
                               show_trace_JSON = F) {

  if(!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    warning(cli::cli_text("OpenAI API key not found! To remedy:\n
            \t 1. Visit {.url https://platform.openai.com/api-keys} to create an account and request an API key\n
            \t 2. Run `Sys.setenv(OPENAI_API_KEY = 'sk-xxxx')` where 'sk-xxx' is the API key provided by OpenAI or add your key to your .env file or .RProfile \n
            \t 3. Try `explain_last_error()` again\n"))
    return()
  }

  e <- tryCatch({
    rlang::last_error()
  },
  error = function(e) {
    message("This function depends on rlang enriched errors. Please run `rlang::global_entrace()` or add it to your .RProfile before running whatever code generated the error you want to explain. Loading it now.`\n")
    rlang::global_entrace()
    return()
  })

  if(show_trace == T) {
    cat("Trace:\n")
    print(e$trace)
  }
  trace <- serialize_trace(e)
  if(show_trace_JSON == T) {
    message(paste("\nTrace JSON:", trace))
  }

  messages <-
    list(
      list(
        "role" = "system",
        "content" = prompt
      ),
      list(
        "role" = "user",
        "content" = paste("error:", e$message)
      ),
      list(
        "role" = "user",
        "content" = paste("JSON object representing the stack trace:", trace)
      )
    )

  err <- openai::create_chat_completion(
    model = model,
    messages = messages)['choices'][[1]]$message.content

  err <- paste("\nError:", e$message, "\n\nExplanation:", err |> unlist())

  message(err)
  invisible(err)
}
