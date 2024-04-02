#' Use ChatGPT to explain the last error
#'
#' @param model Which version of chatgpt would should the error be submitted to?
#' @param prompt What system prompt should be used to interpret the error and stack trace?
#' @param show_trace_JSON Whether to display the serialized trace
#'
#' @return
#' @export
#'
#' @examples
explain_last_error <- function(model = "gpt-3.5-turbo",
                               prompt = "You provide human readable debugging help for the following error and stack trace in the R programing language.",
                               show_trace = T,
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
    print(e$trace)
  }
  trace <- serialize_trace(e)
  if(show_trace_JSON == T) {
    message(paste("Trace JSON:", trace))
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
