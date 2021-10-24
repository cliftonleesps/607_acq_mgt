library(tidyverse)

# Web libraries
library(curl)
library(jsonlite)

# for presenting tables
library(kableExtra)



# NYTimes API endpoint for the hard copy fiction
url <- 'https://api.nytimes.com/svc/books/v3/lists.json?list=hardcover-fiction&api-key=BjKZARyl69wHghJgKnvY3GLPJTh1XcAo'

# create a curl handle for connection
curl_handle <- new_handle()
handle_setopt(curl_handle, customrequest="GET")

# Connect to the API
json_request <- curl_fetch_memory(url, handle=curl_handle)

# Retrieve the request content
json_result <- fromJSON(rawToChar(json_request$content))

json_tibble <- as_tibble(json_result, validate = F) 

# use the results column for the book details
books <- bind_rows( json_tibble$results$book_details )

kable(books) %>%
  kable_styling(latex_options = "scale_down")
