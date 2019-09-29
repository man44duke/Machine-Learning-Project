generate_functions <- list.files("data-raw", full.names = TRUE)
generate_functions <- generate_functions[
  -grep("generate_all", generate_functions)
  ]

for(func_file in generate_functions){
  source(func_file)
}



