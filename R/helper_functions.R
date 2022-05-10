table_pec <- function(my_vec){
    round(table(my_vec) * 100 / length(my_vec), 0)
}

table_prob <- function(my_vec){
    table(my_vec) / length(my_vec)
}
