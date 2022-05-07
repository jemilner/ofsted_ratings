table_pec <- function(my_vec){
    round(table(my_vec) * 100 / length(my_vec), 0)
}
