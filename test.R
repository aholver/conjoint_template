

forced_choice <- function(n,q,o, attr_list){
# n: number of participants; q: number of questions; o: number of options; attr_list: a list of vectors of attributes
    df <- data.frame(respID = c(rep(1:n, each = q*o, times = 1)),
                     qID = c(rep(1:q, each = o, times = n)),
                     altID = c(rep(1:o, times = n*q)))
    # df$OP <- NA
    # df$build <- NA
    # df$price <- NA
    
    all_comb <- expand.grid(attr_list)
    for (i in 1:n){
        rand_rows <- sample(1:nrow(all_comb), size = q*o) #two questions, three alternatives
        rand_alts <- all_comb[rand_rows, ]
        df[df$respID == i, 4:(3+length(attr_list))] <- rand_alts
    } 
    return(df)
}

attributes <- list(type = c("Margherita","Caprese","Salame","Bianca","Prosciutto"),
     price = c(7.5, 8.5, 8.0, 9.0),
     crust = c("thick","thin","average"))

choice_questions <- forced_choice(2000,2,3,attributes)

# add .jpg for images
choice_questions <- choice_questions %>% 
    mutate(image = paste0(str_to_lower(type),".jpg"))

write_csv(choice_questions, "/Users/arneholverscheid/Dropbox/Projects/Armenia/choice_questions.csv")




