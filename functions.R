get_refined_menu <- function(x) {
    x %<>% 
        str_remove_all(pattern = "\\(.{1,10}:.{1,10}\\)") %>%  # (쌀: 국내산) 등 제거 
        str_replace_all(pattern = "/", replacement = " ") %>%  # 쌀밥/잡곡밥 -> 쌀밥 잡곡밥
        str_replace_all(pattern = "  ", replacement = " ") %>% 
        str_replace_all(pattern = ",", replacement = " ") %>% 
        str_trim() %>% 
        str_split(pattern = " ") %>% 
        unlist() %>% 
        setdiff("")
    
    return(x)
}

get_score_dict <- function(x, y) {
    # x: menu vector, y: score vector
    score_dict <- list()
    
    for (i in seq_along(x)) {
        refined_menu <- get_refined_menu(x[i])
        
        for (menu in refined_menu) {
            if (!(menu %in% names(score_dict))) {
                score_dict[[menu]] <- list(total_score = 0, freq = 0)
            }
            
            score_dict[[menu]]$total_score <- score_dict[[menu]]$total_score + y[i]
            score_dict[[menu]]$freq <- score_dict[[menu]]$freq + 1
        }
    }
    
    for (key in names(score_dict)) {
        score_dict[[key]] <- score_dict[[key]]$total_score / score_dict[[key]]$freq
    }
    
    return(score_dict)
}

get_score <- function(x, dict) {
    score <- rep(0, length(x))
    
    for (i in seq_along(x)) {
        refined_menu <- get_refined_menu(x[i])
        n <- length(refined_menu)
        
        if (n == 0) next
        
        for (menu in refined_menu) {
            score[i] <- score[i] + dict[[menu]]    
        }
        
        score[i] <- score[i] / n
    }
    
    return(score)
}