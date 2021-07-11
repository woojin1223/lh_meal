get_refined_menu <- function(x) {
    x %<>%
        str_replace_all(pattern = "\\( New\\)", replacement = "\\(New\\)") %>%
        str_replace_all(pattern = ".*자기 ?(개|계)발의 ?날.*", replacement = "자기개발의날") %>%
        str_replace_all(pattern = "가스", replacement = "까스") %>% # 돈가스 -> 돈까스
        str_replace_all(pattern = "모듬", replacement = "모둠") %>%
        str_replace_all("(탈탈|타르타르)(소스|D)?", "탈탈소스") %>% # 탈탈, 탈탈소스, 타르타르 등 -> 탈탈소스
        str_remove_all(pattern = "(\\([0-9가-힣:,./-]+\\)?|\\(?[0-9가-힣:,./-]+\\))") %>%  # (쌀: 국내산), (2/1컷팅해서-고명) 등 제거 
        str_replace_all(pattern = "(\\* | \\*)", replacement = "\\*") %>% 
        str_replace_all(pattern = "\\*", replacement = "&") %>%
        str_replace_all(pattern = "/", replacement = " ") %>%  # 쌀밥/잡곡밥 -> 쌀밥 잡곡밥
        str_replace_all(pattern = ",", replacement = " ") %>% 
        str_replace_all(pattern = "  ", replacement = " ") %>% 
        str_trim() %>% 
        str_split(pattern = " ") %>% 
        unlist() %>% 
        setdiff(c("", "&"))
    
    return(x)
}

get_score_dict <- function(x, y) {
    # x: menu vector, y: rate vector
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
        n <- 0
        
        if (length(refined_menu) == 0) next
        
        for (menu in refined_menu) {
            if (!is.null(dict[[menu]])) {
                score[i] <- score[i] + dict[[menu]]
                n <- n + 1
            }
        }
        
        if (n > 0) score[i] <- score[i] / n
    }
    
    return(score)
}

get_new_menu <- function(x) {
    return(x %>% str_detect(pattern = "New") %>% as.integer())
}

