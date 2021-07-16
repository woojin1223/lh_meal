`%!in%` <- Negate(`%in%`)

get_refined_menu <- function(x) {
    x %<>%
        str_replace_all(pattern = "\\( New\\)", replacement = "\\(New\\)") %>%
        str_replace_all(pattern = ".*자기 ?(개|계)발의 ?날.*", replacement = "자기개발의날") %>%
        str_replace_all(pattern = "가스", replacement = "까스") %>% # 돈가스 -> 돈까스
        str_replace_all(pattern = "모듬", replacement = "모둠") %>%
        str_replace_all(pattern = "베추", replacement = "배추") %>%
        str_replace_all(pattern = "류산슬", replacement = "유산슬") %>%
        str_replace_all(pattern = "자장", replacement = "짜장") %>%
        str_replace_all(pattern = "도너츠", replacement = "도넛") %>%
        str_replace_all(pattern = "커틀렛", replacement = "커틀릿") %>%
        str_replace_all(pattern = "씨리얼", replacement = "시리얼") %>%
        str_replace_all(pattern = "춘전", replacement = "춘천") %>%
        str_replace_all(pattern = "사과나무", replacement = "사과") %>%
        str_replace_all(pattern = "쥬스", replacement = "주스") %>%
        str_replace_all(pattern = "D", replacement = "드레싱") %>%
        str_replace_all(pattern = "S", replacement = "소스") %>%
        str_replace_all(pattern = "(탈탈|타르타르)(소스|D)?", replacement = "탈탈소스") %>% # 탈탈, 탈탈소스, 타르타르 등 -> 탈탈소스
        str_remove_all(pattern = "(\\([0-9가-힣:,./-]+\\)?|\\(?[0-9가-힣:,./-]+\\))") %>%   # (쌀: 국내산), (2/1컷팅해서-고명) 등 제거
        str_replace_all(pattern = "(\\* | \\*)", replacement = "\\*") %>% 
        str_replace_all(pattern = "\\*", replacement = "&") %>%
        str_remove_all(pattern = "\\&[가-힣]*(오리엔탈|칠리|머스타드|소스|드레싱|장|청|쌈|양념|케찹|소금)[가-힣]*") %>%
        str_replace_all(pattern = "/", replacement = " ") %>%  # 쌀밥/잡곡밥 -> 쌀밥 잡곡밥
        str_replace_all(pattern = ",", replacement = " ") %>% 
        str_replace_all(pattern = "  ", replacement = " ") %>% 
        str_trim() 
    
    return(x)
}

get_refined_menu_vec <- function(x) {
    x %<>% 
        get_refined_menu() %>% 
        str_split(pattern = " ") %>% 
        unlist()
    y <- c("", "&")
    
    return(x[x %!in% y])
}

get_score_dict <- function(x, y) {
    # x: menu vector, y: rate vector
    score_dict <- list()
    
    for (i in seq_along(x)) {
        refined_menu <- get_refined_menu_vec(x[i])
        
        for (menu in refined_menu) {
            if (menu %!in% names(score_dict)) {
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
        refined_menu <- get_refined_menu_vec(x[i])
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

get_menu_encoding_dict <- function() {
    # Kimchi: "[가-힣]+김치|[가-힣]+두기|[가-힣]+겉절이+|[가-힣]+박지"
    # Desert: "요구르트|[가-힣]+D|[가-힣]*음료|[가-힣]*호떡|식혜|청포도|바나나|복숭아|오렌지|수박|[가-힣]*토마토|[가-힣]+주스|[가-힣]*아이스티|[가-힣]+빵|[가-힣]*케익|요플레|[가-힣]*츄러스|[가-힣]*화채"
    # Salad: "[가-힣]*샐러드"
    # Sub_main: "[가-힣]*생채|[가-힣]*무침|[가-힣]*나물|[가-힣]*볶음|[가-힣]*구이|[가-힣]*찜|[가-힣*]+쌈|[가-힣]*조림|[가-힣]*잎지|[가-힣*]*초장|[가-힣]*감자|[가-힣]*고구마|[가-힣]*두부|[가-힣]*말랭이|[가-힣]*파래김|[가-힣]*떡볶이|[가-힣]*부추전|[가-힣]*숙회|[가-힣]*스틱|탕평채|[가-힣]*냉채|[가-힣*]*양념장|[가-힣*]*잡채"
    # Pork: '장조림|바베큐|떡갈비|장육|수육|폭립|차돌|족발|보쌈|돼지|제육|돈육|[^소]불고기|돈|목살|돼지갈비|삼겹|깐풍|유린기|탕수육|두루치기|등갈비|동파육'
    # Beef: '장조림|바베큐|떡갈비|언양식|소고기|쇠|소불고기|쇠|쇠고기|우육|소갈비'
    # Chicken: '후라이드|윙|닭|치킨'
    # Duck: '오리'
    # Processed_meat(가공육): '미트|소세지|너비아니|함박|완자'
    # Fish: '연어|장어|열기|고등어|삼치|꽁치|갈치|가자미|굴비|조기|생선|임연수|동태|명태|코다리|홍어|황태|참치|멸치|적어'
    # Clam: "조개|골뱅이|전복|굴[^(소스)]"
    # Shrimp: "새우|랍스터|꽃게"
    # Squid: '골뱅이|낙지|오징어|쭈꾸미|주꾸미|문어'
    # Cooked_seafood: '유산슬|해물전|찜|탕수어|맛살'
    # Noodle: "파스타|스파게티|쫄면|라면|짬뽕\\b|국수|우동\\b|[^고]사리|나가사키면"
    # China: "중식|중국|유산슬|짬뽕|짜장|탕수육|마파두부|오향장육|깐풍기|팔보채|팔보반|라조기|기스면|꿔봐로우|"
    # Korea: ""
    # Japan: "일본|스시|교자|까스|초밥|우동\\b|스키야키|라멘|일식|소바|가라아게|야(키|끼)|사시미|샤브샤브|나베|오니기리|(규|츠|타|코|텐|슈|케|쿠|끼|니|비|나|게|카)동"
    # Italy: "이탈리아|이태리|파스타|스파게티|피자|스테이크"
    # School: "볶이|김밥|순대|라면|닭강정|[가-힣]*튀김[^(우동)]"
    
    # --- 추가 ---
    # 없음
    
    dict <- list()
    dict[["Kimchi"]]         <- "[가-힣]+김치|[가-힣]+두기|[가-힣]+겉절이+|[가-힣]+박지"
    dict[["Desert"]]         <- "요거트|쥬시쿨|요구르트|[가-힣]+D|[가-힣]*음료|[가-힣]*호떡|식혜|청포도|바나나|복숭아|오렌지|수박|[가-힣]*토마토|[가-힣]+주스|[가-힣]*아이스티|[가-힣]+빵|[가-힣]*케익|요플레|[가-힣]*츄러스|[가-힣]*화채|푸딩"
    dict[["Soup"]]           <- ".국\\b|.탕\\b"
    dict[["Stew"]]           <- "[가-힣]+찌개\\b"
    dict[["Salad"]]          <- "[가-힣]*샐러드"
    dict[["Sub_main"]]       <- "[가-힣]*생채|[가-힣]*무침|[가-힣]*나물|[가-힣]*볶음|[가-힣]*구이|[가-힣]*찜|[가-힣*]+쌈|[가-힣]*조림|[가-힣]*잎지|[가-힣*]*초장|[가-힣]*감자|[가-힣]*고구마|[가-힣]*두부|[가-힣]*말랭이|[가-힣]*파래김|[가-힣]*떡볶이|[가-힣]*부추전|[가-힣]*숙회|[가-힣]*스틱|탕평채|[가-힣]*냉채|[가-힣*]*양념장|[가-힣*]*잡채"
    dict[["Pork"]]           <- "장조림|바베큐|떡갈비|장육|수육|폭립|차돌|족발|보쌈|돼지|제육|돈육|[^소]불고기|돈|목살|돼지갈비|삼겹|깐풍|유린기|탕수육|두루치기|등갈비|동파육"
    dict[["Beef"]]           <- "장조림|바베큐|떡갈비|소고기|쇠|소불고기|쇠|쇠고기|우육|소갈비"
    dict[["Chicken"]]        <- "후라이드|윙|닭|치킨|백숙|삼계"
    dict[["Duck"]]           <- "오리"
    dict[["Processed_meat"]] <- "미트|소세지|너비아니|함박|완자|햄"
    dict[["Fish"]]           <- "연어|장어|열기|고등어|삼치|꽁치|갈치|가자미|굴비|조기|생선|임연수|동태|명태|코다리|홍어|황태|참치|멸치|적어|대구"
    dict[["Clam"]]           <- "조개|골뱅이|전복|굴[^(소스)]"
    dict[["Shrimp"]]         <- "새우|랍스터|꽃게|크랩"
    dict[["Squid"]]          <- "골뱅이|낙지|오징어|쭈꾸미|주꾸미|문어"
    dict[["Cooked_seafood"]] <- "유산슬|해물전|탕수어"
    dict[["Noodle"]]         <- "파스타|스파게티|.면|짬뽕\\b|국수|우동\\b|[^고]사리"
    dict[["China"]]          <- "중식|중국|유산슬|짬뽕|짜장|탕수육|마파두부|오향장육|깐풍기|팔보채|팔보반|라조기|기스면|꿔바로우"
    # dict[["Korea"]]        <- ""
    dict[["Japan"]]          <- "일본|스시|교자|까스|초밥|우동\\b|라멘|일식|소바|가라아게|야(키|끼)|사시미|샤브샤브|나베|오니기리|(규|츠|타|코|텐|슈|케|쿠|끼|니|비|나|게|카)동"
    dict[["Italy"]]          <- "이탈리아|이태리|파스타|스파게티|피자|스테이크"
    dict[["School"]]         <- "볶이|김밥|순대|라면|닭강정|[가-힣]*튀김[^(우동)]"
    dict[["Nuts"]]           <- "견과류|호두|땅콩|잣|아몬드"
    dict[["Bread"]]          <- "빵|꽈배기|도넛"
    dict[["Egg"]]            <- "계란"
    dict[["Vegetable"]]      <- "야채|채소|파프리카"
    dict[["Rice_cake"]]      <- "떡|부꾸미"
    
    
    
    return(dict)
}

menu_encoding_dict <- get_menu_encoding_dict()

get_menu_encoding <- function(x, y, dict) {
    # x: menu_encoding 변수를 추가할 data
    # y: "Lunch" or "Dinner"
    # dict: menu_encoding_dict
    
    refined_menu <- x[[y]] %>%  get_refined_menu()
    
    for (menu in names(dict)) {
        var_name <- paste0(y, "_", menu)
        x[[var_name]] <- refined_menu %>% 
            str_extract_all(pattern = dict[[menu]]) %>% 
            sapply(FUN = length)
    }
    
    return(x)
}

jaccard <- function(x, y) {
    if (sum(x)+sum(y) == 0) {
        return(0) 
    } else {
        return(sum(pmin(x, y)) / (sum(x)+sum(y)))
    }
}
