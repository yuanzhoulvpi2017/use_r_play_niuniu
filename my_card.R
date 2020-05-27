# my first code 
# library(tidyverse)
# library(stringr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 生成扑克

n_card <- rep(c('A', 2:10,'J', 'Q', 'K'), each = 4)
type_card <- rep(c("meihua", "fangkuai", "hongtao", "heitao"), time = 13)
all_card <- paste( n_card, type_card, sep = '_')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 洗牌
random_card <- all_card[sample(x = seq_along(all_card), size = 50, replace = FALSE)]

#这是一场
this_is_a_chunk <- lapply(X = 1:10, FUN = function(i){random_card[(i*5-4):(i*5)]})


# card_parts <- this_is_a_chunk[[1]]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 计算每5张得到的数据
cal_cards_value <- function(card_parts) {
  this_part_detail <- stringr::str_split(card_parts, pattern = '_')
  num_part <- unlist(lapply(this_part_detail, FUN = function(i){i[1]}))
  color_part <- unlist(lapply(this_part_detail, FUN = function(i){i[2]}))
  # 计算分数
  # 将纸牌的数值转换为数字
  trans_card_to_num <- function(single_num_part) {
    if (single_num_part %in% c("J", "Q", "K")) {
      single_num_part <- 10
    } else if (single_num_part %in% "A" ) {
      single_num_part <- 1
    } else {
      single_num_part <- as.numeric(single_num_part)
    }
    return(single_num_part)
  }
  value_part <- purrr::map_dbl(num_part, trans_card_to_num)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 判断是否为五小
  if (sum(value_part) == 10) {wu_xiao <- "yes"} else {wu_xiao <- "no"}
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 判断是否为五花
  if (all(num_part %in% c("J", "Q", "K"))) {wu_hua <- "yes"} else {wu_hua <- "no"}
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #判断是否是3带2或者是否为4带1
  if (length(unique(num_part)) == 2) {
    if (any(unname(table(num_part)) %in% c("2", "3"))) {
      with_3_2 <- "yes"; with_4_1 <- "no"
      } else {with_3_2 <- "no"; with_4_1 <- "yes"}
  } else {
    with_3_2 <- "no"; with_4_1 <- "no"
  }
  
  
  # 计算是否3张牌有10的倍数
  all_3_zhuhe <- matrix(c(c(1,2,3),c(1,2,4),c(1,2,5),c(1,3,4),c(1,3,5),c(1,4,5),
                          c(2,3,4),c(2,3,5),c(3,4,5)), ncol = 3, byrow = TRUE)
  is_niu_value_part <- apply(all_3_zhuhe, 1, FUN = function(i){sum(value_part[i]) %% 10})
  is_niu <- any(is_niu_value_part == 0) #判断是否有牛
  chr_to_unicode_to_df <- function(color_part, num_part) {
    save_df <- data.frame("origin_color" = color_part, stringsAsFactors = FALSE)
    pipei_df <- data.frame("chr" = c("heitao", "hongtao","meihua", "fangkuai"),
                           "shape" = c("♠", "♥", "♣", "♦"), stringsAsFactors = FALSE)
    color_part_shape <- dplyr::left_join(x = save_df, pipei_df, by = c("origin_color" = "chr"))
    all_card_to_beautiful <- paste0(num_part, "_", color_part_shape$shape)
    return(data.frame(t(all_card_to_beautiful)))
  }
  
  if (is_niu) {
    # 如果有牛的话，计算是牛几
    id_niu_card <- all_3_zhuhe[which(is_niu_value_part == 0)[1], ]
    shenxia_part <- sum(value_part[!(c(1:5) %in% id_niu_card)]) %% 10
    if (shenxia_part == 0) {is_have_niu_value = "this_is_niubi"} else {is_have_niu_value = shenxia_part}
    # 在判断是牛几的时候，判断这个牌中最大的是多少
    num_part_with_levels <- factor(x = num_part, levels = c('A', 2:10,'J', 'Q', 'K'), ordered = TRUE)
    get_top_num_of_card <- which(as.numeric(num_part_with_levels) == max(as.numeric(num_part_with_levels)))
    #这个是否牌的最大已经选出来了,接下来选择花色最大
    big_num_card_in_num <- card_parts[get_top_num_of_card]
    # big_num_card_in_num <- card_parts
    color_big_num_card_in_num <- unlist(lapply(stringr::str_split(big_num_card_in_num, pattern = "_"), FUN = function(i){i[2]}))
    color_part_with_levels <- factor(x = color_big_num_card_in_num, levels = c("heitao", "hongtao","meihua", "fangkuai"), ordered = TRUE)
    this_is_big_card_in_card_parts <- big_num_card_in_num[which.max(as.numeric(color_part_with_levels))]
    return_df <- data.frame("is_niu" = is_niu,
                            "niu_value" = is_have_niu_value,
                            "big_card" = this_is_big_card_in_card_parts,
                            "big_card_value" = stringr::str_split(this_is_big_card_in_card_parts, pattern = "_")[[1]][1],
                            "big_card_color" = stringr::str_split(this_is_big_card_in_card_parts, pattern = "_")[[1]][2],
                            stringsAsFactors = FALSE)
    return_df <- cbind(return_df, chr_to_unicode_to_df(color_part = color_part, num_part = num_part))
  } else {
    # 如果没牛，直接判断最大的牌是多少
    num_part_with_levels <- factor(x = num_part, levels = c('A', 2:10,'J', 'Q', 'K'), ordered = TRUE)
    get_big_card_final_data <- card_parts[which(as.numeric(num_part_with_levels) == max(as.numeric(num_part_with_levels)))]
    color_big_num_card_in_num <- unlist(lapply(stringr::str_split(get_big_card_final_data, pattern = "_"), FUN = function(i){i[2]}))
    color_part_with_levels <- factor(x = color_big_num_card_in_num, levels = c("heitao", "hongtao","meihua", "fangkuai"), ordered = TRUE)
    this_is_big_card_in_card_parts <- get_big_card_final_data[which.max(as.numeric(color_part_with_levels))]
    return_df <- data.frame("is_niu" = is_niu,
                            "niu_value" = "no_niu",
                            "big_card" = this_is_big_card_in_card_parts,
                            "big_card_value" = stringr::str_split(this_is_big_card_in_card_parts, pattern = "_")[[1]][1],
                            "big_card_color" = stringr::str_split(this_is_big_card_in_card_parts, pattern = "_")[[1]][2],
                            stringsAsFactors = FALSE)
    return_df <- cbind(return_df, chr_to_unicode_to_df(color_part = color_part, num_part = num_part))
  }
  
  return_df <- cbind(return_df, data.frame("wu_hua" = wu_hua, 
                                           "wu_xiao" = wu_xiao, 
                                           "with_3_2" = with_3_2,
                                           "with_4_1" = with_4_1,
                                           stringsAsFactors = FALSE))
  return(return_df)
  
}





this_chunk_result <- do.call(rbind, lapply(this_is_a_chunk, cal_cards_value))

View(this_chunk_result)






