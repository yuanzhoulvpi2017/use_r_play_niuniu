output_my_result <- function(i) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 生成扑克
  cat("生成纸牌\n")
  
  n_card <- rep(c('A', 2:10,'J', 'Q', 'K'), each = 4)
  type_card <- rep(c("meihua", "fangkuai", "hongtao", "heitao"), time = 13)
  all_card <- paste( n_card, type_card, sep = '_')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("开始洗牌\n")
  # 洗牌
  random_card <- all_card[sample(x = seq_along(all_card), size = 50, replace = FALSE)]
  
  #这是一场
  this_is_a_chunk <- lapply(X = 1:10, FUN = function(i){random_card[(i*5-4):(i*5)]})
  
  
  # card_parts <- this_is_a_chunk[[1]]
  cat("计算牌的大小\n")
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
  return(this_chunk_result)
}

# output_my_result(1)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 开始并行计算
library(parallel)
library(pbapply)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 计算出所有的数据
cores <- detectCores()
cl <- makeCluster(cores)
# clusterExport(cl = cl, varlist = c("output_my_result"))

all_result <- pblapply(cl = cl, X = 1:100000, FUN = output_my_result)




library(tidyverse)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 计算一场牌能有多少牛
simple_result <- pblapply(X = all_result, FUN = function(i){i$is_niu}, cl = cl)
simple_result <- do.call(rbind, simple_result)
youmeiyouniu <- apply(simple_result, 1, sum)

data.frame("is_niu" = youmeiyouniu) %>% 
  group_by(is_niu) %>% summarise(time = n()) %>% 
  ggplot(aes(x = is_niu, y = time)) + geom_col(width = 0.7) + 
  geom_line(size = 2, color = 'red') +   
  geom_label(aes(label = time)) + 
  theme_bw() + 
  scale_x_continuous(breaks = c(0:10), labels = c(0:10)) +
  labs(x = "一场里面有多少牛", y = "牛的数量", title = "10w场斗牛出现牛的频率") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("is_niu.png", width = 12, height = 8, dpi = 400)
# 这里说明，10000场斗牛里面，基本上10副牌里面有6个或者7个牛。
# 10000次斗牛里面，不会出现10副牌都没有牛。

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 出现五花的概率是多少
wuhua <- pblapply(X = all_result, FUN = function(i){i$wu_hua}, cl = cl)
wuhua <- do.call(rbind, wuhua)
wuhua <- apply(wuhua, 1, FUN = function(x){any(x == "yes")})
# 一万次五花出现的概率
sum(wuhua)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 出现3带2的概率是多少
with_3_2 <- pblapply(X = all_result, FUN = function(i){i$with_3_2})
with_3_2 <- do.call(rbind, with_3_2)
with_3_2 <- apply(with_3_2, 1, FUN = function(x){any(x == "yes")})
sum(with_3_2) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 出现4带1的概率是多少

with_4_1 <- pblapply(X = all_result, FUN = function(i){i$with_4_1})
with_4_1 <- do.call(rbind, with_4_1)
with_4_1 <- apply(with_4_1, 1, FUN = function(x){any(x == "yes")})
sum(with_4_1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 出现五小的概率是多少
wuxiao <- pblapply(X = all_result, FUN = function(i){i$wu_xiao}, cl = cl)
wuxiao <- do.call(rbind, wuxiao)
wuxiao <- apply(wuxiao, 1, FUN = function(x){any(x == "yes")})
# 一万次五小出现的概率
sum(wuxiao) 



data.frame("wuhua" = sum(wuhua),
           "with_3_2" = sum(with_3_2),
           "with_4_1" = sum(with_4_1),
           "wuxiao" = sum(wuxiao)) %>% 
  pivot_longer(cols = c("wuhua", "with_3_2", "with_4_1", "wuxiao")) %>% 
  ggplot(aes(x = name, y = value)) + geom_col(width = 0.5) + 
  geom_label(aes(label = value)) + 
  labs(x = "类型", y = "出现的次数", title = "10w次斗牛出现下面的频率") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("wu_duibi.png", width = 10, height = 6, dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 出现最大的牌是什么
big_cards_quanti <- pblapply(X = all_result, FUN = function(i){i$big_card_value})
big_cards_quanti <- unlist(big_cards_quanti)

data.frame("big_cards" = big_cards_quanti, stringsAsFactors = FALSE) %>% 
  group_by(big_cards) %>% summarise(time = n()) %>% 
  mutate(big_cards = reorder(big_cards, time)) %>% 
  ggplot(aes(x = big_cards, y = time)) + geom_col() + 
  geom_line(aes(group = 1), color = 'red', size = 2) +
  geom_label(aes(label = time)) + 
  theme_bw() +
  labs(x = "最大的牌", y = "出现的次数", title = "10w场斗牛出现最大的频率") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("big_card_quanti.png", width = 10, height = 6, dpi = 300)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 没牛的时候 出现最大的牌是什么
big_cards_meiniu <- pblapply(X = all_result, FUN = function(i){i$big_card_value[!c(i$is_niu)]})
big_cards_meiniu <- unlist(big_cards_meiniu)

data.frame("big_cards" = big_cards_meiniu, stringsAsFactors = FALSE) %>% 
  group_by(big_cards) %>% summarise(time = n()) %>% 
  mutate(big_cards = reorder(big_cards, time)) %>% 
  ggplot(aes(x = big_cards, y = time)) + geom_col() + 
  geom_line(aes(group = 1), color = 'red', size = 2) +
  geom_label(aes(label = time)) + 
  theme_bw() +
  labs(x = "最大的牌", y = "出现的次数", title = "10w场斗牛出现最大的频率(没牛的时候)") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("big_card_meiniu.png", width = 10, height = 6, dpi = 300)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 有牛的时候 出现最大的牌是什么
big_cards_youniu<- pblapply(X = all_result, FUN = function(i){i$big_card_value[i$is_niu]})
big_cards_youniu <- unlist(big_cards_youniu)

data.frame("big_cards" = big_cards_youniu, stringsAsFactors = FALSE) %>% 
  group_by(big_cards) %>% summarise(time = n()) %>% 
  mutate(big_cards = reorder(big_cards, time)) %>% 
  ggplot(aes(x = big_cards, y = time)) + geom_col() + 
  geom_line(aes(group = 1), color = 'red', size = 2) +
  geom_label(aes(label = time)) + 
  theme_bw() +
  labs(x = "最大的牌", y = "出现的次数", title = "10w场斗牛出现最大的频率(有牛的时候)") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("big_cards_youniu.png", width = 10, height = 6, dpi = 300)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 统计牛b的出现情况

niu_stat <- pblapply(X = all_result, FUN = function(i){i$niu_value})
niu_stat <- unlist(niu_stat)

data.frame("niu_value" = niu_stat, stringsAsFactors = FALSE) %>% 
  group_by(niu_value) %>% summarise(time = n()) %>% 
  mutate(niu_value = reorder(niu_value, time)) %>% 
  ggplot(aes(x = niu_value, y = time)) + 
  geom_col(aes(fill = time), show.legend = FALSE) +
  geom_line(aes(group = 1), color = 'red', size = 2) +
  geom_label(aes(label = time)) +
  labs(x = "牛b类型",y = "出现次数", title = "10w场斗牛的各种牛出现的次数") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 15, hjust = 1, angle = 45)) 
ggsave("niu_value.png", width = 10, height = 6, dpi = 300)


