library(data.table)
predict_sb <- function(input_text){
  a <- input_text
  a <- tolower(stripWhitespace(gsub("[^A-Za-z |']","", a))); a <- gsub("^\\s+","", a); a <- unlist(strsplit(a, " "))
  #tokenize input text
  input_4gr <- ifelse(length(a) > 3, paste(a[(length(a)-3):length(a)], collapse = " "), "")
  input_3gr <- ifelse(length(a) > 2, paste(a[(length(a)-2):length(a)], collapse = " "), "")
  input_2gr <- ifelse(length(a) > 1, paste(a[(length(a)-1):length(a)], collapse = " "), "")
  input_1gr <- ifelse(length(a) > 0, paste(a[length(a)], collapse = " "), "")
  #prepare search strings
  a4 <- paste0("^", input_4gr, " ")
  a3 <- paste0("^", input_3gr, " ")
  a2 <- paste0("^", input_2gr, " ") 
  a1 <- paste0("^", input_1gr, " ")
  res <- data.table(ngrams = "", s = 0)
  if(length(a)>=4){
    res_temp <- dat5_sb[grep(a4, dat5_sb$ngrams),]
    res <- dat5_sb[grep(a4, dat5_sb$ngrams),]
    res$ngrams <- gsub(a4, "", res$ngrams)
  }
  if(length(a)>=3 & nrow(res)<20){
    res_temp <- dat4_sb[grep(a3, dat4_sb$ngrams),]
    res_temp$ngrams <- gsub(a3, "", res_temp$ngrams)
    res_temp <- res_temp[!is.element(res_temp$ngrams, res$ngrams),]
    res <- rbind(res, res_temp)
  }
  if(length(a)>=2 & nrow(res)<20){
    res_temp <- dat3_sb[grep(a2, dat3_sb$ngrams),]
    res_temp$ngrams <- gsub(a2, "", res_temp$ngrams)
    res_temp <- res_temp[!is.element(res_temp$ngrams, res$ngrams),]
    res <- rbind(res, res_temp)
  }
  if(length(a)>=1 & nrow(res)<20){
    res_temp <- dat2_sb[grep(a1, dat2_sb$ngrams),]
    res_temp$ngrams <- gsub(a1, "", res_temp$ngrams)
    res_temp <- res_temp[!is.element(res_temp$ngrams, res$ngrams),]
    res <- rbind(res, res_temp)
  }
  if(nrow(res)<5){
    res_temp <- data.table(ngrams = c("the", "to", "and", "of", "I"), s = c(1e-20, 1e-20, 1e-20, 1e-20, 1e-20))
    res <- rbind(res, res_temp)
  }
  res <- res[order(-res$s),]
  return(res) 
}
