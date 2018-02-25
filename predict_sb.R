predict_sb <- function(input_text){
  t1 <- Sys.time()
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
  if(length(a)>=4){
    res5 <- dat5_sb[grep(a4, dat5_sb$ngrams),]
    res5$ngrams <- gsub(a4, "", res5$ngrams)
    res <- res5[order(-res5$s),]
  }else{
    res5 <- data.frame(ngrams="", s = 0)
  }
  if(length(a)>=3){
    res4 <- dat4_sb[grep(a3, dat4_sb$ngrams),]
    res4$ngrams <- gsub(a3, "", res4$ngrams)
    res4 <- res4[!is.element(res4$ngrams, res5$ngrams),]
    res <- rbind(res5, res4)
    res <- res[order(-res$s),]
  }else{
    res4 <- data.frame(ngrams="", s = 0)
  }
  if(length(a)>=2){
    res3 <- dat3_sb[grep(a2, dat3_sb$ngrams),]
    res3$ngrams <- gsub(a2, "", res3$ngrams)
    res3 <- res3[!is.element(res3$ngrams, c(res4$ngrams, res5$ngrams)),]
    res <- rbind(res5, res4, res3)
    res <- res[order(-res$s),]
  }else{
    res3 <- data.frame(ngrams="", s = 0)
  }
  if(length(a)>=1){
    res2 <- dat2_sb[grep(a1, dat2_sb$ngrams),]
    res2$ngrams <- gsub(a1, "", res2$ngrams)
    res2 <- res2[!is.element(res2$ngrams, c(res3$ngrams, res4$ngrams, res5$ngrams)),]
    res <- rbind(res5, res4, res3, res2)
    res <- res[order(-res$s),]
  }else{
    res2 <- data.frame(ngrams="", s = 0)
  }
  res1 <- data.frame(ngrams = c("the", "to", "and", "of", "I"), s = c(1e-20, 1e-20, 1e-20, 1e-20, 1e-20))
  res <- rbind(res5, res4, res3, res2, res1)
  res <- res[order(-res$s),]
  return(res) 
total_time <-  Sys.time() - t1
}
