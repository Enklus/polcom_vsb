
#load required packages
  require(quanteda)
  require(devtools)
  require(readtext)
  require(newsmap)
  require(quanteda.textmodels)

#set working directory
  setwd("C:/Users/Well Met/Desktop/SE PolKomm Watanabe")


#WITH FEDERAL  
  
#load data
  dat_vsb <- readtext("doc/no_appendix/*.pdf" , 
                     docvarsfrom = "filenames", 
                     docvarnames = c("state", "year"),
                     sep = "_",
                     encoding = "UTF-8")
  dat_vsb

  #create crime vector
  crime <- c(7961,20431)

 #look up dictionary
  dict_orientation <- dictionary(file = "pol_spectrum.yml")
  
#construct corpus
  corp_vsb <- corpus(dat_vsb)
  

#tokenize

  toks_vsb <- tokens(corp_vsb, remove_punct = TRUE)

#tokenize without stopwords
  toks_vsb_nostop <- tokens_select(toks_vsb, pattern = stopwords('de'), selection = 'remove')




#construct dfm

  toks_orientation <- tokens_lookup(toks_vsb_nostop, dict_orientation, levels = 1:2)
  dfmt_orientation <- dfm(toks_orientation, tolower = FALSE)


#perform statistical analysis


  right_mentions <- sum(dfmt_orientation[,2])
  left_mentions <- sum(dfmt_orientation[,1])
  mentions <- c(left_mentions , right_mentions)
  dfmt_orientation_matrix <- as.matrix(dfmt_orientation)
  barplot(dfmt_orientation_matrix)

  #test for significance
  prop.test(mentions, crime,)


#ANALYZE CONTEXT---------------------------------------------------------- 
 
 #corpus full sentence
  #segment by sentence
   corp_vsb_seg  <- corpus_reshape(corp_vsb)  
  toks_vsb_seg  <- tokens(corp_vsb_seg, remove_punct = TRUE)
  toks_vsb_seg_nostop <- tokens_select(toks_vsb_seg, pattern = stopwords('de'), selection = 'remove', min_nchar=3)
 
  #look up with dictionary
   toks_orientation_seg <- tokens_lookup(toks_vsb_seg_nostop, dict_orientation, levels = 1:2)
  dfmt_orientation_seg <- dfm(toks_orientation_seg, tolower = FALSE)
 
  #add looked up as docvars
   dfmt_original_sentence <- dfm(toks_vsb_seg_nostop)
  docvars(dfmt_original_sentence)   <- convert(dfmt_orientation_seg, "data.frame")
  
 #remove keywords from dfm
  dfmt_original_sentence  <-dfm_remove(dfmt_original_sentence, dict_orientation)
  
  #compare frequency right wing

tstat_keyness_right<-textstat_keyness(dfmt_original_sentence, target = dfmt_original_sentence$political_right > 0)

#visualize right wing
textplot_keyness(tstat_keyness_right)
  
#compare frequency left wing

tstat_keyness_left<-textstat_keyness(dfmt_original_sentence, target = dfmt_original_sentence$political_left > 0)

#visualize left wing
textplot_keyness(tstat_keyness_left)









#Mentions WITHOUT FEDERAL------------------------------------------------

  #load data
  dat_nofed_vsb <- readtext("doc/no_federal/*.pdf" , 
                      docvarsfrom = "filenames", 
                      docvarnames = c("state", "year"),
                      sep = "_",
                      encoding = "UTF-8")
  dat_nofed_vsb
  
  
  #construct corpus
  corp_nofed_vsb <- corpus(dat_nofed_vsb)
  
  #tokenize
  
  toks_nofed_vsb <- tokens(corp_nofed_vsb, remove_punct = TRUE)
  
  #tokenize without stopwords
  toks__nofed_vsb_nostop <- tokens_select(toks_nofed_vsb, pattern = stopwords('de'), selection = 'remove')
  
  
  #look up dictionary
  dict_orientation <- dictionary(file = "pol_spectrum.yml")
  
  #construct dfm
  
  toks_nofed_orientation <- tokens_lookup(toks__nofed_vsb_nostop, dict_orientation, levels = 1:2)
  dfmt_nofed_orientation <- dfm(toks_nofed_orientation, tolower = FALSE)
  
  
  #perform statistical analysis
  
  
  right_mentions_nofed <- sum(dfmt_nofed_orientation[,2])
  left_mentions_nofed <- sum(dfmt_nofed_orientation[,1])
  mentions_nofed <- c("left_mentions_nofed" , "right_mentions_nofed")
  dfmt_nofed_orientation_matrix <- as.matrix(dfmt_nofed_orientation)
  barplot(dfmt_nofed_orientation_matrix)
  
  
#Mentions ONLY FEDERAL-----------------------------------------------------

    #load data
  dat_vsb_fed <- readtext("doc/no_appendix/brd_2018.pdf" , 
                      docvarsfrom = "filenames", 
                      docvarnames = c("state", "year"),
                      sep = "_",
                      encoding = "UTF-8")
  dat_vsb_fed
  
  
  #construct corpus
  corp_vsb_fed <- corpus(dat_vsb_fed)
  
  #tokenize
  
  toks_vsb_fed <- tokens(corp_vsb_fed, remove_punct = TRUE)
  
  #tokenize without stopwords
  toks_vsb_fed_nostop <- tokens_select(toks_vsb_fed, pattern = stopwords('de'), selection = 'remove')
  
  
  #look up dictionary
  dict_orientation <- dictionary(file = "pol_spectrum.yml")
  
  #construct dfm
  
  toks_orientation_fed <- tokens_lookup(toks_vsb_fed_nostop, dict_orientation, levels = 1:2)
  dfmt_orientation_fed <- dfm(toks_orientation_fed, tolower = FALSE)
  
  
  #perform statistical analysis
  
  
  right_mentions_fed <- sum(dfmt_orientation_fed[,2])
  left_mentions_fed <- sum(dfmt_orientation_fed[,1])
  mentions_fed <- c("left_mentions_fed" , "right_mentions_fed")
  dfmt_orientation_matrix_fed <- as.matrix(dfmt_orientation_fed)
  barplot(dfmt_orientation_matrix_fed)
  
  
  
  

  

  
#TO-DO: 
#       chi-quadrat-test auf signifikanz
#       semantic networks
#       test federal level alone
#       test state level alone

#unused code-----------------------------------------------------
  #is_right <- rowSums(dfmt_orientation[,c("political_right")]) > 0
  #is_left <- rowSums(dfmt_orientation[,c("political_left")]) > 0

  #toks_right <- toks_vsb_nostop[is_right] 
  #print(toks_right, max_ntoken = 100)

  #toks_left <- toks_vsb_nostop[is_left] 
  #print(toks_left, max_ntoken = 100)


#keywords in context---------------------------------------------

#keywords rechts
  #kw_rechts <- kwic(toks_vsb_nostop, pattern =  'recht*')
  #head(kw_rechts, 10)

#keywords links
  #kw_links <- kwic(toks_vsb_nostop, pattern =  'link*')
  #head(kw_links, 10)

