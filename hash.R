library('rvest')
library('hash')
content = read_html('collection.html')
text_set = html_nodes(content,"text")  
text1 = text_set[1] %>% html_nodes("p") %>% html_text()
text2 = text_set[2] %>% html_nodes("p") %>% html_text()
text3 = text_set[3] %>% html_nodes("p") %>% html_text()

#PROCESS
process = function(x){
  x_all = paste(x[1:length(x)],collapse = " ")
  x_new = gsub("\n"," ",x_all)
  x_new = gsub("\"","",x_new)
  x_new = gsub(",","",x_new)
  x_new = gsub("\\.","",x_new)
  x_new = gsub("'","",x_new)
  x_new = gsub("\\(","",x_new)
  x_new = gsub("\\)","",x_new)
  x_new = gsub(":","",x_new)
  x_new = gsub("\\?","",x_new)
  x_new = gsub("--"," ",x_new)
  x_new = gsub("&"," ",x_new)
  x_new = gsub("-"," ",x_new)
  x_new = tolower(x_new)
  return(x_new)
}
text1_new = process(text1)
text2_new = process(text2)
text3_new = process(text3)
text_total = paste(text1_new,text2_new,text3_new)


#HASH
words_1<-strsplit(text1_new," ")
words_1.freq<-table(unlist(words_1))
words_2<-strsplit(text2_new," ")
words_2.freq<-table(unlist(words_2))
words_3<-strsplit(text3_new," ")
words_3.freq<-table(unlist(words_3))
words_total<-strsplit(text_total," ")
words_total.freq<-table(unlist(words_total))
hash_table = hash()
for (i in row.names(words_total.freq)[-1]) {
  count1 = words_1.freq[i]
  count2 = words_2.freq[i]
  count3 = words_3.freq[i]
  .set(hash_table, keys = i, values = c(count1,count2,count3))
}



