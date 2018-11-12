# cl helper texts
text_output <- function(py_index,df,filepath, type = 'document', topic = 'none', extra = FALSE){
  if(!dir.exists(filepath)){
    dir.create(filepath)
  }
  
  if(type=='document'){
    df = df[df$py_index==py_index,]
    if(extra==TRUE){
      df <- df %>% mutate(seattle = factor(seattle, levels = c(0,1), labels = c("outside", "inside")))
      filepath = paste0(filepath,paste(df$listingDate,'_', df$seattle, sep='_',df$topic, df$postID),'.txt')
    } else {
      filepath = paste0(filepath,paste(df$topic, df$postID, sep='_'),'.txt') # include the topic and post id in the filename
    }
    # prepare file contents
    contents = paste0("This text was a ",round(df[df$topic],3), " match for ", df$topic, 
                      " and assoicated with postID ", df$postID,
                      " at ", df$matchAddress, " in tract ", df$GEOID10,
                      ", and cost ", df$cleanRent,
                      "\nIt was ", df$seattle," Seattle and was posted on ", df$listingDate,
                      '\n\nTitle: ',df$listingTitle, '\n\nText: ',df$listingText
    ) 
    # write the file
    write_file(contents, filepath)
  }
  if(type=='topic'){
    df = df[df$topic==topic,]
    filepath = paste0(filepath,'/',topic,'.txt') # include the topic in the filename
    # prepare file contents
    write_file(paste0('This document contains texts that were a high match for topic ', topic,'\n\n'), filepath)
    sink(filepath, append = TRUE)
    labelTopics(model, topics = as.integer(str_extract('Topic 1', '\\d')))
    sink()
    for(i in 1:10){
      line = df[i,]
      write_file(paste0("\nExample text number", i, " was a ",round(line[line$topic],3), " match for ", line$topic, 
                        " and assoicated with postID ", line$postID,
                        " at ", line$matchAddress, " in tract ", line$GEOID10,
                        ", and cost ", line$cleanRent,
                        "\nIt was associated with above median ", line$covariate, "\n\n",
                        'Title: ',line$listingTitle, '\n\nText: ',line$listingText
      ) , filepath, append = TRUE) 
    }
    # write the file
  }
}

write_tokenized_sentences <- function(list,fname){
  f <- file(paste0("data/text/",fname))
  writeLines(unlist(tokenize_sentences(list)), f)
  close(f)
}