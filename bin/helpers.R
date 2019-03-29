# get census data
get_census_data <- function(){
  acs_codes <- c('B03002_001E','B03002_003E','B03002_004E','B03002_005E','B03002_006E','B03002_007E','B03002_009E','B03002_012E','B17001_001E','B17001_002E','B19013_001E', 'B06009_005E','B08015_001E','B25003_001E','B25003_002E','B25003_003E', 'B25032_001E','B25032_020E', 'B25032_021E', 'B25036_001E','B25036_015E','B25036_014E')
  acs_names <- c('total_RE','white', 'black', 'aindian', 'asian', 'pacisland', 'other', 'latinx', 'total_poverty', 'under_poverty', 'income', 'col_degree','commute', 'total_tenure', 'owner_occupied', 'rental', 'total_tenure_size', 'rental_20_49','rental_over_50','total_tenure_year', 'built_10_13', 'built_after_14')
  re_cols <- c("white","black", "aindian", "asian","pacisland", "other", "latinx", "all_other") # save a vector of race and ethnicity categories
  names(acs_codes) <- acs_names
  tracts <- get_acs(geography = 'tract', variables = acs_codes, output = 'wide', state = 'WA', geometry = TRUE) %>% #grab census data
    select(-ends_with("M")) %>% mutate(all_other = aindian+pacisland+other) # drop moe columns
  tracts_no_geo <- sf::st_drop_geometry(tracts)
  tracts <- bind_cols(tracts, setNames(tracts_no_geo[re_cols]/tracts_no_geo$total_RE,paste0(re_cols,"_proportion"))) %>% # make racial proportion columns
    mutate(pov_proportion = under_poverty/total_poverty, # poverty proportion variable
           log_income = log(income), # log income variable
           pop_thousands = total_RE/1000, # population in thousands
           share_oo = owner_occupied/total_tenure, # share owner occupied
           share_rental_over_20 = (rental_20_49+rental_over_50)/total_tenure, # share in large rental units
           share_built_after_10 = (built_10_13+built_after_14)/total_tenure,# share of reners in units built after 2010
           share_commuters = commute/total_RE,#share of commuters
           share_college = col_degree/total_RE) #share of college education
}

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