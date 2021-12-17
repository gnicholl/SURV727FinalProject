
# set working directory
setwd("C:\\Users\\gasni\\Desktop\\School work\\SURV727\\Final Project")

# load packages and define helper functions
source("Header.R")

# get list of artist names
load("artistnamelist.RData") # loads object "artistnames"

# For each name in the list, create data of albums and reviews
artistdata = NULL
for (aname in artistnames[694:1000]) {

## SPOTIFY DATA #####################################

  id = '3f766570eef74a6986a646bf7dd73102'
  secret = '5f884eb17551445ba76ab797ec7505b6'
  Sys.setenv(SPOTIFY_CLIENT_ID = id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
  access_token = spotifyr::get_spotify_access_token()

  # artist info
  search = spotifyr::search_spotify(q=aname, type="artist")
  artist_id = search$id[1]
  artist = spotifyr::get_artist(id=artist_id, authorization=access_token)
  artist_name = artist$name

  # album list
  albums = spotifyr::get_artist_albums(id=artist_id,include_groups="album",market="CA",limit=50,authorization=access_token)
  nalb = length(albums)
  offset = 1
  while(nalb>0) {
      offset = offset + 50
      newalbs = spotifyr::get_artist_albums(id=artist_id,include_groups="album",market="CA",limit=50,offset=offset,authorization=access_token)
      nalb = length(newalbs)
      albums = rbind(albums,newalbs)
  }
  
  
  # Dedupe album list
  albums$name = gsub("\\s*\\([^\\)]+\\)","",albums$name) # remove "deluxe" versions, etc.
  if(length(albums$name)>0) { # only proceed if they have albums!
    albums = albums[!duplicated(albums$name),]
    albums = albums[!duplicated(albums$release_date),]
  } else {
    print(paste0("Skipping ", artist_name, "----no albums!"))
  }

  # loop through albums
  album_ids = albums$id
  for (id in album_ids) {
    example_album=spotifyr::get_album(id=id,authorization=access_token)

    ## LAST FM ##############################################################
    api_key = "749b4800592668088e8a563845cf906f"
    # '&' and '+' and '#' cause issues
    url_artist = stringr::str_replace_all(artist_name,"\\+","%2B")
    url_album = stringr::str_replace_all(example_album$name,"\\+","%2B")
    url_artist = stringr::str_replace_all(url_artist,"&","%26")
    url_album = stringr::str_replace_all(url_album,"&","%26")
    url_artist = stringr::str_replace_all(url_artist,"#","%23")
    url_album = stringr::str_replace_all(url_album,"#","%23")

    url = paste0("http://ws.audioscrobbler.com/2.0/",
                 "?method=album.getinfo",
                 "&api_key=",api_key,
                 "&artist=", gsub(" ", "+", url_artist),
                 "&album=", gsub(" ", "+", url_album),
                 "&format=json")
    data_json = httr::GET(url)
    data_json = jsonlite::fromJSON(rawToChar(data_json$content))
    album_scrobbles = as.integer(data_json$album$playcount)
    if (length(album_scrobbles)==0) { # handle missing scrobbles
      album_scrobbles = NA
    }
    #########################################################################

    ## CRITIC SCORES FROM WIKIPEDIA #########################################

    # Use google api to search for the (most likely) correct wikipedia page
    # (note that this is a custom search api that searches only wikipedia)
     google.key = 'AIzaSyBKJ9b9c1zKEEUs-g2PTrosIHO8epctAls'
     google.cx = '2fb4b305ff06c300d'
     
     # keywords to be searched in google, separated by '+'
     keyword = paste0(artist_name," ",example_album$name," ","album")
     keyword = gsub("[[:punct:]]", " ", keyword)
     keyword = stringr::str_squish(keyword)
     keyword = gsub(" ", "+", keyword)
    
     # api call
     url = paste0("https://www.googleapis.com/customsearch/v1?"
                  , "key=", google.key
                  , "&q=", keyword
                  , "&gl=us"         
                  , "&hl=en"                
                  , "&cx=", google.cx
                  , "&fields=items(link)"
     )
     googlesearch = httr::GET(url)
     content = rawToChar(googlesearch$content); Encoding(content) = "UTF-8"
     searchresults = jsonlite::fromJSON(content)
     
     if (length(searchresults)!=0) { # only proceed if google search finds at least one url
     
       wikiurl = searchresults$items$link[1]
  
      # Wikipedia API -> using the page title, parse the album page and extract album scores
        # do some cleaning to the url to get the proper page name
          # deal with special characters
          pgname = curl::curl_unescape(wikiurl)
          # keep the end of the url -> the page name wikipedia api wants
          pgname = gsub("https://.*\\.wikipedia\\.org.*/","",pgname)
        
        # get the language of the wiki article
          wikilang = gsub("https://","",wikiurl)
          wikilang = gsub("\\.wikipedia.org.*","",wikilang)
          
        # obtain page content
          # try english language first, otherwise use language of url
          error = 0
          tryCatch(
              expr    = {webpage = WikipediR::page_content(language="en",project="wikipedia",page_name=pgname)}
            , error   = function(e){}
            , finally = {tryCatch(
                    expr = {webpage = WikipediR::page_content(language=wikilang,project="wikipedia",page_name=pgname)}
                  , error = function(e){print(paste0("Skipping url: ",wikiurl)); assign("error",1,envir = .GlobalEnv)}
                  , finally = {})})
        
        if (error!=1) { # only proceed if wikipedia url worked
        # parse wikipedia content
        webpage = rvest::read_html(webpage$parse$text$`*`)
        wikiscores = rvest::html_nodes(webpage, xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "floatright", " " ))]//td')
        wikiscores_stars = rvest::html_attr(rvest::html_element(wikiscores,css='span'),'title')
        wikiscores = rvest::html_text(wikiscores,trim=T)
  
        # some formatting for the scores -> convert them all into standard numeric out of 100
        wikiscores_data = as.data.frame(matrix(wikiscores,ncol=2,byrow=T))
        wikiscores_stars_data = as.data.frame(matrix(wikiscores_stars,ncol=2,byrow=T))
          # deal with specific review websites
           selector = (!is.na(wikiscores_stars_data$V2) & !(tolower(wikiscores_data$V1)=="tom hull – on the Web"))
           wikiscores_data$V2[selector] = wikiscores_stars_data$V2[selector]
           wikiscores_data = wikiscores_data[wikiscores_data$V1!="Metacritic",] # this is a review aggregator
           wikiscores_data = wikiscores_data[wikiscores_data$V1!="AnyDecentMusic?",] # this is a review aggregator
          # remove [] and content inside
           wikiscores_data$V2 = gsub("\\[.*?\\]","",wikiscores_data$V2)
          # remove () but NOT CONTENT (sometimes the score is contained within ()
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"\\(","")
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"\\)","")
          # fix ill-behaved special characters
           wikiscores_data$V2 = sapply(wikiscores_data$V2 , function(x) utils::URLencode(x) )
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"%E2%88%92","-")
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"%E2%80%93","-")
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"%E2%80%94","-")
           wikiscores_data$V2 = sapply(wikiscores_data$V2 , function(x) utils::URLdecode(x) )
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"\\?\\^\\'","-")
        # covert letter grade to numeric
          wikiscores_data$V2 = sapply(stringr::str_trim(wikiscores_data$V2) , lettergrade_num)
          # remove unit of measure (stars or discs)
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"stars","")
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"discs","")
          # remove %'s
           wikiscores_data$V2 = stringr::str_replace(wikiscores_data$V2,"%","")
          # remove remaining scores that contain no numbers
           wikiscores_data = wikiscores_data[grepl("\\d",wikiscores_data$V2),]
  
          # convert to numeric
          wikiscores_data$V2 = sapply(wikiscores_data$V2,
                                      function(x) tryCatch(eval(parse(text=x)),
                                                           error=function(e){},
                                                           finally=NULL) )
        # final set of scores to put in dataset
          wikiscores_data = wikiscores_data[!sapply(wikiscores_data$V2,is.null),]
          wikiscores_data$V2 = as.numeric(wikiscores_data$V2)
          wikiscores_data$V2[wikiscores_data$V2<=1] = 100*wikiscores_data$V2[wikiscores_data$V2<=1]
      #########################################################################
  
      ## CRITIC SCORES FROM WIKIPEDIA #########################################
        release_date_wiki = rvest::html_nodes(webpage, xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "published", " " ))]')
        release_date_wiki = rvest::html_text(release_date_wiki,trim=T)
        release_date_wiki = release_date_wiki[2]
      #########################################################################
      
      wiki_scores = list(wikiscores_data$V2)
        
    } else { # if no wiki article found, set variables to empty 
      release_date_wiki = NA
      wiki_scores = list(numeric(0))
    }
     
     
    # grow the dataset with the additional tracks and album review info
      artistdata = rbind(artistdata,
                  cbind(artist_name,
                        artist_id,
                        artist$followers$total,
                        example_album$name,
                        id,
                        example_album$popularity,
                        example_album$release_date,
                        album_scrobbles,
                        release_date_wiki,
                        wiki_scores))
      
    print(cbind(artist_name,
                artist_id,
                artist$followers$total,
                example_album$name,
                id,
                example_album$popularity,
                example_album$release_date,
                album_scrobbles,
                release_date_wiki,
                wiki_scores))
    } # end of if(error!=1)
    } # end of album loop
} # end of artist loop
colnames(artistdata) = c("artist_name","artist_id","artist_followers","album_name","album_id",
                         "album_popularity", "album_releasedate","album_scrobbles","release_date_wiki","wiki_scores")
artistdata = as.data.frame(artistdata)
#########################################################



