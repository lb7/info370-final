library(dplyr)

data.dir <- './data/'

read.tsv <- function(filename) {
  return(
      read.delim(
      paste0(data.dir, filename), 
      sep='\t', 
      header=TRUE,
      na.strings = '\\N',
      fill=TRUE,
      stringsAsFactors = FALSE
    )
  )
}

title.basics <- read.tsv('title.basics.tsv')
ratings <- read.tsv('title.ratings.tsv')

awards <- read.csv(
  paste0(data.dir, 'awards.csv'),
  stringsAsFactors = FALSE
)

casting <- read.csv(
  paste0(data.dir, 'AllMoviesCastingRaw.csv'),
  sep = ';',
  stringsAsFactors = FALSE
)
details <- read.csv(
  paste0(data.dir, 'AllMoviesDetailsCleaned.csv'),
  sep = ';',
  stringsAsFactors = FALSE
)

# For some reason this name gets read wrong, correct it
colnames(details)[1] <- 'id'

all.details <- inner_join(details, casting, by='id')
all.details <- all.details %>% 
  select(imdb_id, budget, revenue, popularity, revenue, starts_with('actor'), starts_with('director'))


title.basics <- title.basics %>% 
  filter(titleType == 'movie', isAdult == 0, startYear >= 1927) %>% 
  select(-titleType, -originalTitle, -isAdult, -endYear, -genres -runtimeMinutes)

clean.data <- left_join(title.basics, ratings) %>% 
  inner_join(all.details, by=c("tconst" = "imdb_id")) %>% 
  select(-runtimeMinutes, -genres)

write.csv(clean.data, file='clean.csv')
