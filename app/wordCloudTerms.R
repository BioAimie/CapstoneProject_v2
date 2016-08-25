wordCloudTerms <- function(characterString) {
  
  # clean up the text that is sent in by the user
  text <- paste(tolower(strsplit(characterString, ' ')[[1]]), collapse = ' ')
  text <- paste(strsplit(text, '')[[1]][grep(paste(paste(letters, collapse='|'),' ',sep='|'), strsplit(text, '')[[1]])], collapse='')
  swearJar <- c('cunt','ass','shit','fuck','tit','bitch','whore','slut')
  text <- paste(gsub(paste(paste(swearJar, collapse='|'), ' ', sep='|'), 'swearjar', strsplit(text,' ')[[1]]), collapse=' ')
  
  if(text == '') {
    
    return(data.frame(Words = c('enter some text to generate a word cloud'), Freq = floor(runif(8, 1, 101))))
  }
  
  # find the length of the text and set up some random words for when the algorithm cannot find a match
  l <- length(strsplit(text, ' ')[[1]])
  funWords <- c('homeboy', 'taco', 'snaggletooth', 'sombrero', 'blankie', 'kittens','tequila', 'moose', 'spongebob', 'fireball')
  randomWords <- c(funWords, as.character(one.gram.freq[one.gram.freq$Record > 5000, 'Combo']))
  n <- length(randomWords)
  
  # if the supplied string is greater than or equal to three words in length, use the four-gram data
  if(l >= 3) {
    
    possible <- four.gram.freq[grep(paste('^',paste(text, collapse=' '),'$', sep=''), four.gram.freq$leadGram), ]
    
    if(length(possible[,1]) > 0) {
      
      return(data.frame(Words = possible$gram, Freq = possible$Record))
    }
    
    # if the string cannot be found in the four-gram data, then look in the three-gram
    if(length(possible[,1]) == 0) {
      
      possible <- three.gram.freq[grep(paste('^',paste(text[2:3], collapse=' '),'$', sep=''), three.gram.freq$leadGram), ]
      if(length(possible[,1]) > 0) {
        
        return(data.frame(Words = possible$gram, Freq = possible$Record))
      }
      
      # if the string can't be found in the three-gram, then dive into the two-gram
      else {
        
        possible <- two.gram.freq[grep(paste('^',paste(text[3], collapse=' '),'$', sep=''), two.gram.freq$leadGram), ]
        if(length(possible[,1]) > 0) {
          
          return(data.frame(Words = possible$gram, Freq = possible$Record))
        }
        
        # if the string still cannot be found, generate a random fun word
        else {
          
          return(data.frame(Words = randomWords, Freq = floor(runif(n, 1, 101))))
        }
      }
    }
  }
  
  # if the supplied string is two words in length, check the three-gram, then go to the two-gram, and finally make up a fun word if the pattern isn't matched
  if(l == 2) {
    
    possible <- three.gram.freq[grep(paste('^',paste(text, collapse=' '),'$', sep=''), three.gram.freq$leadGram), ]
    if(length(possible[,1]) > 0) {
      
      return(data.frame(Words = possible$gram, Freq = possible$Record))
    }
    
    # if the string can't be found in the three-gram, then dive into the two-gram
    else {
      
      possible <- two.gram.freq[grep(paste('^',paste(text[2], collapse=' '),'$', sep=''), two.gram.freq$leadGram), ]
      if(length(possible[,1]) > 0) {
        
        return(data.frame(Words = possible$gram, Freq = possible$Record))
      }
      
      # if the string still cannot be found, generate a random fun word
      else {
        
        return(data.frame(Words = randomWords, Freq = floor(runif(n, 1, 101))))
      }
    }
  }
  
  # if the test is only one word long, then use two-gram to find the next word and default to a fun word if the pattern is not matched
  if(l == 1) {
    
    possible <- two.gram.freq[grep(paste('^', paste(text, collapse=' '),'$', sep=''), two.gram.freq$leadGram), ]
    if(length(possible[,1]) > 0 ) {
      
      return(data.frame(Words = possible$gram, Freq = possible$Record))
    } else {
     
      return(data.frame(Words = randomWords, Freq = floor(runif(n, 1, 101))))
    }
  }
  
  else { stop('No text has been submitted yet!') }
}