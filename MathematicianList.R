
library(RCurl)

library(XML)

library(stringr)


mathWiki <- "http://en.wikipedia.org/wiki/Lists_of_mathematicians"

mathLinks <- getHTMLLinks(mathWiki)
  

mathStartPos <- grep(pattern= "mathematicians_(A)", x= mathLinks,fixed = TRUE)
                          

mathEndPos <- grep(pattern= "mathematicians_(Z)", x= mathLinks,fixed = TRUE)

newMathLinks <- mathLinks[mathStartPos:mathEndPos]

fullMathLinks <- paste0("http://en.wikipedia.org",newMathLinks)

alphabetMath <- lapply(1:length(fullMathLinks), function(i) { getHTMLLinks(fullMathLinks[i])})

alphabetMath  <- unlist(alphabetMath)

alphabetMath <- alphabetMath[-grep(pattern = "WikiProject|wikimediafoundation|wikipedia|Wikipedia|mediawiki|wikidata|index|.php", x = alphabetMath)]

alphabetMath <- grep(pattern = "([A-Za-z]+_[A-Z])",x = alphabetMath,value = TRUE)

alphabetMath <- alphabetMath[-which(alphabetMath %in% mathLinks)]


namesMath <- gsub(pattern = "\\W",replacement = "", x = alphabetMath)

namesMath <- gsub(pattern = "wiki",replacement = "", x = namesMath)

namesMath <- gsub(pattern = "_",replacement = "", x = namesMath)

nonMath <- grep(pattern = "[0-9]",x = namesMath)

namesMath <- namesMath[-nonMath]


#capLetters <- lapply(1:length(namesMath),function(i) { na.omit(which(unlist(str_split(namesMath[i],pattern = "")) %in% LETTERS)[2:3])})

#Blanche Descartes - Pseudonym #http://en.wikipedia.org/wiki/Blanche_Descartes

tempMath <- NULL

storeName <- NULL

newTempMath <- NULL


#This for loop puts spaces in between the first, middle, and last names
# If a middle name doesn't exist, then a space is put in between the first and last name
#Cases where a person has four or five names is also handled by the loop.
for(i in 1:length(namesMath))
{
  tempMath <- unlist(str_split(namesMath[i],pattern = "")) #Splits names into characters
  
  secCapPos <- which(tempMath %in% LETTERS)[2] #Finds the second capital letter
  
  #Insert a space right before the second capital letter in the name ans paste the name together
  
  storeName[i] <- paste0(append(x = tempMath,values = " ", after = secCapPos - 1),collapse ="") 
  
  
  newTempMath <- storeName[i]  #Store the name in a temporary variable
  
  newTempMath <- unlist(str_split(newTempMath,pattern = "")) #Split this temp variable into characters
  
  #Checks to see if there is a 3rd capital letter
  if(is.na(which(tempMath %in% LETTERS)[3]) == FALSE)
  {
  thirdCapPos <- which(newTempMath  %in% LETTERS)[3] #Stores the location of the 3rd capital letter
  
   #Adds a space before the appearance of the third capital letter
   storeName[i] <- paste0(append(x = newTempMath, values = " ", after = thirdCapPos - 1),collapse = "")
  }
  
  newTempMath <- unlist(str_split(storeName[i],pattern = "")) #Split this temp variable into characters
  
  #Checks to see if there is a 4th capital letter
  if(is.na(which(tempMath %in% LETTERS)[4]) == FALSE)
  {
    fourthCapPos <- which(newTempMath  %in% LETTERS)[4] #Stores the location of the 4th capital letter
    
    #Adds a space before the appearance of the third capital letter
    storeName[i] <- paste0(append(x = newTempMath, values = " ", after = fourthCapPos - 1),collapse = "")
  }
  
  newTempMath <- unlist(str_split(storeName[i],pattern = "")) #Split this temp variable into characters
  
  #Checks to see if there is a 5th capital letter
  
  if(is.na(which(tempMath %in% LETTERS)[5]) == FALSE)
  {
    fifthCapPos <- which(newTempMath  %in% LETTERS)[5] #Stores the location of the 5th capital letter
    
    #Adds a space before the appearance of the third capital letter
    storeName[i] <- paste0(append(x = newTempMath, values = " ", after = fifthCapPos - 1),collapse = "")
  }
}

mathNameVec <- storeName
mathNameVec <- gsub(pattern = "educator|educationist|mathematician|professor|academic|astronomer|Womeninmathematicsinthe United States|Australian Statistician|statistician",replacement = "", x = mathNameVec)

mathNameVec <- gsub(pattern = "historian|Americanscholar|economist|numbertheorist|inventor|epidemiologist|cryptographer|Three Pillarsof Chinese Catholicism|physicist|psychologist|differentialgeometer|scientist|businessman|logician",replacement = "", x = mathNameVec)

mathNameVec <- gsub(pattern = "metrologist|cryptanalyst|geodesist|Historyofsciencein Classical Antiquity|Hinduand Buddhistcontributiontoscienceinmedieval Islam",replacement = "", x = mathNameVec)

mathNameVec <- gsub(pattern = "Hypatiaof Alexandria",replacement = "Hypatia of Alexandria", x = mathNameVec)

mathNameVec <- gsub(pattern = "engineer|humancomputer|explorer|mathematical|scholar|politician|architect|writer|philosopher",replacement = "", x = mathNameVec)

mathNameVec <- gsub(pattern = "Menelausof Alexandria",replacement = "Menelaus of Alexandria", x = mathNameVec)

mathNameVec <- gsub(pattern = "Polyaenusof Lampsacus",replacement = "Polyaenus of Lampsacus", x = mathNameVec)

mathNameVec <- gsub(pattern = "Theodorusof Cyrene",replacement = "Theodorus of Cyrene", x = mathNameVec)

mathNameVec <- gsub(pattern = "Theodosiusof Bithynia",replacement = "Theodosius of Bithynia", x = mathNameVec)

mathNameVec <- gsub(pattern = "Theonof Alexandria",replacement = "Theon of Alexandria", x = mathNameVec)

#alphabetMath <- alphabetMath[-grep(pattern = "Main_Page", x = alphabetMath)]
#mathEndPos <- mathLinks[which(str_locate(pattern= "Wrangler",string= mathLinks) > 0)[1]

blankPos <- which(mathNameVec == "")

mathNameVec <-mathNameVec[-blankPos] 

mathNameFrame <- data.frame(mathNameVec)

colnames(mathNameFrame) <- "Name of Mathematician"
require(xlsx)

write.xlsx(mathNameFrame,file = "MathematicanNames.xlsx")
#############
#alphabetMath <- alphabetMath[-grep(pattern = "Main_Page", x = alphabetMath)]
#mathEndPos <- mathLinks[which(str_locate(pattern= "Wrangler",string= mathLinks) > 0)[1]
 
