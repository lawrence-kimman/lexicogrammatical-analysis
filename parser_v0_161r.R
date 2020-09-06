rm(list=ls(all=TRUE))
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_241.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
options(java.parameters = c("-XX:+UseConMarkSweepGC", "-Xmx11264m"))
Sys.setenv(WNHOME="/usr/local/Cellar/wordnet/3.1")
library(parallel)
# library(gpuR)
library(Rcpp)
library(textstem)
library(tools)
library(qdap)
library(wordcloud)
library(ggraph)
library(ggplot2)
#library(xlsx)
library(openxlsx)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(udpipe)
library(lsa)
library(quanteda)
library(quanteda.textmodels)
library(wordnet)
#setDict("/usr/local/Cellar/wordnet/3.1/dict")
initDict()
getDict()
# library(NLP)
library(coreNLP)
initCoreNLP()

options(max.print=10000000)
quanteda_options(threads=4)
TagCounter<-0
TagCounterAll<-0
objNo<-0
debug_log_enabled<-1  #to enable debug log, set this value to 1, otherwise to 0
debug_log<-NULL
correction<-NA
xAll<-NULL
POSresultAll<-NULL
DependencyAll<-NULL
ParseAll<-NULL
thematicTableAll<-NULL
x<-NULL
xLineByLine<-NULL
xWordByWord<-NULL
filename<-NULL
filePath<-"//Users//test//Desktop//sfl_project//sfl_scripts//"
output<-NULL
POSresult<-NULL
# setContext(3L)
# currentDevice()


################ initialising the variable
counterInTextualExp <- 0
tmpTextualExpListInContinuative <- NULL
tmpTextualExpListInStructuralConjunction <- NULL
tmpTextualExpListInConjunctivePrep <- NULL
tmpTextualExpListInAdjunct <- NULL
tmpTextualExpListInWhElement <- NULL

tmpTextualExpListInContinuativeAll <- NULL
tmpTextualExpListInStructuralConjunctionAll <- NULL
tmpTextualExpListInConjunctivePrepAll <- NULL
tmpTextualExpListInAdjunctAll <- NULL
tmpTextualExpListInWhElementAll <- NULL

tmpTextualExpListContinuativeVector <- NULL
tmpTextualExpListStructuralConjunctionVector <- NULL
tmpTextualExpListConjunctivePrepVector <- NULL
tmpTextualExpListAdjunctVector <- NULL
tmpTextualExpListWhElementVector <- NULL

matchedTextualExpInContinuative <- NULL
matchedTextualExpInStructuralConjunction <- NULL
matchedTextualExpInConjunctivePrep <- NULL
matchedTextualExpInAdjunct <- NULL
matchedTextualExpInWhElement <- NULL
clauseSeg <- NULL

################ loading the table of extual expressions
textualExp <-read.table(file=paste0(filePath,"tmp//","textual_expressions_v2.txt"), sep="\t", header=TRUE)

################ clean up semicolons in columns of "Continuative", "Conjunction", and "Adjunct"
textualExp$Continuative <- gsub(";", ",", textualExp$Continuative)
textualExp$StructuralConjunction <- gsub(";", ",", textualExp$StructuralConjunction)
textualExp$ConjunctivePrep <- gsub(";", ",", textualExp$ConjunctivePrep)
textualExp$Adjunct <- gsub(";", ",", textualExp$Adjunct)
textualExp$WhElement <- gsub(";", ",", textualExp$WhElement)
textualExp[textualExp=="null"]<- NA



################ A function to check with textual_expressions_v?.txt and to identify whether the POSresult$token[wordPosition] is a textual one
textualTheme <- function(sentencePosition){
  if((nrow(subset(POSresult, POSresult$sentence==sentencePosition)))==0 ){return("Invalid")}  #return "Invalid" if the sentencePosition is out of range
  
    for (counterInTextualExp in 1:nrow(textualExp)){
      tmpTextualExpListInContinuative <-scan(text=textualExp$Continuative[counterInTextualExp], sep=",", what ="", quiet=TRUE)
      tmpTextualExpListInContinuative <-gsub("^\\s+|\\s+$", "", tmpTextualExpListInContinuative) #trim leading and trailing whitespaces
      tmpTextualExpListInStructuralConjunction <-scan(text=textualExp$StructuralConjunction[counterInTextualExp], sep=",", what ="", quiet=TRUE)
      tmpTextualExpListInStructuralConjunction <-gsub("^\\s+|\\s+$", "", tmpTextualExpListInStructuralConjunction)
      tmpTextualExpListInConjunctivePrep <-scan(text=textualExp$ConjunctivePrep[counterInTextualExp], sep=",", what ="", quiet=TRUE)
      tmpTextualExpListInConjunctivePrep <-gsub("^\\s+|\\s+$", "", tmpTextualExpListInConjunctivePrep)
      tmpTextualExpListInAdjunct <-scan(text=textualExp$Adjunct[counterInTextualExp], sep=",", what ="", quiet=TRUE)
      tmpTextualExpListInAdjunct <-gsub("^\\s+|\\s+$", "", tmpTextualExpListInAdjunct) 
      tmpTextualExpListInWhElement <-scan(text=textualExp$WhElement[counterInTextualExp], sep=",", what ="", quiet=TRUE)
      tmpTextualExpListInWhElement <-gsub("^\\s+|\\s+$", "", tmpTextualExpListInWhElement)    
      
      
      tmpTextualExpListInContinuativeAll <- c(tmpTextualExpListInContinuativeAll, tmpTextualExpListInContinuative)
      tmpTextualExpListInStructuralConjunctionAll <- c(tmpTextualExpListInStructuralConjunctionAll, tmpTextualExpListInStructuralConjunction)
      tmpTextualExpListInConjunctivePrepAll <- c(tmpTextualExpListInConjunctivePrepAll, tmpTextualExpListInConjunctivePrep)
      tmpTextualExpListInAdjunctAll <- c(tmpTextualExpListInAdjunctAll, tmpTextualExpListInAdjunct)
      tmpTextualExpListInWhElementAll <- c(tmpTextualExpListInWhElementAll, tmpTextualExpListInWhElement)
    }
  
  
  
  
      for (i in 1:length(tmpTextualExpListInContinuativeAll)) {
        clauseSeg <- subset(POSresult$token, POSresult$sentence==sentencePosition )
        tmpTextualExpListContinuativeVector <- scan(text=tmpTextualExpListInContinuativeAll[i], sep=" ", what="", quiet=TRUE)
        matchedTextualExpInContinuative <- keywords_phrases(clauseSeg, pattern = tmpTextualExpListContinuativeVector) 
        
        if (nrow(matchedTextualExpInContinuative)!=0){
          index<-as.numeric(rownames(subset(POSresult, POSresult$sentence==sentencePosition & POSresult$token==tmpTextualExpListContinuativeVector[1])) )
          if (hasVerbInPhrase(index)=="No verb in this phrase." || hasVerbInPhraseBfWordPosition(index,"all")=="No verb in this phrase before current wordPosition.") {  # should contain no verb in same phrase, e.g. Well, 
            tmp4<<-rbind(tmp4, matchedTextualExpInContinuative) 
          }
        }
        }


      for (i in 1:length(tmpTextualExpListInAdjunctAll)) {
        clauseSeg <- subset(POSresult$token, POSresult$sentence==sentencePosition )
        tmpTextualExpListAdjunctVector <- scan(text=tmpTextualExpListInAdjunctAll[i], sep=" ", what="", quiet=TRUE)
        matchedTextualExpInAdjunct <- keywords_phrases(clauseSeg, pattern = tmpTextualExpListAdjunctVector) 
        if (nrow(matchedTextualExpInAdjunct)!=0){
          index<-as.numeric(rownames(subset(POSresult, POSresult$sentence==sentencePosition & POSresult$token==tmpTextualExpListAdjunctVector[1])) )
          if (hasVerbInPhrase(index)=="No verb in this phrase."  || hasVerbInPhraseBfWordPosition(index,"all")=="No verb in this phrase before current wordPosition.") { # should contain no verb in the same phrase, e.g. In other words,
          tmp4<<-rbind(tmp4, matchedTextualExpInAdjunct)
          }
        }
      }

      for (i in 1:length(tmpTextualExpListInStructuralConjunctionAll)) {
        clauseSeg <- subset(POSresult$token, POSresult$sentence==sentencePosition ) 
        tmpTextualExpListStructuralConjunctionVector <- scan(text=tmpTextualExpListInStructuralConjunctionAll[i], sep=" ", what="", quiet=TRUE)
        matchedTextualExpInStructuralConjunction <- keywords_phrases(clauseSeg, pattern = tmpTextualExpListStructuralConjunctionVector) 
        
        if (nrow(matchedTextualExpInStructuralConjunction)!=0){
          
          index<-as.numeric(rownames(subset(POSresult, POSresult$sentence==sentencePosition & POSresult$token==tmpTextualExpListStructuralConjunctionVector[1])) )
          if (!((POSresult$token[index]=="When" || POSresult$token[index]=="Which" || POSresult$token[index]=="Where") && POSresult$token[which(POSresult$sentence==sentencePosition & POSresult$id==(length(subset(POSresult$id, POSresult$sentence==sentencePosition))))]=="?")){ # excluding "When","Which","Where" if clauses ends with question mark. They are Interpersonal if end with question mark, not textual.
          if ((hasFiniteVerbInClauseBfWordPosition(index)==TRUE) && (hasVerbInRemainingPhrase(index,"finite")==TRUE)) { # should have verb(s) in the same phrase, e.g. They felt sad (since) assult troops were forced to flight their way
          tmp4<<-rbind(tmp4, matchedTextualExpInStructuralConjunction)
          }
          if (((POSresult$POS[index-1]==".") || (POSresult$POS[index-1]=="``") || (POSresult$POS[index-1]=="-RRB-")) 
              && ((hasVerbInRemainingPhrase(index,"finite")==TRUE) || ((POSresult$POS[index+1]==",") && (POSresult$POS[index+3]==",") && hasVerbInRemainingPhrase(index+4,"finite")==TRUE)) ) { # e.g. ". As" "``Although..." Also, "And, yes, that includes..." i.e. if at wordPosition of "And", [index+1]=="," [index+3]=="," and then check hasVerbInRemainingPhrase at wordPosition of "that".
          tmp4<<-rbind(tmp4, matchedTextualExpInStructuralConjunction)
          }
          else{
          if (is.null(tmp4$keyword[nrow(tmp4)])==FALSE) { # to avoid Error in if ((POSresult$token[index - 1] == tmp4$keyword[nrow(tmp4)]) &&  : missing value where TRUE/FALSE needed
          if ((POSresult$token[index-1] == tmp4$keyword[nrow(tmp4)]) && (hasVerbInRemainingPhrase(index,"finite")==TRUE)) { # e.g. ". But if", i.e. [index]=="if", [index-1]=="But", "But" %in% tmp4 is TRUE, then new tmp4 is "But, if". 
          tmp4<<-rbind(tmp4, matchedTextualExpInStructuralConjunction)}  
          }
          }
          }
        }
      }

   
      for (i in 1:length(tmpTextualExpListInConjunctivePrepAll)) {
        clauseSeg <- subset(POSresult$token, POSresult$sentence==sentencePosition ) 
        tmpTextualExpListConjunctivePrepVector <- scan(text=tmpTextualExpListInConjunctivePrepAll[i], sep=" ", what="", quiet=TRUE)
        matchedTextualExpInConjunctivePrep <- keywords_phrases(clauseSeg, pattern = tmpTextualExpListConjunctivePrepVector) 
    
        if (nrow(matchedTextualExpInConjunctivePrep)!=0){
          index<-as.numeric(rownames(subset(POSresult, POSresult$sentence==sentencePosition & POSresult$token==tmpTextualExpListConjunctivePrepVector[1])) )
          if (hasVerbInRemainingPhrase(index,"non-finite")==TRUE && (abs(positionNextVerbInRemainingPhrase(index,"non-finite")-index)<=1)) { # e.g. (after eating) dinner. Also, && (abs(positionNextVerbInRemainingPhrase(index,"non-finite")-index)<=1) is to verify that a next non-finite verb is nearby to (index).
            tmp4<<-rbind(tmp4, matchedTextualExpInConjunctivePrep)
          }
          
   
        if (is.null(tmp4$keyword[nrow(tmp4)])==FALSE) { # to avoid Error in if ((POSresult$token[index - 1] == tmp4$keyword[nrow(tmp4)]) &&  : missing value where TRUE/FALSE needed
          if ((POSresult$token[index-1] == tmp4$keyword[nrow(tmp4)]) && (hasVerbInRemainingPhrase(index,"non-finite")==TRUE)  && (abs(positionNextVerbInRemainingPhrase(index,"non-finite")-index)<=1) ) { # e.g. ". (But) (on) using...", i.e. [index]=="on", the last identified textual item==[index-1]=="But", "But" %in% tmp4 is TRUE, then new tmp4 is "But, on".
          tmp4<<-rbind(tmp4, matchedTextualExpInConjunctivePrep)
          }
        }
        }
      }

      for (i in 1:length(tmpTextualExpListInWhElementAll)) {
        clauseSeg <- subset(POSresult$token, POSresult$sentence==sentencePosition )
        tmpTextualExpListWhElementVector <- scan(text=tmpTextualExpListInWhElementAll[i], sep=" ", what="", quiet=TRUE)
        matchedTextualExpInWhElement <- keywords_phrases(clauseSeg, pattern = tmpTextualExpListWhElementVector) 

        if (nrow(matchedTextualExpInWhElement)!=0){
         index<-as.numeric(rownames(subset(POSresult, POSresult$sentence==sentencePosition & POSresult$token==tmpTextualExpListWhElementVector[1])) )
    
        if (POSresult$token[which(POSresult$sentence==sentencePosition & POSresult$id==(length(subset(POSresult$id, POSresult$sentence==sentencePosition))))]!="?") {# Check if the clause does not end with a question mark, i.e. not interpersonal.
        if ((hasVerbInRemainingPhrase(index,"finite")==TRUE) || hasFiniteVerbInClauseBfWordPosition(index)==TRUE) { # e.g. They will write a computer program, (which) can help us process the large volumn of online data.
          tmp4<<-rbind(tmp4, matchedTextualExpInWhElement)
          
        }
        }
       }
      }
    
      if (is.null(tmp4)==TRUE){tmp4<<-"/"}
  }


##################### Continue loading the table of extual expressions, and clean up semicolons in the "Conjunction" related columns (shared with TextualTheme function).
TextualExpListInConjunctionAll <- NULL  
tmpTextualExpListInConjunctionAll <- NULL

for (counterInTextualExp in 1:nrow(textualExp)){
  tmpTextualExpListInStructuralConjunction <-scan(text=textualExp$StructuralConjunction[counterInTextualExp], sep=",", what ="", quiet=TRUE)
  tmpTextualExpListInStructuralConjunction <-gsub("^\\s+|\\s+$", "", tmpTextualExpListInStructuralConjunction)  #trim leading and trailing whitespaces
  tmpTextualExpListInConjunctivePrep <-scan(text=textualExp$ConjunctivePrep[counterInTextualExp], sep=",", what ="", quiet=TRUE)
  tmpTextualExpListInConjunctivePrep <-gsub("^\\s+|\\s+$", "", tmpTextualExpListInConjunctivePrep)
    
  tmpTextualExpListInConjunctionAll <- c(tmpTextualExpListInConjunctionAll, tmpTextualExpListInStructuralConjunction, tmpTextualExpListInConjunctivePrep)
}
TextualExpListInConjunctionAll <- tmpTextualExpListInConjunctionAll
##################### function of detecting any conjunction in the clause after current wordPosition.
hasConjunctionInRemainingClause <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  if(POSresult$POS[wordPosition]=="." || POSresult$token[wordPosition]=="!?" || POSresult$token[wordPosition]=="?!"){return(FALSE)}
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  sentenceLength <- 0
  idNo <-0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  sentenceLength <-  length(subset(POSresult$id, POSresult$sentence==sentenceNo))
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  
  for (i in (idNo+1):sentenceLength){  # (idNo+1): is to skip the POS of wordPosition itself in the sentence. It it the only verb, e.g (Think) for myself.
    if(tmpPOSresult[i,4] %in% TextualExpListInConjunctionAll){ 
      return (TRUE)}
  }
  return (FALSE)
}


  
################ initialising the variable
counterInInterpAdju <- 0
tmpInterpAdjuListInAdjunctMood <- NULL
tmpInterpAdjuListInAdjunctComment <- NULL
tmpInterpAdjuListInAdjunctAddressing <- NULL
tmpInterpAdjuListAll <- NULL
tmpInterpAdjuListAllVector <- NULL
matchedInterpAdjuInListAll <- NULL
clauseSeg <- NULL

################# loading the table of interpersonal Adjunct
InterpAdju <-read.table(file=paste0(filePath,"tmp//","interpersonal_adjunct_v2.txt"), sep="\t", quote = "", header=TRUE)  # to deal with single quote in interpersonal_adjunct_v?.txt, add  quote = "".

################ clean up semicolons in columns of "AdjunctMood" and "AdjunctComment"
InterpAdju$AdjunctMood <- gsub(";", ",", InterpAdju$AdjunctMood)
InterpAdju$AdjunctComment <- gsub(";", ",", InterpAdju$AdjunctComment)
InterpAdju$AdjunctAddressing <- gsub(";", ",", InterpAdju$AdjunctAddressing)
InterpAdju[InterpAdju=="null"]<- NA

################# A function to check with interpersonal_adjunct_v?.txt and to identify whether the POSresult$token[wordPosition] is an interpersonal one
interpersonalAdjunct <- function(sentencePosition){
  if((nrow(subset(POSresult, POSresult$sentence==sentencePosition)))==0 ){return("Invalid")}  #return "Invalid" if the sentencePosition is out of range
  
  for (counterInInterpAdju in 1:nrow(InterpAdju)){
    tmpInterpAdjuListInAdjunctMood <-scan(text=InterpAdju$AdjunctMood[counterInInterpAdju], sep=",", what ="", quiet=TRUE)
    tmpInterpAdjuListInAdjunctMood <-gsub("^\\s+|\\s+$", "", tmpInterpAdjuListInAdjunctMood) #trim leading and trailing whitespaces
    tmpInterpAdjuListInAdjunctComment <-scan(text=InterpAdju$AdjunctComment[counterInInterpAdju], sep=",", what ="", quiet=TRUE)
    tmpInterpAdjuListInAdjunctComment <-gsub("^\\s+|\\s+$", "", tmpInterpAdjuListInAdjunctComment)
    tmpInterpAdjuListInAdjunctAddressing <-scan(text=InterpAdju$AdjunctAddressing[counterInInterpAdju], sep=",", what ="", quiet=TRUE)
    tmpInterpAdjuListInAdjunctAddressing <-gsub("^\\s+|\\s+$", "", tmpInterpAdjuListInAdjunctAddressing)    
    
    tmpInterpAdjuListAll <- c(tmpInterpAdjuListAll, tmpInterpAdjuListInAdjunctMood, tmpInterpAdjuListInAdjunctComment, tmpInterpAdjuListInAdjunctAddressing)
  }
  

  for (i in 1:length(tmpInterpAdjuListAll)) {
    clauseSeg <- subset(POSresult$token, POSresult$sentence==sentencePosition ) 
    
    tmpInterpAdjuListAllVector <- scan(text=tmpInterpAdjuListAll[i], sep=" ", what="", quiet=TRUE)
    matchedInterpAdjuInListAll <- keywords_phrases(clauseSeg, pattern = tmpInterpAdjuListAllVector) 
    if (nrow(matchedInterpAdjuInListAll)!=0){
      index<-as.numeric(rownames(subset(POSresult, POSresult$sentence==sentencePosition & POSresult$token==tmpInterpAdjuListAllVector[1])) )
      if ((hasVerbInPhrase(index)=="No verb in this phrase.") || (hasFiniteVerbInClauseBfWordPosition(index)==FALSE)) { # As always,... || frankly my dear I don't care.
        tmp5a<<-rbind(tmp5a, matchedInterpAdjuInListAll) 
      }
    }
  }
  
  if (is.null(tmp5a)==TRUE){tmp5a<<-"/"}
}




################ identify the position of the first finite verb in the sentence
positionFirstFiniteVerbInSentence <- function(sentencePosition){
  if((nrow(subset(POSresult, POSresult$sentence==sentencePosition)))==0 ){return("Invalid")}  #return "Invalid" if the sentencePosition is out of range
  sentenceLength <- 0
  
  sentenceLength <-  nrow(subset(POSresult, POSresult$sentence==sentencePosition))
  tmpPOSresult <- subset(POSresult$POS, POSresult$sentence==sentencePosition)
  
  for (i in 1:sentenceLength){
    if(i==1){
    if((tmpPOSresult[i]=="VB") || (tmpPOSresult[i]=="VBP") || (tmpPOSresult[i]=="VBZ") || (tmpPOSresult[i]=="VBD") || (tmpPOSresult[i]=="MD")){ #no VBG, VBN counted in the same clause until end (full stop)
      return (i)}}
    
    if(i>1){
      if(!(tmpPOSresult[i-1]=="TO")){
      if((tmpPOSresult[i]=="VB") || (tmpPOSresult[i]=="VBP") || (tmpPOSresult[i]=="VBZ") || (tmpPOSresult[i]=="VBD") || (tmpPOSresult[i]=="MD")){ #no VBG, VBN counted in the same clause until end (full stop)
        return (i)}}}
    }
  return ("No verb in this clause.")
}


################ identify the position of the next finite and/or non-finite verb in the phrase
positionNextVerbInRemainingPhrase <- function(wordPosition,finiteness){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  if((POSresult$UDPOS[wordPosition]=="." && POSresult$lemma[wordPosition]!="``") || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return("Phrase end.")} # use UDPOS to include ".",",","!","?",  but need to exclude double single quote ' ', as it is the open of a quotation in clause in text source.
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  idNo <-0
  tmpPOSresult <- NULL
  punctuationPosition <- 0
  currentPhraseEnd <- 0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  punctuationPosition <- which(!is.na( match(tmpPOSresult$lemma, c(".",",","!","?","?!","!?","``","''")) ) )  # e.g. [1]  4  7 14 !is.na of [1] NA NA NA  2 NA NA  2 NA NA NA NA NA NA  1
  
  if (length(which((punctuationPosition > idNo))) != 0){  # to avoid warning of "which((  which(!is.na(match(tmpx, tmpy)))  ) > 14)"  with a result of "integer(0)"
    currentPhraseEnd <- min(which((punctuationPosition > idNo))) } # e.g. if wordPosition==5, as 5 >4 and <7, then result is [1] 2
  
  if (currentPhraseEnd !=0 && finiteness=="all"){
    for (i in idNo:as.numeric(punctuationPosition[currentPhraseEnd])){  # check if in the phrase has any verb, e.g since January, ...
      if((tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB") || (tmpPOSresult$POS[i]=="VBP") || (tmpPOSresult$POS[i]=="VBZ") || (tmpPOSresult$POS[i]=="VBG") || (tmpPOSresult$POS[i]=="VBD") || (tmpPOSresult$POS[i]=="VBN")){ 
        return (i-1+as.numeric(rownames(subset(POSresult, POSresult$sentence==sentenceNo & POSresult$id==1)) ) )}
    }
  }
  
  if (currentPhraseEnd !=0 && finiteness=="non-finite"){
    for (i in idNo:as.numeric(punctuationPosition[currentPhraseEnd])){  # check if in the phrase has any verb, e.g since January, ...
      if( (length(tmpPOSresult$POS[i-1])!=0) && !(length(tmpPOSresult$POS[i-2])>1) && is.na(tmpPOSresult$POS[i+1])==FALSE ){   # (i) (length(POSresult$POS[1]))  [1] 1. (ii) (length(POSresult$POS[0]))  [1] 0. (iii) length(POSresult$token[-1])  [1] 1224. (iv) (POSresult$POS[20000])  [1] NA.
        if( ((tmpPOSresult$POS[i-1]=="TO" && tmpPOSresult$POS[i]=="VB") || (tmpPOSresult$POS[i]=="VBG") || (tmpPOSresult$POS[i]=="VBN")) && 
            (!((tmpPOSresult$POS[i-2]=="MD") && (tmpPOSresult$POS[i-1]=="VB")) || !((tmpPOSresult$POS[i-1]=="VBZ" || tmpPOSresult$POS[i-1]=="VBD") && (tmpPOSresult$POS[i+1]=="VBN")) ) ){   # excluding, e.g. might be going / MD VB VBG, or is (was) being developed / VBZ (VBD) VBG VBN
          return (i-1+as.numeric(rownames(subset(POSresult, POSresult$sentence==sentenceNo & POSresult$id==1)) ) )}
      }
    }
  }
  
  if (currentPhraseEnd !=0  && finiteness=="finite"){
    for (i in idNo:as.numeric(punctuationPosition[currentPhraseEnd])){  # check if in the phrase has any verb, e.g since January, ...
      if( length(tmpPOSresult$POS[i-1])!=0 ){
        if( ((tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB") || (tmpPOSresult$POS[i]=="VBP") || (tmpPOSresult$POS[i]=="VBZ") || (tmpPOSresult$POS[i]=="VBD") ) && 
            ((tmpPOSresult$POS[i-1]!="TO") && (tmpPOSresult$POS[i]!="VB")) ){  # excluding, e.g. after wherever to go for / TO VB IN
          return (i-1+as.numeric(rownames(subset(POSresult, POSresult$sentence==sentenceNo & POSresult$id==1)) ) )}
      }
    }
  }
  return (9999) # 9999 means ("No verb in this remaining phrase.")
}



################# identify that there is/are any verb in the phrase
hasVerbInPhrase <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  if((POSresult$UDPOS[wordPosition]=="." && POSresult$lemma[wordPosition]!="``") || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return("Phrase end.")} # use UDPOS to include ".",",","!","?", but need to exclude double single quote ` `, as it is the open of a quotation in clause in text source.
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  idNo <-0
  tmpPOSresult <- NULL
  punctuationPosition <- 0
  currentPhraseEnd <- 0
  currentPhraseStart <- 1
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  punctuationPosition <- which(!is.na( match(tmpPOSresult$lemma, c(".",",","!","?","?!","!?","``","''")) ) )  # e.g. [1]  4  7 14 !is.na of [1] NA NA NA  2 NA NA  2 NA NA NA NA NA NA  1

  if (length(which((punctuationPosition > idNo))) != 0){  # to avoid warning of "which((  which(!is.na(match(tmpx, tmpy)))  ) > 14)"  with a result of "integer(0)"
      currentPhraseEnd <- min(which((punctuationPosition > idNo))) } # e.g. if wordPosition==5, as 5 >4 and <7, then result is [1] 2
  if (currentPhraseEnd!=1){currentPhraseStart <- punctuationPosition[currentPhraseEnd-1] }
  if (currentPhraseEnd !=0){
  for (i in (currentPhraseStart):(as.numeric(punctuationPosition[currentPhraseEnd])-1) ){  # check if in the phrase has any verb, e.g since January, ...
    if((tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB") || (tmpPOSresult$POS[i]=="VBP") || (tmpPOSresult$POS[i]=="VBZ") || (tmpPOSresult$POS[i]=="VBG") || (tmpPOSresult$POS[i]=="VBD") || (tmpPOSresult$POS[i]=="VBN")){ 
      return (TRUE)}
  }
  }
  return ("No verb in this phrase.")
}




################# identify that there is/are any verb in the phrase after the current wordPosition and finiteness (3 types: all/finite/non-finite)
hasVerbInRemainingPhrase <- function(wordPosition,finiteness){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  if(POSresult$POS[wordPosition]=="." || POSresult$POS[wordPosition]=="," || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return("Phrase end.")} 
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  idNo <-0
  tmpPOSresult <- NULL
  punctuationPosition <- 0
  currentPhraseEnd <- 0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  punctuationPosition <- which(!is.na( match(tmpPOSresult$lemma, c(".",",","!","?","?!","!?","``","''")) ) )  # e.g. [1]  4  7 14 !is.na of [1] NA NA NA  2 NA NA  2 NA NA NA NA NA NA  1
  
  if (length(which((punctuationPosition > idNo))) != 0){  # to avoid warning of "which((  which(!is.na(match(tmpx, tmpy)))  ) > 14)"  with a result of "integer(0)"
    currentPhraseEnd <- min(which((punctuationPosition > idNo))) } # e.g. if wordPosition==5, as 5 >4 and <7, then result is [1] 2
  
  if (currentPhraseEnd !=0 && finiteness=="all"){
    for (i in idNo:as.numeric(punctuationPosition[currentPhraseEnd])){  # check if in the phrase has any verb, e.g since January, ...
      if(tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB" || tmpPOSresult$POS[i]=="VBP" || tmpPOSresult$POS[i]=="VBZ" || tmpPOSresult$POS[i]=="VBG" || tmpPOSresult$POS[i]=="VBD" || tmpPOSresult$POS[i]=="VBN"){ 
        return (TRUE)}
    }
  }
  
  if (currentPhraseEnd !=0 && finiteness=="non-finite"){
    for (i in idNo:as.numeric(punctuationPosition[currentPhraseEnd])){  # check if in the phrase has any verb, e.g since January, ...
      if( (length(tmpPOSresult$POS[i-1])!=0) && !(length(tmpPOSresult$POS[i-2])>1) && is.na(tmpPOSresult$POS[i+1])==FALSE ){   # (i) (length(POSresult$POS[1]))  [1] 1. (ii) (length(POSresult$POS[0]))  [1] 0. (iii) length(POSresult$token[-1])  [1] 1224. (iv) (POSresult$POS[20000])  [1] NA.
        if( ((tmpPOSresult$POS[i-1]=="TO" && tmpPOSresult$POS[i]=="VB") || (tmpPOSresult$POS[i]=="VBG") || (tmpPOSresult$POS[i]=="VBN")) && 
            (!((tmpPOSresult$POS[i-2]=="MD") && (tmpPOSresult$POS[i-1]=="VB")) || !((tmpPOSresult$POS[i-1]=="VBZ" || tmpPOSresult$POS[i-1]=="VBD") && (tmpPOSresult$POS[i+1]=="VBN")) ) ){   # excluding, e.g. might be going / MD VB VBG, or is (was) being developed / VBZ (VBD) VBG VBN
          return (TRUE)}
      }
    }
  }
  
  if (currentPhraseEnd !=0  && finiteness=="finite"){
    for (i in idNo:as.numeric(punctuationPosition[currentPhraseEnd])){  # check if in the phrase has any verb, e.g since January, ...
      if( length(tmpPOSresult$POS[i-1])!=0 ){
        if( (tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB" || tmpPOSresult$POS[i]=="VBP" || tmpPOSresult$POS[i]=="VBZ" || tmpPOSresult$POS[i]=="VBD" ) && 
            (!(tmpPOSresult$POS[i-1]=="TO" && tmpPOSresult$POS[i]=="VB")) ){  # excluding, e.g. after wherever to go for / TO VB IN
          return (TRUE)}
      }
    }
  }
  return ("No verb in this remaining phrase.")
}




################# identify that there is/are any verb in the phrase BEFORE the current wordPosition and finiteness (3 types: all/finite/non-finite)
hasVerbInPhraseBfWordPosition <- function(wordPosition,finiteness){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  if(POSresult$POS[wordPosition]=="." || POSresult$POS[wordPosition]=="," || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return("Phrase end.")} 
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  idNo <-0
  idNoStart <-0
  tmpPOSresult <- NULL
  punctuationPosition <- 0
  currentPhraseStart <- 0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  punctuationPosition <- which(!is.na( match(tmpPOSresult$lemma, c(".",",","!","?","?!","!?","``","''")) ) )  # e.g. [1]  4  7 14 !is.na of [1] NA NA NA  2 NA NA  2 NA NA NA NA NA NA  1
  
  if (length(which((punctuationPosition > idNo))) != 0){  # to avoid warning of "which((  which(!is.na(match(tmpx, tmpy)))  ) > 14)"  with a result of "integer(0)"
    currentPhraseStart <- min(which((punctuationPosition > idNo))) -1 } # e.g. if wordPosition==5, as 5 >4 and <7, then result of min(which((punctuationPosition > idNo))) is [1] 2. Then 2-1 =1

  if (finiteness=="all"){
    if(currentPhraseStart==0){idNoStart<-1}else{idNoStart<-as.numeric(punctuationPosition[currentPhraseStart])}
    for (i in  idNoStart:idNo){  # check if in the phrase has any verb, e.g since January, ...
      if(tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB" || tmpPOSresult$POS[i]=="VBP" || tmpPOSresult$POS[i]=="VBZ" || tmpPOSresult$POS[i]=="VBG" || tmpPOSresult$POS[i]=="VBD" || tmpPOSresult$POS[i]=="VBN"){ 
        return (TRUE)}
    }
  }
  
  if (finiteness=="non-finite"){
    if(currentPhraseStart==0){idNoStart<-1}else{idNoStart<-as.numeric(punctuationPosition[currentPhraseStart])}
    for (i in  idNoStart:idNo){  # check if in the phrase has any verb, e.g since January, ...
      if( (length(tmpPOSresult$POS[i-1])!=0) && !(length(tmpPOSresult$POS[i-2])>1) && is.na(tmpPOSresult$POS[i+1])==FALSE ){   # (i) (length(POSresult$POS[1]))  [1] 1. (ii) (length(POSresult$POS[0]))  [1] 0. (iii) length(POSresult$token[-1])  [1] 1224. (iv) (POSresult$POS[20000])  [1] NA.
        if( ((tmpPOSresult$POS[i-1]=="TO" && tmpPOSresult$POS[i]=="VB") || (tmpPOSresult$POS[i]=="VBG") || (tmpPOSresult$POS[i]=="VBN")) && 
            (!((tmpPOSresult$POS[i-2]=="MD") && (tmpPOSresult$POS[i-1]=="VB")) || !((tmpPOSresult$POS[i-1]=="VBZ" || tmpPOSresult$POS[i-1]=="VBD") && (tmpPOSresult$POS[i+1]=="VBN")) ) ){   # excluding, e.g. might be going / MD VB VBG, or is (was) being developed / VBZ (VBD) VBG VBN
          return (TRUE)}
      }
    }
  }
  
  if (finiteness=="finite"){
    if(currentPhraseStart==0){idNoStart<-1}else{idNoStart<-as.numeric(punctuationPosition[currentPhraseStart])}
    for (i in  idNoStart:idNo){  # check if in the phrase has any verb, e.g since January, ...

        if(i==1){
          if(tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB" || tmpPOSresult$POS[i]=="VBP" || tmpPOSresult$POS[i]=="VBZ" || tmpPOSresult$POS[i]=="VBD"){
            return (TRUE)}
        }
      
        if(i>1){
          if( (tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB" || tmpPOSresult$POS[i]=="VBP" || tmpPOSresult$POS[i]=="VBZ" || tmpPOSresult$POS[i]=="VBD") && 
            (!(tmpPOSresult$POS[i-1]=="TO" && tmpPOSresult$POS[i]=="VB")) ){  # excluding, e.g. after wherever to go for / TO VB IN
          return (TRUE)}
        }
#    }
    }}
  return ("No verb in this phrase before current wordPosition.")
  
}




#loading default verbs serving as PROCESS table
#verbsAsProcess<-scan(file=paste0(filePath,"tmp//","verbs serving as Process_table1_v1.txt"), what=character(), sep="\t", quiet=TRUE, blank.lines.skip=TRUE)
verbsAsProcess<-read.table(file=paste0(filePath,"tmp//","verbs serving as Process_table1_v1_3.txt"), sep="\t", header=TRUE)

#clean up semicolons in columns of "middle_ranged", "middle_non.ranged", and "effective"
verbsAsProcess$middle_ranged <- gsub(";", ",", verbsAsProcess$middle_ranged)
verbsAsProcess$middle_non.ranged <- gsub(";", ",", verbsAsProcess$middle_non.ranged)
verbsAsProcess$effective <- gsub(";", ",", verbsAsProcess$effective)
verbsAsProcess[verbsAsProcess=="null"]<- NA


counterInVerbsAsProcess<-0
counterinSynonymsOfLemma <- 0
matchedRowInLevinVerbClasses <- 0
tmpVerbListInMidRangedCol<-NULL
tmpVerbListInMidNonRangedCol<-NULL
tmpVerbListInEffectiveCol<-NULL
ngramMatched<-"N"


experientialPROCESStype <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range

    for (counterInVerbsAsProcess in 1:nrow(verbsAsProcess)){
    tmpVerbListInMidRangedCol <-scan(text=verbsAsProcess$middle_ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidRangedCol) #trim leading and trailing whitespaces
    tmpVerbListInMidNonRangedCol <-scan(text=verbsAsProcess$middle_non.ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidNonRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidNonRangedCol)
    tmpVerbListInEffectiveCol <-scan(text=verbsAsProcess$effective[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInEffectiveCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInEffectiveCol)    



    ## the following line of code checks bigram of lemma(s) against verbsAsProcess
    if((paste(POSresult$lemma[wordPosition],POSresult$lemma[wordPosition+1]) %in% tmpVerbListInMidRangedCol) || (paste(POSresult$lemma[wordPosition],POSresult$lemma[wordPosition+1]) %in% tmpVerbListInMidNonRangedCol) || (paste(POSresult$lemma[wordPosition],POSresult$lemma[wordPosition+1]) %in% tmpVerbListInEffectiveCol)) {  # pasting the [wordPosition] and the [wordPosition+1] is to search lemma in bigram, e.g. "set up"
    InVerbsAsProcess <<- "Y"
    ngramMatched<-"Y"

      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "material"){tmp10 <<- "material"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "behavioural"){tmp11 <<- "behavioural"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "mental"){tmp12 <<- "mental"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "verbal"){tmp13 <<- "verbal"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "relational"){tmp14 <<- "relational"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "existential"){tmp15 <<- "existential"}

      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("PROCESS via VerbAsProcess table = ", verbsAsProcess$process.type[counterInVerbsAsProcess]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched bi-gram or not? ", ngramMatched))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched ones in the row of verbsAsProcess table= Row", counterInVerbsAsProcess))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemmas in text= ", paste(POSresult$lemma[wordPosition], POSresult$lemma[wordPosition+1]))) ) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Tokens in text= ", paste(POSresult$token[wordPosition], POSresult$token[wordPosition+1]))) ) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
    
      theVerb<<-paste(POSresult$token[wordPosition], POSresult$token[wordPosition+1])
    }
    }
  
  for (counterInVerbsAsProcess in 1:nrow(verbsAsProcess)){
    tmpVerbListInMidRangedCol <-scan(text=verbsAsProcess$middle_ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidRangedCol) #trim leading and trailing whitespaces
    tmpVerbListInMidNonRangedCol <-scan(text=verbsAsProcess$middle_non.ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidNonRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidNonRangedCol)
    tmpVerbListInEffectiveCol <-scan(text=verbsAsProcess$effective[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInEffectiveCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInEffectiveCol)    
    
      
    if(ngramMatched=="N" && ((POSresult$lemma[wordPosition] %in% tmpVerbListInMidRangedCol) || (POSresult$lemma[wordPosition] %in% tmpVerbListInMidNonRangedCol) || (POSresult$lemma[wordPosition] %in% tmpVerbListInEffectiveCol)) ){  # ngramMatched=="N" is to count only those bi-gram item(s), e.g. counting "occur to", no double counts on "occur" + "occur to" as total.
        InVerbsAsProcess <<- "Y"
        
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "material"){tmp10 <<- "material"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "behavioural"){tmp11 <<- "behavioural"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "mental"){tmp12 <<- "mental"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "verbal"){tmp13 <<- "verbal"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "relational"){tmp14 <<- "relational"}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "existential"){tmp15 <<- "existential"}
      
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("PROCESS via VerbAsProcess table = ", verbsAsProcess$process.type[counterInVerbsAsProcess]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched one in the row of verbsAsProcess table= Row", counterInVerbsAsProcess))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemma in text= ", POSresult$lemma[wordPosition]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Token in text= ", POSresult$token[wordPosition]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
        
      theVerb<<-POSresult$token[wordPosition]
    }
    }

  
  if (is.null(tmp10)==TRUE){tmp10<<-"/"}
  if (is.null(tmp11)==TRUE){tmp11<<-"/"}
  if (is.null(tmp12)==TRUE){tmp12<<-"/"}
  if (is.null(tmp13)==TRUE){tmp13<<-"/"}
  if (is.null(tmp14)==TRUE){tmp14<<-"/"}
  if (is.null(tmp15)==TRUE){tmp15<<-"/"}
}


##### A function to verify the synonyms (from WordNet) of the POSresult$lemma[wordPosition] that exists in verbsAsProcess
experientialPROCESStypeViaSynonyms <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  synonymsOfLemma <- synonyms(POSresult$lemma[wordPosition], "VERB")
  
  if (length(synonymsOfLemma) != 0){  #### do not continue to run this function if no matched synonyms can be found in the WordNet
    
  for (counterinSynonymsOfLemma in 1:length(synonymsOfLemma)){
  
  for (counterInVerbsAsProcess in 1:nrow(verbsAsProcess)){
    tmpVerbListInMidRangedCol <-scan(text=verbsAsProcess$middle_ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidRangedCol) #trim leading and trailing whitespaces
    tmpVerbListInMidNonRangedCol <-scan(text=verbsAsProcess$middle_non.ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidNonRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidNonRangedCol)
    tmpVerbListInEffectiveCol <-scan(text=verbsAsProcess$effective[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInEffectiveCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInEffectiveCol)    
    

    ##### the following line of code can check unigram and bigram of synonymsOflemma(s) against verbsAsProcess
    if((synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInMidRangedCol) || (synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInMidNonRangedCol) || (synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInEffectiveCol) ){
      ViaSynonyms <<- "Y"
      
      
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "material"){tmp10 <<- "material"; tmp10Counter <<- tmp10Counter + 1}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "behavioural"){tmp11 <<- "behavioural"; tmp11Counter <<- tmp11Counter + 1}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "mental"){tmp12 <<- "mental"; tmp12Counter <<- tmp12Counter + 1}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "verbal"){tmp13 <<- "verbal"; tmp13Counter <<- tmp13Counter + 1}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "relational"){tmp14 <<- "relational"; tmp14Counter <<- tmp14Counter + 1}
      if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "existential"){tmp15 <<- "existential"; tmp15Counter <<- tmp15Counter + 1}

      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("PROCESS via WordNet's Synonyms= ", verbsAsProcess$process.type[counterInVerbsAsProcess]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched WordNet's synonyms in the row of verbsAsProcess table= Row", counterInVerbsAsProcess))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemma in text= ", POSresult$lemma[wordPosition]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Token in text= ", POSresult$token[wordPosition]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched WordNet's synonyms= ", synonymsOfLemma[counterinSynonymsOfLemma]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
      
      theVerb<<-POSresult$token[wordPosition]
    }
  }
  if (is.null(tmp10)==TRUE){tmp10<<-"/"}
  if (is.null(tmp11)==TRUE){tmp11<<-"/"}
  if (is.null(tmp12)==TRUE){tmp12<<-"/"}
  if (is.null(tmp13)==TRUE){tmp13<<-"/"}
  if (is.null(tmp14)==TRUE){tmp14<<-"/"}
  if (is.null(tmp15)==TRUE){tmp15<<-"/"}
}
}
  
}



############### A function to verify the synonyms (from QDAP Dicts) of the POSresult$lemma[wordPosition] that exists in verbsAsProcess
experientialPROCESStypeViaQdapSynonyms <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  maxListUsedfromDict <- 3
  synonymsOfLemma <- unname(unlist(qdap::synonyms(POSresult$lemma[wordPosition], return.list = TRUE)[1:maxListUsedfromDict]))
  
  if (length(synonymsOfLemma) != 0){  #### do not continue to run this function if no matched synonyms can be found in the WordNet
    
    for (counterinSynonymsOfLemma in 1:length(synonymsOfLemma)){
      
      for (counterInVerbsAsProcess in 1:nrow(verbsAsProcess)){
        tmpVerbListInMidRangedCol <-scan(text=verbsAsProcess$middle_ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
        tmpVerbListInMidRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidRangedCol) #trim leading and trailing whitespaces
        tmpVerbListInMidNonRangedCol <-scan(text=verbsAsProcess$middle_non.ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
        tmpVerbListInMidNonRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidNonRangedCol)
        tmpVerbListInEffectiveCol <-scan(text=verbsAsProcess$effective[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
        tmpVerbListInEffectiveCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInEffectiveCol)    
        
        
        ##### the following line of code can check unigram and bigram of synonymsOflemma(s) against verbsAsProcess
        if((synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInMidRangedCol) || (synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInMidNonRangedCol) || (synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInEffectiveCol) ){
          ViaQdapSynonyms <<- "Y"
          
          
          if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "material"){tmp10 <<- "material"; tmp10Counter <<- tmp10Counter + 1}
          if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "behavioural"){tmp11 <<- "behavioural"; tmp11Counter <<- tmp11Counter + 1}
          if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "mental"){tmp12 <<- "mental"; tmp12Counter <<- tmp12Counter + 1}
          if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "verbal"){tmp13 <<- "verbal"; tmp13Counter <<- tmp13Counter + 1}
          if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "relational"){tmp14 <<- "relational"; tmp14Counter <<- tmp14Counter + 1}
          if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "existential"){tmp15 <<- "existential"; tmp15Counter <<- tmp15Counter + 1}
          
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("PROCESS via Qdap's Synonyms= ", verbsAsProcess$process.type[counterInVerbsAsProcess]))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched Qdap's synonyms in the row of verbsAsProcess table= Row", counterInVerbsAsProcess))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemma in text= ", POSresult$lemma[wordPosition]))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Token in text= ", POSresult$token[wordPosition]))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched Qdap's synonyms= ", synonymsOfLemma[counterinSynonymsOfLemma]))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
          if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
          
          theVerb<<-POSresult$token[wordPosition]
        }
      }
      if (is.null(tmp10)==TRUE){tmp10<<-"/"}
      if (is.null(tmp11)==TRUE){tmp11<<-"/"}
      if (is.null(tmp12)==TRUE){tmp12<<-"/"}
      if (is.null(tmp13)==TRUE){tmp13<<-"/"}
      if (is.null(tmp14)==TRUE){tmp14<<-"/"}
      if (is.null(tmp15)==TRUE){tmp15<<-"/"}
    }
  }
  
}



######loading default Levin verb classes for PROCESS matching
levinVerbClasses <-read.table(file=paste0(filePath,"tmp//","levin_verb_classes_v5.txt"), sep="\t", header=TRUE)

###### A function to verify the synonyms (from Levin Verb Classes) of the POSresult$lemma[wordPosition] that exists in levinVerbClasses
experientialPROCESStypeViaLevinSynonyms <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  matchedRowInLevinVerbClasses <- grep(POSresult$lemma[wordPosition], levinVerbClasses$List)

  if (length(matchedRowInLevinVerbClasses) != 0){ ###### do not continue to run this function if no matched synonyms can be found in the Levin verb classes
  
  synonymsOfLemma <- scan(text=as.character(levinVerbClasses$List[matchedRowInLevinVerbClasses]), sep=" ", what ="", quiet=TRUE)
  for (counterinSynonymsOfLemma in 1:length(synonymsOfLemma)){
    
    for (counterInVerbsAsProcess in 1:nrow(verbsAsProcess)){
      tmpVerbListInMidRangedCol <-scan(text=verbsAsProcess$middle_ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
      tmpVerbListInMidRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidRangedCol) #trim leading and trailing whitespaces
      tmpVerbListInMidNonRangedCol <-scan(text=verbsAsProcess$middle_non.ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
      tmpVerbListInMidNonRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidNonRangedCol)
      tmpVerbListInEffectiveCol <-scan(text=verbsAsProcess$effective[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
      tmpVerbListInEffectiveCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInEffectiveCol)    
      
      
      ###### the following line of code can check unigram and bigram of synonymsOflemma(s) against verbsAsProcess, BUT Levin seems only contain one-word verb, i.e. unigram. Anyway, searching in verbsAsProcess supports bigram.
      if((synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInMidRangedCol) || (synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInMidNonRangedCol) || (synonymsOfLemma[counterinSynonymsOfLemma] %in% tmpVerbListInEffectiveCol) ){
        ViaLevinSynonyms <<- "Y"
        
        
        if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "material"){tmp10 <<- "material"; tmp10Counter <<- tmp10Counter + 1}
        if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "behavioural"){tmp11 <<- "behavioural"; tmp11Counter <<- tmp11Counter + 1}
        if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "mental"){tmp12 <<- "mental"; tmp12Counter <<- tmp12Counter + 1}
        if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "verbal"){tmp13 <<- "verbal"; tmp13Counter <<- tmp13Counter + 1}
        if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "relational"){tmp14 <<- "relational"; tmp14Counter <<- tmp14Counter + 1}
        if (verbsAsProcess$process.type[counterInVerbsAsProcess] == "existential"){tmp15 <<- "existential"; tmp15Counter <<- tmp15Counter + 1}

        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("PROCESS via Levin's Synonyms= ", verbsAsProcess$process.type[counterInVerbsAsProcess]))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched Levin's synonyms in the row of verbsAsProcess table= Row", counterInVerbsAsProcess))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemma in text= ", POSresult$lemma[wordPosition]))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Token in text= ", POSresult$token[wordPosition]))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched Levin's synonyms= ", synonymsOfLemma[counterinSynonymsOfLemma]))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
        
        theVerb<<-POSresult$token[wordPosition]
      }
    }
    if (is.null(tmp10)==TRUE){tmp10<<-"/"}
    if (is.null(tmp11)==TRUE){tmp11<<-"/"}
    if (is.null(tmp12)==TRUE){tmp12<<-"/"}
    if (is.null(tmp13)==TRUE){tmp13<<-"/"}
    if (is.null(tmp14)==TRUE){tmp14<<-"/"}
    if (is.null(tmp15)==TRUE){tmp15<<-"/"}
  }
}   
}


#######loading custom verb classes for PROCESS matching
customVerbAsProcess <-read.table(file=paste0(filePath,"tmp//","custom_verb_as_process_v1.txt"), sep="\t", header=TRUE)

#######clean up semicolons in columns of "middle_ranged", "middle_non.ranged", and "effective"
customVerbAsProcess$middle_ranged <- gsub(";", ",", customVerbAsProcess$middle_ranged)
customVerbAsProcess$middle_non.ranged <- gsub(";", ",", customVerbAsProcess$middle_non.ranged)
customVerbAsProcess$effective <- gsub(";", ",", customVerbAsProcess$effective)
customVerbAsProcess[customVerbAsProcess=="null"]<- NA

####### A function to verify the verb (from custom list of Verb Classes) of the POSresult$lemma[wordPosition] or POSresult$token[wordPosition] that exists in custom_verb_as_process_v?.txts
experientialPROCESStypeViaCustomVerbAsProcess <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  for (counterInVerbsAsProcess in 1:nrow(customVerbAsProcess)){
    tmpVerbListInMidRangedCol <-scan(text=customVerbAsProcess$middle_ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidRangedCol) #trim leading and trailing whitespaces
    tmpVerbListInMidNonRangedCol <-scan(text=customVerbAsProcess$middle_non.ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidNonRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidNonRangedCol)
    tmpVerbListInEffectiveCol <-scan(text=customVerbAsProcess$effective[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInEffectiveCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInEffectiveCol)    
    
    
    ####### the following line of code checks unigram and bigram of lemma(s) or token(s) against customVerbAsProcess, e.g. taste (lemma) and tasting (token).
    if( (paste(POSresult$lemma[wordPosition],POSresult$lemma[wordPosition+1]) %in% tmpVerbListInMidRangedCol) || (paste(POSresult$lemma[wordPosition],POSresult$lemma[wordPosition+1]) %in% tmpVerbListInMidNonRangedCol) || (paste(POSresult$lemma[wordPosition],POSresult$lemma[wordPosition+1]) %in% tmpVerbListInEffectiveCol) ||
        (paste(POSresult$token[wordPosition],POSresult$token[wordPosition+1]) %in% tmpVerbListInMidRangedCol) || (paste(POSresult$token[wordPosition],POSresult$token[wordPosition+1]) %in% tmpVerbListInMidNonRangedCol) || (paste(POSresult$token[wordPosition],POSresult$token[wordPosition+1]) %in% tmpVerbListInEffectiveCol) ){
      InCustomVerbsAsProcess <<- "Y"
      ngramMatched<-"Y"
      
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "material"){tmp10 <<- "material"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "behavioural"){tmp11 <<- "behavioural"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "mental"){tmp12 <<- "mental"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "verbal"){tmp13 <<- "verbal"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "relational"){tmp14 <<- "relational"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "existential"){tmp15 <<- "existential"}

      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("PROCESS via Custom Verb or Inflection= ", customVerbAsProcess$process.type[counterInVerbsAsProcess]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched bi-gram or not? ", ngramMatched))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched ones in the row of customVerbsAsProcess table= Row", counterInVerbsAsProcess))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemmas in text= ", paste(POSresult$lemma[wordPosition], POSresult$lemma[wordPosition+1]))) ) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Tokens in text= ", paste(POSresult$token[wordPosition], POSresult$token[wordPosition+1]))) ) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
      
      theVerb<<-paste(POSresult$token[wordPosition], POSresult$token[wordPosition+1])
    }
  }
  
  for (counterInVerbsAsProcess in 1:nrow(customVerbAsProcess)){
    tmpVerbListInMidRangedCol <-scan(text=customVerbAsProcess$middle_ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidRangedCol) #trim leading and trailing whitespaces
    tmpVerbListInMidNonRangedCol <-scan(text=customVerbAsProcess$middle_non.ranged[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInMidNonRangedCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInMidNonRangedCol)
    tmpVerbListInEffectiveCol <-scan(text=customVerbAsProcess$effective[counterInVerbsAsProcess], sep=",", what ="", quiet=TRUE)
    tmpVerbListInEffectiveCol <-gsub("^\\s+|\\s+$", "", tmpVerbListInEffectiveCol)    
    
    
    ####### the following line of code checks unigram of lemma(s) or token(s) against customVerbAsProcess, e.g. taste (lemma) and tasting (token).
    if( ngramMatched=="N" && ((POSresult$lemma[wordPosition] %in% tmpVerbListInMidRangedCol) || (POSresult$lemma[wordPosition] %in% tmpVerbListInMidNonRangedCol) || (POSresult$lemma[wordPosition] %in% tmpVerbListInEffectiveCol) ||
       (POSresult$token[wordPosition] %in% tmpVerbListInMidRangedCol) || (POSresult$token[wordPosition] %in% tmpVerbListInMidNonRangedCol) || (POSresult$token[wordPosition] %in% tmpVerbListInEffectiveCol)) ){
      InCustomVerbsAsProcess <<- "Y"

      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "material"){tmp10 <<- "material"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "behavioural"){tmp11 <<- "behavioural"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "mental"){tmp12 <<- "mental"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "verbal"){tmp13 <<- "verbal"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "relational"){tmp14 <<- "relational"}
      if (customVerbAsProcess$process.type[counterInVerbsAsProcess] == "existential"){tmp15 <<- "existential"}
      
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("PROCESS via Custom Verb or Inflection= ", customVerbAsProcess$process.type[counterInVerbsAsProcess]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched one in the row of customVerbsAsProcess table= Row", counterInVerbsAsProcess))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemmas in text= ", POSresult$lemma[wordPosition])) ) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Tokens in text= ", POSresult$token[wordPosition])) ) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
      
      theVerb<<-POSresult$token[wordPosition]
    }
  }  
  
  if (is.null(tmp10)==TRUE){tmp10<<-"/"}
  if (is.null(tmp11)==TRUE){tmp11<<-"/"}
  if (is.null(tmp12)==TRUE){tmp12<<-"/"}
  if (is.null(tmp13)==TRUE){tmp13<<-"/"}
  if (is.null(tmp14)==TRUE){tmp14<<-"/"}
  if (is.null(tmp15)==TRUE){tmp15<<-"/"}
}


######## A function to check the interpersonal MOOD of the POSresult$lemma[wordPosition] that starts with (Wh*/how/has/have/do/does/did/should/could/would) then (i/he/she/it/they/we/that/there/those/a tax/the company/the Airlines/the years) then end with question mark  (interrogative), "." "VB" (imperative), remaining (declarative).
interpersonalMOOD <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
hasNotEtc <- 0
clauseToCheck <- NULL
######## in the line below, "." for last sentence in full stop, "!?" or "?!" for interrobang
  if ((POSresult$POS[wordPosition-1]=="." || POSresult$token[wordPosition-1]=="!?" || POSresult$token[wordPosition-1]=="?!") && ((tolower(POSresult$token[wordPosition])=="have") || (tolower(POSresult$token[wordPosition])=="has") || (tolower(POSresult$token[wordPosition])=="do") || (tolower(POSresult$token[wordPosition])=="does") || (tolower(POSresult$token[wordPosition])=="did") || (tolower(POSresult$token[wordPosition])=="should") || (tolower(POSresult$token[wordPosition])=="could") || (tolower(POSresult$token[wordPosition])=="would") || (tolower(POSresult$token[wordPosition])=="might") || (tolower(POSresult$token[wordPosition])=="may") || (tolower(POSresult$token[wordPosition])=="can") || (tolower(POSresult$token[wordPosition])=="is") || (tolower(POSresult$token[wordPosition])=="are") || (tolower(POSresult$token[wordPosition])=="was") || (tolower(POSresult$token[wordPosition])=="were") || (tolower(POSresult$token[wordPosition])=="will") || (tolower(POSresult$token[wordPosition])=="shall") ) ){
    if (tolower(POSresult$token[wordPosition+1])=="not" || tolower(POSresult$token[wordPosition+1])=="n't") {hasNotEtc <- 1}  # if it has "not" or "n't"
      if ( (POSresult$POS[wordPosition+1+hasNotEtc]=="NN") || (POSresult$POS[wordPosition+1+hasNotEtc]=="NNS") || (POSresult$POS[wordPosition+1+hasNotEtc]=="NNP") || (POSresult$POS[wordPosition+1+hasNotEtc]=="NNPS") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="i") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="it") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="those") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="you") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="he") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="she") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="they") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="we") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="that") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="there") || ((POSresult$POS[wordPosition+1+hasNotEtc]=="DT") && (POSresult$POS[wordPosition+2+hasNotEtc]=="NN")) || ((POSresult$POS[wordPosition+1+hasNotEtc]=="DT") && (POSresult$POS[wordPosition+2+hasNotEtc]=="NNS")) || ((POSresult$POS[wordPosition+1+hasNotEtc]=="DT") && (POSresult$POS[wordPosition+2+hasNotEtc]=="NNP")) || ((POSresult$POS[wordPosition+1+hasNotEtc]=="DT") && (POSresult$POS[wordPosition+2+hasNotEtc]=="NNPS"))  ) {
            if (POSresult$token[which(POSresult$sentence==i & POSresult$id==(length(subset(POSresult$id, POSresult$sentence==i))))]=="?"){  ######## check the last token of each interrrogative MOOD is a question mark (?) or not
            tmp7 <<- "interrogative"}
      }
  }

  if ((POSresult$POS[wordPosition-1]=="." || POSresult$token[wordPosition-1]=="!?" || POSresult$token[wordPosition-1]=="?!") && ((POSresult$POS[wordPosition]=="WRB") || (POSresult$POS[wordPosition]=="WDT"))){
    if ( (POSresult$POS[wordPosition+1]=="VB") || (POSresult$POS[wordPosition+1]=="VBZ") || (POSresult$POS[wordPosition+1]=="VBD") || ((POSresult$POS[wordPosition+1]=="MD")) || (POSresult$POS[wordPosition+1]=="NN") || ((POSresult$POS[wordPosition+1]=="IN") && (POSresult$POS[wordPosition+2]=="DT")) ) { #When/What/Which/How..., then VB/VBZ/VBD//MD/(which) NN, e.g. team/(Which) IN DT, e.g. of the/
      if (POSresult$token[which(POSresult$sentence==i & POSresult$id==(length(subset(POSresult$id, POSresult$sentence==i))))]=="?"){  ######## check the last token of each interrrogative MOOD is a question mark (?) or not
        tmp7 <<- "interrogative"}
      }
  }



  if (( (is.null(tmp7)==TRUE)   # Check by custom syntax
      && ( ((POSresult$POS[wordPosition-1]==".") || (POSresult$POS[wordPosition-2]=="." && POSresult$NER[wordPosition-1]=="PERSON") || (POSresult$POS[wordPosition-3]=="." && POSresult$NER[wordPosition-2]=="PERSON" && POSresult$POS[wordPosition-1]==",")) 
        && (POSresult$POS[wordPosition]=="VB" || POSresult$POS[wordPosition]=="VBP") ) ) 
        || (POSresult$token[which(POSresult$sentence==i & POSresult$id==(length(subset(POSresult$id, POSresult$sentence==i))))]=="!")){  ######## "." "?" "!" in $POS is ".", thus, no need to specify one by one. Also, "VBP" is due to the CoreNLP occusionally mark the first verb to "VBP" instead of "VB". Finally, OR match the end of clause with "!".
    tmp8 <<- "imperative"
  }

  if (is.null(tmp7)==TRUE) {  # Check by qdap::imperative
      if (POSresult$NER[wordPosition]=="PERSON"){
      clauseToCheck <- paste(subset(POSresult$token, POSresult$sentence==i), collapse = " ")
      imperativeDf <- data.frame(name=c(POSresult$token[wordPosition]), statement=c(clauseToCheck), stringsAsFactors = FALSE)
      imperativeDfResult <- imperative(imperativeDf, "name", "statement", lock.incomplete = TRUE, parallel = TRUE)
        if (substring(imperativeDfResult$statement, (nchar(imperativeDfResult$statement)-1), (nchar(imperativeDfResult$statement)-1)) == "*"){
          tmp8 <<- "imperative"}
        }
      }
  
  if (is.null(tmp8)==TRUE && is.null(tmp7)==TRUE){
    tmp6 <<- "declarative"
  }
  
  
  
  if (is.null(tmp6)==TRUE){tmp6<<-"/"}
  if (is.null(tmp7)==TRUE){tmp7<<-"/"}
  if (is.null(tmp8)==TRUE){tmp8<<-"/"}
  
}


######### A function to find the unmarked theme when the interpersonal MOOD of the POSresult$lemma[wordPosition] that starts with (Wh/has/have/do/does/did/should/could/would) then (i/he/she/it/they/we/that/there/those/a tax/the company/the Airlines/the years) then end with question mark  (interrogative), "." "VB" (imperative).
unmarkedThemeUponMOOD <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  hasNotEtc <- 0
  subjectInUnmarkedTopicalTheme <- 1
  predicatorInUnmarkedTopicalTheme <- 0
  rangeUntilVerb <- 1
  imperativeHasFinite <- 0
  ######### in the line below, "." for last sentence in full stop, "!?" or "?!" for interrobang
  ######### Interrogative (yes/no) - interpersonal (tmp5) and topical (tmp3) themes
  if ((POSresult$POS[wordPosition-1]=="." || POSresult$token[wordPosition-1]=="!?" || POSresult$token[wordPosition-1]=="?!") && ((tolower(POSresult$token[wordPosition])=="have") || (tolower(POSresult$token[wordPosition])=="has") || (tolower(POSresult$token[wordPosition])=="had") || (tolower(POSresult$token[wordPosition])=="do") || (tolower(POSresult$token[wordPosition])=="does") || (tolower(POSresult$token[wordPosition])=="did") || (tolower(POSresult$token[wordPosition])=="should") || (tolower(POSresult$token[wordPosition])=="could") || (tolower(POSresult$token[wordPosition])=="would") || (tolower(POSresult$token[wordPosition])=="might") || (tolower(POSresult$token[wordPosition])=="may") || (tolower(POSresult$token[wordPosition])=="can") || (tolower(POSresult$token[wordPosition])=="is") || (tolower(POSresult$token[wordPosition])=="are") || (tolower(POSresult$token[wordPosition])=="was") || (tolower(POSresult$token[wordPosition])=="were") || (tolower(POSresult$token[wordPosition])=="will") || (tolower(POSresult$token[wordPosition])=="shall") || (tolower(POSresult$token[wordPosition])=="am") || (tolower(POSresult$token[wordPosition])=="must") || ((tolower(POSresult$token[wordPosition])=="ought") && (tolower(POSresult$token[wordPosition+1])=="to")) ) ){
    if ((tolower(POSresult$token[wordPosition+1])=="not") || (tolower(POSresult$token[wordPosition+1])=="n't")){hasNotEtc <- 1}  # if it has "not" or "n't"
    if ( (POSresult$POS[wordPosition+1+hasNotEtc]=="NN") || (POSresult$POS[wordPosition+1+hasNotEtc]=="NNS") || (POSresult$POS[wordPosition+1+hasNotEtc]=="NNP") || (POSresult$POS[wordPosition+1+hasNotEtc]=="NNPS") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="i") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="it") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="those") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="you") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="he") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="she") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="they") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="we") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="that") || (tolower(POSresult$token[wordPosition+1+hasNotEtc])=="there") || ((POSresult$POS[wordPosition+1+hasNotEtc]=="DT") && (POSresult$POS[wordPosition+2+hasNotEtc]=="NN")) || ((POSresult$POS[wordPosition+1+hasNotEtc]=="DT") && (POSresult$POS[wordPosition+2+hasNotEtc]=="NNS")) || ((POSresult$POS[wordPosition+1+hasNotEtc]=="DT") && (POSresult$POS[wordPosition+2+hasNotEtc]=="NNP")) || ((POSresult$POS[wordPosition+1+hasNotEtc]=="DT") && (POSresult$POS[wordPosition+2+hasNotEtc]=="NNPS"))  ) {
      if (POSresult$token[which(POSresult$sentence==i & POSresult$id==(length(subset(POSresult$id, POSresult$sentence==i))))]=="?"){  ######## check the last token of each interrrogative MOOD is a question mark (?) or not
        tmp5 <<- POSresult$token[wordPosition]
        NoOfInterpersonal<<-NoOfInterpersonal+1  # Include the Finite of Interpersonal theme, e.g. (Did) you wake up early today?
        
        if (hasNotEtc==1){tmp5 <<- paste(tmp5, POSresult$token[wordPosition+1], collapse = " ")} 
        while (is.na(POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc])==FALSE && subjectInUnmarkedTopicalTheme==1){
          if (!((POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc]==".") || (POSresult$token[wordPosition+rangeUntilVerb+hasNotEtc]=="!?") || (POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc]=="VB") || (POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc]=="VBG") || (POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc]=="VBP") || (POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc]=="RB") ||   (((POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc-1]=="NN") || (POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc-1]=="NNS") || (POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc-1]=="NNP") || (POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc-1]=="NNPS") ) && (POSresult$POS[wordPosition+rangeUntilVerb+hasNotEtc]=="JJ") && (POSresult$token[wordPosition+rangeUntilVerb+hasNotEtc+1]=="?")) )){  # the last one, =="RB" is to identify topical theme "does it (really) matter?" or "will you (please) do me a favour?" or "Did that man (deliberately) walk...", "(RB) VB". Just keep "it", "you" or "that man" in the themes, not "it really" or "you please" or "that man deliberately".   Also, ignore the last adjective in topical theme(i.e. subject), e.g. Is n't the book (good)?,i.e. NN JJ ?, in progrmam, to serve different scenarious, (NN||NNS||NNP||NNPS) && JJ && "?"
            tmp3 <<- paste(tmp3, POSresult$token[wordPosition + rangeUntilVerb + hasNotEtc], collapse = " ")
            rangeUntilVerb <- rangeUntilVerb+1
            tmp3UsedIn_unmarkedThemeUponMOOD <<- 1  #to stop using the value of tmp3 in default codes for unmarked theme identification
          }
          else {subjectInUnmarkedTopicalTheme <- 0}
        }
      }
    }
  }
  
  ######### Interrogative (Wh*) - interpersonal (tmp5) and topical (tmp3) themes
  if ((POSresult$POS[wordPosition-1]=="." || POSresult$token[wordPosition-1]=="!?" || POSresult$token[wordPosition-1]=="?!") && ((POSresult$POS[wordPosition]=="WRB") || (POSresult$POS[wordPosition]=="WDT"))){
    if ( (POSresult$POS[wordPosition+1]=="VB") || (POSresult$POS[wordPosition+1]=="VBZ") || (POSresult$POS[wordPosition+1]=="VBP") || (POSresult$POS[wordPosition+1]=="VBD") || ((POSresult$POS[wordPosition+1]=="MD")) || (POSresult$POS[wordPosition+1]=="NN") || ((POSresult$POS[wordPosition+1]=="IN") && (POSresult$POS[wordPosition+2]=="DT")) ) { #When/What/Which/How..., then VB/VBZ/VBP/VBD/MD/(which) NN, e.g. team/(Which) IN DT, e.g. of the/
      if (POSresult$token[which(POSresult$sentence==i & POSresult$id==(length(subset(POSresult$id, POSresult$sentence==i))))]=="?"){  ######## check the last token of each interrrogative MOOD is a question mark (?) or not

        tmp3 <<- POSresult$token[wordPosition]
        tmp5 <<- paste("[",POSresult$token[wordPosition], "]", collapse = "")
        NoOfInterpersonal<<-NoOfInterpersonal+1  # Include the Wh-element of Interpersonal theme, e.g. ([Why]) did you wake up early today?
        
        tmp3UsedIn_unmarkedThemeUponMOOD <<- 1  #to stop using the value of tmp3 in default codes for unmarked theme identification
      }
    }
  }
  
  
  ######### Imperative - interpersonal (tmp5) and topical (tmp3) themes
  if ((is.null(tmp5)==TRUE) && (((POSresult$POS[wordPosition-1]==".") && (POSresult$POS[wordPosition]=="VB" || POSresult$POS[wordPosition]=="VBP")) || (POSresult$token[which(POSresult$sentence==i & POSresult$id==(length(subset(POSresult$id, POSresult$sentence==i))))]=="!"))){  ######## "." "?" "!" in $POS is ".", thus, no need to specify one by one. Also, "VBP" is due to the CoreNLP occusionally mark the first verb to "VBP" instead of "VB". Finally, OR match the end of clause with "!".
    if (tolower(POSresult$token[wordPosition+1])=="not" || tolower(POSresult$token[wordPosition+1])=="n't" || tolower(POSresult$token[wordPosition+1])=="'s"){hasNotEtc <- 1}  # if it has "not", "n't", or "'s".
    
    if ( (tolower(POSresult$token[wordPosition])=="have") || (tolower(POSresult$token[wordPosition])=="has") || (tolower(POSresult$token[wordPosition])=="had") || ((tolower(POSresult$token[wordPosition])=="do") && POSresult$POS[wordPosition+1+hasNotEtc]=="VB") || (tolower(POSresult$token[wordPosition])=="does") || (tolower(POSresult$token[wordPosition])=="did") || (tolower(POSresult$token[wordPosition])=="should") || (tolower(POSresult$token[wordPosition])=="could") || (tolower(POSresult$token[wordPosition])=="would") || (tolower(POSresult$token[wordPosition])=="might") || (tolower(POSresult$token[wordPosition])=="may") || (tolower(POSresult$token[wordPosition])=="can") || 
         (tolower(POSresult$token[wordPosition])=="is") || (tolower(POSresult$token[wordPosition])=="are") || (tolower(POSresult$token[wordPosition])=="was") || (tolower(POSresult$token[wordPosition])=="were") || (tolower(POSresult$token[wordPosition])=="will") || (tolower(POSresult$token[wordPosition])=="shall") || (tolower(POSresult$token[wordPosition])=="am") || (tolower(POSresult$token[wordPosition])=="must") || (tolower(POSresult$token[wordPosition])=="ought") ) {
      tmp5 <<- POSresult$token[wordPosition]
      NoOfInterpersonal<<-NoOfInterpersonal+1  # Include the Finite of Interpersonal theme in imperative clause, e.g. (Don't) wake up late today!
      
      if (hasNotEtc==1){tmp5 <<- paste(tmp5, POSresult$token[wordPosition+1], collapse = " ")}
      if ((tolower(POSresult$token[wordPosition])=="ought") && (tolower(POSresult$token[wordPosition+1])=="to")) {tmp5 <<- paste(tmp5, POSresult$token[wordPosition+1], collapse = " ")}
      if ((tolower(POSresult$token[wordPosition])=="ought") && (tolower(POSresult$token[wordPosition+2])=="to")) {tmp5 <<- paste(tmp5, POSresult$token[wordPosition+2], collapse = " ")} # [wordPosition+2] is to deal with "ought not to"
      imperativeHasFinite <- 1
    } 

    # imperative clauses that start with finite and follow a predictor, e.g. (do/do n't) (wake)
    if (imperativeHasFinite==1 && ((POSresult$POS[wordPosition+1+hasNotEtc]=="VB") || (POSresult$POS[wordPosition+1+hasNotEtc]=="VBP")) ){
      tmp3 <<- paste(POSresult$token[wordPosition+1+hasNotEtc])
      tmp3UsedIn_unmarkedThemeUponMOOD <<- 1}
    
    # all general imperative clauses that start with VB or VBP. Also, unlike "do", "don't", etc. listed in the several lines about finite verbs, add special case for "Let's". It is topical, thus, it uses tmp3 instead of tmp5
    if (imperativeHasFinite==0 && ((POSresult$POS[wordPosition]=="VB") || (POSresult$POS[wordPosition]=="VBP")) ){
      tmp3 <<- paste(POSresult$token[wordPosition])
      tmp3UsedIn_unmarkedThemeUponMOOD <<- 1}
    
    if (imperativeHasFinite==0 && hasNotEtc==1 && ((POSresult$lemma[wordPosition]=="let") || (POSresult$lemma[wordPosition+1]=="'s")) ){  # including "Let's", as "Let's" should be topical and is interpreted as Subject
      tmp3 <<- paste(POSresult$token[wordPosition], POSresult$token[wordPosition+1])
      tmp3UsedIn_unmarkedThemeUponMOOD <<- 1}
    
  }
  
  
  if (is.null(tmp3)==TRUE){tmp3<<-"/"}
  if (is.null(tmp5)==TRUE){tmp5<<-"/"}
  
}



############## In Is/Are there ... OR Isn't it a good book? Make sure that the "Is" at the clause beginning, is the only verb-like, no other VB, VBP, VBZ in the same clause.
hasVerbInRemainingClause <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  if(POSresult$POS[wordPosition]=="." || POSresult$token[wordPosition]=="!?" || POSresult$token[wordPosition]=="?!"){return(FALSE)}
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  sentenceLength <- 0
  idNo <-0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  sentenceLength <-  length(subset(POSresult$id, POSresult$sentence==sentenceNo))
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  
  for (i in (idNo+1):sentenceLength){  # (idNo+1): is to skip the POS of wordPosition itself in the sentence. It it the only verb, e.g (Think) for myself.
    if((tmpPOSresult[i,7]=="VB") || (tmpPOSresult[i,7]=="VBP") || (tmpPOSresult[i,7]=="VBZ") || (tmpPOSresult[i,7]=="VBG") || (tmpPOSresult[i,7]=="VBD") || (tmpPOSresult[i,7]=="VBN")){ 
      return (TRUE)}
  }
  return (FALSE)
}


############## detect -ing verb, i.e. VBG
hasPresentParticipleInRemainingClause <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  if(POSresult$POS[wordPosition]=="."  || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return(FALSE)}
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  sentenceLength <- 0
  idNo <-0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  sentenceLength <-  length(subset(POSresult$id, POSresult$sentence==sentenceNo))
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  
  for (i in (idNo+1):sentenceLength){  # (idNo+1): is to skip the POS of wordPosition itself in the sentence. It it the only verb, e.g (Think) for myself.
    if(tmpPOSresult[i,7]=="VBG"){ 
      return (TRUE)}
  }
  return (FALSE)
}


############## detect any relative pronoun "who", "which", "whose" and "that" AFTER the wordPosition
hasRelativePronounInRemainingClause <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  if(POSresult$POS[wordPosition]=="." || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return(FALSE)}
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  sentenceLength <- 0
  idNo <-0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  sentenceLength <-  length(subset(POSresult$id, POSresult$sentence==sentenceNo))
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  
  for (i in idNo:sentenceLength){  #  # idNo not idNo: is not to skip the word of wordPosition itself in the sentence.
    if(tmpPOSresult[i,4]=="who" || tmpPOSresult[i,4]=="which" || tmpPOSresult[i,4]=="whose" || tmpPOSresult[i,4]=="whom" || (tmpPOSresult[i,4]=="that" && tmpPOSresult[i,7]=="that") ){ 
      return (TRUE)}
  }
  return (FALSE)
}



############## detect any relative pronoun "who", "which", "whose" and "that" BEFORE the wordPosition
hasRelativePronounInClauseBfwordPosition <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  if(POSresult$POS[wordPosition]=="." || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return(FALSE)}
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  idNo <-0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  
  for (i in 1:idNo){  #  # idNo not idNo: is not to skip the word of wordPosition itself in the sentence.
    if(tmpPOSresult[i,4]=="who" || tmpPOSresult[i,4]=="which" || tmpPOSresult[i,4]=="whose" || tmpPOSresult[i,4]=="whom" || (tmpPOSresult[i,4]=="that" && tmpPOSresult[i,7]=="that") ){ 
      return (TRUE)}
  }
  return (FALSE)
}


############## detect "there", the determiner in the whole clause.
hasWordThereInClause <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  if(POSresult$POS[wordPosition]=="."  || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return("Clause end")}
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  sentenceLength <- 0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  sentenceLength <-  length(subset(POSresult$id, POSresult$sentence==sentenceNo))
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  
  for (i in 1:sentenceLength){  
    if(tmpPOSresult[i,4]=="there"){ 
      return (TRUE)}
  }
  return (FALSE)
}

############## detect "there", the determiner in the phrase, and within a range, the "there" is having a postion next to a verb.
hasWordThereNextToVerbInPhrase <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  if((POSresult$UDPOS[wordPosition]=="." && POSresult$lemma[wordPosition]!="``") || POSresult$token[wordPosition]=="?!" || POSresult$token[wordPosition]=="!?"){return("Phrase end.")} # use UDPOS to include ".",",","!","?", but need to exclude double single quote ' ', as it is the open of a quotation in clause in text source.
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  idNo <-0
  tmpPOSresult <- NULL
  punctuationPosition <- 0
  currentPhraseEnd <- 0
  currentPhraseStart <- 1
  verbPosition <- NULL
  therePosition <- 0
  rangeBetweenVerbAndThere <- 2 # Define the position difference between the finite verb and the word "there" in the same phrase.
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  punctuationPosition <- which(!is.na( match(tmpPOSresult$lemma, c(".",",","!","?","?!","!?","``","''")) ) )  # e.g. [1]  4  7 14 !is.na of [1] NA NA NA  2 NA NA  2 NA NA NA NA NA NA  1
  
  if (length(which((punctuationPosition > idNo))) != 0){  # to avoid warning of "which((  which(!is.na(match(tmpx, tmpy)))  ) > 14)"  with a result of "integer(0)"
    currentPhraseEnd <- min(which((punctuationPosition > idNo))) } # e.g. if wordPosition==5, as 5 >4 and <7, then result is [1] 2
  if (currentPhraseEnd!=1){currentPhraseStart <- punctuationPosition[currentPhraseEnd-1] }
  if (currentPhraseEnd !=0){
    for (i in (currentPhraseStart):(as.numeric(punctuationPosition[currentPhraseEnd])-1) ){  # check if in the phrase has any verb, e.g since January, ...
      if(tmpPOSresult$POS[i]=="MD" || tmpPOSresult$POS[i]=="VB" || tmpPOSresult$POS[i]=="VBP" || tmpPOSresult$POS[i]=="VBZ" || tmpPOSresult$POS[i]=="VBG" || tmpPOSresult$POS[i]=="VBD" || tmpPOSresult$POS[i]=="VBN"){ 
        verbPosition <- c(verbPosition, i) }
    }
    
    for (i in (currentPhraseStart):(as.numeric(punctuationPosition[currentPhraseEnd])-1) ){  # check if in the phrase has any "there".
      if(tmpPOSresult$lemma[i]=="there"){ 
        therePosition <- i }
    }

    if (therePosition != 0 && min(abs(therePosition - verbPosition)) <= rangeBetweenVerbAndThere){
      return (TRUE)}
  }
  return ("No word 'there' identified which is next to a verb in this phrase.")
}

################# Detect any verb before the argument inserted, mainly for finding out conjunction
################# First verb (i==1 below) and non-finite verb (infinitive-to and -ing) are excluded.
hasFiniteVerbInClauseBfWordPosition <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  if(POSresult$POS[wordPosition]=="." || POSresult$token[wordPosition]=="!?" || POSresult$token[wordPosition]=="?!"){return("Clause end.")}

  if(POSresult$POS[wordPosition-1]=="." || POSresult$token[wordPosition-1]=="!?" || POSresult$token[wordPosition-1]=="?!"){return("Clause start")} # to avoid the missing value TRUE/FALSE in the for loop below.
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.
  idNo <-0
  
  sentenceNo <- POSresult[wordPosition,]$sentence
  idNo <- as.numeric(POSresult[wordPosition,]$id)
  tmpPOSresult <- subset(POSresult, POSresult$sentence==sentenceNo)
  for (i in 1:(idNo)){  # e.g. used for checking any verb before the conjunction, "I (believe that) they will come.". Original it was (idNo-1) is to avoid to count for itself at the wordPosition, but it triggers error when tmpPOSresult[1-1] in looping condition.
    if (length(tmpPOSresult[i-1,7])!=0){
    if (!( ((tmpPOSresult[i-1,7]=="VBG") && (tmpPOSresult[i,7]=="DT")) || ((tmpPOSresult[i,7]=="VBG") && (tmpPOSresult[i+1,7]=="NN")) || ((tmpPOSresult[i,7]=="VBG") && (tmpPOSresult[i+1,7]=="NNS")) 
        || ((tmpPOSresult[i-1,7]=="TO") && (tmpPOSresult[i,7]=="VB")) || ((tmpPOSresult[i-1,7]=="IN") && (tmpPOSresult[i,7]=="VBG"))) ){  # excluding nonfinite "to get", "in developing country"
    if((tmpPOSresult[i,7]=="VB") || (tmpPOSresult[i,7]=="VBP") || (tmpPOSresult[i,7]=="VBZ") || (tmpPOSresult[i,7]=="VBG") || (tmpPOSresult[i,7]=="VBD") || (tmpPOSresult[i,7]=="VBN")){ 
      return (TRUE)}
    }
  }
  }
  return (FALSE)
}


################# loading ploysemous verb classes for PROCESS matching
polysemousVerbAsProcess <-read.table(file=paste0(filePath,"tmp//","polysemous_verb_as_process_v6.txt"), sep="\t", header=TRUE)

################# clean up ( ) [ ] semicolons and single quote in columns of "material", "behavioural", "mental", "verbal", "relational" and "existential"
polysemousVerbAsProcess$PROCESS.material <- gsub("\\(", ",", polysemousVerbAsProcess$PROCESS.material)
polysemousVerbAsProcess$PROCESS.behavioural <- gsub("\\(", ",", polysemousVerbAsProcess$PROCESS.behavioural)
polysemousVerbAsProcess$PROCESS.mental <- gsub("\\(", ",", polysemousVerbAsProcess$PROCESS.mental)
polysemousVerbAsProcess$PROCESS.verbal <- gsub("\\(", ",", polysemousVerbAsProcess$PROCESS.verbal)
polysemousVerbAsProcess$PROCESS.relational <- gsub("\\(", ",", polysemousVerbAsProcess$PROCESS.relational)
polysemousVerbAsProcess$PROCESS.existential <- gsub("\\(", ",", polysemousVerbAsProcess$PROCESS.existential)
polysemousVerbAsProcess$PROCESS.material <- gsub("\\)", ",", polysemousVerbAsProcess$PROCESS.material)
polysemousVerbAsProcess$PROCESS.behavioural <- gsub("\\)", ",", polysemousVerbAsProcess$PROCESS.behavioural)
polysemousVerbAsProcess$PROCESS.mental <- gsub("\\)", ",", polysemousVerbAsProcess$PROCESS.mental)
polysemousVerbAsProcess$PROCESS.verbal <- gsub("\\)", ",", polysemousVerbAsProcess$PROCESS.verbal)
polysemousVerbAsProcess$PROCESS.relational <- gsub("\\)", ",", polysemousVerbAsProcess$PROCESS.relational)
polysemousVerbAsProcess$PROCESS.existential <- gsub("\\)", ",", polysemousVerbAsProcess$PROCESS.existential)
polysemousVerbAsProcess$PROCESS.material <- gsub("\\[", ",", polysemousVerbAsProcess$PROCESS.material)
polysemousVerbAsProcess$PROCESS.behavioural <- gsub("\\[", ",", polysemousVerbAsProcess$PROCESS.behavioural)
polysemousVerbAsProcess$PROCESS.mental <- gsub("\\[", ",", polysemousVerbAsProcess$PROCESS.mental)
polysemousVerbAsProcess$PROCESS.verbal <- gsub("\\[", ",", polysemousVerbAsProcess$PROCESS.verbal)
polysemousVerbAsProcess$PROCESS.relational <- gsub("\\[", ",", polysemousVerbAsProcess$PROCESS.relational)
polysemousVerbAsProcess$PROCESS.existential <- gsub("\\[", ",", polysemousVerbAsProcess$PROCESS.existential)
polysemousVerbAsProcess$PROCESS.material <- gsub("\\]", ",", polysemousVerbAsProcess$PROCESS.material)
polysemousVerbAsProcess$PROCESS.behavioural <- gsub("\\]", ",", polysemousVerbAsProcess$PROCESS.behavioural)
polysemousVerbAsProcess$PROCESS.mental <- gsub("\\]", ",", polysemousVerbAsProcess$PROCESS.mental)
polysemousVerbAsProcess$PROCESS.verbal <- gsub("\\]", ",", polysemousVerbAsProcess$PROCESS.verbal)
polysemousVerbAsProcess$PROCESS.relational <- gsub("\\]", ",", polysemousVerbAsProcess$PROCESS.relational)
polysemousVerbAsProcess$PROCESS.existential <- gsub("\\]", ",", polysemousVerbAsProcess$PROCESS.existential)
polysemousVerbAsProcess$PROCESS.material <- gsub(";", ",", polysemousVerbAsProcess$PROCESS.material)
polysemousVerbAsProcess$PROCESS.behavioural <- gsub(";", ",", polysemousVerbAsProcess$PROCESS.behavioural)
polysemousVerbAsProcess$PROCESS.mental <- gsub(";", ",", polysemousVerbAsProcess$PROCESS.mental)
polysemousVerbAsProcess$PROCESS.verbal <- gsub(";", ",", polysemousVerbAsProcess$PROCESS.verbal)
polysemousVerbAsProcess$PROCESS.relational <- gsub(";", ",", polysemousVerbAsProcess$PROCESS.relational)
polysemousVerbAsProcess$PROCESS.existential <- gsub(";", ",", polysemousVerbAsProcess$PROCESS.existential)
polysemousVerbAsProcess$PROCESS.material <- gsub("\`", ",", polysemousVerbAsProcess$PROCESS.material)
polysemousVerbAsProcess$PROCESS.behavioural <- gsub("\`", ",", polysemousVerbAsProcess$PROCESS.behavioural)
polysemousVerbAsProcess$PROCESS.mental <- gsub("\`", ",", polysemousVerbAsProcess$PROCESS.mental)
polysemousVerbAsProcess$PROCESS.verbal <- gsub("\`", ",", polysemousVerbAsProcess$PROCESS.verbal)
polysemousVerbAsProcess$PROCESS.relational <- gsub("\`", ",", polysemousVerbAsProcess$PROCESS.relational)
polysemousVerbAsProcess$PROCESS.existential <- gsub("\`", ",", polysemousVerbAsProcess$PROCESS.existential)
polysemousVerbAsProcess$PROCESS.material <- gsub(",", "", polysemousVerbAsProcess$PROCESS.material)
polysemousVerbAsProcess$PROCESS.behavioural <- gsub(",", "", polysemousVerbAsProcess$PROCESS.behavioural)
polysemousVerbAsProcess$PROCESS.mental <- gsub(",", "", polysemousVerbAsProcess$PROCESS.mental)
polysemousVerbAsProcess$PROCESS.verbal <- gsub(",", "", polysemousVerbAsProcess$PROCESS.verbal)
polysemousVerbAsProcess$PROCESS.relational <- gsub(",", "", polysemousVerbAsProcess$PROCESS.relational)
polysemousVerbAsProcess$PROCESS.existential <- gsub(",", "", polysemousVerbAsProcess$PROCESS.existential)
polysemousVerbAsProcess[polysemousVerbAsProcess=="null"]<- NA

################# LSA analysis for polysemous verbs
################# A function to verify the cosine similarity between the query string and the synonyms of all items (from polysemous Verbs in different PROCESS types "polysemous_verb_as_process_v?.txt") based on the verb achieved within the clause having the POSresult$lemma[wordPosition].
LSAverbsAsProcessSimilarity <- function(wordPosition){
  if((is.na(POSresult$POS[wordPosition])) ){return("Invalid")}  #return "Invalid" if the wordPosition is out of range
  
  rankSpace<-0
  tmpPolysemousVerbAsProcess<-NULL
  testSentence <- NULL
  testSentenceOriginal<-NULL
  PROCESStype1 <- NULL
  PROCESStype2 <- NULL
  PROCESStype3 <- NULL
  PROCESStype4 <- NULL
  PROCESStype5 <- NULL
  PROCESStype6 <- NULL
  PROCESStypeAll <- NULL
  tmpPROCESStypeAll <- NULL
  
  
  sentenceNo <- 0   #do not use "sentence" as variable name, it seems that it collides with the column name of POSresult, or another program has used it for other purposes. Otherwise, my R program will show unexpected results.

  sentenceNo <- POSresult[wordPosition,]$sentence

  testSentence <- subset(POSresult$lemma, POSresult$sentence==sentenceNo)  
  testSentenceOriginal <- subset(POSresult$token, POSresult$sentence==sentenceNo)  
  
  testSentence[which(testSentence==".")]<-""  #replace "." with ""
  testSentence[which(testSentence==",")]<-""  #replace "," with ""
  testSentence[which(testSentence=="?")]<-""  #replace "?" with ""
  testSentence[which(testSentence=="!")]<-""  #replace "!" with ""
  testSentence[which(testSentence==";")]<-""  #replace ";" with ""
  testSentence[which(testSentence=="\`")]<-""  #replace "`" with ""
  testSentence[which(testSentence=="\`\`")]<-""  #replace "``" with ""
  testSentenceOriginal[which(testSentenceOriginal ==".")]<-""  #replace "." with ""
  testSentenceOriginal[which(testSentenceOriginal ==",")]<-""  #replace "," with ""
  testSentenceOriginal[which(testSentenceOriginal=="?")]<-""  #replace "?" with ""
  testSentenceOriginal[which(testSentenceOriginal=="!")]<-""  #replace "!" with ""
  testSentenceOriginal[which(testSentenceOriginal==";")]<-""  #replace ";" with ""
  testSentenceOriginal[which(testSentenceOriginal=="\`")]<-""  #replace "`" with ""
  testSentenceOriginal[which(testSentenceOriginal=="\`\`")]<-""  #replace "``" with ""
  
  for (counterInVerbsAsProcess in 1:nrow(polysemousVerbAsProcess)){
    
    testPolysemousVerb <-polysemousVerbAsProcess$item[counterInVerbsAsProcess]

    ################# the following line of code checks unigram and bigram of lemma(s) against polysemousVerbAsProcess
    if((POSresult$lemma[wordPosition] %in% testPolysemousVerb) || (paste(POSresult$lemma[wordPosition],POSresult$lemma[wordPosition+1]) %in% testPolysemousVerb) ){
      tmpPolysemousVerbAsProcess<-subset(polysemousVerbAsProcess, polysemousVerbAsProcess$item==testPolysemousVerb)
      if (nrow(tmpPolysemousVerbAsProcess) >= 2){
        if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Duplicated polysemous verbs in table= ", testPolysemousVerb))) };
        return("Duplicated polysemous verbs in table!")}
      
    ViaPolysemousVerbsAsProcess <<- "Y"

################# Only 1 PROCESS type matched in table
    if (sum(!is.na(tmpPolysemousVerbAsProcess[1,5:10]))==1){
      if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.material)) {tmp10 <<- "material"} else {tmp10<<-"/"}
      if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.behavioural)) {tmp11 <<- "behavioural"} else {tmp11<<-"/"}
      if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.mental)) {tmp12 <<- "mental"} else {tmp12<<-"/"}
      if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.verbal)) {tmp13 <<- "verbal"} else {tmp13<<-"/"}
      if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.relational)) {tmp14 <<- "relational"} else {tmp14<<-"/"}
      if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.existential)) {tmp15 <<- "existential"} else {tmp15<<-"/"}
      
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched LSA item= ", testPolysemousVerb))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Only 1 PROCESS type matched in table.", testPolysemousVerb))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemma in text= ", POSresult$lemma[wordPosition]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Token in text= ", POSresult$token[wordPosition]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
      
      return("Only 1 PROCESS type matched.")
      
      theVerb<<-POSresult$token[wordPosition]
    }

################# 2 to 6 PROCESS types matched in table
    if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.material)) {PROCESStype1 <-polysemousVerbAsProcess$PROCESS.material[counterInVerbsAsProcess]}
    if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.behavioural)) {PROCESStype2 <-polysemousVerbAsProcess$PROCESS.behavioural[counterInVerbsAsProcess]}
    if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.mental)) {PROCESStype3 <-polysemousVerbAsProcess$PROCESS.mental[counterInVerbsAsProcess]}
    if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.verbal)) {PROCESStype4 <-polysemousVerbAsProcess$PROCESS.verbal[counterInVerbsAsProcess]}
    if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.relational)) {PROCESStype5 <-polysemousVerbAsProcess$PROCESS.relational[counterInVerbsAsProcess]}
    if(!is.na(tmpPolysemousVerbAsProcess$PROCESS.existential)) {PROCESStype6 <-polysemousVerbAsProcess$PROCESS.existential[counterInVerbsAsProcess]}
    

    
    if (!is.null(PROCESStype1)) {PROCESStype1<-scan(text=PROCESStype1, sep=" ", what="", quiet=TRUE)}
    if (!is.null(PROCESStype2)) {PROCESStype2<-scan(text=PROCESStype2, sep=" ", what="", quiet=TRUE)}
    if (!is.null(PROCESStype3)) {PROCESStype3<-scan(text=PROCESStype3, sep=" ", what="", quiet=TRUE)}
    if (!is.null(PROCESStype4)) {PROCESStype4<-scan(text=PROCESStype4, sep=" ", what="", quiet=TRUE)}
    if (!is.null(PROCESStype5)) {PROCESStype5<-scan(text=PROCESStype5, sep=" ", what="", quiet=TRUE)}
    if (!is.null(PROCESStype6)) {PROCESStype6<-scan(text=PROCESStype6, sep=" ", what="", quiet=TRUE)}


    print("after initialisation")
    print(PROCESStype1)
    print(PROCESStype2)
    print(PROCESStype3)
    print(PROCESStype4)
    print(PROCESStype5)
    print(PROCESStype6)
    print(testSentence)
    print(testSentenceOriginal)

    PROCESStype1<-c(PROCESStype1, lemmatize_words(PROCESStype1)) # add itself to increase weighting of orginal wording, e.g. "tasting" instead of the respective lemma "taste". 
    PROCESStype2<-c(PROCESStype2, lemmatize_words(PROCESStype2))
    PROCESStype3<-c(PROCESStype3, lemmatize_words(PROCESStype3))
    PROCESStype4<-c(PROCESStype4, lemmatize_words(PROCESStype4))
    PROCESStype5<-c(PROCESStype5, lemmatize_words(PROCESStype5))
    PROCESStype6<-c(PROCESStype6, lemmatize_words(PROCESStype6))
    testSentence<-c(testSentenceOriginal, testSentence, lemmatize_words(testSentenceOriginal)) # add itself to increase weighting, e.g. "I am observing" after lemmatization. It becomes "I be observe"

    
        
    PROCESStype1<-unique(PROCESStype1)
    PROCESStype2<-unique(PROCESStype2)
    PROCESStype3<-unique(PROCESStype3)
    PROCESStype4<-unique(PROCESStype4)
    PROCESStype5<-unique(PROCESStype5)
    PROCESStype6<-unique(PROCESStype6)
    testSentence<-unique(testSentence)

    
    

    if (!is.null(PROCESStype1)) {PROCESStype1<-qdap::rm_stopwords(PROCESStype1, unlist=TRUE, Top25Words)}
    if (!is.null(PROCESStype2)) {PROCESStype2<-qdap::rm_stopwords(PROCESStype2, unlist=TRUE, Top25Words)}
    if (!is.null(PROCESStype3)) {PROCESStype3<-qdap::rm_stopwords(PROCESStype3, unlist=TRUE, Top25Words)}
    if (!is.null(PROCESStype4)) {PROCESStype4<-qdap::rm_stopwords(PROCESStype4, unlist=TRUE, Top25Words)}
    if (!is.null(PROCESStype5)) {PROCESStype5<-qdap::rm_stopwords(PROCESStype5, unlist=TRUE, Top25Words)}
    if (!is.null(PROCESStype6)) {PROCESStype6<-qdap::rm_stopwords(PROCESStype6, unlist=TRUE, Top25Words)}
    testSentence  <-qdap::rm_stopwords(testSentence, unlist=TRUE, Top25Words)

    
    if (!is.null(PROCESStype1)) {PROCESStype1<-paste( PROCESStype1, qdap::synonyms(PROCESStype1, return.list = FALSE))}  # to give more weighting to the original words than respective synonmyns, repeat testPolysemousVerb and PROCESStype? on each synonmyn.
    if (!is.null(PROCESStype2)) {PROCESStype2<-paste( PROCESStype2, qdap::synonyms(PROCESStype2, return.list = FALSE))}
    if (!is.null(PROCESStype3)) {PROCESStype3<-paste( PROCESStype3, qdap::synonyms(PROCESStype3, return.list = FALSE))}
    if (!is.null(PROCESStype4)) {PROCESStype4<-paste( PROCESStype4, qdap::synonyms(PROCESStype4, return.list = FALSE))}
    if (!is.null(PROCESStype5)) {PROCESStype5<-paste( PROCESStype5, qdap::synonyms(PROCESStype5, return.list = FALSE))}
    if (!is.null(PROCESStype6)) {PROCESStype6<-paste( PROCESStype6, qdap::synonyms(PROCESStype6, return.list = FALSE))}
    testSentence  <-paste(testSentence, qdap::synonyms(testSentence, return.list = FALSE))  # but no testPolysemousVerb added here, as the clause mush have that word already.

    
    PROCESStype1<-sort(PROCESStype1)
    PROCESStype2<-sort(PROCESStype2)
    PROCESStype3<-sort(PROCESStype3)
    PROCESStype4<-sort(PROCESStype4)
    PROCESStype5<-sort(PROCESStype5)
    PROCESStype6<-sort(PROCESStype6)
    testSentence<-sort(testSentence)

    
    if (!is.null(PROCESStype1)) {PROCESStype1<-paste(PROCESStype1, collapse = " ")}
    if (!is.null(PROCESStype2)) {PROCESStype2<-paste(PROCESStype2, collapse = " ")}
    if (!is.null(PROCESStype3)) {PROCESStype3<-paste(PROCESStype3, collapse = " ")}
    if (!is.null(PROCESStype4)) {PROCESStype4<-paste(PROCESStype4, collapse = " ")}
    if (!is.null(PROCESStype5)) {PROCESStype5<-paste(PROCESStype5, collapse = " ")}
    if (!is.null(PROCESStype6)) {PROCESStype6<-paste(PROCESStype6, collapse = " ")}
    testSentence  <-paste(testSentence, collapse = " ")

    
    PROCESStypeAll <- c(PROCESStype1, PROCESStype2, PROCESStype3, PROCESStype4, PROCESStype5, PROCESStype6) # before the next several lines od code, so that PROCESStypeAll do not contain any "/" for next LSA process.

    
    if (is.null(PROCESStype1)==TRUE) {PROCESStype1<-"/"}
    if (is.null(PROCESStype2)==TRUE) {PROCESStype2<-"/"}
    if (is.null(PROCESStype3)==TRUE) {PROCESStype3<-"/"}
    if (is.null(PROCESStype4)==TRUE) {PROCESStype4<-"/"}
    if (is.null(PROCESStype5)==TRUE) {PROCESStype5<-"/"}
    if (is.null(PROCESStype6)==TRUE) {PROCESStype6<-"/"}
    
    tmpPROCESStypeAll <- c(PROCESStype1, PROCESStype2, PROCESStype3, PROCESStype4, PROCESStype5, PROCESStype6)

    
    withResultInPROCESStypePosition <- which(tmpPROCESStypeAll!="/") # e.g. c("a","b","/","d","/","e") [1] 1 2 4 6
    
    rankSpace<-length(withResultInPROCESStypePosition)
    
################# Add dummy string if only 2 PROCESS types matched in table    
    if (sum(!is.na(tmpPolysemousVerbAsProcess[1,5:10]))==2){
      PROCESStypeAll <- c(PROCESStypeAll, "abcdefghijklmnopqrstuvwxyz1234567890") 
      rankSpace <- rankSpace+1 }
    
    
    mydfm <- dfm(PROCESStypeAll)

    mylsa <- textmodel_lsa(mydfm)

    mylsa$docs[,1:rankSpace]
    querydfm <- dfm(c(testSentence)) %>% dfm_select(pattern=mydfm)

    
    newq <- predict(mylsa, newdata = querydfm)

    newq$docs_newspace[,1:rankSpace]
  
    PROCESStype1Value<- -9999
    PROCESStype2Value<- -9999
    PROCESStype3Value<- -9999
    PROCESStype4Value<- -9999
    PROCESStype5Value<- -9999
    PROCESStype6Value<- -9999  
    i <- 1
    
    
    for (PROCESStypeNo in withResultInPROCESStypePosition){ # loop over the vector with PROCESStype results
      assign(paste0("PROCESStype",PROCESStypeNo,"Value"), cosine(newq$docs_newspace[,1:rankSpace],mylsa$docs[i,1:rankSpace]) )
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, paste0("LSA's PROCESStype",PROCESStypeNo,"Value= ", get(paste0("PROCESStype",PROCESStypeNo,"Value"))) ) }
      i <- i+1}
      
      if (PROCESStype1Value==max(PROCESStype1Value,PROCESStype2Value,PROCESStype3Value,PROCESStype4Value,PROCESStype5Value,PROCESStype6Value)){tmp10 <<- "material"} else {tmp10<<-"/"}
      if (PROCESStype2Value==max(PROCESStype1Value,PROCESStype2Value,PROCESStype3Value,PROCESStype4Value,PROCESStype5Value,PROCESStype6Value)){tmp11 <<- "behavioural"} else {tmp11<<-"/"}
      if (PROCESStype3Value==max(PROCESStype1Value,PROCESStype2Value,PROCESStype3Value,PROCESStype4Value,PROCESStype5Value,PROCESStype6Value)){tmp12 <<- "mental"} else {tmp12<<-"/"}
      if (PROCESStype4Value==max(PROCESStype1Value,PROCESStype2Value,PROCESStype3Value,PROCESStype4Value,PROCESStype5Value,PROCESStype6Value)){tmp13 <<- "verbal"} else {tmp13<<-"/"}
      if (PROCESStype5Value==max(PROCESStype1Value,PROCESStype2Value,PROCESStype3Value,PROCESStype4Value,PROCESStype5Value,PROCESStype6Value)){tmp14 <<- "relational"} else {tmp14<<-"/"}
      if (PROCESStype6Value==max(PROCESStype1Value,PROCESStype2Value,PROCESStype3Value,PROCESStype4Value,PROCESStype5Value,PROCESStype6Value)){tmp15 <<- "existential"} else {tmp15<<-"/"}

      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Matched LSA item= ", testPolysemousVerb))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Lemma in text= ", POSresult$lemma[wordPosition]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("Token in text= ", POSresult$token[wordPosition]))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp10 =", tmp10))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp11 =", tmp11))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp12 =", tmp12))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp13 =", tmp13))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp14 =", tmp14))) }
      if (debug_log_enabled==1) {debug_log <<- rbind(debug_log, (paste("tmp15 =", tmp15))) }
    
      return(paste("LSA for", testPolysemousVerb, "completed."))
    
      theVerb<<-POSresult$token[wordPosition]
    }
    }
  if (is.null(tmp10)==TRUE){tmp10<<-"/"}
  if (is.null(tmp11)==TRUE){tmp11<<-"/"}
  if (is.null(tmp12)==TRUE){tmp12<<-"/"}
  if (is.null(tmp13)==TRUE){tmp13<<-"/"}
  if (is.null(tmp14)==TRUE){tmp14<<-"/"}
  if (is.null(tmp15)==TRUE){tmp15<<-"/"}
  }


######################## Identify next verb after 1st predictor
## identify the predicator after the unmarked theme
##VBZ, VBD, VBZ VBN (is developed), VBZ/VBD VBN (is/was developed), VBP (are, have, do), VBZ/VBP/VBD VBN VBN (has/have/had been invented), MD VB (will do)
MainVerbInUnmarkedTheme <- function(indexC){
  

  if ((!is.na(POSresult$POS[indexC+1]))  ){    
  if (!((POSresult$POS[indexC-1]=="." || POSresult$POS[indexC-1]=="?" || POSresult$POS[indexC-1]=="!" || POSresult$POS[indexC-1]=="!?" || POSresult$POS[indexC-1]=="?!") && 
        ((POSresult$POS[indexC]=="WDT") || (POSresult$POS[indexC]=="WRB") || (POSresult$POS[indexC]=="WP")) && (hasVerbInRemainingClause(indexC+2)==TRUE)) ){ #Skip "Why (did you) do...?" "How (did we) escape..?" or "What (different sounds) can you make...?"
    
    if (!((POSresult$POS[indexC-1] == "NNS" || POSresult$POS[indexC-1] == "NN" || POSresult$POS[indexC-1] == "NNP" || POSresult$POS[indexC-1] == "NNPS") && POSresult$POS[indexC] == "VBG" && hasVerbInRemainingClause(indexC+1)==TRUE) ){  # As of Sunday, 25,782 migrant (workers living / NNS VBG) in dormitories, or nearly 8 per cent, have (tested) positive for Covid-19. Also, (NN || NNP || NNPS) && VBG as well. Also, hasVerbInRemainingClause(indexC+1)==TRUE, noit (indexC+2), which is to avoid to count the verb in next clause if the VBG is the last word, e.g. How is the project (going)?
      
      if (!((POSresult$POS[indexC-1] == "TO") && (POSresult$POS[indexC] == "VB") && (hasVerbInRemainingClause(indexC)==TRUE) && hasRelativePronounInRemainingClause(indexC)==FALSE && hasConjunctionInRemainingClause(indexC)==FALSE) ) { # e.g. (To forgive) is divine.
        
        ############## Skip the finite to non-finite verb for verbs as PROCESS identification, e.g How (did/do/does) we/he escape from the prison? || How (is/are/was/were) the project(s) going?
        if (!(((POSresult$token[indexC] == "did" || POSresult$token[indexC] == "do" || POSresult$token[indexC] == "does") && hasVerbInRemainingClause(indexC)==TRUE) || 
              ((POSresult$token[indexC] == "is" || POSresult$token[indexC] == "are" || POSresult$token[indexC] == "was" || POSresult$token[indexC] == "were" ) && hasVerbInRemainingClause(indexC)==TRUE && hasPresentParticipleInRemainingClause(indexC)==TRUE && hasRelativePronounInRemainingClause(indexC)==FALSE && hasConjunctionInRemainingClause(indexC)==FALSE) ||
              (POSresult$POS[indexC] == "VBN" && hasVerbInRemainingClause(indexC)==TRUE && hasPresentParticipleInRemainingClause(indexC)==FALSE && hasRelativePronounInRemainingClause(indexC)==FALSE && hasConjunctionInRemainingClause(indexC)==FALSE) )) {  # e.g. Nelson Mandela , the newly (elected) President of South Africa , (was invited). Also, hasPresentParticipleInRemainingClause(indexC)==FALSE is to avoid any unprocession of "The government has (cautioned) people against (shaking) hands as a form of (greeting)."
          
          ############# might be good to check the punctuation before the "Isn't",e.g. ". Isn't the book good?" vs "isn't the book good?", also case sensitive 
          if (!((POSresult$token[indexC] == "Is" || POSresult$token[indexC] == "Are" || POSresult$token[indexC] == "Were" || POSresult$token[indexC] == "Might" || POSresult$token[indexC] == "Let" || POSresult$token[indexC] == "Do" || POSresult$token[indexC] == "Does" || POSresult$token[indexC] == "Did" || POSresult$token[indexC] == "Have" || POSresult$token[indexC] == "Has" || POSresult$token[indexC] == "Had") && hasVerbInRemainingClause(indexC)==TRUE) ){ ############# Is there a mass being held at noon? getting the predicator "held" instead of "Is", so on.
            if (!((POSresult$POS[indexC+1] == "VBN") || (POSresult$POS[indexC+1] == "VBG") || ((POSresult$POS[indexC+1] == "RB") && (POSresult$POS[indexC+2] == "VBN")) || ((POSresult$token[indexC-1] == ",") && (POSresult$POS[indexC] == "VBG")) || ((POSresult$POS[indexC-1] == ".") && (POSresult$POS[indexC] == "VBG")) || ((POSresult$POS[indexC-1] == "CC") && (POSresult$POS[indexC] == "VBG")) || ((POSresult$POS[indexC-1] == "IN") && (POSresult$POS[indexC] == "VBG")) || ((POSresult$POS[indexC+1] == "TO") && (POSresult$POS[indexC+2] == "VB")) || ((POSresult$POS[indexC-1] == "VBP") && (POSresult$POS[indexC] == "VBN") && (POSresult$POS[indexC+1] == "VBZ")) || (POSresult$POS[indexC+1] == "VBZ") || (POSresult$POS[indexC+1] == "VBD")  )){   ##VBP RB VBN RB VBN (have also been widely shared) || "," "VBG" (, including DBS) || "." "VBG" (. Squeezing) || "CC" "VBG" (and eating the right food is...) || "IN" "VBG" (by building management) || "TO" for 2nd verbal group is the relevant one for PROCESS type || " ... that I (have developed represents) a position / ... IN PRP (VBP VBN VBZ) DT NN || What he (said is) nonsense. || What they (lacked was) a good plan.
              if ((POSresult$POS[indexC] == "VBZ") || (POSresult$POS[indexC] == "VBD") || (POSresult$POS[indexC] == "VBP") || (POSresult$POS[indexC] == "VB") || (POSresult$POS[indexC] == "VBG") || (POSresult$POS[indexC] == "VBN")){
                experientialPROCESStypeViaCustomVerbAsProcess(indexC) #######Call function of experientialPROCESStypeViaCustomVerbAsProcess() to check PROCESS type of that a specific POSresult$token[indexC] or POSresult$lemma[indexC]
                if (InCustomVerbsAsProcess=="N"){experientialPROCESStype(indexC)}  
                if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N"){experientialPROCESStypeViaQdapSynonyms(indexC)}  ###############Call function of experientialPROCESStypeViaQdapSynonyms() to check each synonyms of that POSresult$lemma[indexC]
                if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N"){experientialPROCESStypeViaSynonyms(indexC)}  #####Call function of experientialPROCESStypeViaSynonyms() to check each synonyms of that POSresult$lemma[indexC]
                if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N" && ViaQdapSynonyms=="N" && ViaSynonyms=="N"){experientialPROCESStypeViaLevinSynonyms(indexC)}  ######Call function of experientialPROCESStypeViaLevinSynonyms() to check each synonyms of that POSresult$lemma[indexC]
                tmp10to15Counter <<- c(tmp10Counter, tmp11Counter, tmp12Counter, tmp13Counter, tmp14Counter, tmp15Counter)
                if ( ((InCustomVerbsAsProcess=="Y" || InVerbsAsProcess=="Y") && length(which(c(tmp10,tmp11,tmp12,tmp13,tmp14,tmp15)==c("/")))!=5) ||
                     (any(tmp10to15Counter!=c(0,0,0,0,0,0)) && (length(which(tmp10to15Counter==max(tmp10to15Counter)))>1)) ||
                     (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N" && ViaQdapSynonyms=="N" && ViaSynonyms=="N" && ViaLevinSynonyms=="N") ) {LSAverbsAsProcessSimilarity(indexC)}
                
                stopFindNextVerbsAsProcess <- 1
              }
            }
          }
        }
      }
    }
  }
}

}


######################## Write row result to main thematicTable 
MainWriteRowResult <- function(indexC){
tmpRow<<-data.table(i-1,tmp4,tmp5,tmp,tmp3,tmp2,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11,tmp12,tmp13,tmp14,tmp15, theVerb,InVerbsAsProcess,ViaLevinSynonyms,ViaSynonyms, ViaQdapSynonyms, InCustomVerbsAsProcess, ViaPolysemousVerbsAsProcess, NoOfTextual, NoOfInterpersonal, NoOfComplexes, x[1], x[2], (as.numeric(x[3])), x[4], x[5], x[6])
names(tmpRow)<<-c("Item","Textual_Theme","Interpersonal_Theme","Topical_Theme_Marked","Topical_Theme_Unmarked","Clause", "MOOD_declarative", "MOOD_interrogative", "MOOD_imperative", "MOOD_bound_OR_complex", "PROCESS_material", "PROCESS_behavioural", "PROCESS_mental", "PROCESS_verbal", "PROCESS_relational", "PROCESS_existential", "The_Verb_as_PROCESS", "Lookup_Verbs_as_PROCESS_CMIMM_2018", "Lookup_Verb_Classes_Levin_1993", "Lookup_via_WordNet_Synonyms", "Lookup_via_Collins_Qdap_Synonyms", "Lookup_via_Custom_Verbs_as_PROCESS", "Lookup_via_LSA", "No_of_Textual", "No_of_Interpersonal", "No_of_Bound_or_Complex", "Article_Title", "Data_Subject", "Date", "Genre", "Co_Org_Corp","From_which_Country_or_City")

thematicTable<<-rbindlist(list(thematicTable, tmpRow), fill=TRUE)
}




#Plotting the clause dependency by UDPIPE POS tags
plot_annotation <- function(x, size = 3){
  stopifnot(is.data.frame(x) & all(c("sentence_id", "token_id", "head_token_id", "dep_rel",
                                     "token_id", "token", "lemma", "upos", "xpos", "feats") %in% colnames(x)))
  x <- x[!is.na(x$head_token_id), ]
  x <- x[x$sentence_id %in% min(x$sentence_id), ]
  edges <- x[x$head_token_id != 0, c("token_id", "head_token_id", "dep_rel")]
  edges$label <- edges$dep_rel
  g <- graph_from_data_frame(edges,
                             vertices = x[, c("token_id", "token", "lemma", "upos", "xpos", "feats")],
                             directed = TRUE)
  ggraph(g, layout = "linear") +
    geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                  arrow = grid::arrow(length = unit(4, 'mm'), ends = "last", type = "closed"),
                  end_cap = ggraph::label_rect("wordswordswords"),
                  label_colour = "red", check_overlap = TRUE, label_size = size) +
    geom_node_label(ggplot2::aes(label = token), col = "darkgreen", size = size, fontface = "bold") +
    geom_node_text(ggplot2::aes(label = upos), nudge_y = -0.35, size = size) +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "last sentence of text source", subtitle = "POS tagging & dependency relations")
}



#Parser
setwd(filePath)
fileList <- list.files(path=paste0(filePath,"sourceTexts//"))
fileList <- fileList[ !grepl("tmp", fileList)]

################## Loop for all text files
for (filename in fileList){
  x<-scan(file=paste0(filePath,"sourceTexts//",filename), what=character(),sep="\n", quiet=TRUE, blank.lines.skip=TRUE)
  xLineByLine<-readLines(paste0(filePath,"sourceTexts//",filename))
  
  x <-gsub(":", ".", x) # replace and clean ":" into ".". It helps the identification of imperative clause.
  
  xLineByLine<-gsub("\"", "", xLineByLine)
  write(xLineByLine, file=paste0(filePath,"sourceTexts//","tmp//", file_path_sans_ext(filename),"-NoQuote.txt"))
  
  xWordByWord<-scan(file=paste0(filePath,"sourceTexts//","tmp//", file_path_sans_ext(filename),"-NoQuote.txt"), what=character(), sep="", quiet=TRUE, blank.lines.skip=TRUE)
  
  
  xWordByWordNoCommasNoPeriods<-(gsub("\\.", "", gsub(",", ".", xWordByWord)))
  xWordByWordNoCommasNoPeriodsNonCase<-tolower(xWordByWordNoCommasNoPeriods)
  WordList<-unique(xWordByWord)
  WordListNoCommasNoPeriods<-(gsub("\\.", "", gsub(",", ".", WordList)))
  WordListNoCommasNoPeriodsNonCase<-unique(tolower(WordListNoCommasNoPeriods))

  
  
  position<-c(which(xWordByWordNoCommasNoPeriodsNonCase=="to"))
  xWordByWord[position]
  
  output = annotateString(x)
  
  POSresult<-getToken(output)[,c(1:9)]
  



POStag<-c("CC","CD","DT","EX","FW","IN","JJ","JJR","JJS","LS","MD","NN","NNS","NNP","NNPS","PDT","POS","PRP","PRP$","RB","RBR","RBS","RP","SYM","TO","UH","VB","VBD","VBG","VBN","VBP","VBZ","WDT","WP","WP$","WRB")
POSname<-c("Coordinating conjunction","Cardinal number","Determiner","Existential there","Foreign word","Preposition","Adjective","Adjective, comparative","Adjective, superlative","List item marker","Modal","Noun, singular or mass","Noun, plural","Proper noun, singular","Proper noun, plural","Predeterminer","Possessive ending","Personal pronoun","Possessive pronoun","Adverb","Adverb, comparative","Adverb superlative","Particle","Symbol","to","Interjection","Verb, base form","Verb, past tense","Verb, gerund or present participle","Verb, past participle","Verb, non3rd person singular present","Verb, 3rd person singular present","Wh-determiner","Wh-pronoun","Possessive wh-pronoun","Wh-adverb")


#Correction based on English grammar rules or in correct POS sequences
#"an electric (current)."
for (objNo in 1:nrow(POSresult)){
if(POSresult[objNo,7]=="DT"){
  if(POSresult[(objNo+1),7]=="JJ"){
    if(POSresult[(objNo+2),7]=="JJ"){
      if(POSresult[(objNo+3),7]=="."){
        POSresult[(objNo+2),7]<-"NN"
        POSresult[(objNo+2),8]<-"O"
        correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3]))
      }
    }
  }
}  
}
correction


#"in the (past) peope"
for (objNo in 1:nrow(POSresult)){
  if(length(POSresult[objNo-1,7]!=0)) {
  if(POSresult[objNo-1,7]=="."){
  if(POSresult[objNo,7]=="IN"){
    if(POSresult[(objNo+1),7]=="DT"){
      if(POSresult[(objNo+2),7]=="JJ"){
        if(POSresult[(objNo+3),7]=="NNS"){
          if(POSresult[(objNo+4),7]=="VBN"){
            POSresult[(objNo+2),7]<-"NN"
            POSresult[(objNo+4),7]<-"VBD"
            correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3],POSresult[(objNo+4),3]))
        }}}
      }
    }
  }  
}}
correction

#"bank, DBS, (evacuated) "
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="NN"){
    if(!(is.na(POSresult[(objNo+1),7])) && POSresult[(objNo+1),7]==","){
      if(POSresult[(objNo+2),7]=="NNP"){
        if(POSresult[(objNo+3),7]==","){
          if(POSresult[(objNo+4),7]=="VBN"){
            POSresult[(objNo+4),7]<-"VBD"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3],POSresult[(objNo+4),3]))
        }}
      }
    }
  }  
}
correction

#"fax machines send (printed) information."
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="NNS"){
    if((POSresult[(objNo+1),7]=="VBP") && !((POSresult[(objNo+1),4]=="have") || (POSresult[(objNo+1),4]=="be"))){
      if(POSresult[(objNo+2),7]=="VBN"){
        if(POSresult[(objNo+3),7]=="NN"){
          POSresult[(objNo+2),7]<-"JJ"
          POSresult[(objNo+2),8]<-"O"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3]))
        }
      }
    }
  }  
}
correction

#"I can (dread) / I can dread indeed / I can dread him"
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="PRP"){
    if(POSresult[(objNo+1),7]=="MD"){
      if(POSresult[(objNo+2),7]=="NN"){
        if((POSresult[(objNo+3),7]==".") || (POSresult[(objNo+3),7]=="RB") || (POSresult[(objNo+3),7]=="PRP")){
          POSresult[(objNo+2),7]<-"VB"
          POSresult[(objNo+2),8]<-"O"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3]))
        }
      }
    }
  }  
}
correction

#"I can profoundly dread"
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="PRP"){
    if(POSresult[(objNo+1),7]=="MD"){
      if(POSresult[(objNo+2),7]=="RB"){
        if(POSresult[(objNo+3),7]=="NN"){
          POSresult[(objNo+3),7]<-"VB"
          POSresult[(objNo+3),8]<-"O"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3], POSresult[(objNo+3),3]))
        }
      }
    }
  }  
}
correction

#"... you (please) do..."
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="PRP"){
    if(POSresult[(objNo+1),4]=="please"){
      if(POSresult[(objNo+2),7]=="VB"){
        if(POSresult[(objNo+1),7]=="VB"){
          POSresult[(objNo+1),7]<-"RB"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3]))
        }
      }
    }
  }  
}
correction

#". (May) I ..."
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="NNP"){
    if(POSresult[(objNo+1),7]=="PRP"){
      if(POSresult[(objNo+2),7]=="VBP"){
        if((POSresult[(objNo),4]=="May") && (POSresult[(objNo),8]=="DATE")){
          POSresult[(objNo),7]<-"MD"
          POSresult[(objNo+2),7]<-"VB"
          POSresult[(objNo),8]<-"O"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3]))
        }
      }
    }
  }  
}
correction


#". (Shall) I ..."
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="NNP"){
    if(POSresult[(objNo+1),7]=="PRP"){
      if(POSresult[(objNo+2),7]=="VBP"){
        if(POSresult[(objNo),4]=="Shall"){
          POSresult[(objNo),7]<-"MD"
          POSresult[(objNo+2),7]<-"VB"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3]))
        }
      }
    }
  }  
}
correction


#". NN NN/NNS .." Match the first "NN" with WordNet, and see whether it can be a verb but wrongly tagged. Make sure the NN is the only verb-like, no other VB, VBP, VBZ in the same clause (ignore VBD, i.e. still change NN to VB, as some *ed are adjectives, e.g. the evidence (gained) from observation).
# e.g. Test ideas... Question authority...
tmpUntilSentenceEnd <- 0
hasMatchedVerbinRange <-0
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="."){
    if (!(is.na(POSresult[(objNo+1),7])) && (POSresult[(objNo+1),7]=="NN" || POSresult[(objNo+1),7]=="NNP")){  #is.na is too avoid the error triggerred at end of text
      if (!(is.na(POSresult[(objNo+2),7]))){
      if((POSresult[(objNo+2),7]=="NN") || (POSresult[(objNo+2),7]=="NNS")){

        tmpUntilSentenceEnd <- which(POSresult[,3]==".") - objNo
        for (objCount in 1:tmpUntilSentenceEnd[ which(tmpUntilSentenceEnd >0)[1] ] ){ # >0 not >=0, is to avoid "." hit on itself and generate 0 range
          if(!(POSresult[(objNo+1),7]=="VB")){  # =="VB" to avoid repeated change from "NN" to "VB", and to safe computing power
             if((POSresult[(objNo+objCount),7]=="VB") || (POSresult[(objNo+objCount),7]=="VBP") || (POSresult[(objNo+objCount),7]=="VBZ") || ((POSresult[(objNo+objCount),7]=="VBD") && (POSresult[(objNo+objCount+1),7]=="VBN"))){ #Copy VB to NN one time, if no VB, VBP, and VBZ in the same clause until end (full stop)
               hasMatchedVerbinRange <- 1
            }
          }
        }
        # The synonyms() is to verify that the token tagged with "NN" can be a verb too. Also, synonyms(POSresult[(objNo+1),3],"VERB"), where 3(=token), not lemma as checking "testing" instead of "test" in against VERB 
        # but (identical(synonyms(POSresult$lemma[(objNo+2)],"VERB"), character(0))) can be double-verifing lemma[(objNo+2)] is not a verb
        if((hasMatchedVerbinRange==0) && !(identical(synonyms(POSresult[(objNo+1),3],"VERB"), character(0))) 
           && (identical(synonyms(POSresult$lemma[(objNo+2)],"VERB"), character(0))) ){ POSresult[(objNo+1),7]<-"VB" 
        correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3])) }
      }
    }}
  }
  hasMatchedVerbinRange <- 0
}
correction


#"./PRP NN/NNP (NNS) RB IN... " Match the seond item, "NN" with WordNet, and see whether it can be a verb but wrongly tagged. Except with conjunction and pronoun in remaining clause, make sure the NN is the only verb-like, no other VB, VBP, VBZ in the same clause (ignore VBD, i.e. still change NN to VB, as some *ed are adjectives, e.g. the evidence (gained) from observation).
# e.g. Doctor (checks) again ...
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="." || POSresult[objNo,7]=="DT"){
    if (!(is.na(POSresult[(objNo+2),7])) && (POSresult[(objNo+1),7]=="NN" || POSresult[(objNo+1),7]=="NNP")){  #is.na is too avoid the error triggerred at end of text
      if (POSresult[(objNo+2),7]=="NNS" && POSresult[(objNo+3),7]=="RB" && POSresult[(objNo+4),7]=="IN"){
        
        # The synonyms() is to verify that the token tagged with "NN" can be a verb too.
        # but !(identical(synonyms(POSresult$lemma[(objNo+2)],"VERB"), character(0))) can be double-verifing lemma[(objNo+2)] can be a verb
        if(( (hasVerbInRemainingPhrase(objNo+2,"finite")==TRUE && (hasConjunctionInRemainingClause(objNo+2)==TRUE || hasRelativePronounInRemainingClause(objNo+2)==TRUE) ) || (hasVerbInRemainingPhrase(objNo+2,"finite")=="No verb in this remaining phrase." ) ) 
            && !(identical(synonyms(POSresult$lemma[(objNo+2)],"VERB"), character(0))) 
            ){ POSresult[(objNo+2),7]<-"VBZ" 
           correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3])) }
      }
    }
  }
}
correction


#". NN CC VB .." Match the "NN" with WordNet, and see whether it is a verb but wrongly tagged, e.g. (Taste) and drink... / NN CC VB should be VB CC VB
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="."){
    if (!(is.na(POSresult[(objNo+2),7])) && POSresult[(objNo+1),7]=="NN"){  #is.na is too avoid the error triggerred at end of text
      if((POSresult[(objNo+2),7]=="CC") && (POSresult[(objNo+3),7]=="VB")){
        
        if(!(identical(synonyms(POSresult[(objNo+1),3],"VERB"), character(0)))){ POSresult[(objNo+1),7]<-"VB" # The synonyms() is to verify that the token tagged with "NN" can be a verb too. Also, synonyms(POSresult[(objNo+1),3],"VERB"), where 3(=token), not lemma as checking "testing" instead of "test" in against VERB 
        correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3])) }
      }
    }
  }
}
correction


#". DT NN NNS JJ .." Match the "NNS" with WordNet, and see whether it is a verb but wrongly tagged. e.g. The tea (tastes) sour flavour / . DT NN (NNS) JJ NN. NNS is wrong. It should be VBZ. Make sure the NN is the only verb-like, no other VB, VBP, VBZ in the same clause (ignore VBD, i.e. still change NN to VB, as some *ed are adjectives, e.g. the evidence (gained) from observation).
tmpUntilSentenceEnd <- 0
hasMatchedVerbinRange <-0
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="."){
    if (!(is.na(POSresult[(objNo+2),7])) && POSresult[(objNo+1),7]=="DT"){  #is.na is too avoid the error triggerred at end of text
      if((POSresult[(objNo+2),7]=="NN") && (POSresult[(objNo+3),7]=="NNS") && (POSresult[(objNo+4),7]=="JJ") && (POSresult[(objNo+5),7]=="NN")){
        
        tmpUntilSentenceEnd <- which(POSresult[,3]==".") - objNo
        for (objCount in 1:tmpUntilSentenceEnd[ which(tmpUntilSentenceEnd >0)[1] ] ){ # >0 not >=0, is to avoid "." hit on itself and generate 0 range
          if(!(POSresult[(objNo+1),7]=="VB")){  # =="VB" to avoid repeated change from "NN" to "VB"
            if((POSresult[(objNo+objCount),7]=="VB") || (POSresult[(objNo+objCount),7]=="VBP") || (POSresult[(objNo+objCount),7]=="VBZ") || ((POSresult[(objNo+objCount),7]=="VBD") && (POSresult[(objNo+objCount+1),7]=="VBN"))){ #Copy VB to NN one time, if no VB, VBP, and VBZ in the same clause until end (full stop)
              hasMatchedVerbinRange <- 1
            }

          }
        }
        if((hasMatchedVerbinRange==0) && !(identical(synonyms(POSresult[(objNo+3),4],"VERB"), character(0)))){ POSresult[(objNo+3),7]<-"VBZ" # The synonyms() is to verify that the lemma tagged with "NNS" can be a verb too. Also, synonyms(POSresult[(objNo+1),4],"VERB"), where 4(=lemma), using lemma as checking "taste" instead of "tastes", synonyns("tastes","VERB") return "character(0)"
        correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3])) }
      }
    }
  }
  hasMatchedVerbinRange <- 0
}
correction


#". Usain Bolt ...", correct "...VB NN..." into "NNP NNP". This correction can amend up to 4-word of pronoun, i.e. [(objNo+1):(objNo+4)]
for (objNo in 1:nrow(POSresult)){
  if(!(POSresult[objNo,7]==".")){
    if(!(is.na(POSresult[(objNo+1),7])) && ((POSresult[(objNo+1),7]=="VB") && identical(synonyms(POSresult[(objNo+1),3],"VERB"), character(0)) ) ){  #if not end-of-data && [(objNo+1),7] wrongly tagged "VB" and no VERB synonyms() can be found, i.e. return "character(0)
      if(!(substring(POSresult[(objNo+1),3],1,1)==tolower(substring(POSresult[(objNo+1),3],1,1)) )){  #check whether - !(substring("Usain",1,1)==tolower(substring("Usain",1,1)) ) - return TRUE, i.e. !("U"=="u")
          POSresult[(objNo+1),7]<-"NNP"

          if(!(substring(POSresult[(objNo+2),3],1,1)==tolower(substring(POSresult[(objNo+2),3],1,1)) )){
            POSresult[(objNo+2),7]<-"NNP"
          }
          if(!(substring(POSresult[(objNo+3),3],1,1)==tolower(substring(POSresult[(objNo+3),3],1,1)) )){
            POSresult[(objNo+3),7]<-"NNP"
          }
          if(!(substring(POSresult[(objNo+4),3],1,1)==tolower(substring(POSresult[(objNo+4),3],1,1)) )){
            POSresult[(objNo+4),7]<-"NNP"
          }
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3]))
      }
    }
  }  
}
correction


#". To have requested leave ...", correct ". TO VB (VBN VBP)..." into "JJ NN".
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="."){
    if((!is.na(POSresult[(objNo+1),7]) && (POSresult[objNo+1,7]=="TO"))){ #!is.na(POSresult[(objNo+1),7]) is to avoid error "missing value where TRUE/FALSE needed"
     if(POSresult[objNo+2,3]=="have"){
      if(!(is.na(POSresult[(objNo+3),7])) && ((POSresult[(objNo+3),7]=="VBN") && !identical(synonyms(POSresult[(objNo+3),3],"ADJ"), character(0)) ) ){  #if not end-of-data && [(objNo+2),7] wrongly tagged "VBN" and it can be adjective in synonyms(), i.e. do not return "character(0)"
        if((POSresult[objNo+4,7]=="VBP") && !identical(synonyms(POSresult[(objNo+4),3],"NOUN"), character(0)) ){
          POSresult[(objNo+3),7]<-"JJ"
          POSresult[(objNo+4),7]<-"NN"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3]))
        }
        }
      }
    }
  }  
}
correction


#"... have (further) damped business ...", correct "... VBP (JJ) VBN..." into "VBP (RB) VBN".
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="VBP" || POSresult[objNo,7]=="VBZ"){
    if(!is.na(POSresult[(objNo+1),7])){ #!is.na(POSresult[(objNo+1),7]) is to avoid error "missing value where TRUE/FALSE needed"
        if(is.na(POSresult[(objNo+1),7])==FALSE){  #if not end-of-data && [(objNo+1),7] wrongly tagged "JJ" and it can be adverb in synonyms(), i.e. do not return "character(0)"
          if(POSresult[(objNo+1),7]=="JJ" 
           && POSresult[(objNo+2),7]=="VBN" && !identical(synonyms(POSresult[(objNo+1),3],"ADV"), character(0)) ){ 
            POSresult[(objNo+1),7]<-"RB"
            correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3]))
          }
        }
    }
  }  
}
correction


#"that really (upset) them."
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="DT"){
    if(POSresult[(objNo+1),7]=="RB"){
      if((POSresult[(objNo+2),7]=="JJ") && 
         !identical(synonyms(POSresult[(objNo+2),3],"VERB"), character(0)) &&
         (substring(POSresult[(objNo+2),3], nchar(POSresult[(objNo+2),3]), nchar(POSresult[(objNo+2),3]) ) != "s")) {  # detect the last character of "upsets" is "s" or not. If no "s" then JJ to VBP, if "s" then JJ to VBZ
        if(POSresult[(objNo+3),7]=="PRP"){
          POSresult[(objNo+2),7]<-"VBP"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3]))
        }
      }
    }
  }  
}
correction


#"that really (upsets) them." The 4 in " synonyms(POSresult[(objNo+2),4],"VERB") " below is to check the lemma instead of token, as synonyms("upsets","VERB") return character(0).
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="DT"){
    if(POSresult[(objNo+1),7]=="RB"){
      if(is.na(POSresult[(objNo+2),7])==FALSE){
      if((POSresult[(objNo+2),7]=="NNS") && 
         !identical(synonyms(POSresult[(objNo+2),4],"VERB"), character(0)) &&
         (substring(POSresult[(objNo+2),3], nchar(POSresult[(objNo+2),3]), nchar(POSresult[(objNo+2),3]) ) == "s")) {  # detect the last character of "upsets" is "s" or not. If no "s" then JJ to VBP, if "s" then JJ to VBZ
        if(POSresult[(objNo+3),7]=="PRP"){
          POSresult[(objNo+2),7]<-"VBZ"
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3],POSresult[(objNo+3),3]))
        }
      }
    }}
  }  
}
correction


#"I am tasting the wine.", i.e. PRP "am" NN to PRP "am" VBG. Also correct the lemma "tasting" to "taste".
for (objNo in 1:nrow(POSresult)){
  if(POSresult[objNo,7]=="PRP"){
    if(POSresult[(objNo+1),3]=="am"){
      if((POSresult[(objNo+2),7]=="NN") && 
         !identical(lemmatize_words(POSresult[(objNo+2),3]), POSresult[(objNo+2),3]) &&
         !identical(synonyms(lemmatize_words(POSresult[(objNo+2),3]),"VERB"), character(0)) &&
         (substring(POSresult[(objNo+2),3], (nchar(POSresult[(objNo+2),3]) -2), nchar(POSresult[(objNo+2),3]) ) == "ing")) 
        {  # detect the last 3 characters of "tasting" is "ing" or not. If "ing" then NN to VBG
          POSresult[(objNo+2),7]<-"VBG"
          POSresult[(objNo+2),4]<-lemmatize_words(POSresult[(objNo+2),3])
          correction<-c(correction, paste(POSresult[objNo,3],POSresult[(objNo+1),3],POSresult[(objNo+2),3]))
      }
    }
  }  
}
correction


############ Matching the Penn Treebank POS to one-letter POS tags
############ This conversion must be done after the POS corrections above which can increase the rate of accuracy
#Matching the Penn Treebank POS to Universial Tagset
POSresult<-cbind(POSresult,universalTagset(POSresult$POS))
colnames(POSresult)[10]<-"UDPOS"
POSresult<-cbind(POSresult,as_phrasemachine(POSresult$POS, type=c("penn-treebank")))
colnames(POSresult)[11]<-"OneLetterPOS"



#Chunked into clause complexes and clauses, also Analysis of THEME,MOOD,PROCESS
#.-IN-DT-JJ-NNS or .-IN-DT-NNS .-IN-DT-NN .-IN-DT-NN,
#.-IN-CD-,NNP or.-IN-CD-NNP .-IN-CD-,PRP .-IN-CD-PRP .-IN-CD-,DT .-IN-CD-DT
#.-IN-CD-NNP-CD,


#rule? is the POS sequence, themeLen? is the length of the matched theme, ruleType? is the output tmp? in tmpRow data.table 
#ruleType<-1 is marked theme; ruleType<-3 is unmarked theme; ruleType<-2 is long marked theme; ruleType<-4 is long unmarked theme
#themeLen?? is the effective length of theme. It counts from the beginning; themeEndToRuleEnd?? is the position difference between the theme end and the rule end.
rule1<-c(".","IN","DT","NN",",")
themeLen1<-3
ruleType1<-1
rule2<-c("[.,''``]PDNMMVAV") #e.g. (That the food might not be fresh) didn't occur to them. Also, the ?? matching "Previously" ","
themeEndToRuleEnd2<-1
ruleType2<-4
rule3<-c(".","IN","DT","NNS",",")
themeLen3<-3
ruleType3<-1
rule4<-c(".","IN","DT","JJ","NNS")
themeLen4<-4
ruleType4<-1
rule5<-c(".","IN","CD","NNP","CD")
themeLen5<-4
ruleType5<-1
rule6<-c(".","IN","CD",",","NNP")
themeLen6<-2
ruleType6<-1
rule7<-c(".","IN","CD","NNP")
themeLen7<-2
ruleType7<-1
rule8<-c(".","IN","CD",",","PRP")
themeLen8<-2
ruleType8<-1
rule9<-c(".","IN","CD","PRP")
themeLen9<-2
ruleType9<-1
rule10<-c(".","IN","CD",",","DT")
themeLen10<-2
ruleType10<-1
rule11<-c(".","IN","CD","DT")
themeLen11<-2
ruleType11<-1
rule12<-c(".","IN","IN","NN",",")
themeLen12<-3
ruleType12<-1
rule13<-c(".","``","IN","DT","JJ","NN",",")
themeLen13<-5
ruleType13<-1
rule14<-c(".","IN","DT","JJ","NN",",")
themeLen14<-4
ruleType14<-1
rule15<-c(".","WDT","DT","JJ","NN",",") #e.g. (What an impetuous boy), he is!
themeLen15<-4
ruleType15<-3
rule16<-c(".","WP","DT","JJ","NN",",") #e.g. (What a lovely day), it is!
themeLen16<-4
ruleType16<-3
rule17<-c("[.''``]PVANMM") #e.g. #e.g. (To have requested leave just now) would have been a bit unreasonable.
themeEndToRuleEnd17<-0
ruleType17<-4
rule18<-c("[.''``]PVODPOPD") #e.g. To see him all by himself like that... To work from home by myself like that...
themeEndToRuleEnd18<-0
ruleType18<-4
rule19<-c("[.''``]OOVVVD") #e.g. (What I am thinking) is this
themeEndToRuleEnd19<-2
ruleType19<-4
rule20<-c("[.''``]OOVVV") #e.g. (What I have been thinking) is this
themeEndToRuleEnd20<-0
ruleType20<-4
rule21<-c("[.''``]ONVPDN") #e.g.  What people see on the screen
themeEndToRuleEnd21<-0
ruleType21<-4
rule22<-c("[.''``]OOVPDN") #e.g.  What he did (does) for a living
themeEndToRuleEnd22<-0
ruleType22<-4
rule23<-c("[.''``]OOVVOO") #e.g.  What you see (is what you)
themeEndToRuleEnd23<-3
ruleType23<-4
rule24<-c("[.''``]OOVVPV") #e.g.  What I am proposing about compounding
themeEndToRuleEnd24<-0
ruleType24<-4
rule25<-c("[.''``]PVPNPOPD") #e.g. To see him all by himself like that. To work from home (home) by myself like that 
themeEndToRuleEnd25<-0
ruleType25<-4
rule26<-c("[.''``]PN*[C]?N*,PDN[V]?N*[P]?N*[NC]*[V]?N*,") #e.g. At Sezana, on the Yugoslav border, they were very naughty, too.
themeEndToRuleEnd26<-0
ruleType26<-2
rule27<-c("NNS",",","IN","NNP",",") # e.g. in other (words, since January,)
themeLen27<-3
ruleType27<-1
rule28<-c(".","IN","IN","NNP",",") # e.g. . As of Sunday , 
themeLen28<-3
ruleType28<-1
rule29<-c("[.''``]DNOVOA") #e.g. The teacher who understand him best was Mary Smith.
themeEndToRuleEnd29<-0
ruleType29<-4
rule30<-c(".","WP","PRP","VBD","VBZ","NN") #e.g.  What he said is nonsense .
themeLen30<-3
ruleType30<-3
rule31<-c(".","WP","PRP","VBD","VBD","DT","JJ") #e.g.  What they lacked was a good plan .
themeLen31<-3
ruleType31<-3
rule32<-c(".","WP","PRP","VBD","VBZ","DT","JJ") #e.g.  What they lacked is a good plan .
themeLen32<-3
ruleType32<-3
rule33<-c(".","IN","DT","NN",",") # e.g. In the future, 
themeLen33<-3
ruleType33<-1
rule34<-c(".","IN","NNP",",") # e.g. Since January,
themeLen34<-2
ruleType34<-1
rule34<-c(".","TO","VB","VBZ") # e.g.  To forgive is divine.
themeLen34<-2
ruleType34<-3
rule35<-c("[.''``]DNP.POVV") #e.g. (The view of English that I have developed) represents...
themeEndToRuleEnd35<-0
ruleType35<-4
rule36<-c(".","RB","CD","TO","VB","RP")
themeLen36<-5
ruleType36<-1
rule37<-c(".","IN","NN","NN","CD","DT")
themeLen37<-4
ruleType37<-1
rule38<-c("[.''``]PDNNVA") # e.g. In the past people used colourful ink ...
themeEndToRuleEnd38<-3
ruleType38<-2
rule39<-c("[.''``]PD((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*N{0,8}[,])") # At the (N) + N{0,8}, i.e. max N N N N N N N N, ... Also, By the end of the month, ...
themeEndToRuleEnd39<-1
ruleType39<-2
rule40<-c("[.''``]PNPN[,])") # For someone wihtout something, ...
themeEndToRuleEnd40<-1
ruleType40<-2
rule41<-c("[.''``]P((A|N)*N(P+D*(A|N)*N)*P*(M|V)*V(M|V)*|(M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+|(A|N)*N(P+D*(A|N)*N)*P*((M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+))[,])") # To do something very special, 
themeEndToRuleEnd41<-1
ruleType41<-2
rule42<-c("[.``]PA[,])") # e.g. For one,
themeEndToRuleEnd42<-1
ruleType42<-2
rule43<-c("[.''``]AN[,]O)") # e.g. Last year, it ...
themeEndToRuleEnd43<-2
ruleType43<-2
rule44<-c("[.''``]AN[,]N)") # e.g. Last year, loss ...
themeEndToRuleEnd44<-2
ruleType44<-2
rule45<-c("[.''``]PVMPDA(A|N)*N(P+D*(A|N)*N)*((A|N)*N(P+D*(A|N)*N)*P*(M|V)*V(M|V)*|(M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+|(A|N)*N(P+D*(A|N)*N)*P*((M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+))[,]N)") # e.g. Despite growing rapidly in the past {+ noun phrase + verb phrase}, ...
themeEndToRuleEnd45<-1
ruleType45<-2
rule46<-c("[.''``]PDNAA{0,2}(A|N)*N(P+D*(A|N)*N)*)") # e.g. During this century many more forms ...
themeEndToRuleEnd46<-3
ruleType46<-2
rule47<-c("[.''``]PVMON[,]") # e.g. In handing down his decision ... 
themeEndToRuleEnd47<-1
ruleType47<-2
rule48<-c("[.''``]PVPAN[,]") # e.g. After losing in last season ... 
themeEndToRuleEnd48<-1
ruleType48<-2
rule49<-c("[.''``]PVPAN..[,]") # e.g. After losing in last season 's final ... 
themeEndToRuleEnd49<-3
ruleType49<-2
rule50<-c("[.''``]POANN{0,2}[,]") # e.g. Between his frequent hospital visits ... 
themeEndToRuleEnd50<-1
ruleType50<-2
rule51<-c("[.''``]PD(A|N)*N(P+D*(A|N)*N)*PD(A|N)*N(P+D*(A|N)*N)*N") # e.g. At the start of the day England/N ... 
themeEndToRuleEnd51<-1
ruleType51<-2
rule52<-c("[.''``]MPAM[,]") # e.g. Just after 6.30 am, ... 
themeEndToRuleEnd52<-1
ruleType52<-2
rule53<-c("[.''``]PDNM[,]") # e.g. Around an hour later, ... 
themeEndToRuleEnd53<-1
ruleType53<-2
rule54<-c("[.''``]P(A|N)*N(P+D*(A|N)*N)*[,]") # e.g. . Before sentencing, ... 
themeEndToRuleEnd54<-1
ruleType54<-2
rule55<-c("[.''``]PAN[,]") # e.g. . " Among individual companies, ... 
themeEndToRuleEnd55<-1
ruleType55<-2
rule56<-c("[.''``]PDNDN((A|N)*N(P+D*(A|N)*N)*P*(M|V)*V(M|V)*|(M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+|(A|N)*N(P+D*(A|N)*N)*P*((M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+))[,]") # e.g. . In a letter this month asking for , ... 
themeEndToRuleEnd56<-1
ruleType56<-2
rule57<-c("[.''``]PDN((A|N)*N(P+D*(A|N)*N)*P*(M|V)*V(M|V)*|(M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+|(A|N)*N(P+D*(A|N)*N)*P*((M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+))[,]") # e.g. . In a letter asking for, ... 
themeEndToRuleEnd57<-1
ruleType57<-2
rule58<-c("[.''``]PNPDA(A|N)*N(P+D*(A|N)*N)*[,]") # e.g. . On top of the normal stresses + Noun phrase, ... 
themeEndToRuleEnd58<-3
ruleType58<-2
rule59<-c("[.``]PO[,]") # e.g. . For me, ... . Also, deliberately remove '' as it cause false positive.
themeEndToRuleEnd59<-1
ruleType59<-2
rule60<-c(".","IN","PRP",",") # e.g. . For me, ... 
themeLen60<-2
ruleType60<-1
#rule60<-c("[.``]PN{0,2}[,]") # this rule has many false positives.
#themeEndToRuleEnd60<-1
#ruleType60<-2
rule61<-c("[.''``]POVNN{0,2}[,]") # e.g. In his opening statement ... 
themeEndToRuleEnd61<-1
ruleType61<-2
rule62<-c("[.''``]ANN{0,2}[,]") # e.g. Last Sunday ... Alsom compare with rule43, 44 above.
themeEndToRuleEnd62<-1
ruleType62<-2
rule63<-c("[.''``]PMN[,]") # e.g. . With only sailing, ... 
themeEndToRuleEnd63<-1
ruleType63<-2
rule64<-c("[.''``]PONN{0,3}[,]") # e.g. . In his tweet, ... 
themeEndToRuleEnd64<-1
ruleType64<-2
rule65<-c("[.''``]PPDN[,]") # e.g. . For over a decade, ... 
themeEndToRuleEnd65<-1
ruleType65<-2
rule66<-c("[.''``]PNON[,]") # e.g. . After Friday's votes, ... 
themeEndToRuleEnd66<-1
ruleType66<-2
rule67<-c("[.''``]POANN{0,4}.[,]") # e.g. . With his 2020 re-election heating + up, ... 
themeEndToRuleEnd67<-1
ruleType67<-2
rule68<-c("[.''``]POANPDN[,]") # e.g. . In their final game of the decade, ... 
themeEndToRuleEnd68<-1
ruleType68<-2
rule69<-c("[.''``]CNM[,]") # e.g. . But hours later, ... 
themeEndToRuleEnd69<-1
ruleType69<-2
rule70<-c("[.''``]PDANPAP(A|N)*N(P+D*(A|N)*N)*[,]") # e.g. . In the 50th installment of one of sport/N 's greatest... , ... 
themeEndToRuleEnd70<-1
ruleType70<-2
rule71<-c("[.''``]CPDN[,]") # e.g. . But in the evening, ... 
themeEndToRuleEnd71<-1
ruleType71<-2
#rule72<-c("[.''``]DN{0,2}[,]") # e.g. . This time, ... . Also, this rule has many false positives, e.g. (The Agriculture ,) Fisheries and Conservation Depart
#themeEndToRuleEnd72<-1
#ruleType72<-2
rule72<-c("[.''``]C{0,2}PDNPAN((A|N)*N(P+D*(A|N)*N)*P*(M|V)*V(M|V)*|(M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+|(A|N)*N(P+D*(A|N)*N)*P*((M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+))[,]") # e.g. . But with a slew of new restrictions announced on Friday/N + verb phrase.
themeEndToRuleEnd72<-1
ruleType72<-2
rule73<-c("[.''``]PDN{0,3}PN{0,2}[,]")
themeEndToRuleEnd73<-1
ruleType73<-2
rule74<-c("[.''``]PPDAN{0,2}[,]")
themeEndToRuleEnd74<-1
ruleType74<-2
rule75<-c("[.''``]PDNPNO(A|N)*N(P+D*(A|N)*N)*[,]") # e.g. . In a speech at Dresden's palace/N of culture, ... 
themeEndToRuleEnd75<-1
ruleType75<-2

NoOfRules<-75


ruleCounter<-1
ngram_maxRatio <- 1.3 # the factor that is used to produce the value of ngram_max=(length of rule?? * ngram_MaxRatio)
NoOfThemesDetected<-0
counterForMatchRuleinPOS<-0
AccMatchRuleinPOS<- vector(mode="list", length=5)
topicalThemeMarked<-data.table(TokenOne=rep(c("."), NoOfRules), TokenTwo=rep(c("."), NoOfRules), TokenThree=rep(c("."), NoOfRules), TokenFour=rep(c("."), NoOfRules), TokenFive=rep(c("."), NoOfRules))

POSsource<-POSresult[,7]



#Approach 3, function "keywords_phrases" from UDPIPE
#Generate AccMatchRuleinPOS, the info of marked themes
for (ruleCounter in 1:NoOfRules){
  if ((get(paste0("ruleType",ruleCounter))=="1") ||
    (get(paste0("ruleType",ruleCounter))=="3")) {  ########### obtain info of marked and unmarked themes for whole text, ruleType 1 and 3 in POS sequence
  MatchRuleinPOS <-keywords_phrases(POSresult$POS, POSresult$token, pattern = get(paste0("rule",ruleCounter)), ngram_max = as.integer(nchar(get(paste0("rule",ruleCounter))) * ngram_maxRatio) )}

  if ((get(paste0("ruleType",ruleCounter))=="2") ||
      (get(paste0("ruleType",ruleCounter))=="4")) {  ########### obtain info of marked and unmarked themes for whole text, ruleType 2 and 4 in RegEx
  MatchRuleinPOS <-keywords_phrases(POSresult$OneLetterPOS, POSresult$token, pattern = get(paste0("rule",ruleCounter)), ngram_max = as.integer(nchar(get(paste0("rule",ruleCounter))) * ngram_maxRatio), is_regex = TRUE) }

    
  NoInEachMatchRuleinPOS <- nrow(MatchRuleinPOS)

  if(NoInEachMatchRuleinPOS != 0){


  MatchRuleinPOS<-cbind(MatchRuleinPOS, matchedRule=rep(paste0("rule",ruleCounter),nrow(MatchRuleinPOS)))
  MatchRuleinPOS<-cbind(MatchRuleinPOS, ruleType=rep(paste0("ruleType",(get(paste0("ruleType",ruleCounter))) ),nrow(MatchRuleinPOS)))
  for (counterForMatchRuleinPOS in 1:NoInEachMatchRuleinPOS){
    if ( identical(grep(gsub('^..|..$','',MatchRuleinPOS$keyword[counterForMatchRuleinPOS]),textualExp[,]), integer(0)) 
       && identical(grep(gsub('^..|..$','',MatchRuleinPOS$keyword[counterForMatchRuleinPOS]),InterpAdju[,]), integer(0))  ){ # remove a specific row of MatchRuleinPOS if it is actually a textual or interpersonal element. This removal can avoid that element is repeatedly displayed in both textual/interpersonal and marked/unmarked theme, e.g. "In other words", "For me".  Also, gsub('^..|..$','',MatchRuleinPOS$keyword[counterForMatchRuleinPOS]) is to ignore the first & second characters as well as the last & second last ones, e.g. the "." and "," in ". For me ,"

      
          AccMatchRuleinPOS<-rbind(AccMatchRuleinPOS,MatchRuleinPOS[counterForMatchRuleinPOS,])  # add only the row of MatchRuleinPOS which does not a textual or interpersonal element.
    }
  }
  }
}


if (is.null(nrow(AccMatchRuleinPOS))==FALSE){  # run distinct if any matched item found
AccMatchRuleinPOS<-AccMatchRuleinPOS %>% distinct(start, .keep_all=TRUE)
AccMatchRuleinPOS<-AccMatchRuleinPOS[order(AccMatchRuleinPOS$start),]
}


# MAIN PROGRAM
#Compile the Analysis of THEME,MOOD,PROCESS
indexT<-0
theVerb<-NULL
tmp<-NULL
tmp2<-NULL
tmp3<-NULL

############### initialising the global variables of function of textualTheme() and textual count
tmp4<-NULL
tmp4vec<-NULL
NoOfTextual<-0
tmp4RowNoBfDistinct<-0
tmp4RowNoAfterDistinct<-0


######## initialising the global variables of function of interpersonalMOODtype()
tmp6<-NULL
tmp7<-NULL
tmp8<-NULL
tmp9<-NULL
hasBound<-"N"

################# initialising the global variables of function of interpersonalAdjunct(), i.e. tmp5a. And interpersonal count.
tmp5a<-NULL
tmp5avec<-NULL
NoOfInterpersonal<-0


######### initialising the global variables of function of unmarkedThemeUponMOOD(), i.e. tmp5 and tmp3UsedIn_unmarkedThemeUponMOOD
######### initialising the flag of tmp3 used upon ruleType<-3 in main program.
tmp5<-NULL
tmp3UsedIn_unmarkedThemeUponMOOD <- 0
tmp3UsedIn_unmarkedThemeUponRuleType <- 0

######### initialising the flag of tmp used upon ruleType<-2 (long marked theme) in main program.
tmpUsedInLongThemeBy_ruleType <-0

######### initialising the global variables of function of longThemeByRule() and the flag of tmp3 used upon ruleType<-4 in main program.
tmp3UsedInLongThemeBy_ruleType <- 0

## initialising the global variables of function of experientialPROCESStype()
tmp10<-NULL
tmp11<-NULL
tmp12<-NULL
tmp13<-NULL
tmp14<-NULL
tmp15<-NULL
InVerbsAsProcess <- "N"
##### initialising the global variables of the function experientialPROCESStypeViaSynonyms(), experientialPROCESStypeViaQdapSynonyms(), experientialPROCESStypeViaLevinSynonyms(), experientialPROCESStypeViaCustomVerbAsProcess(), and LSAverbsAsProcessSimilarity()
InCustomVerbsAsProcess <- "N"
ViaSynonyms <- "N"
ViaQdapSynonyms <- "N"
ViaLevinSynonyms <- "N"
ViaPolysemousVerbsAsProcess <- "N"
tmp10Counter<-0
tmp11Counter<-0
tmp12Counter<-0
tmp13Counter<-0
tmp14Counter<-0
tmp15Counter<-0
tmp10to15Counter<-0

AccMatchRuleCounter<-1
themeIn_AccMatchRuleinPOS<-0
### if only thematic, no MOOD, no PROCESS
### thematicTable<-data.table(Item=integer(),Textual_Theme=character(),Interpersonal_Theme=character(),Topical_Theme_Marked=character(),Topical_Theme_Unmarked=character(), Clause=character(), stringAsFactors=FALSE)
thematicTable<-data.table(Item=integer(),Textual_Theme=character(),Interpersonal_Theme=character(),Topical_Theme_Marked=character(),Topical_Theme_Unmarked=character(), Clause=character(), MOOD_declarative=character(), MOOD_interrogative=character(), MOOD_imperative=character(), MOOD_bound_OR_complex=character(), PROCESS_material=character(), PROCESS_behavioural=character(), PROCESS_mental=character(), PROCESS_verbal=character(), PROCESS_relational=character(), PROCESS_existential=character(), The_Verb_as_PROCESS=character(), Lookup_Verbs_as_PROCESS_CMIMM_2018=character(), Lookup_Verb_Classes_Levin_1993=character(), Lookup_via_WordNet_Synonyms=character(), Lookup_via_Collins_Qdap_Synonyms=character(), Lookup_via_Custom_Verbs_as_PROCESS=character(), Lookup_via_LSA=character(), No_of_Textual=integer(), No_of_Interpersonal=integer(), No_of_Bound_or_Complex=integer(), Article_Title=character(), Data_Subject=character(), Date=integer(),  Genre=character(), Co_Org_Corp=character(), From_which_Country_or_City=character() )
for (i in 2:max(POSresult$sentence)){   #start from 2 as the 1st line is supposed to be headline, i.e. 1st line-title; 2nd line-media type; 3rd line-date; 4th line-source location; 5th line-web link

## initialising glabal variables of loops and function calls  
    topicalUnmarkedThemeStop <- 0
    stopFindNextVerbsAsProcess <- 0
    indexStrtOfSrchNextVerb <- 0
    diffPositionToNextVerb <- 0
    themeIn_AccMatchRuleinPOS <- 0

    themeLenEnd <- 0

# Detect topical marked and unmarked themes based on rule?? and themeLen??
      for (j in 1:length(subset(POSresult$id, POSresult$sentence==i))){
      indexT<-which(POSresult$sentence==i & POSresult$id==j)
      
      
      if (is.na(AccMatchRuleinPOS$start[AccMatchRuleCounter]) == FALSE && is.null(nrow(AccMatchRuleinPOS))==FALSE){

      if (get(paste0("ruleType", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+"))))==4 ||
          get(paste0("ruleType", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+"))))==2){
        themeLenEnd <- AccMatchRuleinPOS$ngram[AccMatchRuleCounter] -1 - get(paste0("themeEndToRuleEnd", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+")))) }
        else {
        themeLenEnd <- get(paste0("themeLen", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+"))))}    

      if (((indexT -1) >= AccMatchRuleinPOS$start[AccMatchRuleCounter]) && (indexT <= (AccMatchRuleinPOS$start[AccMatchRuleCounter] + themeLenEnd ) )){
          

########## identify topical marked themes (i.e. ruleType<-1) based on rule?? and themeLen??
      if (get(paste0("ruleType", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+"))))==1){
         tmp<-paste(tmp, POSresult[indexT,3], collapse = " ")}
########## identify topical unmarked themes (i.e. ruleType<-3) based on rule?? and themeLen??
      if (get(paste0("ruleType", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+"))))==3){
         tmp3<-paste(tmp3, POSresult[indexT,3], collapse = " ")
         tmp3UsedIn_unmarkedThemeUponRuleType <- 1 }
########## identify topical marked long themes (i.e. ruleType<-2) based on rule?? and themeEndToRuleEnd?
      if (get(paste0("ruleType", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+"))))==2){
        tmp<-paste(tmp, POSresult[indexT,3], collapse = " ")
        tmpUsedInLongThemeBy_ruleType <- 1
        indexStrtOfSrchNextVerb <- AccMatchRuleinPOS$end[AccMatchRuleCounter] +1 - get(paste0("themeEndToRuleEnd", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+")))) }  # Start position of searching a verb after an unmarked Theme upon ruleType
########## identify topical unmarked long themes (i.e. ruleType<-4) based on rule?? and themeEndToRuleEnd??
      if (get(paste0("ruleType", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+"))))==4){
        tmp3<-paste(tmp3, POSresult[indexT,3], collapse = " ")
        tmp3UsedInLongThemeBy_ruleType <- 1

        indexStrtOfSrchNextVerb <- AccMatchRuleinPOS$end[AccMatchRuleCounter] +1 - get(paste0("themeEndToRuleEnd", as.numeric(str_extract(AccMatchRuleinPOS$matchedRule[AccMatchRuleCounter], "[0-9]+")))) }  # Start position of searching a verb after an unmarked Theme upon ruleType
        
      }
      }

      if (is.na(AccMatchRuleinPOS$end[AccMatchRuleCounter]) == FALSE && is.null(nrow(AccMatchRuleinPOS))==FALSE){
      if (indexT==AccMatchRuleinPOS$end[AccMatchRuleCounter]){AccMatchRuleCounter<-AccMatchRuleCounter+1
        themeIn_AccMatchRuleinPOS<-1 }
      }
      
# Detect topical unmarked themes
      ########## only when no marked theme identified && no unmarked theme of tmp3 identified in any ruleType<-3 && ruleType<-4, then identify the subject in full clause as the topical unmarked theme until verb token reached.
      if ((is.null(tmp) == TRUE) && (tmp3UsedIn_unmarkedThemeUponRuleType==0) && (tmp3UsedInLongThemeBy_ruleType==0)){
      
      if (POSresult$id >= 1 && topicalUnmarkedThemeStop == 0){
      ######### to stop using the value of tmp3 in default codes for unmarked theme identification
      ######### only combine words of [indexT] into an unmarked theme, if no tmp3 resulted in unmarkedThemeUponMOOD(), i.e. tmp3UsedIn_unmarkedThemeUponMOOD==0, otherwise, use the resulted tmp3 in unmarkedThemeUponMOOD()    
      if (tmp3UsedIn_unmarkedThemeUponMOOD==0 && !((POSresult$POS[indexT] == "VB") || (POSresult$POS[indexT] == "VBD") || (POSresult$POS[indexT] == "VBP") || (POSresult$POS[indexT] == "MD") || (POSresult$POS[indexT] == "VBZ"))){
#        tmp3<-paste(tmp3, POSresult[indexT,3], "TEST",collapse = " ")}
        tmp3<-paste(tmp3, POSresult[indexT,3],collapse = " ")}
        else {topicalUnmarkedThemeStop <- 1}

      }
      }
      tmp2<-paste(tmp2, POSresult[indexT,3], collapse = " ")  #The full clause

##Call function of experientialPROCESStype() starting at here

      ############ identify the predicator (the next verb) as PROCESS after the long Theme by rule?? matching.
      if ((!is.na(POSresult$POS[indexT+1])) && (stopFindNextVerbsAsProcess == 0) && (tmp3UsedInLongThemeBy_ruleType==1 || tmpUsedInLongThemeBy_ruleType==1) && (!is.na(POSresult$POS[indexStrtOfSrchNextVerb+1+diffPositionToNextVerb])) ){
############## Skip the finite to non-finite verb for verbs as PROCESS identification, e.g That the food might not be fresh (did n't occur) to them., and so on.
        if (!(( POSresult$token[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "might" || POSresult$token[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "let" ||
                POSresult$token[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "do" || POSresult$token[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "does" || POSresult$token[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "did")
                && hasVerbInRemainingClause(indexStrtOfSrchNextVerb+diffPositionToNextVerb)==TRUE) ){ 

        if (!((POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb]=="VBZ" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb]=="VBP") && (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb+1]=="RB" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb+1]=="RBS"  || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb+1]=="RBR") && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb+2]=="VBN") ){  # "... (is further developed)..." Should identify the "develope" instead of "is".
            
        if (!( (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1]=="VBZ" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1]=="VBD" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1]=="VBP") && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb]=="VBN" && hasRelativePronounInClauseBfwordPosition(indexStrtOfSrchNextVerb+diffPositionToNextVerb-2)==TRUE)){  # ABC, (which) (is operated) by him, run it in great success. Also, (who) (have developed).
              
        if (!( (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-2]=="VBZ" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-2]=="VBD" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-2]=="VBP") && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1]=="VBG" && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb]=="VBN" && hasRelativePronounInClauseBfwordPosition(indexStrtOfSrchNextVerb+diffPositionToNextVerb-3)==TRUE)){  # ABC, (which) (is being operated)
                
        if (!( (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-2]=="VBZ" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-2]=="VBD" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-2]=="VBP") && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1]=="VBN" && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb]=="VBN" && hasRelativePronounInClauseBfwordPosition(indexStrtOfSrchNextVerb+diffPositionToNextVerb-3)==TRUE)){  # ABC, (which) (have been operated)
                  
        if (!( (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1]=="VBZ" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1]=="VBD") && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb]=="VB" && hasRelativePronounInClauseBfwordPosition(indexStrtOfSrchNextVerb+diffPositionToNextVerb-2)==TRUE)){  # ABC, (who) (does show) they will have to do so.
          
        if (!( (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-2]=="WRB" || hasRelativePronounInClauseBfwordPosition(indexStrtOfSrchNextVerb+diffPositionToNextVerb-2)==TRUE) && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1]=="EX" && (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb]=="VBP" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb]=="VBZ")) ){  # ABC, ... (where) (there) (is/are)...
                    
        if (!((POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1] == "NNS" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1] == "NN" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1] == "NNP" || POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1] == "NNPS") && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VBG" && hasVerbInRemainingClause(indexStrtOfSrchNextVerb+diffPositionToNextVerb+1)==TRUE) ){  # As of Sunday, 25,782 migrant (workers living / NNS VBG) in dormitories, or nearly 8 per cent, have (tested) positive for Covid-19. Also, (NN || NNP || NNPS) && VBG as well. Also, hasVerbInRemainingClause(indexStrtOfSrchNextVerb+diffPositionToNextVerb+1)==TRUE, noit (indexStrtOfSrchNextVerb+diffPositionToNextVerb+2), which is to avoid to count the verb in next clause if the VBG is the last word, e.g. How is the project (going)?
        
        if (!(hasVerbInPhraseBfWordPosition(indexStrtOfSrchNextVerb+diffPositionToNextVerb-1,"finite")=="No verb in this phrase before current wordPosition." &&
                POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb-1] == "TO" && POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VB" && hasVerbInRemainingPhrase(indexStrtOfSrchNextVerb+diffPositionToNextVerb,"finite")==TRUE && hasRelativePronounInRemainingClause(indexStrtOfSrchNextVerb+diffPositionToNextVerb)==FALSE ) ) { # e.g. As far as the ability (to carry) electrity (is concerned). 
                    
        if (!((POSresult$POS[indexStrtOfSrchNextVerb+1+diffPositionToNextVerb] == "VBN") || (POSresult$POS[indexStrtOfSrchNextVerb+1+diffPositionToNextVerb] == "VBG") || ((POSresult$POS[indexStrtOfSrchNextVerb+1+diffPositionToNextVerb] == "RB") && (POSresult$POS[indexStrtOfSrchNextVerb+2+diffPositionToNextVerb] == "VBN")) || ((POSresult$POS[indexStrtOfSrchNextVerb-1+diffPositionToNextVerb] == ",") && (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VBG")) || ((POSresult$POS[indexStrtOfSrchNextVerb-1+diffPositionToNextVerb] == "IN") && (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VBG")) || ((POSresult$POS[indexStrtOfSrchNextVerb+1+diffPositionToNextVerb] == "TO") && (POSresult$POS[indexStrtOfSrchNextVerb+2+diffPositionToNextVerb] == "VB")))){   ############ after the long theme, identify next verb/predicator, and shift the check of proper one by the most right of Verb POS tag, e.g. VBP RB VBN RB VBN (have also been widely shared) || "," "VBG" (, including DBS) || "IN" "VBG" (by building management) || "TO" for 2nd verbal group is the relevant one for PROCESS type
          if ((POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VBZ") || (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VBD") || (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VBP") || (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VB") || (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VBG") || (POSresult$POS[indexStrtOfSrchNextVerb+diffPositionToNextVerb] == "VBN")){
            experientialPROCESStypeViaCustomVerbAsProcess(indexStrtOfSrchNextVerb+diffPositionToNextVerb) #######Call function of experientialPROCESStypeViaCustomVerbAsProcess() to check PROCESS type of that a specific POSresult$token or POSresult$lemma in verb tag after long Theme
            if (InCustomVerbsAsProcess=="N"){experientialPROCESStype(indexStrtOfSrchNextVerb+diffPositionToNextVerb)}  
            if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N"){experientialPROCESStypeViaQdapSynonyms(indexStrtOfSrchNextVerb+diffPositionToNextVerb)}  ###############Call function of experientialPROCESStypeViaQdapSynonyms() to check each synonyms of that POSresult$lemma in verb tag after long Theme
            if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N"){experientialPROCESStypeViaSynonyms(indexStrtOfSrchNextVerb+diffPositionToNextVerb)}  #####Call function of experientialPROCESStypeViaSynonyms() to check each synonyms of that POSresult$lemma in verb tag after long Theme
            if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N" && ViaQdapSynonyms=="N" && ViaSynonyms=="N"){experientialPROCESStypeViaLevinSynonyms(indexStrtOfSrchNextVerb+diffPositionToNextVerb)}  ######Call function of experientialPROCESStypeViaLevinSynonyms() to check each synonyms of that POSresult$lemma in verb tag after long Theme

            tmp10to15Counter <- c(tmp10Counter, tmp11Counter, tmp12Counter, tmp13Counter, tmp14Counter, tmp15Counter)
            if ( ((InCustomVerbsAsProcess=="Y" || InVerbsAsProcess=="Y") && length(which(c(tmp10,tmp11,tmp12,tmp13,tmp14,tmp15)==c("/")))!=5) ||
                 (any(tmp10to15Counter!=c(0,0,0,0,0,0)) && (length(which(tmp10to15Counter==max(tmp10to15Counter)))>1)) ||
                 (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N" && ViaQdapSynonyms=="N" && ViaSynonyms=="N" && ViaLevinSynonyms=="N") ) {LSAverbsAsProcessSimilarity(indexStrtOfSrchNextVerb+diffPositionToNextVerb)}
            
            stopFindNextVerbsAsProcess <- 1
          } }
        } }}}}}}}
        }
      }
      

      ############## avoid to find a verb that is actually belonged to the next sentence, e.g. in "(To see him all by himself like that) really upset them. To work from home...", if the "upset" was wrongly tagged as JJ instaed of VBP, R will find the next verb of "work" in next sentence. But "," should continue the verb searching.
      if (indexStrtOfSrchNextVerb!=0 ){


      if ( (indexStrtOfSrchNextVerb+diffPositionToNextVerb) >= 
           (as.numeric(rownames(subset(POSresult, POSresult$sentence==i & POSresult$id==1)) ) + 
            nrow(subset(POSresult, POSresult$sentence==i)) -1) ){
        stopFindNextVerbsAsProcess <- 1}
        else {diffPositionToNextVerb <- diffPositionToNextVerb +1}
      }

      
      
      
      ## identify the predicator after the unmarked theme
      ##VBZ, VBD, VBZ VBN (is developed), VBZ/VBD VBN (is/was developed), VBP (are, have, do), VBZ/VBP/VBD VBN VBN (has/have/had been invented), MD VB (will do)
      if ((!is.na(POSresult$POS[indexT+1])) && (stopFindNextVerbsAsProcess == 0) && (tmp3UsedInLongThemeBy_ruleType==0) && tmpUsedInLongThemeBy_ruleType==0 ){

        if (!((POSresult$POS[indexT-1]=="." || POSresult$POS[indexT-1]=="``" || POSresult$POS[indexT-1]=="''" || POSresult$POS[indexT-1]=="!?" || POSresult$POS[indexT-1]=="?!" || POSresult$POS[indexT-1] == "-LRB-" || POSresult$POS[indexT-1] == "-RRB-") && 
              (POSresult$POS[indexT]=="WDT" || POSresult$POS[indexT]=="WRB" || POSresult$POS[indexT]=="WP") && hasVerbInRemainingClause(indexT+2)==TRUE) ){ #Skip "Why (did you) do...?" "How (did we) escape..?" or "What (different sounds) can you make...?"

        if (!((POSresult$POS[indexT-1]=="." || POSresult$POS[indexT-1]=="``" || POSresult$POS[indexT-1]=="''" || POSresult$POS[indexT-1]=="!?" || POSresult$POS[indexT-1]=="?!" || POSresult$POS[indexT-1]=="-LRB-" || POSresult$POS[indexT-1]=="-RRB-") && 
                (POSresult$POS[indexT]=="VBN" || POSresult$POS[indexT]=="VBG") && hasVerbInRemainingClause(indexT+1)==TRUE) ){ #Skip "(Known) as the Manufacturers Alliance for Global..." "(Getting) something good is..."
        
        if (!(POSresult$POS[indexT-1]=="VBG" && POSresult$POS[indexT]=="VBN" && hasVerbInPhraseBfWordPosition(indexT,"finite")=="No verb in this phrase before current wordPosition." && hasVerbInRemainingPhrase(indexT,"finite")=="No verb in this remaining phrase.") ){ #Skip "(Judging) by the manufacturing solutions currently (being designed) for Covid-19 vaccination, the best-case scenario for vaccine delivery ..."
            
        if (!((POSresult$POS[indexT]=="VBZ" || POSresult$POS[indexT]=="VBP") && (POSresult$POS[indexT+1]=="RB" || POSresult$POS[indexT+1]=="RBS"  || POSresult$POS[indexT+1]=="RBR") && POSresult$POS[indexT+2]=="VBN") ){  # "... (is further developed)..." Should identify the "develope" instead of "is".
        
        if (!( (POSresult$POS[indexT-1]=="VBZ" || POSresult$POS[indexT-1]=="VBD" || POSresult$POS[indexT-1]=="VBP") && POSresult$POS[indexT]=="VBN" && hasRelativePronounInClauseBfwordPosition(indexT-2)==TRUE)){  # ABC, (which) (is operated) by him, run it in great success. Also, (who) (have developed).
        
        if (!( (POSresult$POS[indexT-2]=="VBZ" || POSresult$POS[indexT-2]=="VBD" || POSresult$POS[indexT-2]=="VBP") && POSresult$POS[indexT-1]=="VBG" && POSresult$POS[indexT]=="VBN" && hasRelativePronounInClauseBfwordPosition(indexT-3)==TRUE)){  # ABC, (which) (is being operated)
              
        if (!( (POSresult$POS[indexT-2]=="VBZ" || POSresult$POS[indexT-2]=="VBD" || POSresult$POS[indexT-2]=="VBP") && POSresult$POS[indexT-1]=="VBN" && POSresult$POS[indexT]=="VBN" && hasRelativePronounInClauseBfwordPosition(indexT-3)==TRUE)){  # ABC, (which) (have been operated)
          
        if (!( (POSresult$POS[indexT-1]=="VBZ" || POSresult$POS[indexT-1]=="VBD") && POSresult$POS[indexT]=="VB" && hasRelativePronounInClauseBfwordPosition(indexT-2)==TRUE)){  # ABC, (who) (does show) they will have to do so.

        if (!( (POSresult$POS[indexT-2]=="WRB" || hasRelativePronounInClauseBfwordPosition(indexT-2)==TRUE) && POSresult$POS[indexT-1]=="EX" && (POSresult$POS[indexT]=="VBP" || POSresult$POS[indexT]=="VBZ")) ){  # ABC, ... (where) (there is/are)...
                                  
        if (!((POSresult$POS[indexT-1] == "NNS" || POSresult$POS[indexT-1] == "NN" || POSresult$POS[indexT-1] == "NNP" || POSresult$POS[indexT-1] == "NNPS") && POSresult$POS[indexT] == "VBG" && hasVerbInRemainingClause(indexT+1)==TRUE) ){  # As of Sunday, 25,782 migrant (workers living / NNS VBG) in dormitories, or nearly 8 per cent, have (tested) positive for Covid-19. Also, (NN || NNP || NNPS) && VBG as well. Also, hasVerbInRemainingClause(indexT+1)==TRUE, noit (indexT+2), which is to avoid to count the verb in next clause if the VBG is the last word, e.g. How is the project (going)?

        if (!((POSresult$POS[indexT-2] == "." || POSresult$POS[indexT-2] == "``" || POSresult$POS[indexT-2]=="''" || POSresult$POS[indexT-2]=="!?" || POSresult$POS[indexT-2]=="?!" || POSresult$POS[indexT-2] == "-LRB-" || POSresult$POS[indexT-2] == "-RRB-") &&
               POSresult$POS[indexT-1] == "TO" && POSresult$POS[indexT] == "VB" && hasVerbInRemainingPhrase(indexT,"finite")==TRUE && hasRelativePronounInRemainingClause(indexT)==FALSE ) ) { # e.g. (To forgive) is divine.
          
        if (!(hasVerbInPhraseBfWordPosition(indexT-1,"finite")=="No verb in this phrase before current wordPosition." &&
                POSresult$POS[indexT-1] == "TO" && POSresult$POS[indexT] == "VB" && hasVerbInRemainingPhrase(indexT,"finite")==TRUE && hasRelativePronounInRemainingClause(indexT)==FALSE ) ) { # e.g. As far as the ability (to carry) electrity (is concerned).
            
############## Skip the finite to non-finite verb for verbs as PROCESS identification, e.g How (did/do/does) we/he escape from the prison? || How (is/are/was/were) the project(s) going?
        if (!(((POSresult$token[indexT] == "did" || POSresult$token[indexT] == "do" || POSresult$token[indexT] == "does") && hasVerbInRemainingClause(indexT)==TRUE) || 
              ((POSresult$token[indexT] == "is" || POSresult$token[indexT] == "are" || POSresult$token[indexT] == "was" || POSresult$token[indexT] == "were" ) && hasVerbInRemainingClause(indexT)==TRUE && hasPresentParticipleInRemainingClause(indexT)==TRUE && hasRelativePronounInRemainingClause(indexT)==FALSE && hasConjunctionInRemainingClause(indexT)==FALSE) ||
              (POSresult$POS[indexT] == "VBN" && hasVerbInRemainingClause(indexT)==TRUE && hasPresentParticipleInRemainingClause(indexT)==FALSE && hasRelativePronounInRemainingClause(indexT)==FALSE && hasConjunctionInRemainingClause(indexT)==FALSE) )) {  # e.g. Nelson Mandela , the newly (elected) President of South Africa , (was invited). Also, hasPresentParticipleInRemainingClause(indexT)==FALSE is to avoid any unprocession of "The government has (cautioned) people against (shaking) hands as a form of (greeting)."

############# might be good to check the punctuation before the "Isn't",e.g. ". Isn't the book good?" vs "isn't the book good?", also case sensitive 
        if (!((POSresult$token[indexT] == "Is" || POSresult$token[indexT] == "Are" || POSresult$token[indexT] == "Were" || POSresult$token[indexT] == "Might" || POSresult$token[indexT] == "Let" || POSresult$token[indexT] == "Do" || POSresult$token[indexT] == "Does" || POSresult$token[indexT] == "Did" || POSresult$token[indexT] == "Have" || POSresult$token[indexT] == "Has" || POSresult$token[indexT] == "Had") && hasVerbInRemainingClause(indexT)==TRUE) ){ ############# Is there a mass being held at noon? getting the predicator "held" instead of "Is", so on.
         if (!((POSresult$POS[indexT+1] == "VBN") || (POSresult$POS[indexT+1] == "VBG") || ((POSresult$POS[indexT+1] == "RB") && (POSresult$POS[indexT+2] == "VBN")) || ((POSresult$token[indexT-1] == ",") && (POSresult$POS[indexT] == "VBG")) || ((POSresult$POS[indexT-1] == ".") && (POSresult$POS[indexT] == "VBG")) || ((POSresult$POS[indexT-2] == ".") && (POSresult$POS[indexT-1] == "``") && (POSresult$POS[indexT] == "VBG")) || ((POSresult$POS[indexT-1] == "CC") && (POSresult$POS[indexT] == "VBG")) || ((POSresult$POS[indexT-1] == "IN") && (POSresult$POS[indexT] == "VBG")) || ((POSresult$POS[indexT+1] == "TO") && (POSresult$POS[indexT+2] == "VB")) || ((POSresult$POS[indexT-1] == "VBP") && (POSresult$POS[indexT] == "VBN") && (POSresult$POS[indexT+1] == "VBZ")) || (POSresult$POS[indexT+1] == "VBZ") || (POSresult$POS[indexT+1] == "VBD")  )){   ##VBP RB VBN RB VBN (have also been widely shared) || "," "VBG" (, including DBS) || "." "VBG" (. Squeezing) || "." "``" "VBG" (Judging by the) || "CC" "VBG" (and eating the right food is...) || "IN" "VBG" (by building management) || "TO" for 2nd verbal group is the relevant one for PROCESS type || " ... that I (have developed represents) a position / ... IN PRP (VBP VBN VBZ) DT NN || What he (said is) nonsense. || What they (lacked was) a good plan.
           if ((POSresult$POS[indexT] == "VBZ") || (POSresult$POS[indexT] == "VBD") || (POSresult$POS[indexT] == "VBP") || (POSresult$POS[indexT] == "VB") || (POSresult$POS[indexT] == "VBG") || (POSresult$POS[indexT] == "VBN")){
             experientialPROCESStypeViaCustomVerbAsProcess(indexT) #######Call function of experientialPROCESStypeViaCustomVerbAsProcess() to check PROCESS type of that a specific POSresult$token[indexT] or POSresult$lemma[indexT]
             if (InCustomVerbsAsProcess=="N"){experientialPROCESStype(indexT)}  
             if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N"){experientialPROCESStypeViaQdapSynonyms(indexT)}  ###############Call function of experientialPROCESStypeViaQdapSynonyms() to check each synonyms of that POSresult$lemma[indexT]
             if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N"){experientialPROCESStypeViaSynonyms(indexT)}  #####Call function of experientialPROCESStypeViaSynonyms() to check each synonyms of that POSresult$lemma[indexT]
             if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N" && ViaQdapSynonyms=="N" && ViaSynonyms=="N"){experientialPROCESStypeViaLevinSynonyms(indexT)}  ######Call function of experientialPROCESStypeViaLevinSynonyms() to check each synonyms of that POSresult$lemma[indexT]
             tmp10to15Counter <- c(tmp10Counter, tmp11Counter, tmp12Counter, tmp13Counter, tmp14Counter, tmp15Counter)
             if ( ((InCustomVerbsAsProcess=="Y" || InVerbsAsProcess=="Y") && length(which(c(tmp10,tmp11,tmp12,tmp13,tmp14,tmp15)==c("/")))!=5) ||
                  (any(tmp10to15Counter!=c(0,0,0,0,0,0)) && (length(which(tmp10to15Counter==max(tmp10to15Counter)))>1)) ||
                  (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N" && ViaQdapSynonyms=="N" && ViaSynonyms=="N" && ViaLevinSynonyms=="N") ) {LSAverbsAsProcessSimilarity(indexT)}
             
         stopFindNextVerbsAsProcess <- 1
          }
        }
        }
        }
        }}}}
        }}}}
        }}}}
      }


      
      ##In case of the last lemma of the whole text is VBZ/VBD/VBP/VB
      if ((is.na(POSresult$POS[indexT+1])) && ((POSresult$POS[indexT] == "VBZ") || (POSresult$POS[indexT] == "VBD") || (POSresult$POS[indexT] == "VBP") || (POSresult$POS[indexT] == "VB") || (POSresult$POS[indexT] == "VBG") || (POSresult$POS[indexT] == "VBN"))  ){
        experientialPROCESStypeViaCustomVerbAsProcess(indexT)   #######Call function of experientialPROCESStypeViaCustomVerbAsProcess() to check PROCESS type of that a specific POSresult$token[indexT] or POSresult$lemma[indexT]
        if (InCustomVerbsAsProcess=="N"){experientialPROCESStype(indexT)}
        if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N"){experientialPROCESStypeViaQdapSynonyms(indexT)}  ###############Call function of experientialPROCESStypeViaQdapSynonyms() to check each synonyms of that POSresult$lemma[indexT]
        if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N"){experientialPROCESStypeViaSynonyms(indexT)}  #####Call function of experientialPROCESStypeViaSynonyms() to check each synonyms of that POSresult$lemma[indexT]
        if (InVerbsAsProcess=="N" && InCustomVerbsAsProcess=="N" && ViaQdapSynonyms=="N" && ViaSynonyms=="N"){experientialPROCESStypeViaLevinSynonyms(indexT)}  ######Call function of experientialPROCESStypeViaLevinSynonyms() to check each synonyms of that POSresult$lemma[indexT]
        
      }
##Call function of experientialPROCESStype() ending at here


      interpersonalMOOD(indexT)
      
      unmarkedThemeUponMOOD(indexT)
      
    }

    textualTheme(i)
    if (!(tmp4=="/")){
      tmp4<-tmp4 %>% distinct(start, .keep_all=TRUE)
      tmp4RowNoBfDistinct<-nrow(tmp4) # row number before distinct(start and distinct(end
      
      tmp4vec<-tmp4[order(tmp4$start),] # as get tmp4vec before distinct(end, the nrow(tmp4vec) might get larger than needed, but it can ensure the loop of clause complex iteration goes throught all textual expressions identified.
      
      tmp4<-tmp4[order(tmp4$ngram, decreasing = TRUE),] %>% distinct(end, .keep_all = TRUE) # e.g. always keep the one with longer ngram, e.g. remove the "which" from "for which" textual process (also, for topical too) 
#      > tmp4vec[order(tmp4vec$ngram, decreasing = TRUE),]
#      keyword ngram   pattern start end
#      2 for which     2 for which     9  10
#      1     which     1     which    10  10
#      > tmp4vec[order(tmp4vec$ngram, decreasing = TRUE),] %>% distinct(end, .keep_all = TRUE)
#      keyword ngram   pattern start end
#      1 for which     2 for which     9  10      

      tmp4RowNoAfterDistinct<-nrow(tmp4) # row number after distinct(start and distinct(end
      
      tmp4<-tmp4[order(tmp4$start),]  # After removal duplicate one, sort by $start again in order to display a correct order in tmp4
      tmp4<-paste(tmp4$keyword[order(tmp4$start)], collapse = " ") # get the tmp4$keyword vector according to the order of tmp4$start.
      
      
      if (tmp3UsedIn_unmarkedThemeUponMOOD==0){  #################  remove the identified textual items from non-rule-based unmarked theme
      tmp3<-paste(gsub(",", "",  setdiff(scan(text=tmp3, sep=" ", what="", quiet=TRUE),    scan(text=tmp4, sep=" ", what="", quiet=TRUE)) ), collapse = " ") 
      }
    }
    
    
        
    interpersonalAdjunct(i)
    if (!(tmp5a=="/")){
      tmp5a<-tmp5a %>% distinct(start, .keep_all=TRUE)
      tmp5avec<-tmp5a[order(tmp5a$start),]
      tmp5a<-paste(tmp5a$keyword[order(tmp5a$start)], collapse = ", ") # get the tmp5a$keyword vector according to tmp5a$start
      if (!(tmp5=="/")){
        tmp5<-paste(tmp5,",", tmp5a)

      }
      else {tmp5<-tmp5a}


      if (tmp3UsedIn_unmarkedThemeUponMOOD==0){  #################  remove the identified textual items from non-rule-based unmarked theme
################# remove the identified interpersonal items from no-rule-based unmarked theme
      tmp3<-paste(gsub(",", "",  setdiff(scan(text=tmp3, sep=" ", what="", quiet=TRUE),    scan(text=tmp5, sep=" ", what="", quiet=TRUE)) ), collapse = " ") 
      }
    }

    if (is.null(tmp)==TRUE){tmp<-"/"}
    if (is.null(tmp3)==TRUE){tmp3<-"/"}
    if (is.null(tmp2)==TRUE){tmp2<-"/"}

    if (debug_log_enabled==1) {debug_log <- rbind(debug_log, (paste("tmp ",tmp))) }
    if (debug_log_enabled==1) {debug_log <- rbind(debug_log, (paste("tmp3 ", tmp3))) }
    if (themeIn_AccMatchRuleinPOS==1){if (debug_log_enabled==1) {debug_log <- rbind(debug_log, (paste("AccMatchRuleCounter, i.e. index row in AccMatcdRuleinPOS table= ", AccMatchRuleCounter-1)))}  } # do not show this line when theme is not in AccMatchRuleinPOS, i.e. themeIn_AccMatchRuleinPOS==0
    
    if (debug_log_enabled==1) {debug_log <- rbind(debug_log, (paste("tmp2 ", tmp2))) }
    if (debug_log_enabled==1) {debug_log <- rbind(debug_log, (paste('This sentence end with "', POSresult$token[indexT],'" at indexT ', indexT))) }
    
    
#####in the PROCESS types achieved via synonyms, only the max count(s) of them will be adopted. If double or triple max in same value, all of them will be used.    
    if (ViaSynonyms=="Y" || ViaQdapSynonyms=="Y" || ViaLevinSynonyms=="Y"){
      if (tmp10Counter!=max(tmp10Counter,tmp11Counter,tmp12Counter,tmp13Counter,tmp14Counter,tmp15Counter)){tmp10 <- NULL}
      if (tmp11Counter!=max(tmp10Counter,tmp11Counter,tmp12Counter,tmp13Counter,tmp14Counter,tmp15Counter)){tmp11 <- NULL}
      if (tmp12Counter!=max(tmp10Counter,tmp11Counter,tmp12Counter,tmp13Counter,tmp14Counter,tmp15Counter)){tmp12 <- NULL}
      if (tmp13Counter!=max(tmp10Counter,tmp11Counter,tmp12Counter,tmp13Counter,tmp14Counter,tmp15Counter)){tmp13 <- NULL}
      if (tmp14Counter!=max(tmp10Counter,tmp11Counter,tmp12Counter,tmp13Counter,tmp14Counter,tmp15Counter)){tmp14 <- NULL}
      if (tmp15Counter!=max(tmp10Counter,tmp11Counter,tmp12Counter,tmp13Counter,tmp14Counter,tmp15Counter)){tmp15 <- NULL}
    }

####################### if the clause has the word of "there", assign tmp15 <- "existential".    
      if (hasWordThereNextToVerbInPhrase( min(which(POSresult$sentence==i)) ) ==TRUE){  # min(which(POSresult$sentence==i)) is to get the start of index of each clause.
      tmp10 <- NULL
      tmp11 <- NULL
      tmp12 <- NULL
      tmp13 <- NULL
      tmp14 <- NULL
      tmp15 <- "existential"
    }    
    
##Set NULL to "/", in case the sequence of POS tags of verb groups above did not trigger the any call of experientialPROCESStype()
    if (is.null(tmp10)==TRUE){tmp10<-"/"}
    if (is.null(tmp11)==TRUE){tmp11<-"/"}
    if (is.null(tmp12)==TRUE){tmp12<-"/"}
    if (is.null(tmp13)==TRUE){tmp13<-"/"}
    if (is.null(tmp14)==TRUE){tmp14<-"/"}
    if (is.null(tmp15)==TRUE){tmp15<-"/"}

########
######## No need to set NULL to "/" for interpersonalMOOD(), as it always triggers declarative (tmp6) if tmp7 or tmp8 is NULL.
########
#########         Set NULL to "/", in case the sequence of POS tags in unmarkedThemeUponMOOD() did not trigger any tmp5 result
################# Set NULL to "/", in case the sequence of POS tags in interpersonalAdjunct() did not trigger any tmp5a result
    if ((is.null(tmp5)==TRUE) && (is.null(tmp5a)==TRUE) ){tmp5<-"/"}

############### Set NULL to "/", in case the sequence of POS tags in textualExp() did not trigger any tmp4 result
    if (is.null(tmp4)==TRUE){tmp4<-"/"} 

#########################  (Move from below. Should reinstate if further proceeding verbsAsProcess identification within clause complex)  
#########################  loop to process "Main" within clause complex iteratively upon textual expressions identified.
    phraseEndOfComplexClause<-0
    NoOfComplexes<-0
    NoOfTextual<-nrow(tmp4vec)
    if (is.null(NoOfTextual)==FALSE){
      for ( NoOfChunkInClauseComplex in 1:NoOfTextual ){
#        if (tmp4vec$keyword[NoOfChunkInClauseComplex]!="and"){
          if (tmp4vec$keyword[NoOfChunkInClauseComplex]=="as far as" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="as soon as" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="until" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="once" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="while" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="whereas" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="when" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="as" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="since" 
              || tmp4vec$keyword[NoOfChunkInClauseComplex]=="before" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="after" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="if" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="although" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="because" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="that" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="whether" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="whilst"
              || tmp4vec$keyword[NoOfChunkInClauseComplex]=="who" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="whom" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="which" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="whoever" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="whomever" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="whichever" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="whatever" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="where" || tmp4vec$keyword[NoOfChunkInClauseComplex]=="how" ) {
            indexCStart <- tmp4vec$start[NoOfChunkInClauseComplex] + as.numeric(rownames(subset(POSresult, POSresult$sentence==i & POSresult$id==1)))  # id of "$start" in "keyword ngram pattern start end" + 1st indexT of POSresult$sentence==i
            
            if (NoOfTextual==1) { # if the keywords_phrases table only has 1 row
              indexCEnd <- nrow(subset(POSresult, POSresult$sentence==i)) -2 + as.numeric(rownames(subset(POSresult, POSresult$sentence==i & POSresult$id==1))) }

            if ( NoOfChunkInClauseComplex>=1 && NoOfChunkInClauseComplex < NoOfTextual ){
              indexCEnd <- tmp4vec$start[NoOfChunkInClauseComplex + 1] -2 + as.numeric(rownames(subset(POSresult, POSresult$sentence==i & POSresult$id==1))) }

            if ( NoOfChunkInClauseComplex == NoOfTextual ){ # for the last row of the keywords_phrases table
              indexCEnd <- nrow(subset(POSresult, POSresult$sentence==i)) -2 + as.numeric(rownames(subset(POSresult, POSresult$sentence==i & POSresult$id==1))) } # -2 is due to avoid the process at the clause end ("." "?" "!" etc.).

            for (jj in (indexCStart-1):indexCEnd){
              ####        MainVerbInUnmarkedTheme(jj)
              if (is.na(POSresult$UDPOS[jj])==FALSE){   # added, deal to the special character, likely present in question mark in some text coding format, e.g. ". (soft return) ?Establish a better society."
                
              if ((POSresult$UDPOS[jj]!="." || hasVerbInRemainingClause(jj)==FALSE) && phraseEndOfComplexClause==0) 
                {tmp9<-paste(tmp9, POSresult$token[jj])}
              else{phraseEndOfComplexClause<-1
                }
              }
            }
            
            NoOfComplexes<-NoOfComplexes+1
            
            ####      print(paste(i-1,tmp4,tmp5,tmp,tmp3,tmp2,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11,tmp12,tmp13,tmp14,tmp15,InVerbsAsProcess,ViaLevinSynonyms,ViaSynonyms, ViaQdapSynonyms, InCustomVerbsAsProcess, ViaPolysemousVerbsAsProcess))
            ####      MainWriteRowResult(jj)
            if (NoOfChunkInClauseComplex>=1 && (NoOfChunkInClauseComplex < NoOfTextual) &&
                tmp4vec$keyword[NoOfChunkInClauseComplex]!="and") {tmp9<-paste0(tmp9, "...")}            
          }
#        }

        phraseEndOfComplexClause<-0
      }
    }
#########################  end of the loop to process "Main" within clause complex iteratively upon textual expressions identified.
#########################  (Move from below. Should reinstate if further proceeding verbsAsProcess identification within clause complex)  
   
############### Set NULL to "/", if no verb in the clause
    if (is.null(theVerb)==TRUE){theVerb<-"/"}

        
############### Set NULL to "/", in case the sequence of POS tags in textualExp() did not trigger any tmp9 result
    if (is.null(tmp9)==TRUE){tmp9<-"/"}
    
############### Set NULL to "0", in case the textual matching table of nrow(tmp4vec) did not trigger any NoOfTextual result
    if (is.null(NoOfTextual)==TRUE){NoOfTextual<-0}

########################## If not NULL. Correct the NoOfTextual by subtracting the number of lines removed in distinct(start and distinct(end
    NoOfTextual<-NoOfTextual-(tmp4RowNoBfDistinct - tmp4RowNoAfterDistinct)
    
############### No need to add, in case the interpersonal matching table of nrow(tmp5avec) did not have any matched interpersonal adjunct.
    if (is.null(nrow(tmp5avec))==FALSE)
      {NoOfInterpersonal<-NoOfInterpersonal + nrow(tmp5avec)}   # Porepare the sum of interpersonal adjunct and finite, e.g. (Will) you (please) wake up early tomorrow?
        
###############    tmpRow<-data.table(i-1,"-",tmp5,tmp,tmp3,tmp2,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11,tmp12,tmp13,tmp14,tmp15,InVerbsAsProcess,ViaLevinSynonyms,ViaSynonyms, ViaQdapSynonyms, InCustomVerbsAsProcess, ViaPolysemousVerbsAsProcess)
    tmpRow<-data.table(i-1,tmp4,tmp5,tmp,tmp3,tmp2,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11,tmp12,tmp13,tmp14,tmp15,theVerb,InVerbsAsProcess,ViaLevinSynonyms,ViaSynonyms, ViaQdapSynonyms, InCustomVerbsAsProcess, ViaPolysemousVerbsAsProcess, NoOfTextual, NoOfInterpersonal, NoOfComplexes, x[1], x[2], as.numeric(x[3]), x[4], x[5], x[6])
    names(tmpRow)<-c("Item","Textual_Theme","Interpersonal_Theme","Topical_Theme_Marked","Topical_Theme_Unmarked","Clause", "MOOD_declarative", "MOOD_interrogative", "MOOD_imperative", "MOOD_bound_OR_complex", "PROCESS_material", "PROCESS_behavioural", "PROCESS_mental", "PROCESS_verbal", "PROCESS_relational", "PROCESS_existential", "The_Verb_as_PROCESS", "Lookup_Verbs_as_PROCESS_CMIMM_2018", "Lookup_Verb_Classes_Levin_1993", "Lookup_via_WordNet_Synonyms", "Lookup_via_Collins_Qdap_Synonyms", "Lookup_via_Custom_Verbs_as_PROCESS", "Lookup_via_LSA", "No_of_Textual", "No_of_Interpersonal", "No_of_Bound_or_Complex", "Article_Title", "Data_Subject", "Date", "Genre", "Co_Org_Corp","From_which_Country_or_City")

    thematicTable<-rbindlist(list(thematicTable, tmpRow), fill=TRUE)

######################### loop to process "Main" within clause complex iteratively upon textual expressions identified.

######################### end of the loop to process "Main" within clause complex iteratively upon textual expressions identified.
    
    
    lastClauseForDependPlot<-tmp2

############### Resetting the global variables    
    theVerb<-NULL
    tmp<-NULL
    tmp2<-NULL
    tmp3<-NULL

############### Resetting the global variables of the function, textualTheme()
    tmp4<-NULL
    tmp4vec<-NULL
    tmp4RowNoBfDistinct<-0
    tmp4RowNoAfterDistinct<-0
    
################# Resetting the global variables of the function, interpersonalAdjunct()
    tmp5a<-NULL    
    tmp5avec<-NULL
    NoOfInterpersonal<-0
########Resetting the global variables of the function, unmarkedThemeUponMOOD() and the flag of tmp3 used upon ruleType<-3 in main program.
    tmp5<-NULL
    tmp3UsedIn_unmarkedThemeUponMOOD <- 0
    tmp3UsedIn_unmarkedThemeUponRuleType <- 0

###########Resetting the flag of tmp used upon ruleType<-2 (long marked theme) in main program.
    tmpUsedInLongThemeBy_ruleType <- 0
    
###########Resetting the global variables of the function, longThemeByRule(), i.e. the flag of tmp3 used upon ruleType<-4 in main program.
    tmp3UsedInLongThemeBy_ruleType <- 0
    
###########Resetting the global variable of the function experientialPROCESStype() and experientialPROCESStypeViaCustomVerbAsProcess()
    ngramMatched<-"N"
    
########Resetting the global variables of the function, interpersonalMOOD()
    tmp6<-NULL
    tmp7<-NULL
    tmp8<-NULL
    tmp9<-NULL
        
##Resetting the global variables of the function, experientialPROCESStype()
    tmp10<-NULL
    tmp11<-NULL
    tmp12<-NULL
    tmp13<-NULL
    tmp14<-NULL
    tmp15<-NULL
    InVerbsAsProcess <- "N"
#####Resetting the global variables of the function experientialPROCESStypeViaSynonyms(), experientialPROCESStypeViaQdapSynonyms(), experientialPROCESStypeViaLevinSynonyms() and LSAverbsAsProcessSimilarity()
    InCustomVerbsAsProcess <- "N"
    ViaSynonyms <- "N"
    ViaQdapSynonyms <- "N"
    ViaLevinSynonyms <- "N"
    ViaPolysemousVerbsAsProcess <- "N"
    tmp10Counter<-0
    tmp11Counter<-0
    tmp12Counter<-0
    tmp13Counter<-0
    tmp14Counter<-0
    tmp15Counter<-0
    tmp10to15Counter<-0
    

    if (debug_log_enabled==1) {debug_log <- rbind(debug_log, (paste("^  ^   ^   Analysis for this sentence is done!   ^   ^   ^"))) }
    if (debug_log_enabled==1) {debug_log <- rbind(debug_log, (paste("     "))) }
    
}

# ready for plotting the distribution of POS frequencies
for (i in 1:length(POStag)){
TagCounter[i]<-sum(POSresult[,7]==POStag[i])
}
TagCounter



# Dependency output for the current text file
Dependency<-getDependency(output)
getSentiment(output)
Parse<-getParse(output)



#################### aggregate result of every text source file
x<-cbind(x,filename)
colnames(x)[2]<-"FileName"
xAll<-rbind(xAll, x)
POSresult<-cbind(POSresult,filename)
colnames(POSresult)[12]<-"FileName"
POSresultAll<-rbind(POSresultAll, POSresult)
Dependency<-cbind(Dependency,filename)
colnames(Dependency)[9]<-"FileName"
DependencyAll<-rbind(DependencyAll, Dependency)
Parse<-cbind(Parse,filename)
colnames(Parse)[2]<-"FileName"
ParseAll<-rbind(ParseAll, Parse)
thematicTable<-cbind(thematicTable,filename)
colnames(thematicTable)[33]<-"FileName"
thematicTableAll<-rbind(thematicTableAll, thematicTable)
TagCounterAll<-TagCounterAll + TagCounter


} ################## End of looping all the text source files

# plotting the distribution of POS frequencies
POSNameTag<-paste(POSname," (",POStag,")")
POStagsetCounter<-data.frame(POSNameTag,TagCounterAll)
figure<-ggplot(POStagsetCounter,aes(POSNameTag,TagCounterAll)) + geom_pointrange(ymin=0,ymax=TagCounterAll) + coord_flip() +ggtitle("Accumlative distribution of POS tags among all source texts")


#Plot last sentence
library(igraph)
xUdpipe <- udpipe(lastClauseForDependPlot , "english")
figure2<-plot_annotation(xUdpipe, size = 4)

########################### Add web link if available. Also, set all NA in the metadata of source texts to "/"
k<-0
webLink4thematicTable<-NULL
webLink4thematicTableAll<-NULL

webLink<-scan(file=paste0(filePath,"tmp//","web_link.txt"), what=character(),sep="\n", quiet=TRUE, blank.lines.skip=TRUE)

for (k in 1:nrow(thematicTableAll)){
  
  webLink4thematicTable <- webLink[ as.numeric( tools::file_path_sans_ext(thematicTableAll$FileName[k]) ) ]
  webLink4thematicTableAll<-rbind(webLink4thematicTableAll, webLink4thematicTable)
}
thematicTableAll<-cbind(thematicTableAll,webLink4thematicTableAll)
colnames(thematicTableAll)[34]<-"WebLink"

thematicTableAll$Data_Subject[is.na(thematicTableAll$Data_Subject)]<-"/"
thematicTableAll$Date[is.na(thematicTableAll$Date)]<-"/"
thematicTableAll$Genre[is.na(thematicTableAll$Genre)]<-"/"
thematicTableAll$Co_Org_Corp[is.na(thematicTableAll$Co_Org_Corp)]<-"/"
thematicTableAll$From_which_Country_or_City[is.na(thematicTableAll$From_which_Country_or_City)]<-"/"
thematicTableAll$WebLink[is.na(thematicTableAll$WebLink)]<-"/"


############################ prepare final results and figures
splitMapLevinVerbClassesVsPROCESS<-levinVerbClasses
NoOfThematicTableAll<-0
matchedRowInSplitMap<-0
NoOfMatchedRowInSplitMap<-0
splitMapLevinVerbClassesVsPROCESS["PROCESS_material"]<-0
splitMapLevinVerbClassesVsPROCESS["PROCESS_behavioural"]<-0
splitMapLevinVerbClassesVsPROCESS["PROCESS_mental"]<-0
splitMapLevinVerbClassesVsPROCESS["PROCESS_verbal"]<-0
splitMapLevinVerbClassesVsPROCESS["PROCESS_relational"]<-0
splitMapLevinVerbClassesVsPROCESS["PROCESS_existential"]<-0



for (NoOfThematicTableAll in 1:nrow(thematicTableAll)) {
  
  matchedRowInSplitMap<- grep(paste0("\\b",noquote(tolower(lemmatize_words( thematicTableAll$The_Verb_as_PROCESS[NoOfThematicTableAll] ))),"\\b"), levinVerbClasses$List)
  
  if ( !(identical(matchedRowInSplitMap, integer(0))) ){
  for (NoOfMatchedRowInSplitMap in 1:length(matchedRowInSplitMap) ){
  
  if ( any(thematicTableAll[NoOfThematicTableAll, 11:16]==c("material","0","0","0","0","0")) )
    {splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9]<- splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9] + c(1,0,0,0,0,0)}
  if ( any(thematicTableAll[NoOfThematicTableAll, 11:16]==c("0","behavioural","0","0","0","0")) )
    {splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9]<- splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9] + c(0,1,0,0,0,0)}
  if ( any(thematicTableAll[NoOfThematicTableAll, 11:16]==c("0","0","mental","0","0","0")) )
    {splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9]<- splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9] + c(0,0,1,0,0,0)}
  if ( any(thematicTableAll[NoOfThematicTableAll, 11:16]==c("0","0","0","verbal","0","0")) )
    {splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9]<- splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9] + c(0,0,0,1,0,0)}
  if ( any(thematicTableAll[NoOfThematicTableAll, 11:16]==c("0","0","0","0","relational","0")) )
    {splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9]<- splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9] + c(0,0,0,0,1,0)}
  if ( any(thematicTableAll[NoOfThematicTableAll, 11:16]==c("0","0","0","0","0","existential")) )
    {splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9]<- splitMapLevinVerbClassesVsPROCESS[matchedRowInSplitMap[NoOfMatchedRowInSplitMap], 4:9] + c(0,0,0,0,0,1)}
  }  
}
}


############################# Reformat the splitMapLevinVerbClassesVsPROCESS to fit ggplot() 
splitMapLevinVerbClassesVsPROCESSreformat<-NULL
splitMapLevinVerbClassesVsPROCESSreformatTmp<-NULL
PROCESStypeInSplitMap<-NULL
for (levinIndex in 1:nrow(splitMapLevinVerbClassesVsPROCESS)){
  for (counterForPROCESS in 4:9){
    if (counterForPROCESS==4){PROCESStypeInSplitMap<-"material"}
    if (counterForPROCESS==5){PROCESStypeInSplitMap<-"behavioural"}
    if (counterForPROCESS==6){PROCESStypeInSplitMap<-"mental"}
    if (counterForPROCESS==7){PROCESStypeInSplitMap<-"verbal"}
    if (counterForPROCESS==8){PROCESStypeInSplitMap<-"relational"}
    if (counterForPROCESS==9){PROCESStypeInSplitMap<-"existential"}
    
    splitMapLevinVerbClassesVsPROCESSreformatTmp<-cbind(splitMapLevinVerbClassesVsPROCESS[levinIndex,1:3], PROCESStypeInSplitMap, splitMapLevinVerbClassesVsPROCESS[levinIndex, counterForPROCESS])
    splitMapLevinVerbClassesVsPROCESSreformat<-rbind(splitMapLevinVerbClassesVsPROCESSreformat, splitMapLevinVerbClassesVsPROCESSreformatTmp)
  }
  
}
colnames(splitMapLevinVerbClassesVsPROCESSreformat)[4]<-"PROCESS_TYPE"
colnames(splitMapLevinVerbClassesVsPROCESSreformat)[5]<-"No_of_Verbs_Identified"

############################# Reorder the factor order to avoid plotting 10.1 10.2 10.3... 11.1 11.2 11.3 ... etc. in ahead to 9.1 9.2 9.3, because 9 in alphanumeric order is later than 1x.x.
splitMapLevinVerbClassesVsPROCESSreformat$Section<-factor(splitMapLevinVerbClassesVsPROCESSreformat$Section, levels = unique(splitMapLevinVerbClassesVsPROCESSreformat$Section))


############################# Splits plots between material and relational
figure3<- ggplot(data=subset(splitMapLevinVerbClassesVsPROCESSreformat, PROCESS_TYPE=="material" | PROCESS_TYPE=="relational"), aes(Section,No_of_Verbs_Identified, colour=PROCESS_TYPE)) +theme(legend.position = "none") + geom_area() +ggtitle("No. of Verbs Indentified by 'material' and 'relational'") +scale_x_discrete(labels=(paste(splitMapLevinVerbClassesVsPROCESS$Section,splitMapLevinVerbClassesVsPROCESS$Verb.Classes))) + theme(axis.text.x=element_text(angle=60, hjust=1, size = 5)) + facet_wrap(~PROCESS_TYPE, dir="v")


tmpMaterial<-unique(sort(subset(thematicTableAll$The_Verb_as_PROCESS, thematicTableAll$PROCESS_material=="material")))
tmpRelational<-unique(sort(subset(thematicTableAll$The_Verb_as_PROCESS, thematicTableAll$PROCESS_relational=="relational")))
tmpMaterialLemma<-unique(sort(lemmatize_words(subset(thematicTableAll$The_Verb_as_PROCESS, thematicTableAll$PROCESS_material=="material"))))
tmpRelationalLemma<-unique(sort(lemmatize_words(subset(thematicTableAll$The_Verb_as_PROCESS, thematicTableAll$PROCESS_relational=="relational"))))


# No lemmatised - OK
figure4<- ggplot(data=subset(splitMapLevinVerbClassesVsPROCESSreformat, PROCESS_TYPE=="material" | PROCESS_TYPE=="relational"), aes(Section,No_of_Verbs_Identified, colour=PROCESS_TYPE)) +theme(legend.position = "none") + geom_area() +ggtitle("Sum of the No. of Verbs Indentified by 'material' and 'relational'") +scale_x_discrete(labels=(paste(splitMapLevinVerbClassesVsPROCESS$Section,splitMapLevinVerbClassesVsPROCESS$Verb.Classes))) + theme(axis.text.x=element_text(angle=60, hjust=1, size = 5))  +annotate("text", x=60, y=156, label=c(paste("Material: \n ", str_wrap(paste(tmpMaterial, collapse = " "), 200))), colour="red", size=2) +annotate("text", x=60, y=105, label=c(paste("Relational: \n ", str_wrap(paste(tmpRelational, collapse = " "), 200))), colour="cyan4", size=2)
# Lemmatised - OK
figure5<- ggplot(data=subset(splitMapLevinVerbClassesVsPROCESSreformat, PROCESS_TYPE=="material" | PROCESS_TYPE=="relational"), aes(Section,No_of_Verbs_Identified, colour=PROCESS_TYPE)) +theme(legend.position = "none") + geom_area() +ggtitle("Sum of the No. of Verbs Indentified by 'material' and 'relational' (Lemmatised but Case-Sensitive)") +scale_x_discrete(labels=(paste(splitMapLevinVerbClassesVsPROCESS$Section,splitMapLevinVerbClassesVsPROCESS$Verb.Classes))) + theme(axis.text.x=element_text(angle=60, hjust=1, size = 5))  +annotate("text", x=60, y=156, label=c(paste("Material (Lemmatised): \n ", str_wrap(paste(tmpMaterialLemma, collapse = " "), 200))), colour="red", size=2) +annotate("text", x=60, y=105, label=c(paste("Relational (Lemmatised): \n ", str_wrap(paste(tmpRelationalLemma, collapse = " "), 200))), colour="cyan4", size=2)

############################## plotting the distribution of verb as Process frequencies
materialCounter<-0
relationalCounter<-0
tmpMaterialLemmaLowerCaseNonUnique<-tolower(lemmatize_words(subset(thematicTableAll$The_Verb_as_PROCESS, thematicTableAll$PROCESS_material=="material")))
tmpRelationalLemmaLowerCaseNonUnique<-tolower(lemmatize_words(subset(thematicTableAll$The_Verb_as_PROCESS, thematicTableAll$PROCESS_relational=="relational")))
tmpMaterialLemmaLowerCase<-unique(sort(tmpMaterialLemmaLowerCaseNonUnique))
tmpRelationalLemmaLowerCase<-unique(sort(tmpRelationalLemmaLowerCaseNonUnique))

for (i in 1:length(tmpMaterialLemmaLowerCase)){
  materialCounter[i]<-sum(tmpMaterialLemmaLowerCaseNonUnique==tmpMaterialLemmaLowerCase[i])
}
materialCounter

verbAsMaterialProcess<-tmpMaterialLemmaLowerCase
dtmMaterialFreq<-data.frame(verb=verbAsMaterialProcess, freq=materialCounter)
dtmMaterialFreq<-with(dtmMaterialFreq, dtmMaterialFreq[order(verb, decreasing = TRUE),]) # reorder factor in descending
dtmMaterialFreq$verb<-factor(dtmMaterialFreq$verb, levels = unique(dtmMaterialFreq$verb)) # reorder the factors of the identified verbs

figure6<-ggplot(dtmMaterialFreq,aes(verb, freq)) + geom_pointrange(ymin=0,ymax=dtmMaterialFreq$freq, size=0.05, colour="red") +xlab("Verb - Lemmatised and Case-Insensitive") +ylab("Frequency") +ggtitle("Frequencies of the Verbs Serving as 'Material' Process Type") + theme(axis.text.x=element_text(angle=60, hjust=1, size = 3))

for (i in 1:length(tmpRelationalLemmaLowerCase)){
  relationalCounter[i]<-sum(tmpRelationalLemmaLowerCaseNonUnique==tmpRelationalLemmaLowerCase[i])
}
relationalCounter

verbAsRelationalProcess<-tmpRelationalLemmaLowerCase
dtmRelationalFreq<-data.frame(verb=verbAsRelationalProcess, freq=relationalCounter)
dtmRelationalFreq<-with(dtmRelationalFreq, dtmRelationalFreq[order(verb, decreasing = TRUE),]) # reorder factor in descending
dtmRelationalFreq$verb<-factor(dtmRelationalFreq$verb, levels = unique(dtmRelationalFreq$verb)) # reorder the factors of the identified verbs

figure7<-ggplot(dtmRelationalFreq,aes(verb, freq)) + geom_pointrange(ymin=0,ymax=dtmRelationalFreq$freq, size=0.05, colour="cyan4") +xlab("Verb - Lemmatised and Case-Insensitive") +ylab("Frequency") +ggtitle("Frequencies of the Verbs Serving as 'Relational' Process Type") + theme(axis.text.x=element_text(angle=60, hjust=1, size = 3))


############################### plotting the word cloud all frequencies of 'verb as Process' 
set.seed(3219)
dev.new(width=12,height=8,noRStudioGD = TRUE, units='in', res=1200)  # define a larger screen device, otherwise flowing error if plotting more verbs
figure8<-wordcloud(words=dtmMaterialFreq$verb, freq=dtmMaterialFreq$freq, min.freq = 1, max.words = 400, scale=c(8,.2), random.order = FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2"))

quartz.save("RPlot1_Material.jpg", type="jpeg", device = dev.cur(), dpi = 1200)
figure9<-wordcloud(words=dtmRelationalFreq$verb, freq=dtmRelationalFreq$freq, min.freq = 1, max.words = 300, scale=c(8,.2), random.order = FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2"))
quartz.save("RPlot1_Relational.jpg", type="jpeg", device = dev.cur(), dpi = 1200)




#Output to Excel
wb<-createWorkbook()

hs1<-createStyle(fgFill="#F0F8FF", halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin") # Light blue
hs2<-createStyle(fgFill="#FFFACD", halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin") # Light yellow
hs3<-createStyle(fgFill="#FFCCCB", halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin") # Light red
hs4<-createStyle(fgFill="#ADD8E6", halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin") # Light sky blue
hs5<-createStyle(fgFill="#90EE90", halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin") # Light green
ts4<-createStyle(fgFill="#FFFACD", halign="LEFT", border = c("top","bottom","left","right"), borderStyle = "thin", wrapText=TRUE) # Light yellow
ts5<-createStyle(fgFill="#FFCCCB", halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin", wrapText=TRUE) # Light red
ts8<-createStyle(fgFill="#FFCCCB", halign="LEFT", border = c("top","bottom","left","right"), borderStyle = "thin", wrapText=TRUE) # Light red
ts6<-createStyle(fgFill="#ADD8E6", halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin", wrapText=TRUE) # Light sky blue
ts7<-createStyle(fgFill="#90EE90", halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin", wrapText=TRUE) # Light green
ts1<-createStyle(halign="CENTER")
ts2<-createStyle(wrapText=TRUE)
ts3<-createStyle(halign="LEFT", border = c("top","bottom","left","right"), borderStyle = "thin", wrapText=TRUE)
ts9<-createStyle(halign="CENTER", border = c("top","bottom","left","right"), borderStyle = "thin", wrapText=TRUE)

addWorksheet(wb,sheetName="Original Text")
writeData(wb,sheet="Original Text",xAll)
addStyle(wb,sheet="Original Text",ts2,cols=2,rows=1:length(xAll),gridExpand = TRUE)
setColWidths(wb,sheet="Original Text",cols=1,widths=200)
setColWidths(wb,sheet="Original Text",cols=2,widths=200)
addWorksheet(wb,sheetName="POS Tagging")
writeData(wb,sheet="POS Tagging",POSresultAll)
addStyle(wb,sheet="POS Tagging",ts1,cols=1:12,rows=1:(nrow(POSresultAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="POS Tagging",hs1,cols=1:12,rows=1,gridExpand = TRUE)
setColWidths(wb,sheet="POS Tagging",cols=1:12,widths=15)
addWorksheet(wb,sheetName="Dependencies")
writeData(wb,sheet="Dependencies",DependencyAll)
addStyle(wb,sheet="Dependencies",ts1,cols=1:9,rows=1:(nrow(DependencyAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="Dependencies",hs1,cols=1:9,rows=1,gridExpand = TRUE)
setColWidths(wb,sheet="Dependencies",cols=1:9,widths=15)
addWorksheet(wb,sheetName="Parse Trees")
writeData(wb,sheet="Parse Trees",ParseAll)
addStyle(wb,sheet="Parse Trees",ts2,cols=1,rows=1:length(ParseAll),gridExpand = TRUE)
setColWidths(wb,sheet="Parse Trees",cols=1,widths=100)
addWorksheet(wb,sheetName="Analysis of THEME,MOOD,PROCESS")
writeData(wb,sheet="Analysis of THEME,MOOD,PROCESS",thematicTableAll)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",ts3,cols=1:34,rows=1:(nrow(thematicTableAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",hs1,cols=1:34,rows=1,gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",hs2,cols=2:5,rows=1,gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",hs3,cols=7:10,rows=1,gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",hs4,cols=11:17,rows=1,gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",hs5,cols=18:23,rows=1,gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",ts4,cols=2:5,rows=2:(nrow(thematicTableAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",ts5,cols=7:9,rows=2:(nrow(thematicTableAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",ts8,cols=10,rows=2:(nrow(thematicTableAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",ts6,cols=11:17,rows=2:(nrow(thematicTableAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",ts7,cols=18:23,rows=2:(nrow(thematicTableAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",ts9,cols=1,rows=2:(nrow(thematicTableAll)+1),gridExpand = TRUE)
addStyle(wb,sheet="Analysis of THEME,MOOD,PROCESS",ts9,cols=24:26,rows=2:(nrow(thematicTableAll)+1),gridExpand = TRUE)
setColWidths(wb,sheet="Analysis of THEME,MOOD,PROCESS",cols=1:33,widths="auto")
setColWidths(wb,sheet="Analysis of THEME,MOOD,PROCESS",cols=4:6,widths=70)
setColWidths(wb,sheet="Analysis of THEME,MOOD,PROCESS",cols=10,widths=70)
setColWidths(wb,sheet="Analysis of THEME,MOOD,PROCESS",cols=34,widths=70)
addWorksheet(wb,sheetName="Analytical results")
print(figure3)
insertPlot(wb,sheet="Analytical results", width=16, height=8, xy=NULL, startRow=1,startCol = 1,fileType = "png", units="in", dpi = 600)
print(figure4)
insertPlot(wb,sheet="Analytical results", width=16, height=8, xy=NULL, startRow=42,startCol = 1,fileType = "png", units="in", dpi = 600)
print(figure5)
insertPlot(wb,sheet="Analytical results", width=16, height=8, xy=NULL, startRow=83,startCol = 1,fileType = "png", units="in", dpi = 600)
print(figure6)
insertPlot(wb,sheet="Analytical results", width=16, height=8, xy=NULL, startRow=83,startCol = 17,fileType = "png", units="in", dpi = 600)
print(figure7)
insertPlot(wb,sheet="Analytical results", width=16, height=8, xy=NULL, startRow=83,startCol = 34,fileType = "png", units="in", dpi = 600)
print(figure8)
insertPlot(wb,sheet="Analytical results", width=16, height=8, xy=NULL, startRow=83,startCol = 51,fileType = "png", units="in", dpi = 600)
print(figure9)
insertPlot(wb,sheet="Analytical results", width=16, height=8, xy=NULL, startRow=83,startCol = 68,fileType = "png", units="in", dpi = 600)

addWorksheet(wb,sheetName="POS Tags Counts")
print(figure)
insertPlot(wb,sheet="POS Tags Counts", width=6, height=4, xy=NULL, startRow=1,startCol = 1,fileType = "png", units="in", dpi = 600)
addWorksheet(wb,sheetName="Dependency relations")
print(figure2)
insertPlot(wb,sheet="Dependency relations", width=16, height=8, xy=NULL, startRow=1,startCol = 1,fileType = "png", units="in", dpi = 600)
saveWorkbook(wb,file=paste0(filePath,"ParsingResult.xlsx"), overwrite = TRUE)

if (debug_log_enabled==1){write(debug_log, file=paste0(filePath,"tmp//","debug_log.txt"))}

# END
