#Accessing the data set
setwd("D:/data_analysis/data_editing/datasets_folder")
getwd()
#Step to merge datasets
files_name <- c("advancements_in_ai_result_in_better_game.txt", 
                "ai_and_NPC.txt", 
                "ai_for_games_hasnot_progressed_much_in_the_last_30_years.txt", 
                "ai_npcs_in_gaming_first_of_its_kind.txt", 
                "ai_ruins_the_game.txt", 
                "best_ai_developed_for_npc_enemies_in_games.txt", 
                "Chat_ai_in_games.txt", 
                "games_industry_look_like_in_the_age_of_ai.txt", 
                "games_with_ai_that_learns_from_player_actions.txt", 
                "games_with_smart_ai.txt", 
                "halo_three_builds_large_scale_ai_battles.txt", 
                "how_far_are_we_from_playing_an_ai_generated_game.txt", 
                "indie_games_are_there_that_feature_advanced_ai_or_machine_learning_mechanics.txt", 
                "machine_learning_ai_in_games.txt", 
                "machine_learning_for_game_ai.txt", 
                "strategy_game_devs_ai.txt", 
                "why_ai_feels_stale_in_most_games_and_how_sc_ai_subsumption_can_do_better.txt" )

#Combining datasets into one file
merged_text <- character(0)
for (file_name in files_name) {
  file_content <- readLines(file_name)
  merged_text <- c(merged_text, file_content)
}
writeLines(merged_text, "merged_data_set.txt")
#<----------------------------------------------------------------------------------------------------->

#Text editing
library(tm)
#Merge datasets
merged_text <- readLines("merged_data_set.txt", warn = FALSE)
#Cleaning of spaces
cleaned_text <- gsub("\\s+", " ", merged_text)
#Converting uppercase letters to lowercase letters
cleaned_text <- tolower(cleaned_text)
#Numbers are translated into words
cleaned_text <- gsub("\\b\\d+\\b", "number", cleaned_text)
#Special characters are cleared
cleaned_text <- gsub("[^[:alpha:] ]", "", cleaned_text)
#Saving the cleaned text to a new txt file
writeLines(cleaned_text, "cleaned_and_merged_data_set.txt")
#<----------------------------------------------------------------------------------------------------->

setwd("D:/data_analysis/data_editing/datasets_folder")
getwd()
file_path <- "cleaned_and_merged_data_set.txt"
text_content <- readLines(file_path)
#Tokenized process
library(tokenizers)
#Function written to tokenize text
tokenized_text <- function(text) {
  tokens <- unlist(tokenize_words(text))
  return(tokens)
}
tokens <- lapply(text_content, tokenized_text)
print(tokens)
#<----------------------------------------------------------------------------------------------------->

#Stop words cleaning process
library(tidytext)
#Create a data frame
data_reddit <- data.frame(tokens = unlist(tokens))
#Stop words list
custom_stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves",
                       "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them",
                       "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am",
                       "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did",
                       "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at",
                       "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after",
                       "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again",
                       "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both",
                       "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same",
                       "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now", "d", "ll", "m",
                       "o", "re", "ve", "y", "ain", "aren", "couldn", "didn", "doesn", "hadn", "hasn", "haven", "isn", "ma",
                       "mightn", "mustn", "needn", "shan", "shouldn", "wasn", "weren", "won", "wouldn", "im", "get", "thot", "us")
#Clearing phase of specified stop words in the data frame
cleaned_tokens <- data_reddit %>%
  filter(!tokens %in% custom_stop_words)
print(cleaned_tokens)
#<----------------------------------------------------------------------------------------------------->

#Conjunction cleaning phase
library(tidytext)
#Conjunction list
conjunctions <- c("and", "or", "but", "nor", "so", "for", "yet", "because", "eventually", "therefore", "finally", 
                  "since", "unless", "if", "after", "while", "then", "during", "until", "hence", "as", "only", "oh", 
                  "wherever", "however", "before", "when", "whenever", "though", "although", "nonetheless", "moreover", "besides", "also", "be")
#Clearing phase of specified conjunction in the data frame
cleaned_tokens <- cleaned_tokens %>%
  filter(!tokens %in% conjunctions)
print(cleaned_tokens)

#Printing data cleared of stop words and conjunctions to txt file
cleaned_data <- cleaned_tokens
output_file <- "D:/data_analysis/data_editing/cleaned_stop_words_and_conjunctions.txt" 
writeLines(cleaned_data$tokens, output_file)
#<----------------------------------------------------------------------------------------------------->