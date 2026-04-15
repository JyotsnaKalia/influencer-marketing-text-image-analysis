# =============================================================================
# Digital Marketing Intelligence – Assignment 2
# =============================================================================

rm(list = ls())

#Step 0 - Install packages ----
install.packages("readxl") # CRAN version
#Install packages (run once if not installed)
#install.packages("tidytext")
#install.packages("tidyr")
#install.packages("textdata")
#install.packages("vader")
#install.packages("remotes")
#remotes::install_github("nikita-moor/ldatuning")

library(readxl)
library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)
library(textdata)
library(vader)
library(janitor)
library(lubridate)

#Step 1 - Data loading & cleaning for Image Analysis ----
base_folder <- "C:/Users/LENOVO/Desktop/scrapping_dmi/"   

input_excel       <- paste0(base_folder, "scrapping_dmi_final.xlsx")
image_folder      <- paste0(base_folder, "downloaded_images")
color_output      <- paste0(base_folder, "color_analysis_results.xlsx")
detr_file         <- paste0(base_folder, "detr_object_detection_results.xlsx")  

dir.create(image_folder, showWarnings = FALSE, recursive = TRUE)

# ──── IMAGE ANALYSIS (Download + Color Features) ────────────────────────
cat("=== IMAGE ANALYSIS ===\n")

# 1. Download images
df <- read_excel(input_excel) |> clean_names()

df <- df |>
  mutate(post_id = row_number(),
         image_name = paste0(str_replace_all(influencer, "[^a-zA-Z0-9]", "_"), "_image_", post_id, ".jpg"),
         save_path  = file.path(image_folder, image_name))

cat("Downloading", nrow(df), "images...\n")
pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
for (i in seq_len(nrow(df))) {
  url <- df$image_url[i]  # hoặc cột 'image url' của bạn
  if (!is.na(url) && str_starts(url, "http")) {
    tryCatch({
      resp <- GET(url, timeout(20))
      if (status_code(resp) == 200) writeBin(content(resp, "raw"), df$save_path[i])
    }, error = function(e) NULL)
  }
  setTxtProgressBar(pb, i)
}
close(pb)

# 2 Color Features (brightness, saturation, contrast, warmth)
cat("\n=== Extracting Color Features (magick) ===\n")
image_files <- list.files(image_folder, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)

color_results <- map_dfr(image_files, ~{
  tryCatch({
    img <- image_read(.x) |> image_scale("256x256!")
    hsv <- image_convert(img, colorspace = "HSV")
    brightness <- mean(image_data(hsv)[,,3]) / 100
    saturation <- mean(image_data(hsv)[,,2]) / 100
    contrast   <- sd(image_data(hsv)[,,3])   / 100
    hue        <- image_data(hsv)[,,1]
    warmth     <- mean((hue < 30 | hue > 330))
    
    tibble(file_name = basename(.x),
           brightness = round(brightness, 3),
           saturation = round(saturation, 3),
           brightness_contrast = round(contrast, 3),
           warmth = round(warmth, 4))
  }, error = function(e) NULL)
})

write_xlsx(color_results, color_output)
cat("Color analysis saved →", color_output, "\n")

# Step 2: Merge Merging Color Analysis and DETR Object Detection Results into the main scraping dataset

base_path <- "C:/Users/LENOVO/Desktop/scrapping_dmi/"

main_file     <- paste0(base_path, "scrapping_dmi_final.xlsx")
color_file    <- paste0(base_path, "color_analysis_results.xlsx")
detr_file     <- paste0(base_path, "detr_object_detection_results.xlsx")
output_file   <- paste0(base_path, "scrapping_dmi_final_1.xlsx")

# ──── Read All Three Files ───────────────────────────────────────────────
cat("Reading main scraping dataset...\n")
df_main <- read_excel(main_file) %>% clean_names()

cat("Reading color analysis results...\n")
df_color <- read_excel(color_file) %>% clean_names()

cat("Reading DETR object detection results...\n")
df_detr <- read_excel(detr_file) %>% clean_names()

# ──── Prepare Join Key (Image Filename) ──────────────────────────────────
# Create a consistent 'file_name' column for merging

df_main <- df_main %>%
  mutate(
    # Adjust the line below according to your actual column name that contains image filename
    # Common options: image_name, downloaded_image_path, image_url, file_name
    file_name = basename(image_name)          # ← CHANGE THIS if your column name is different
    # Alternative examples:
    # file_name = basename(downloaded_image_path)
    # file_name = str_extract(image_url, "[^/]+$")
  )

# Standardize column names in color and DETR files
df_color <- df_color %>%
  rename(file_name = file_name)   # Change if the column is named "File_Name"

df_detr <- df_detr %>%
  rename(file_name = file_name)   # Change if needed

# ──── Merge Color and DETR Features into Main Dataset ────────────────────
df_final <- df_main %>%
  left_join(
    df_color %>% 
      select(file_name, 
             brightness, 
             saturation, 
             brightness_contrast, 
             warmth),
    by = "file_name"
  ) %>%
  left_join(
    df_detr %>% 
      select(file_name,
             total_detected_objects,      # Adjust column name if different
             number_of_unique_objects,    # Adjust if needed
             all_label_names),            # Optional: keep if useful
    by = "file_name"
  )

# ──── Check Merge Quality ────────────────────────────────────────────────
cat("\n=== Merge Summary ===\n")
cat("Rows before merge :", nrow(df_main), "\n")
cat("Rows after merge  :", nrow(df_final), "\n\n")

cat("Missing values in newly added image features:\n")
df_final %>%
  select(brightness, saturation, brightness_contrast, warmth,
         total_detected_objects, number_of_unique_objects) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  print()

# ──── Save the Final Merged Dataset ──────────────────────────────────────
write_xlsx(df_final, output_file)

cat("\nSUCCESS! Final dataset has been saved to:\n")
cat(output_file, "\n\n")

cat("You can now use 'scrapping_dmi_final_1.xlsx' for text analysis, LDA, and regression.\n")

# Step 3: Data loading & cleaning for Text Analysis and Regression ----

#0 - Data loading & cleaning ----
scrapping_file_raw <- read_excel("C:/Users/LENOVO/Desktop/scrapping_dmi/scrapping_dmi_final_1.xlsx")
sc_file <- scrapping_file_raw

##Cleaning column names ----
sc_file <- sc_file %>%
  clean_names()

names(sc_file)

str(sc_file)
summary(sc_file)

#correct structure of the file
#Fix numeric columns
sc_file <- sc_file %>%
  mutate(
    likes = as.numeric(likes),
    number_of_likes = as.numeric(number_of_likes)
  )

#there are 2 like columns "likes" and "number_of_likes", check if they are the same

all.equal(sc_file$likes,sc_file$number_of_likes)

#remove one since they are the same
sc_file <- sc_file %>%
  select(-likes)


summary(sc_file)
str(sc_file)

#Checking missing
sum(is.na(sc_file$number_of_likes))
sum(is.na(sc_file$number_of_comments))


check_na <- sc_file %>%
  filter(is.na(number_of_likes))

check_na

sc_file$number_of_likes[is.nan(sc_file$number_of_likes)] <- NA

#create a working caption column
sc_file$caption_raw <- sc_file$caption


#inspect structure
str(sc_file$caption_raw)
head(sc_file$caption_raw, 5)

##Lower words ----
sc_file$caption_clean <- stringr::str_to_lower(sc_file$caption_raw)

##Remove line breaks ----
sc_file$caption_clean <- gsub("[\r\n]", " ", sc_file$caption_clean)

# check result
head(sc_file$caption_clean, 3)

# replace multiple spaces with single space
sc_file$caption_clean <- gsub("\\s+", " ", sc_file$caption_clean)

# trim leading and trailing spaces
sc_file$caption_clean <- trimws(sc_file$caption_clean)

# check result
head(sc_file$caption_clean, 3)

##Remove URLs (http, https, www) ----
sc_file$caption_clean <- gsub("http[s]?://\\S+|www\\.\\S+", "", sc_file$caption_clean)

# normalize spaces again after URL removal
sc_file$caption_clean <- gsub("\\s+", " ", sc_file$caption_clean)
sc_file$caption_clean <- trimws(sc_file$caption_clean)

# check result
head(sc_file$caption_clean, 3)

## Creating Feature variables----
# IMPORTANT: create these from caption_raw so that counts are based on the original caption
# count hashtags
sc_file$hashtag_count <- stringr::str_count(sc_file$caption_raw, "#")

# count mentions
sc_file$mention_count <- stringr::str_count(sc_file$caption_raw, "@")

# count exclamation marks
sc_file$exclamation_count <- stringr::str_count(sc_file$caption_raw, "!")

# count question marks
sc_file$question_count <- stringr::str_count(sc_file$caption_raw, "\\?")

#emoji count
#these emojis seem limited, expand if required 
#Not using  this technique
#emoji_pattern <- "[\\U0001F300-\\U0001F6FF\\U0001F900-\\U0001F9FF\\U00002600-\\U000026FF]"


#sc_file$emoji_count <- stringr::str_count(sc_file$caption_raw, emoji_pattern)

#extract emojis
library(stringr)

emoji_pattern <- "[\\U0001F300-\\U0001FAFF\\U00002700-\\U000027BF]"

sc_file$emoji_list <- str_extract_all(sc_file$caption_raw, emoji_pattern)
sc_file$emoji_count <- lengths(sc_file$emoji_list)

str(sc_file$emoji_list)
str(sc_file$emoji_count)

#see all the emojis that i have
all_emojis <- unlist(sc_file$emoji_list)
sort(table(all_emojis), decreasing = TRUE)

#remove emojis
remove_emojis <- c("♀", "🇨", "🇫", "🇭", "🇮", "🇷", "🇹", "↕")

sc_file$emoji_list_clean <- lapply(sc_file$emoji_list, function(x) {
  x[!x %in% remove_emojis]
})

sc_file$emoji_count_clean <- lengths(sc_file$emoji_list_clean)

all_emojis_clean <- unlist(sc_file$emoji_list_clean)
sort(table(all_emojis_clean), decreasing = TRUE)

#create emoji dictionary
emoji_dict <- c(
  
  # Love / affection
  "🤍"="love","♥"="love","❤"="love","🩷"="love","💕"="love","💜"="love",
  "🤎"="love","💛"="love","💙"="love","🖤"="love",
  
  # Positive emotion
  "🙂"="positive","😊"="positive","😌"="positive","😁"="positive","😇"="positive",
  
  # Adoration / attraction
  "😍"="adoration","🥰"="adoration","🤩"="adoration",
  
  # Laughter / fun
  "😂"="laugh","😆"="laugh","😅"="laugh",
  
  # Emotional / touched
  "🥹"="emotional","😭"="emotional",
  
  # Aesthetic / beauty
  "✨"="aesthetic","🌹"="aesthetic","💅"="beauty","📸"="photo",  
  
  # Excitement / hype
  "🔥"="excited","🙌"="excited","🥳"="excited","🥂"="celebration","🏆"="achievement",
  
  # CTA / interaction
  "👉"="cta","👇"="cta","⬇"="cta","🔗"="cta",
  
  # Nature / vibe
  "🌴"="nature","🌊"="nature","☀"="nature","🏝"="nature",
  "🌸"="nature","🌺"="nature","🌅"="nature","🌿"="nature","🎄"="nature" ,
  
  # Misc useful signals
  "👀"="attention","💋"="kiss","💨"="fast","💭"="thought",
  
  # Approval / rejection
  "✅"="approved","❌"="rejected"
)

#convert emojis into text
sc_file$emoji_text <- sapply(sc_file$emoji_list_clean, function(x) {
  words <- emoji_dict[x]
  words <- words[!is.na(words)]
  paste(words, collapse = " ")
})



head(sc_file[, c("caption_raw", "hashtag_count", "mention_count",
                 "exclamation_count", "question_count", "emoji_count_clean","caption_clean")], 10)



summary(sc_file$hashtag_count)
summary(sc_file$mention_count)
summary(sc_file$emoji_count_clean)

##Define hastags and brand mentions  ----
# IMPORTANT: detect brand mentions in raw captions so that text, hashtags and mentions are all captured
bbw_pattern <- paste(
  "bath\\s*&\\s*body\\s*works",
  "bath\\s*and\\s*body\\s*works",
  "bathbodyworks",
  "\\bbbw\\b",
  "@bathandbodyworks",
  "@bathbodyworks",
  "#bbw",
  "#bathandbodyworks",
  "#bathbodyworks",
  sep = "|"
)

sc_file$bbw_count <- stringr::str_count(
  stringr::str_to_lower(sc_file$caption_raw),
  bbw_pattern
)

sc_file$bbw_mentioned <- ifelse(sc_file$bbw_count > 0, 1, 0)

table(sc_file$bbw_mentioned)

#remove these variables as they are not adding any meaning and will later cause multicollinearity issues
sc_file <- sc_file %>% select (-bbw_count, -bbw_mentioned)

head(sc_file$caption_clean,10)


## Remove mentions ----
# IMPORTANT: rebuild caption_clean from raw text before final cleaning for sentiment analysis
sc_file$caption_clean <- stringr::str_to_lower(sc_file$caption_raw)
head(sc_file$caption_clean,10)
sc_file$caption_clean <- gsub("[\r\n]", " ", sc_file$caption_clean)
sc_file$caption_clean <- gsub("http[s]?://\\S+|www\\.\\S+", "", sc_file$caption_clean)
sc_file$caption_clean <- gsub("@[^[:space:]]+", "", sc_file$caption_clean)

##Convert hastags to words ----
sc_file$caption_clean <- gsub("#", "", sc_file$caption_clean)

sc_file %>% count(sc_file$hashtag_count == 0)

#clean extra spaces again
sc_file$caption_clean <- gsub("\\s+", " ", sc_file$caption_clean)
sc_file$caption_clean <- trimws(sc_file$caption_clean)

sc_file$caption_clean <- gsub("[[:cntrl:]]", " ", sc_file$caption_clean)
sc_file$caption_clean <- gsub("\\s+", " ", sc_file$caption_clean)
sc_file$caption_clean <- trimws(sc_file$caption_clean)

sc_file$caption_clean <- gsub("\u2063|\u200d|\ufe0f", "", sc_file$caption_clean)
sc_file$caption_clean <- gsub("\\s+", " ", sc_file$caption_clean)
sc_file$caption_clean <- trimws(sc_file$caption_clean)

head(sc_file$caption_clean,10)

# Descriptives
summary(sc_file[, c("hashtag_count", "emoji_count_clean", "mention_count", 
                    "question_count", "exclamation_count")])

#check if there are any empty captions 
sum(is.na(sc_file$caption_clean))
sum(sc_file$caption_clean == "", na.rm = TRUE)
#none


#1 - Sentiment Analysis ----

##BING ----
#create id for each post so that it can be tokenised and then later join
sc_file$post_id <- 1:nrow(sc_file)

# get Bing lexicon
bing_lexicon <- get_sentiments("bing")

# tokenize captions into words
bing_tokens <- sc_file %>%
  select(post_id, caption_clean) %>%
  unnest_tokens(word, caption_clean)

# match words with Bing lexicon
bing_matches <- bing_tokens %>%
  inner_join(bing_lexicon, by = "word")

# count positive and negative words per post
bing_counts <- bing_matches %>%
  count(post_id, sentiment) %>%
  tidyr::pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  )

# rename variables
bing_counts <- bing_counts %>%
  rename(
    bing_negative = negative,
    bing_positive = positive
  ) %>%
  mutate(
    bing_net = bing_positive - bing_negative
  )

# merge back to main file
sc_file <- sc_file %>%
  left_join(bing_counts, by = "post_id")

# replace NAs with 0 for posts with no matched sentiment words
sc_file$bing_positive[is.na(sc_file$bing_positive)] <- 0
sc_file$bing_negative[is.na(sc_file$bing_negative)] <- 0
sc_file$bing_net[is.na(sc_file$bing_net)] <- 0

# inspect
head(sc_file[, c("post_id", "caption_clean", "bing_positive", "bing_negative", "bing_net")], 6)
summary(sc_file$bing_positive)
summary(sc_file$bing_negative)
summary(sc_file$bing_net)

hist(sc_file$bing_net)

plot(sc_file$bing_net, sc_file$number_of_likes/1000)

cor(sc_file$bing_net, sc_file$number_of_likes, use = "complete.obs")
cor(sc_file$bing_net, sc_file$number_of_comments)


##AFINN ----


# get AFINN lexicon
afinn_lexicon <- get_sentiments("afinn")

# tokenize captions into words
afinn_tokens <- sc_file %>%
  select(post_id, caption_clean) %>%
  unnest_tokens(word, caption_clean)

# match words with AFINN lexicon
afinn_matches <- afinn_tokens %>%
  inner_join(afinn_lexicon, by = "word")

# calculate total AFINN score per post
afinn_scores <- afinn_matches %>%
  group_by(post_id) %>%
  summarise(afinn_score = sum(value), .groups = "drop")

# merge back to main file
sc_file <- sc_file %>%
  left_join(afinn_scores, by = "post_id")

head(sc_file[, c("post_id", "caption_clean", "afinn_score")], 6)
summary(sc_file$afinn_score)
#around 27 NAs. Can prefer Bing

sc_file$afinn_score[is.na(sc_file$afinn_score)] <- 0

# inspect
head(sc_file[, c("post_id", "caption_clean", "afinn_score")], 6)
summary(sc_file$afinn_score)

#Inspecting captions where AFINN Score is negative
afinn_zero_posts <- sc_file %>%
  filter(afinn_score == 0)

# view captions
check <- afinn_zero_posts %>%
  select(post_id, caption_raw, caption_clean) %>%
  head(10)

hist(sc_file$afinn_score, main="Histogram of AFINN Scores", col="lightblue")
plot(sc_file$afinn_score, sc_file$number_of_likes/1000, 
     main="AFINN Score vs Likes", xlab="AFINN Score", ylab="Likes / 1000")

#inspecting outlier (AFINN = -10)
# Extract the exact -10 outlier post
outlier_1 <- sc_file %>% 
  filter(afinn_score == -10) %>%
  select(post_id, influencer, caption_raw, caption_clean, 
         number_of_likes, number_of_comments, bing_net, afinn_score)

print(outlier_1)

#inspecting outlier (AFINN > 50)
# Extract the exact -10 outlier post
outlier_2 <- sc_file %>% 
  filter(afinn_score > 50) %>%
  select(post_id, influencer, caption_raw, caption_clean, 
         number_of_likes, number_of_comments, bing_net, afinn_score)

print(outlier_2)

cor(sc_file$afinn_score, sc_file$number_of_likes, use = "complete.obs")
cor(sc_file$afinn_score, sc_file$number_of_comments)

##NRC ----
# get NRC lexicon
nrc_lexicon <- get_sentiments("nrc")

# tokenize captions into words
nrc_tokens <- sc_file %>%
  select(post_id, caption_clean) %>%
  unnest_tokens(word, caption_clean)

# match words with NRC lexicon
nrc_matches <- nrc_tokens %>%
  inner_join(nrc_lexicon, by = "word",relationship = "many-to-many")

# count NRC categories per post
nrc_counts <- nrc_matches %>%
  count(post_id, sentiment) %>%
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  )

# merge back to main file
sc_file <- sc_file %>%
  left_join(nrc_counts, by = "post_id")

sc_file <- sc_file %>%
  rename(
    nrc_positive = positive,
    nrc_negative = negative,
    nrc_anger = anger,
    nrc_anticipation = anticipation,
    nrc_fear = fear,
    nrc_joy = joy,
    nrc_trust = trust,
    nrc_surprise = surprise,
    nrc_disgust = disgust,
    nrc_sadness = sadness
  )

# replace NAs with 0 in case a category is absent
nrc_cols <- c("nrc_anger", "nrc_anticipation", "nrc_disgust", "nrc_fear", "nrc_joy",
              "nrc_negative", "nrc_positive", "nrc_sadness", "nrc_surprise", "nrc_trust")

for (col in nrc_cols) {
  if (!col %in% names(sc_file)) {
    sc_file[[col]] <- 0
  } else {
    sc_file[[col]][is.na(sc_file[[col]])] <- 0
  }
}

# optional net NRC sentiment
sc_file$nrc_net <- sc_file$nrc_positive - sc_file$nrc_negative

# inspect NRC results
head(sc_file[, c("post_id", "caption_clean", "nrc_positive", "nrc_negative",
                 "nrc_joy", "nrc_trust", "nrc_surprise", "nrc_net")], 6)

summary(sc_file$nrc_positive)
summary(sc_file$nrc_negative)
summary(sc_file$nrc_joy)
summary(sc_file$nrc_trust)
summary(sc_file$nrc_surprise)
summary(sc_file$nrc_anticipation)
summary(sc_file$nrc_fear)
summary(sc_file$nrc_net)

names(sc_file)

cor(sc_file$nrc_joy, sc_file$number_of_likes, use = "complete.obs")
cor(sc_file$nrc_trust, sc_file$number_of_likes, use = "complete.obs")
cor(sc_file$nrc_anticipation, sc_file$number_of_likes, use = "complete.obs")
cor(sc_file$nrc_net, sc_file$number_of_likes, use = "complete.obs")

plot(sc_file$nrc_joy, sc_file$number_of_likes/1000, main="NRC Joy vs Likes")
plot(sc_file$nrc_trust, sc_file$number_of_likes/1000, main="NRC Trust vs Likes")

##VADER ----
# run VADER on cleaned captions
vader_scores <- vader::vader_df(sc_file$caption_clean)

# inspect returned column names first
names(vader_scores)

# bind VADER results to main dataframe
sc_file <- bind_cols(sc_file, vader_scores)

# rename key columns if present
# common useful VADER outputs include compound plus polarity proportions
if ("compound" %in% names(sc_file)) {
  sc_file <- sc_file %>% rename(vader_compound = compound)
}
if ("pos" %in% names(sc_file)) {
  sc_file <- sc_file %>% rename(vader_pos = pos)
}
if ("neu" %in% names(sc_file)) {
  sc_file <- sc_file %>% rename(vader_neu = neu)
}
if ("neg" %in% names(sc_file)) {
  sc_file <- sc_file %>% rename(vader_neg = neg)
}

# inspect VADER results
vader_view_cols <- intersect(c("post_id", "caption_clean",
                               "vader_compound", "vader_pos",
                               "vader_neu", "vader_neg"),
                             names(sc_file))

head(sc_file[, vader_view_cols], 6)

if ("vader_compound" %in% names(sc_file)) summary(sc_file$vader_compound)
if ("vader_pos" %in% names(sc_file)) summary(sc_file$vader_pos)
if ("vader_neu" %in% names(sc_file)) summary(sc_file$vader_neu)
if ("vader_neg" %in% names(sc_file)) summary(sc_file$vader_neg)

head(sc_file,5)

# Create log versions 
sc_file$log_likes <- log(sc_file$number_of_likes + 1)
sc_file$log_comments <- log(sc_file$number_of_comments + 1)

cor(sc_file$vader_compound, sc_file$log_likes, use = "complete.obs")
cor(sc_file$vader_pos, sc_file$log_likes, use = "complete.obs")


plot(sc_file$vader_compound, sc_file$number_of_likes/1000, 
     main="VADER Score vs Likes", xlab="VADER", ylab="Likes / 1000")


#comparision across lexicons
sc_file[is.na(sc_file$vader_compound), c("post_id", "caption_clean")]
sc_file$vader_compound[is.na(sc_file$vader_compound)] <- 0

comparison <- sc_file %>%
  select(
    post_id,
    caption_clean,
    
    # Bing
    bing_net,
    
    # AFINN
    afinn_score,
    
    # NRC
    nrc_net, nrc_joy, nrc_trust, nrc_surprise,
    
    # VADER
    vader_compound
  )

head(comparison, 6)

#How much do the methods agree
cor(comparison[, c("bing_net", "afinn_score", "nrc_net", "vader_compound")])

pairs(comparison[, c("bing_net", "afinn_score", "nrc_net", "vader_compound")])

#Scale the scores for better comparison
sc_file <- sc_file %>%
  mutate(
    bing_z  = as.numeric(scale(bing_net)),
    afinn_z = as.numeric(scale(afinn_score)),
    nrc_z   = as.numeric(scale(nrc_net)),
    vader_z = as.numeric(scale(vader_compound))
  )

#correlations wont change but checking for robustess
cor(sc_file[, c("bing_z", "afinn_z", "nrc_z", "vader_z")])

#visual comparison
boxplot(
  sc_file$bing_z,
  sc_file$afinn_z,
  sc_file$nrc_z,
  sc_file$vader_z,
  names = c("Bing", "AFINN", "NRC", "VADER"),
  main = "Standardized Sentiment Score Comparison"
)

#specifically check how other lexicons are able to capture the sentiment where AFINN = 0
afinn_zero_check <- sc_file %>%
  filter(afinn_score == 0) %>%
  select(
    post_id,
    caption_clean,
    
    # other lexicons
    bing_net,
    nrc_net,
    nrc_joy,
    nrc_trust,
    vader_compound
  )

head(afinn_zero_check, 10)



# Re-run correlations on log scale + use Spearman (rank correlation)
cor(sc_file$bing_net, sc_file$log_likes, use = "complete.obs")
cor(sc_file$afinn_score, sc_file$log_likes, use = "complete.obs")
cor(sc_file$nrc_joy, sc_file$log_likes, use = "complete.obs")
cor(sc_file$nrc_trust, sc_file$log_likes, use = "complete.obs")

# Re-run correlations on log scale + use Spearman (rank correlation)
cor(sc_file$bing_net, sc_file$log_likes, method = "spearman", use = "complete.obs")
cor(sc_file$afinn_score, sc_file$log_likes, method = "spearman", use = "complete.obs")
cor(sc_file$nrc_joy, sc_file$log_likes, method = "spearman", use = "complete.obs")
cor(sc_file$nrc_trust, sc_file$log_likes, method = "spearman", use = "complete.obs")

# correlations on log scale for comments + use Spearman (rank correlation)
cor(sc_file$bing_net, sc_file$log_comments, method = "spearman", use = "complete.obs")
cor(sc_file$afinn_score, sc_file$log_comments, method = "spearman", use = "complete.obs")
cor(sc_file$nrc_joy, sc_file$log_comments, method = "spearman", use = "complete.obs")
cor(sc_file$nrc_trust, sc_file$log_comments, method = "spearman", use = "complete.obs")
cor(sc_file$vader_compound, sc_file$log_comments,method = "spearman", use = "complete.obs")


#2 - LDA ----
#import libraries
library(openxlsx)
library(tidyverse)
library(scales)
library(randomForest)
library(dplyr)
library(text2vec)
library(textmineR)
library(wordcloud)
library(RColorBrewer)
library(ldatuning)
library(data.table)
library(textstem)
library(textmineR)
library(stopwords)

str(sc_file)

## Step 1 - pre-processing data for LDA ----
lda_data <- sc_file[,c("post_id","influencer","caption_clean")]

#removing missing or empty captions
lda_data <- lda_data[!is.na(lda_data$caption_clean) & lda_data$caption_clean != "", ]

# create a simple internal ID for LDA
lda_data$ID <- seq_len(nrow(lda_data))

# tokenize cleaned captions into words
tokens <- lda_data %>%
  tidytext::unnest_tokens(word, caption_clean)

# remove emojis
# this catches most common emoji ranges
tokens$word <- gsub("[\U0001F300-\U0001FAFF\u2600-\u27BF]+", "", tokens$word, perl = TRUE)

# remove punctuation and numbers
tokens$word <- gsub("[[:punct:]]+", "", tokens$word)
tokens$word <- gsub("[[:digit:]]+", "", tokens$word)

# remove extra whitespace just in case
tokens$word <- trimws(tokens$word)

# remove empty tokens
tokens <- tokens %>%
  filter(word != "")

# remove stopwords
stop_words_custom <- c(stopwords::stopwords("en"),
                       stopwords::stopwords(source = "smart"))

tokens <- tokens %>%
  filter(!word %in% stop_words_custom)

# lemmatize tokens
tokens$word <- textstem::lemmatize_words(tokens$word)

# remove empty tokens again after lemmatization
tokens <- tokens %>%
  filter(word != "")

# remove custom repetitive junk words specific to influencer captions
# edit this list after inspecting your own top frequencies
custom_junk_words <- c(
  "link", "bio", "comment", "follow", "like", "share", "subscribe",
  "code", "shop", "shopping", "discount", "sale", "ad", "gifted",
  "sponsored", "partner", "partnership", "collab", "promo",
  "lol", "omg", "yeah", "u", "ur", "im", "ive", "id", "dont", "didnt",
  "cant", "wont", "ya", "amp", "lt", "gt"
)

tokens <- tokens %>%
  filter(!word %in% custom_junk_words)

# optional: remove very short tokens
tokens <- tokens %>%
  filter(nchar(word) > 2)

# check whether some posts became empty after cleaning
remaining_docs <- unique(tokens$ID)
remaining_docs

# keep document IDs for merging topic probabilities back later
lda_keep_ids <- lda_data %>%
  filter(ID %in% remaining_docs) %>%
  select(ID, post_id, influencer)

# rebuild cleaned text per post
tokens_rebuilt <- tokens %>%
  group_by(ID) %>%
  summarise(text = paste(word, collapse = " "), .groups = "drop")

# trim spaces
tokens_rebuilt$text <- trimws(tokens_rebuilt$text)

# remove any rows that are still empty after full cleaning
tokens_rebuilt <- tokens_rebuilt %>%
  filter(!is.na(text) & text != "")

# keep only IDs that survived final cleaning
lda_keep_ids <- lda_keep_ids %>%
  filter(ID %in% tokens_rebuilt$ID)

# create DTM
dtm <- CreateDtm(
  doc_vec = tokens_rebuilt$text,
  doc_names = tokens_rebuilt$ID,
  ngram_window = c(1, 1),
  stopword_vec = character(),   # already removed stopwords above
  lower = TRUE,
  remove_punctuation = TRUE,
  remove_numbers = TRUE,
  verbose = FALSE
)

# remove empty rows/columns just in case
dtm <- dtm[rowSums(dtm) > 0, ]
dtm <- dtm[, colSums(dtm) > 0]

# inspect term/document frequencies
tf <- TermDocFreq(dtm)

#Inspecting document frequency 
tf

# Summary statistics 
summary(tf$term_freq)
summary(tf$doc_freq)
quantile(tf$term_freq, probs = c(0.5, 0.8, 0.9, 0.95, 0.99))
quantile(tf$doc_freq, probs = c(0.5, 0.8, 0.9, 0.95, 0.99))

# 2. Top 40 most frequent terms (these are the ones that matter)
freq <- tf %>% 
  arrange(desc(term_freq)) %>% 
  slice_head(n = 40) %>% 
  print()

head(freq,40)

# 3. Terms that appear in the most documents (high doc_freq)
tf %>% 
  arrange(desc(doc_freq)) %>% 
  slice_head(n = 30) %>% 
  print()

# 4. How many terms appear only once (very common problem in short captions)
sum(tf$term_freq == 1)
sum(tf$term_freq >= 3)
sum(tf$term_freq >= 5)

# filter vocabulary: remove very rare and very common words
vocabulary <- tf$term[
  tf$term_freq >= 3 & 
    tf$doc_freq <= 0.75 * nrow(dtm)
]

print(paste("Terms kept:", length(vocabulary)))

# apply vocabulary filtering to DTM
dtm <- dtm[, colnames(dtm) %in% vocabulary]

# remove empty rows again after vocabulary filtering
dtm <- dtm[rowSums(dtm) > 0, ]

# keep only IDs still present after final vocabulary filtering
final_ids <- as.integer(rownames(dtm))

lda_keep_ids <- lda_keep_ids %>%
  filter(ID %in% final_ids)

tokens_rebuilt <- tokens_rebuilt %>%
  filter(ID %in% final_ids)

# quick checks
dim(dtm)
head(lda_keep_ids)
head(tokens_rebuilt)

nrow(tokens_rebuilt)
nrow(lda_keep_ids)

#only 3 empty captions were dropped

## Step 2 - Determine number of topics ----

library(ldatuning)

result_k <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 8, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 2L,
  verbose = TRUE
)

# view results table
result_k


dev.off()

# plot results
FindTopicsNumber_plot(result_k)


## Step 3 - LDA LOOP - Test multiple k values ----

library(textmineR)
set.seed(12345)   # for reproducibility

# Define range of k to test (change if you want)
k_values <- seq(from = 4, to = 8, by = 1)   # recommended for your data

# Store results
results_list <- list()

cat("Starting LDA models for k =", paste(k_values, collapse=", "), "\n\n")

for(k in k_values) {
  cat("Running model for k =", k, "...\n")
  
  model <- FitLdaModel(
    dtm = dtm,
    k = k,
    iterations = 2000,      # increased for stability (was 500)
    burnin = 500,
    alpha = 0.05,
    beta = 0.05,
    smooth = TRUE,
    optimize_alpha = TRUE,
    calc_likelihood = TRUE,
    calc_coherence = TRUE,
    calc_r2 = TRUE,
    cpus = 2
  )
  
  # Store key stats
  results_list[[paste0("k", k)]] <- list(
    k = k,
    coherence_mean = mean(model$coherence, na.rm = TRUE),
    coherence_median = median(model$coherence, na.rm = TRUE),
    r2 = model$r2,
    top_terms = GetTopTerms(phi = model$phi, M = 15)
  )
  
  cat("  → Coherence (mean):", round(results_list[[paste0("k", k)]]$coherence_mean, 4), 
      " | R²:", round(results_list[[paste0("k", k)]]$r2, 4), "\n\n")
}


# SUMMARY TABLE
summary_table <- data.frame(
  k = sapply(results_list, `[[`, "k"),
  coherence_mean = sapply(results_list, `[[`, "coherence_mean"),
  coherence_median = sapply(results_list, `[[`, "coherence_median"),
  r2 = sapply(results_list, `[[`, "r2")
)

print("=== SUMMARY TABLE ===")
print(summary_table)

# TOP 15 TERMS FOR EACH k (for easy inspection)
cat("\n=== TOP 15 TERMS PER TOPIC (for each k) ===\n")
for(k_name in names(results_list)) {
  cat("\n--- k =", results_list[[k_name]]$k, "---\n")
  print(t(results_list[[k_name]]$top_terms))
}

set.seed(12345)

final_model <- FitLdaModel(
  dtm = dtm,
  k = 5,
  iterations = 2000,
  burnin = 500,
  alpha = 0.05,
  beta = 0.05,
  smooth = TRUE,
  optimize_alpha = TRUE,
  calc_likelihood = TRUE,
  calc_coherence = TRUE,
  calc_r2 = TRUE,
  cpus = 2
)

# Top 20 terms per topic
top_terms <- GetTopTerms(phi = final_model$phi, M = 20)
print(t(top_terms))

theta <- as.data.frame(final_model$theta)
top_terms <- GetTopTerms(final_model$phi, M = 15)



## Step 4 - Create a clean topic table for PPT / report ----
top_terms <- GetTopTerms(phi = final_model$phi, M = 20)   # 20 words as requested in assignment

# Create nice table
top_terms_df <- as.data.frame(t(top_terms))
top_terms_df$topic <- paste0("Topic_", 1:5)   # since you chose k=5

# Reorder columns
top_terms_df <- top_terms_df[, c("topic", colnames(top_terms_df)[1:20])]

print(top_terms_df)   # check it looks good


## Step 5 - Create topic probability dataset ----

# theta = topic probabilities per document
theta_df <- as.data.frame(theta)

# add ID from DTM rownames
theta_df$ID <- as.integer(rownames(theta_df))

# merge topic probabilities back with kept document info
topic_data <- merge(lda_keep_ids, theta_df, by = "ID")

# inspect
head(topic_data)
dim(topic_data)


## Step 6 - Merge topic probabilities back to main dataset ----

# merge with original working file using post_id and Influencer
sc_topics <- merge(
  sc_file,
  topic_data,
  by = c("post_id", "influencer"),
  all.x = FALSE,
  all.y = TRUE
)

# inspect merged data
head(sc_topics,2)
dim(sc_topics)
names(sc_topics)


## Step 7 - Assign dominant topic to each post ----

topic_cols <- grep("^t_", names(sc_topics), value = TRUE)

sc_topics$dominant_topic <- topic_cols[max.col(sc_topics[, topic_cols], ties.method = "first")]

# inspect
head(sc_topics[, c("post_id", "influencer", topic_cols, "dominant_topic")])


# Overall pivot table of dominant topics (for PPT)
overall_pivot <- sc_topics %>%
  count(dominant_topic) %>%
  mutate(share = n / sum(n)) %>%
  arrange(desc(n))

print(overall_pivot)

## Step 8 - Summarise topic prevalence by influencer ----

library(dplyr)

topic_by_influencer <- sc_topics %>%
  group_by(influencer) %>%
  summarise(
    across(all_of(topic_cols), mean, na.rm = TRUE),
    n_posts = n(),
    .groups = "drop"
  )

topic_by_influencer

# dominant topic counts by influencer
dominant_topic_counts <- sc_topics %>%
  count(influencer, dominant_topic) %>%
  group_by(influencer) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

dominant_topic_counts


## Step 9 - inspect representative posts for each topic ----

# top 5 posts per topic
top_posts_t1 <- sc_topics %>%
  arrange(desc(t_1)) %>%
  select(post_id, influencer, caption_clean, t_1) %>%
  head(5)

top_posts_t2 <- sc_topics %>%
  arrange(desc(t_2)) %>%
  select(post_id, influencer, caption_clean, t_2) %>%
  head(5)

top_posts_t3 <- sc_topics %>%
  arrange(desc(t_3)) %>%
  select(post_id, influencer, caption_clean, t_3) %>%
  head(5)

top_posts_t1
top_posts_t2
top_posts_t3


## Step 10 - Topic proportions and sentiment ----
# only run this if sentiment columns already exist in sc_file

# check available sentiment columns first
names(sc_topics)

# example: correlation with sentiment variables if present
# replace the variable names below with your actual sentiment columns
cor(sc_topics[, c("t_1", "t_2", "t_3", "t_4","t_5", "bing_net", "afinn_score", "nrc_net", "vader_compound")],
    use = "complete.obs")

# average sentiment by dominant topic
sentiment_by_topic <- sc_topics %>%
  group_by(dominant_topic) %>%
  summarise(
    mean_sentiment = mean(vader_compound, na.rm = TRUE),
    n_posts = n(),
    .groups = "drop"
  )
sentiment_by_topic

## Step 11 - Topic proportions and engagement ----
# only run this if engagement variables already exist

# check column names first
names(sc_topics)

# example: average engagement by dominant topic
engagement_by_topic <- sc_topics %>%
  group_by(dominant_topic) %>%
  summarise(
    avg_likes = mean(number_of_likes, na.rm = TRUE),
    avg_comments = mean(number_of_comments, na.rm = TRUE),
    n_posts = n(),
    .groups = "drop"
  )
engagement_by_topic



#3 - Regression ----

#checking final data
names(sc_topics)

#logging time_elapsed
sc_topics$log_time_elapsed <- log(sc_topics$time_elapsed + 1)

summary(sc_topics$log_time_elapsed)

# example: linear regression for a continuous engagement variable
model_1 <- lm(number_of_likes ~ t_1 + t_2 + t_3 +t_4,  data = sc_topics)
summary(model_1)

#changing dependent variable into log terms
model_2  <- lm(log_likes ~ t_1 + t_2 + t_3 +t_4,  data = sc_topics)
summary(model_2)

# example: regression with sentiment control
model_3 <- lm(number_of_likes ~ vader_compound + t_1 + t_2 + t_3 +t_4, data = sc_topics)
summary(model_3)

#changing dependent variable into log terms
model_4 <- lm(log_likes ~ vader_compound + t_1 + t_2 + t_3 +t_4, data = sc_topics)
summary(model_4)


# note:
# not including all 5 topic variables together because t_1 + t_2 + t_3 + t_4 +t_5 = 1
# leave one topic out as the reference category


# FINAL REGRESSION MODEL - All Features + Controls

# Create missing control variables (if not already created)
sc_topics$caption_length <- str_count(sc_topics$caption_clean, "\\w+")


# Full model (log_likes as DV)
model_5 <- lm(log_likes ~  t_1 + t_2 + t_3 + t_4 + afinn_score + 
                nrc_joy + nrc_trust + vader_compound +
                brightness + saturation +  brightness_contrast + 
                warmth + total_detected_objects +
                hashtag_count + mention_count + question_count + exclamation_count + 
                emoji_count_clean + caption_length + log_time_elapsed +
                factor(influencer), data = sc_topics)

# Model Summary
summary(model_5)


# Check for multicollinearity (VIF < 5 is good)
library(car)
vif_results <- vif(model_5)
print(vif_results)
#All Vifs are under 5

# Check engagement with comments as the dependent variable
# Full model for comments (identical structure)
model_6 <- lm(log_comments ~ 
                t_1 + t_2 + t_3 + t_4 +
                afinn_score + nrc_joy + nrc_trust + vader_compound +
                brightness + saturation + brightness_contrast + 
                total_detected_objects +
                caption_length + hashtag_count + mention_count + 
                question_count + exclamation_count + emoji_count_clean +
                log_time_elapsed  +
                factor(influencer),
              data = sc_topics)

summary(model_6)


#centering the variables
sc_topics <- sc_topics %>%
  mutate(
    brightness_c = brightness - mean(brightness, na.rm = TRUE),
    saturation_c = saturation - mean(saturation, na.rm = TRUE),
    object_c = total_detected_objects - mean(total_detected_objects, na.rm = TRUE),
    hashtag_c = hashtag_count - mean(hashtag_count, na.rm = TRUE),
    time_c = log_time_elapsed - mean(log_time_elapsed, na.rm = TRUE),
    t4_c = t_4 - mean(t_4, na.rm = TRUE)
  )

#Checking for nonlinear effects for likes
model_7 <- lm(log_likes ~ 
                t_1 + t_2 + t_3 + t4_c +
                afinn_score + nrc_joy + nrc_trust + vader_compound +
                brightness_c + I(brightness_c^2) + 
                saturation_c + I(saturation_c^2) +
                object_c + I(object_c^2) +
                hashtag_c + I(hashtag_c^2) +
                time_c + I(time_c^2) +
                caption_length  +
                factor(influencer),
              data = sc_topics)

summary(model_7)

#checking for interaction for likes

model_8 <- lm(log_likes ~ 
                t_1 + t_2 + t_3 + t4_c +
                afinn_score + nrc_joy + nrc_trust + vader_compound + 
                brightness_c + object_c +
                hashtag_c + time_c +
                t4_c * object_c +          # emotional topic × visual complexity
                t4_c * hashtag_c +                  # emotional topic × hashtags
                object_c * brightness_c +   # objects × brightness
                factor(influencer),
              data = sc_topics)

summary(model_8)

#Checking for nonlinear effects for comments

model_9 <- lm(log_comments ~ 
                t_1 + t_2 + t_3 + t4_c +
                afinn_score + nrc_joy + nrc_trust + vader_compound +
                brightness_c + I(brightness_c^2) + 
                saturation_c + I(saturation_c^2) +
                object_c + I(object_c^2) +
                hashtag_c + I(hashtag_c^2) +
                time_c + I(time_c^2) +
                caption_length  +
                factor(influencer),
              data = sc_topics)

summary(model_9)

#checking for interaction for comments

model_10 <- lm(log_comments ~ 
                 t_1 + t_2 + t_3 + t4_c +
                 afinn_score + nrc_joy + nrc_trust + vader_compound + 
                 brightness_c + object_c +
                 hashtag_c + time_c +
                 t4_c * object_c +          # emotional topic × visual complexity
                 t4_c * hashtag_c +                  # emotional topic × hashtags
                 object_c * brightness_c +   # objects × brightness
                 factor(influencer),
               data = sc_topics)

summary(model_10)

#visualization
library(ggplot2)
install.packages("effects")
library(effects)


# Quadratic plot (for total_detected_object)
ggplot(sc_topics, aes(x = total_detected_objects, y = log_likes)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red") +
  labs(title = "Quadratic Effect of Number of Objects on Log Likes")


# Quadratic Plot for Saturation
ggplot(sc_topics, aes(x = saturation, y = log_likes)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red", se = TRUE) +
  labs(title = "Quadratic Effect of Saturation on Log Likes",
       x = "Saturation", y = "Log Likes")

# Quadratic Plot for Saturation (log_comments)
ggplot(sc_topics, aes(x = saturation, y = log_comments)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red", se = TRUE) +
  labs(title = "Quadratic Effect of Saturation on Log Comments",
       x = "Saturation", y = "Log Comments")
#Quadratic plot for brightness


#Plot for interaction
# Step 1: Calculate the means once (from training data)
mean_t4   <- mean(sc_topics$t_4, na.rm = TRUE)
mean_brightness <- mean(sc_topics$brightness, na.rm = TRUE)
mean_objects <- mean(sc_topics$total_detected_objects, na.rm = TRUE)
mean_hashtag <- mean(sc_topics$hashtag_count, na.rm = TRUE)
mean_time <- mean(sc_topics$log_time_elapsed, na.rm = TRUE)

# Step 2: Create centered versions in pred_grid too
pred_grid <- pred_grid %>%
  mutate(
    t4_c   = t_4 - mean_t4,
    brightness_c = brightness - mean_brightness,
    object_c = total_detected_objects - mean_objects,
    hashtag_c = hashtag_count - mean_hashtag,
    time_c = log_time_elapsed - mean_time
  )

# Step 3: Now predict using the model (it will find t4_c, brightness_c, etc.)
pred_grid$predicted <- predict(model_8, newdata = pred_grid)

# Step 4: Plot (same as before, but now it works)
ggplot(pred_grid, aes(x = total_detected_objects, y = predicted,
                      color = t_4, group = t_4)) +
  geom_line(linewidth = 1.2) +
  scale_color_gradient(low = "blue", high = "red",
                       name = "t_4 Probability\n(Niche Notes Topic)") +
  labs(title = "Interaction: Niche Notes Topic × Number of Objects on Likes",
       subtitle = "Higher t_4 strengthens the positive effect of more objects",
       x = "Number of Detected Objects",
       y = "Predicted Log(Likes)") +
  theme_minimal(base_size = 14)

##Visualization for slides --

# 1. Classify each post as Negative / Neutral / Positive
#    using the existing sentiment variables in sc_file
sentiment_classes <- sc_file %>%
  select(post_id, bing_net, afinn_score, nrc_net, vader_compound) %>%
  mutate(
    bing_class = case_when(
      bing_net < 0  ~ "Negative",
      bing_net == 0 ~ "Neutral",
      bing_net > 0  ~ "Positive"
    ),
    afinn_class = case_when(
      afinn_score < 0  ~ "Negative",
      afinn_score == 0 ~ "Neutral",
      afinn_score > 0  ~ "Positive"
    ),
    nrc_class = case_when(
      nrc_net < 0  ~ "Negative",
      nrc_net == 0 ~ "Neutral",
      nrc_net > 0  ~ "Positive"
    ),
    vader_class = case_when(
      vader_compound <= -0.05 ~ "Negative",
      vader_compound >=  0.05 ~ "Positive",
      TRUE                    ~ "Neutral"
    )
  )

# 2. Long-format summary table for stacked column chart
sentiment_summary <- sentiment_classes %>%
  select(post_id, bing_class, afinn_class, nrc_class, vader_class) %>%
  pivot_longer(
    cols = c(bing_class, afinn_class, nrc_class, vader_class),
    names_to = "lexicon",
    values_to = "sentiment_class"
  ) %>%
  mutate(
    lexicon = recode(
      lexicon,
      bing_class  = "Bing",
      afinn_class = "AFINN",
      nrc_class   = "NRC",
      vader_class = "VADER"
    ),
    sentiment_class = factor(
      sentiment_class,
      levels = c("Negative", "Neutral", "Positive")
    ),
    lexicon = factor(
      lexicon,
      levels = c("Bing", "AFINN", "NRC", "VADER")
    )
  ) %>%
  count(lexicon, sentiment_class, name = "n_posts") %>%
  group_by(lexicon) %>%
  mutate(
    pct_posts = n_posts / sum(n_posts)
  ) %>%
  ungroup()

# View final table
sentiment_summary

sentiment_summary_top3 <- sentiment_summary %>%
  filter(lexicon %in% c("AFINN", "NRC", "VADER"))

sentiment_summary_top3

library(ggplot2)
library(scales)

ggplot(sentiment_summary_top3,
       aes(x = lexicon, y = pct_posts, fill = sentiment_class)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Most captions are classified as positive across selected lexicons",
    x = NULL,
    y = "Share of posts",
    fill = NULL
  ) +
  theme_minimal(base_size = 13)
