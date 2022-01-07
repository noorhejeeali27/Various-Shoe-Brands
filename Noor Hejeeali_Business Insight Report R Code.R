.libPaths("C:/Rlibs")

# Importing all PDF files from the same folder
#install.packages("pdftools")
#library(pdftools) # we need this library to use pdf_text
#setwd("/Users/noorh/OneDrive/Desktop/Text Analytics/insight_finding")
#nm <- list.files(path="/Users/noorh/OneDrive/Desktop/Text Analytics/insight_finding")
#my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))

#Mizuno Reviews Text
library(dplyr)
library(tidytext)

#Mizuno Reviews
text1 <- c("Enerzy is what Mizuno needed to stay relevant in the game. The Wave Rider 25 is energetic with the energy return and bounce that hits just right without being too soft or too jarring. The midsole and Wave Plate are in total sync with each other to make every stride smooth. Speaking of that plate, it provides just a little bit of stability for those who don't quite pronate to the extreme.")
text2 <- c("I'm impressed that Mizuno kept the slipper-like step-in feel that has decent lockdown. When I first tried these on, I was reminded of what made the previous versions good for their era. However, these aren't the Riders of the early 2000s, as Mizuno does a good job of blending the new with the classic. If you like Mizuno, you will like this shoe, and it'll probably last forever.")
text3 <- c("Mizuno's X10 rubber outsole is tough as a bowl of nails, without any milk. I put it through plenty of reasonable surfaces around town, yet it keeps coming back like it's brand new.")
text4 <- c("On top of the outsole, Mizuno spread its Enerzy foam from the heel to the rest of the shoe for a full-length midsole. I'm a big fan of the improved softness combined with this more subtle version of the Wave Plate. The Wave Rider 25 isn't as much of a stability shoe, so the Wave Plate isn't nearly as extreme as the one on the Wave Inspire 17. This shoe feels much more neutral rolling through your stride.")
text5 <- c("Moving to the upper, I have no complaints about the recycled mesh. As mentioned earlier, it's green in more ways than one which is a huge plus for me. A well-padded heel counter keeps my foot in place and eliminates any heel slippage. The gusseted tongue also contributes to the secure fit, but let's not mention it too much (say "gusseted tongue" three times and Thomas appears, just like Beetlejuice).")
text6 <- c("This is a backloaded shoe, with much more stack in the heel (hello, 12 mm drop). Heel strikers will like this shoe, but the midfooters of the world, like myself, may feel a little left out. That 12 mm drop is noticeable, especially if you're used to 4-8 mm drop shoes.")
text7 <- c("The Mizuno truthers out there won't like this take: I don't like the 12 mm drop. I didn't love it in the Wave Inspire 17, and it doesn't work here either. It pushes me to heel strike and is a bit too steep for my liking. If you can teach me to love the extra drop height, I'm all ears.")

mizuno_rating <- rbind(text1, text2, text3, text4, text5, text6, text7)
colnames(mizuno_rating)[1] <- "text"
mizuno_rating <- as.data.frame(mizuno_rating)

id <- c(1,2,3,4,5,6,7)
id <- matrix(id, ncol=1)

mizuno_rating <- cbind(id,mizuno_rating)

#create our structured data
tidy_mizuno <- mizuno_rating %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

#Nike Pegasus Review
text8 <- c("When I tried it on for the first time, the updated upper felt way softer and more comfortable than the previous version. It fit like a glove.")
text9 <- c("On that first run, the ride felt really comfortable with plenty of long-distance cushioning but it didn't feel like the versatile Pegasus models of days gone by.")
text10 <- c("The Pegasus 38 is more suited to easy runs and struggles with faster-paced runs because of how much softer it is. This workhorse has lost its wings.")
text11 <- c("The main reason you'd choose the Pegasus 38 over any other daily trainer in its price class is durability. It feels like a really well-built, durable trainer with an outsole that could easily do over 1000 kilometres.")
text12 <- c("It's also a great option for beginner runners who only want to buy one pair of shoes that has a comfortable fit and will last a very, very long time.")
text13 <- c("It's on those tough days that I'm especially thankful for shoes like the Nike Air Zoom Pegasus 38. The workhorse trainer-which was first introduced in 1983-continues to be a shoe that any runner, at any level, can use and love.")
text14 <- c("I always find that Nike runs a bit narrow and a bit small. ")
text15 <- c("In many ways the Nike Air Zoom Pegasus 38 sneakers really are the Goldilocks of running shoes: Just the right amount of cushion and responsiveness, plus a low-profile silhouette and several stylish color options that make these an easy choice to wear while running errands or with any athleisure look.")
text16 <- c("Though they're a bit heavier than one might expect, I didn't really notice this until I was on slightly longer runs. Perhaps best of all, they provided an adequate amount of support so that my knees and hips felt good from start to finish.")

nike_rating <- rbind(text8, text9, text10, text11, text12,text13,text14,text15,text16)
colnames(nike_rating)[1] <- "text"
nike_rating <- as.data.frame(nike_rating)

id <- c(8,9,10,11,12,13,14,15,16)
id <- matrix(id, ncol=1)

nike_rating <- cbind(id,nike_rating)

#create our structured data
tidy_nike <- nike_rating %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))


#Asics Gel Kayano 27 Review
text17 <- c(" The heel portion is firm and responsive while the forefoot is plushy and springy.")
text18 <- c("Durability is a strength of the Kayano 27. They show minimal wear even after putting close to 100 miles on them. These are built to last through many miles at many different paces.")
text19 <- c("The material used for the outsole is AHAR (ASICS High Abrasion Rubber), a highly durable material that provides traction in both wet and dry conditions. After a hundred miles, a minimal amount of wear showed on the outsole.")
text20 <- c("Overall, the ASICS Kayano 27 is a dependable premier stability trainer. The ride they provided brought me back to them again and again after my review mileage.")
text21 <- c("If you are a runner looking for a premier stability shoe that delivers every time you lace them up I would recommend the ASICS Kayano 27.")
text22 <- c("These aspects can cause the back portion of the trainers to be inflexible, stiff and heavy.")
text23 <- c("Two of the most noticeable things about the Kayano, though, are its step-in feel and accommodating fit.")
text24 <- c("The GEL pad, positioned in the heel, squishes when you land to soak up shock and disperses it evenly throughout the shoe. It also helps create a smooth transition from heel to toe, which testers appreciated.")
text25 <- c("All experts agree this shoe can't pass as a fast shoe. It's heavy, clunky, has a lot of support and, therefore, can't help you pick up the pace. ")
text26 <- c("While the price tag is high, those who commented on it said Kayano 27 is worth the money. ")

asics_rating <- rbind(text17, text18, text19,text20,text21,text22,text23,text24,text25,text26)
colnames(asics_rating)[1] <- "text"
asics_rating <- as.data.frame(asics_rating)

id <- c(17,18,19,20,21,22,23,24,25,26)
id <- matrix(id, ncol=1)

asics_rating <- cbind(id,asics_rating)

#create our structured data
tidy_asics <- asics_rating %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))


#Brooks Adrenaline GTS 21 Review
text27 <- c("I knew from experience Brooks trainers take a little longer to break-in. ")
text28 <- c("They provided a natural and comfortable ride from the first stride. By the end of the run, I was looking forward to more miles in them.")
text29 <- c("It was a reliable ride whether it was 2 miles or 12 miles. The midsole was built to last for hundreds of miles.")
text30 <- c("Although a great shoe, and one I'll continue to use, the shoe is not perfect. There are two main negatives that linger in my mind. The biggest issue for me was the 12mm drop.")
text31 <- c("The other main drawback for me was the break-in time. Most running shoes are ready to go straight out of the box, yet these took more than 30 miles to get a good feel for them.")
text32 <- c("Priced at 130 dollars, the Brooks Adrenaline is worth every penny. They provided me with countless comfortable miles because of the execution of all aspects.")
text33 <- c("As expected, the ride feels smooth and very stable. This is definitely a firm shoe, but not unpleasantly so. The rigid design ensures good transfer of energy as your foot rolls, while preventing unwanted twisting.")
text34 <- c("While this shoe is lighter, smoother and more flexible than previous editions, the Adrenaline GTS doesn't have the liveliness to handle faster track and fartlek workouts as well as some performance-oriented stability models.")
text35 <- c("The cushioning makes it a great shoe for longer rides and daily use. The upper is breathable and supportive with an updated engineered mesh. ")
text36 <- c(" It's not recommended for sprints or faster workouts. Some users also suggested that a lower heel drop-off would be preferable.")

brooks_rating <- rbind(text27,text28,text29,text30,text31,text32,text33,text34,text35,text36)
colnames(brooks_rating)[1] <- "text"
brooks_rating <- as.data.frame(brooks_rating)

id <- c(27,28,29,30,31,32,33,34,35,36)
id <- matrix(id, ncol=1)

brooks_rating <- cbind(id,brooks_rating)

tidy_brooks <- brooks_rating %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))


###SENTIMENT ANALYSIS

###BROOKS####
#Getting sentiment analysis for Brooks

brooks_afinn <- tidy_brooks %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

brooks_bing_and_nrc <- bind_rows(
  tidy_brooks%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  tidy_brooks %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(brooks_afinn, brooks_bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

#Most common positive and negative words for brooks
brooks_bing_counts <- tidy_brooks %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

brooks_bing_counts

brooks_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


###ASICS######
#Getting sentiment analysis for Asics

asics_afinn <- tidy_asics %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

asics_bing_and_nrc <- bind_rows(
  tidy_asics%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  tidy_asics %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(asics_afinn, asics_bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

#Most common positive and negative words for asics
asics_bing_counts <- tidy_asics %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

asics_bing_counts

asics_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

###NIKE####
#Getting sentiment analysis for Nike

nike_afinn <- tidy_nike %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

nike_bing_and_nrc <- bind_rows(
  tidy_nike%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  tidy_brooks %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(nike_afinn, nike_bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

#Most common positive and negative words for nike
nike_bing_counts <- tidy_nike %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

nike_bing_counts

nike_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

###MIZUNO####
#Getting sentiment analysis for mizuno

mizuno_afinn <- tidy_mizuno %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

mizuno_bing_and_nrc <- bind_rows(
  tidy_mizuno%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  tidy_mizuno %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(mizuno_afinn, mizuno_bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

#Most common positive and negative words for mizuno
mizuno_bing_counts <- tidy_mizuno %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

mizuno_bing_counts

mizuno_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#ZIPFs law 
###MIZUNO####
mizuno <- mizuno_rating %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort=TRUE) %>%
  ungroup()

mizuno_total_words <- mizuno %>%
  group_by(id) %>%
  summarize(total=sum(n))

mizuno_words <- left_join(mizuno, mizuno_total_words)%>%
  filter(word %in% c("smooth", "secure", "strike", "jarring", "steep", "love", "stability"))

print(mizuno_words)

#Calculate rank based on frequency instead of calculating ZIPF's law
mizuno_freq_by_rank <- mizuno_words %>%
  group_by(id) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
mizuno_freq_by_rank

#Stop words are in the upper left
#Unique words are in the bottom right (high rank, low frequency)
mizuno_freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=word))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

#TF-IDF
mizuno_brand_words <- mizuno_words %>%
  bind_tf_idf(word, id,n)

mizuno_brand_words 

mizuno_brand_words %>%
  arrange(desc(tf_idf))

mizuno_uniqueness <- mizuno_brand_words %>%
  arrange(desc(tf_idf))

#Graph for Term Frequency
mizuno_brand_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(id) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=id))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~id, ncol=2, scales="free")+
  coord_flip()

##NIKE###
nike <- nike_rating %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort=TRUE) %>%
  ungroup()

nike_total_words <- nike %>%
  group_by(id) %>%
  summarize(total=sum(n))

#Left Joins takes everything from the left side and intersect
nike_words <- left_join(nike, nike_total_words)%>%
  filter(word %in% c("easy", "faster", "struggles", "soft", "price", "comfortable", "durable"))

print(nike_words)

#Calculate rank based on frequency instead of calculating ZIPF's law
nike_freq_by_rank <- nike_words %>%
  group_by(id) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
nike_freq_by_rank

#Stop words are in the upper left
#Unique words are in the bottom right (high rank, low frequency)
nike_freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=word))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

#TF-IDF
nike_brand_words <- nike_words %>%
  bind_tf_idf(word, id,n)

nike_brand_words 

nike_brand_words %>%
  arrange(desc(tf_idf))

nike_uniqueness <- nike_brand_words %>%
  arrange(desc(tf_idf))


#Graph for Term Frequency
nike_brand_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(id) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=id))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~id, ncol=2, scales="free")+
  coord_flip()

##Asics###
asics <- asics_rating %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort=TRUE) %>%
  ungroup()

asics_total_words <- asics %>%
  group_by(id) %>%
  summarize(total=sum(n))

#Left Joins takes everything from the left side and intersect
asics_words <- left_join(asics, asics_total_words)%>%
  filter(word %in% c("durability", "dependable", "worth","recommend", "stiff", "price", "clunky"))

print(asics_words)

#Calculate rank based on frequency instead of calculating ZIPF's law
asics_freq_by_rank <- asics_words %>%
  group_by(id) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
asics_freq_by_rank

#Stop words are in the upper left
#Unique words are in the bottom right (high rank, low frequency)
asics_freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=word))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

#TF-IDF
asics_brand_words <- asics_words %>%
  bind_tf_idf(word, id,n)

asics_brand_words 

asics_brand_words %>%
  arrange(desc(tf_idf))

asics_uniqueness <- asics_brand_words %>%
  arrange(desc(tf_idf))

#Graph for Term Frequency
asics_brand_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(id) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=id))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~id, ncol=2, scales="free")+
  coord_flip()

##Brooks###
brooks <- brooks_rating %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort=TRUE) %>%
  ungroup()

brooks_total_words <- brooks %>%
  group_by(id) %>%
  summarize(total=sum(n))

brooks_words <- left_join(brooks, brooks_total_words)%>%
  filter(word %in% c("comfortable", "stable","worth", "perfect", "supportive", "rigid", "issue", "break"))

print(brooks_words)

#Calculate rank based on frequency instead of calculating ZIPF's law
brooks_freq_by_rank <- brooks_words %>%
  group_by(id) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
brooks_freq_by_rank

#Stop words are in the upper left
#Unique words are in the bottom right (high rank, low frequency)
brooks_freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=word))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

#TF-IDF
brooks_brand_words <- brooks_words %>%
  bind_tf_idf(word, id,n)

brooks_brand_words 

brooks_brand_words %>%
  arrange(desc(tf_idf))

brooks_uniqueness <- brooks_brand_words %>%
  arrange(desc(tf_idf))

#Graph for Term Frequency
brooks_brand_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(id) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=id))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~id, ncol=2, scales="free")+
  coord_flip()

###MIZUNO Bigrams
mizuno_bigram <- mizuno_rating %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

mizuno_bigram #We want to see the bigrams (words that appear together, "pairs")

mizuno_bigram%>%
  count(bigram, sort = TRUE) 

#to remove stop words from the bigram data
library(tidyr)
#Separating the bigram and tokenizing
mizuno_bigrams_separated <- mizuno_bigram%>%
  separate(bigram, c("word1", "word2"), sep = " ")

mizuno_bigrams_filtered <- mizuno_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
mizuno_bigram_counts <- mizuno_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

mizuno_bigram_counts

###BROOKS

brooks_bigram <- brooks_rating %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)


brooks_bigram

brooks_bigram%>%
  count(bigram, sort = TRUE) 

library(tidyr)
#Separating the bigram and tokenizing
brooks_bigrams_separated <- brooks_bigram%>%
  separate(bigram, c("word1", "word2"), sep = " ")

brooks_bigrams_filtered <- brooks_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
brooks_bigram_counts <- brooks_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

brooks_bigram_counts

###ASICS
asics_bigram <- asics_rating %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)


asics_bigram 

asics_bigram%>%
  count(bigram, sort = TRUE)

library(tidyr)

asics_bigrams_separated <- asics_bigram%>%
  separate(bigram, c("word1", "word2"), sep = " ")

asics_bigrams_filtered <- asics_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
asics_bigram_counts <- asics_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

asics_bigram_counts

###NIKE

nike_bigram <- nike_rating %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

nike_bigram

nike_bigram%>%
  count(bigram, sort = TRUE) 

library(tidyr)
#Separating the bigram and tokenizing
nike_bigrams_separated <- nike_bigram%>%
  separate(bigram, c("word1", "word2"), sep = " ")

nike_bigrams_filtered <- nike_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
nike_bigram_counts <- nike_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

nike_bigram_counts


###VISUALIZING A BIGRAM NETWORK
##MIZUNO
library(igraph)
mizuno_bigram_graph <- mizuno_bigram_counts %>%
  filter(n>0) %>%
  graph_from_data_frame()

mizuno_bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(mizuno_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

###Brooks
library(igraph)
brooks_bigram_graph <- brooks_bigram_counts %>%
  filter(n>0) %>%
  graph_from_data_frame()

brooks_bigram_graph

library(ggraph)
ggraph(brooks_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

##Asics
library(igraph)
asics_bigram_graph <- asics_bigram_counts %>%
  filter(n>0) %>%
  graph_from_data_frame()

asics_bigram_graph

library(ggraph)
ggraph(asics_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

##Nike
library(igraph)
nike_bigram_graph <- nike_bigram_counts %>%
  filter(n>0) %>%
  graph_from_data_frame()

nike_bigram_graph

library(ggraph)
ggraph(nike_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


