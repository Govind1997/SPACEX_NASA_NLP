#importing essential datasets from twitter.
#install.packages("rtweet")
library(rtweet)
SpaceX<- search_tweets(
  "#SpaceX", n = 9000, include_rts = FALSE
)
NASA <- search_tweets(
  "#NASA", n = 9000, include_rts = FALSE
)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(scales)
data(stop_words)
#creating tokens and removing stop words from SpaceX DF
tidy_SpaceX <- SpaceX %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_SpaceX)

#Calling for the word counts
tidy_SpaceX %>%
  count(word, sort=TRUE)
tidy_NASA <- NASA %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_NASA)
tidy_NASA %>%
  count(word, sort=TRUE)
frequency <- bind_rows(mutate(tidy_SpaceX, company = "SpaceX"),
                       mutate(tidy_NASA, company= "NASA")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(company, word) %>%
  group_by(company) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, 'SpaceX')
ggplot(frequency, aes(x=proportion, y=`NASA`,
                      color = abs(`NASA`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~company, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "NASA", x=NULL)
cor.test(data=frequency[frequency$company == "SpaceX",],
         ~proportion + `NASA`)
freq_words <- bind_rows(mutate(tidy_SpaceX, company = "SpaceX"),
                        mutate(tidy_NASA, company= "NASA")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(company, word) %>%
  group_by(company)
plot_freq <- freq_words %>%
  bind_tf_idf(word, company, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(company, levels = c("SpaceX","NASA")))
plot_freq %>%
  group_by(company) %>%
  top_n(20, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()