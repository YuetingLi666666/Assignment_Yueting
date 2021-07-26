### Visualising Words in newsgroups(take long to run)

set.seed(1234)
words_by_newsgroup %>%
  filter(n > 0) %>%
  ggplot(aes(label = word,
             size = n)) +
  geom_text_wordcloud() +
  theme_minimal() +
  facet_wrap(~newsgroup)


This graph visualise the correlation between news data sources provided as a network

set.seed(2017)
newsgroup_cors %>%
  filter(correlation > .7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, 
                     width = correlation)) +
  geom_node_point(size = 6, 
                  color = "lightblue") +
  geom_node_text(aes(label = name),
                 color = "red",
                 repel = TRUE) +
  theme_void()



set.seed(2017)
newsgroup_cors %>%
  filter(correlation > .75) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, 
                     width = correlation)) +
  geom_node_point(size = 6, 
                  color = "lightblue") +
  geom_node_text(aes(label = name),
                 color = "red",
                 repel = TRUE) +
  theme_void()


tf_idf %>%
  group_by(newsgroup) %>%
  slice_max(tf_idf, 
            n = 5) %>%
  ungroup() %>%
  mutate(word = reorder(word, 
                        tf_idf)) %>%
  ggplot(aes(tf_idf, 
             word, 
             fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, 
             scales = "free") +
  labs(x = "tf-idf", 
       y = NULL)


### Visualizing a network of bigrams with ggraph

set.seed(123)
ggraph(bigram_graph, layout = "nicely") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), 
                 vjust = 1, 
                 hjust = 1)

This graph visualise the correlation between news data sources provided as a network

set.seed(2017)
newsgroup_cors %>%
  filter(correlation > .7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, 
                     width = correlation)) +
  geom_node_point(size = 6, 
                  color = "lightblue") +
  geom_node_text(aes(label = name),
                 color = "red",
                 repel = TRUE) +
  theme_void()



unique(tf_idf$newsgroup)


require(tidygraph)
gr <- create_notable('bull') %>%
  mutate(class = sample(letters[1:3], n(), replace = TRUE))

GAStech_graph <- tbl_graph(nodes = GAStech_nodes_new,
                           edges = GAStech_edges_aggregated, 
                           directed = TRUE)
glimpse(GAStech_graph)

GAStech_graph %>%
  activate(edges) %>%
  arrange(desc(Weight))


# NOT RUN {
fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(fruit, "a")
str_detect(fruit, "^a")
str_detect(fruit, "a$")
str_detect(fruit, "b")
str_detect(fruit, "[aeiou]")

# Also vectorised over pattern
str_detect("aecfg", letters)

# Returns TRUE if the pattern do NOT match
str_detect(fruit, "^p", negate = TRUE)
# }

require(devtools)
install_github("Displayr/flipTime")

GAStech_nodes_new <- GAStech_nodes
GAStech_nodes_new$Name[GAStech_nodes_new$Name == "sten.sanjorge.jr."] <- "sten.sanjorge.jr"

GAStech_graph <- tbl_graph(nodes = GAStech_nodes_new,
                           edges = GAStech_edges_aggregated, 
                           directed = TRUE)

GAStech_graph %>%
  activate(edges) %>%
  arrange(desc(Weight))

GAStech_edges_aggregated <- GAStech_edges_new1 %>%
  left_join(GAStech_nodes_new, by = c("From" = "Name")) %>%
  rename(from = id) %>%
  left_join(GAStech_nodes_new, by = c("To" = "Name")) %>%
  rename(to = id) %>%
  group_by(from, to) %>%
  summarise(weight = n()) %>%
  filter(weight > 1)
  