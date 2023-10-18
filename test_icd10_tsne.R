library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(stringr)
# Download the locations of the embeddings.
tf = tempfile()
download.file(
  "https://github.com/kaneplusplus/icd-10-cm-embedding/raw/main/icd10_dl.rds",
  tf
)
dl = readRDS(tf)

# Read in the unspecified injury codes.
tf = tempfile()
download.file(
  dl$url[dl$year == 2019 & dl$emb_dim == 50],
  tf
)

icd10s = read_csv(tf) |>
  filter(str_detect(code, "^(G|I|J|K)")) |>
  mutate(desc = tolower(desc)) |>
  mutate('Leading Letter' = str_sub(code, 1, 1))

# Fit tSNE to the embedding.
tsne_fit = icd10s |>
  select(starts_with("V")) |>
  scale() |>
  Rtsne(perplexity = 10)
# Bind the tSNE values to the data set.
icd10p = bind_cols(
  icd10s |>
    select(-starts_with("V")),
  tsne_fit$Y |>
    as.data.frame() |>
    rename(tSNE1="V1", tSNE2="V2") |>
    as_tibble()
)
# Visualize the results.
ggplot(icd10p, aes(x = tSNE1, y = tSNE2, color = ‘Leading Letter‘)) +
  geom_point() +
  theme_minimal()
