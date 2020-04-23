---
title: "TCEC Season 15 SuFi"
author: "Josephus Viridis"
date: "2020-04-23"
output:
  html_document:
    df_print: paged
code_folding: yes
tags:
- chess
- TCEC
- SuFi
- TCEC Season 17
categories:
- R
- chess
- TCEC
- SuFi
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = F, message = F, warning = F, fig.align = 'center')
options(scipen = 1, digits = 5)
```


```{r}
library(tidyverse)
library(elo)
library(janitor)
library(flextable)
library(officer)
library(rchess)
library(bigchess)
library(expss)
f <- file.path("~/josephviruses.github.io/content/post/TCEC_Season_17_-_Superfinal.pgn")
sufi17 <- read.pgn(f, add.tags = c("WhiteElo", "BlackElo", "ECO", "GameDuration", "Opening", "Variation", "PlyCount", "TerminationDetails"))
sufi17_raw <- readLines(f)
tr <- tree_move(subset(sufi17, W1== "e4"), c("B2"))
g <- file.path("~/josephviruses.github.io/content/post/TCEC 17_Noomen Select.pgn")
noomen <- read.pgn(g, add.tags = c("White", "Black", "PlyCount", "ECO"))
open_plies <- as.numeric(noomen$PlyCount)
open_plies <- rep(open_plies, each=2)
gameplies <- as.numeric(sufi17$PlyCount)
with_evals <- gameplies - open_plies
#sum(with_evals)
evals_list <- str_match_all(sufi17_raw, "wv=(.*?),")
evals <- vector(length=length(evals_list))
for (i in 1:length(evals_list)) {evals[i] = evals_list[[i]][2]}
#evals <- as.numeric(evals)
evals <- na.omit(evals)
#length(evals)
#evals2 = vector(mode = "list", length = 100)
totalplies <- sum(as.numeric(gameplies))
#sufi17$Movetext
evals <- data.frame(evals = evals)
evals$game <- rep(1:100, times = with_evals)
engine = vector(mode = "list", length=100)
for (i in 1:100){
engine[[i]] = rep_len(c("SF", "Lc0"),  with_evals[i])
}
#evals$engine = unlist(engine)
evals_split <- split(evals, evals$game)
na_bind <- vector(mode="list", length=100)
for (i in 1:100){
  na_bind[[i]]$evals = rep(NA, times = open_plies[i])
  na_bind[[i]]$game = rep(i, times = open_plies[i])
}
na_bind <- bind_rows(na_bind)
#expand(na_bind, id = evals$engine, nesting(evals, game))
#evals$engine = NULL
evals <- bind_rows(na_bind, evals) %>% arrange(game)
#evals
evals <- evals %>% group_by(game) %>% mutate(plies = seq_along(evals))
evals <- evals %>% group_by(game) %>% mutate(
  
  engine = case_when(game %% 2 == 1 ~ rep_len(c("SF", "Lc0"), last(plies)),
                     TRUE ~ rep_len(c("Lc0", "SF"), last(plies)))
  
) 
evals <- evals %>% group_by(game) %>% mutate(Color = rep_len(c("White", "Black"), last(plies)))
evals_nona <- na.omit(evals)
library(hablar)
evals_nona <- evals_nona %>% convert(num(evals))
evals_2pl <- evals_nona %>% 
  split(evals_nona$game) %>% 
  map(~first(., n=2L)) %>% 
  bind_rows() 
```

The Superfinals of the 17th season of the [Top Engine Chess Championship (TCEC)](https://www.tcec-chess.com/) has just concluded and [Leela Chess Zero](https://lczero.org/) emerged as the champion against the mighty [Stockfish](https://stockfishchess.org/) with a final score of 52.5-47.5. Leela won 17 games (16 as white and 1 as black), drew 71 games, and lost 12 games (11 as white and 1 as black), to become the TCEC champion for the second time, after failing to qualify in season 16 although it was undefeated in the Premier Division. 

The breakdown of the results is shown in the table below.

<div align='center'>
```{r}
names <- sufi17 %>% select(TerminationDetails, White, Result)
names <- apply_labels(names, 
                      TerminationDetails = "Termination Details",
                      White = "Engine",
                      Result = "Result")
names %>%  calc_cro(TerminationDetails, list(total(), White %nest% Result)) %>% set_caption("Breakdown of results by details of termination and engine")

#table2[, 5] = NULL
#table1[,1] = NULL
#table1 <- bind_cols(table2, table1)
#table1_flex <- table1 %>% flextable()  %>% autofit()
#table1_flex %>% add_header_row(c("","Stockfish","Stockfish","Stockfish", "Leela Chess Zero", "Leela Chess Zero", "Leela Chess Zero", "Overall","Overall","Overall"), top = FALSE) %>% merge_h(part = "header") %>% autofit()
```
</div>

It shall be noted that the cutechess implementation by TCEC was not updated to properly convert Leela's evaluation into centipawns. The result was the very low centipawn evaluation scores shown by Lc0 even at more than 90% losing rate. This may account for the 7 losses from mates seen during the SuFi.




```{r, eval=F}
evals_2pl %>%  
  tab_cells(evals) %>%
  tab_rows(game) %>%
  tab_cols(engine %nest% Color) %>%
  tab_stat_mean() %>%
  tab_pivot()
#  calc_cro(evals, list(total(), engine %nest% Color))
```

```{r}
opening_evals <- evals_2pl %>% select(-plies, -Color) %>% pivot_wider(names_from = engine, values_from = evals) 
sufi17 <- bind_cols(sufi17, opening_evals)
```

Below is the plot of the results of games for each engine playing as white. No opening book was evaluated negatively by the engines. Note that because of contempt[^1], SF evaluates the opening positions very conservatively as black compared to when it is playing as white. Be that as it may, the difference between Leela's evaluation as white and SF's evaluation as black is remarkable. Some chatters say that this could have affected SF's performance. It has been claimed in chat that a contempt of 0 performs better against Leela. It is notable that many of the wins of Leela as white came from when its opening book evaluation was around 1 or when SF's opening book evaluation was less than 0.5. Most notable is game 94, (Queen's pawn game, Chigorin variation), which SF evaluated as 0.03 out of the opening book; Leela gave an evaluation of 1.18. In the reverse game that SF won as white, SF gave an evaluation of 1.55; on the other hand, Leela gave an evaluation of 1.1, which was not far from its evaluation when playing as white. While both of Leela and SF's evaluations agree when SF was playing as white, there were some notable exceptions, specially game 7, which Leela evaluated 0.6 and Stockfish evaluated as 1.43.


[^1]: SF submitted a special binary that is supposed to have a contempt of 24 when playing as white, and a contempt of 0 when playing as black (DC=double contempt in the TCEC label for Stockfish). However, the settings were  "misfixed" since with black the contempt $c=0$ applies only to the first three moves in every iteration, but not after; hence, the contempt is effectively $c=24$.

<center>
```{r, fig.width=8, fig.height=4, fig.cap="Results as white. The x-axis corresponds to the opening book evaluation by Leela, while the y-axis corresponds to the opening book evaluation by SF. The subplot on the left shows the results with Leela playing as white. The subplot on the right shows the results with Stockfish playing as white."}
library(stickylabeller)
library(plotly)
ann_text <- data.frame(Lc0 = 1.5, SF=0.1, White = factor("LCZero v0.24-sv-t60-3010", levels = c("LCZero v0.24-sv-t60-3010", "Stockfish 20200407DC")), Result=factor(c("1-0", levels = c("1-0", "1/2-1/2", "0-1"))), game = factor(94, levels=1:100))
ann_text2 <- data.frame(Lc0 = 0.4, SF=1.55, White = factor("Stockfish 20200407DC", levels = c("LCZero v0.24-sv-t60-3010", "Stockfish 20200407DC")), Result=factor(c("1-0", levels = c("1-0", "1/2-1/2", "0-1"))), game = factor(7, levels=1:100))
ann_texta <- data.frame(Lc0 = 0.1, SF=1.7, White = factor("LCZero v0.24-sv-t60-3010", levels = c("LCZero v0.24-sv-t60-3010", "Stockfish 20200407DC")), Result=factor(c("1-0", levels = c("1-0", "1/2-1/2", "0-1"))), game = factor(94, levels=1:100))
ann_textb <- data.frame(Lc0 = 0.1, SF=1.7, White = factor("Stockfish 20200407DC", levels = c("LCZero v0.24-sv-t60-3010", "Stockfish 20200407DC")), Result=factor(c("1-0", levels = c("1-0", "1/2-1/2", "0-1"))), game = factor(7, levels=1:100))
p <- sufi17 %>% 
  ggplot(aes(Lc0, SF, color = Result, group=game)) + geom_point() + geom_abline(data = data.frame(x=c(0,1.7), y = c(0,1.7)), alpha=0.5) + xlim(c(0,1.7)) + ylim(c(0,1.7))  + 
  facet_wrap(.~White#, labeller = label_glue('({.l}) White = {White}')
             ) + geom_text(data = ann_text, label = "Game 94", show.legend = F, color = "black") + geom_text(data = ann_text2, label = "Game 7", show.legend = F, color = "black") + geom_text(data = ann_texta, label = "(a)", show.legend = F, color = "black", size=3.5) + geom_text(data = ann_textb, label = "(b)", show.legend = F, color = "black", size=3.5)
ggplotly(p)
```
</center>

There were only two games that were won as black--one by Stockfish in game 16, where Leela, playing as white, gave an opening evaluation of 0.78 and Stockfish gave an opening evaluation of 0.16; and the other one by Leela in game 95, where Stockfish, playing as white, gave an opening evaluation of 0.90 and Leela gave an opening evaluation of 0.93. 

In game 16, Leela was optimistic about its position, giving evaluations $>1$ up to move 115, slowly declining afterwards. But typical of Leela, its overzealousness to push for the win could sometimes backfire, specially during the endgame, when Leela doesn't have enough time to analyze its position more deeply. In this case, Leela blundered the draw by moving `130 g6??`.

<div id="blundera", align='center'>
<iframe width=600 height=371 src="https://lichess.org/study/embed/fQhbqakr/C1LauzY1#259" frameborder=0></iframe>
</div>

```{r, eval=F}
sufi17 %>% filter(game=="7")
```

In game 95, a French opening, Leela showed its mastery of the French, overturning a great opening advantage for white by closing the position and converting the game into the start of the only reverse wins in the entire Superfinals.

<div id="fishblunder", align='center'>
<iframe width=600 height=371 src="https://lichess.org/study/embed/Wyz8ewGU/EGLZwmdW#73" frameborder=0></iframe>
</div>

One of the more memorable moments in the Superfinals for me was game 66, when Leela's evaluation jumped from +1.3 to +1.69 after the pawn sacrifice `25. c5!!`. Leela also attempted to sacrifice another pawn on c4 afterwards on move 28 (`28 ... Qxc4` does not work because  `28 ... Qxc4 29. Bb3 Qb4 30. Bxf7+ Kxf7 31. Rxd6 Bg4 32.f5 gxf5 33. Bd2 Qc4 34. Qg3 Kg8 35. e5 Red8` `36. Bc3 h5 37. Qe3 f4 38. Qd2 Rxd6 39. exd6` is totally winning for white) and successfully sacrificed a pawn on `29. h5` (en route to a thorn pawn?). After `29... gxh5`, Stockfish's pawn structure looked so bad.

<div id="pawnpush", align="center">
  <iframe width=600 height=371
    src="https://lichess.org/study/embed/Wyz8ewGU/ahYoKyxE#58"
    frameborder=0>
  </iframe>
</div>

The opening books for the Superfinals were provided by [Jeroen Noomen](http://blogchess2016.blogspot.com/2020/03/opening-selection-tcec-17-superfinal.html). The distribution of ECO codes as specified in Noomen's PGN is shown in the table below[^2]. 


[^2]: http://blogchess2016.blogspot.com/2020/03/opening-selection-tcec-17-superfinal.html


<center>
```{r}
noomen %>% mutate(ECO = str_extract(ECO, "[A-Z]+")) %>%
  group_by(ECO) %>%
  summarise(Total = n(), Openings = paste(White, collapse = "; ")) %>%
  pander::pander(justify = "left")
```
</center>

The table below shows the game numbers, the openings, variations, and ECO codes after transposition, the win rate (by Leela), the elo difference after each game (`elodiff`), the standard error of the elo differences, the likelihoods of superiority, the opening evaluations by Leela (`Lc0`), the opening evaluations by Stockfish (`SF`), and the result as white. Note that each opening is played as white by both engines in turns. SF plays each opening as white first.





```{r}
library(tidyverse)
library(elo)
library(expss)
library(rchess)
library(flextable)
library(officer)
```



```{r}
elo <- function(win_ratio) {400 * log10(win_ratio / (1-win_ratio))}
```



```{r}
denom95 <- function(win_ratio, total) qnorm(0.975) * sqrt(win_ratio * (1-win_ratio)/(total-1))
```


```{r}
LOS <- function(wins_losses, total) pnorm(total/2, sd = wins_losses)
LOS2 <- function(wins, losses) pnorm((wins-losses)/sqrt(wins+losses))
LOS3 <- function(wins, losses, draws) {
  total = wins + losses + draws
  exp = (wins/total)^wins * (losses/total)^losses * (draws/total)^draws
  factorials = factorial(total)/(factorial(wins)*factorial(losses)*factorial(draws))
  P = factorials * exp
  1-P
}
```



```{r}
library(hablar)
data <- sufi17 %>% separate(Result, into = c("points.White", "points.Black"), sep = "-") %>%
  convert(num(points.White), num(points.Black)) %>% mutate(points.White = replace_na(points.White, 0.5), points.Black = replace_na(points.Black, 0.5))
library(chron)
data <- data %>% mutate(GameDuration=chron(times. = GameDuration))
data <- data %>%
  mutate(points.Leela = (White == "LCZero v0.24-sv-t60-3010") * points.White + (Black == "LCZero v0.24-sv-t60-3010") * points.Black) %>%
  # calculate SF's scores
  mutate(points.SF = (White == "Stockfish 20200407DC") * points.White + (Black == "Stockfish 20200407DC") * points.Black) %>%
  mutate(results.Leela = case_when(points.Leela == 1~"Win", 
                                   points.Leela == 0.5~"Draw",
                                   points.Leela == 0~"Loss")) %>%
  mutate(results.SF = case_when(points.SF == 1~"Win", 
                                   points.SF == 0.5~"Draw",
                                   points.SF == 0~"Loss")) %>%
  # calculate cumulative scores
  mutate(Score.Leela = cumsum(points.Leela)) %>%
  mutate(Score.SF = cumsum(points.SF)) %>%
  mutate(total = row_number()) %>%
  mutate(draw_ratio = cumsum(points.Leela == points.SF)/total) %>%
  mutate(wins.Leela = cumsum(results.Leela=="Win")) %>%
  mutate(losses.Leela = cumsum(results.Leela=="Loss")) %>%
  mutate(wins.SF = cumsum(results.SF=="Win")) %>%
  mutate(losses.SF = cumsum(results.SF=="Loss")) %>%
  mutate(Draws = cumsum(results.Leela=="Draw")) %>%
  # calculate win rate of Leela
  mutate(win_rate.Leela = Score.Leela/total) %>%
  mutate(elodiff = elo(win_rate.Leela)) %>%
  # calculate ELO's and LOS's
  mutate(SE = elo(win_rate.Leela + denom95(win_rate.Leela, total))-elodiff) %>%
  mutate(LOS = LOS(total*(1-draw_ratio), total)) %>%
  mutate(LOS2 = LOS2(wins.Leela, losses.Leela)) %>%
  mutate(LOS3 = LOS3(wins.Leela, losses.Leela, Draws)) 
data_df <- data %>% 
  mutate(game = as.character(game)) %>%
  select(game, Opening, Variation, ECO, win_rate.Leela:LOS, Lc0, SF) %>%
  bind_cols(data.frame(Result = sufi17$Result)) %>%
  mutate(Result = as.character(Result)) %>%
  rename("Win Rate"=win_rate.Leela) 
data_df %>%
  flextable() %>% colformat_num(digits=2) %>% font(fontname = "Cormorant", part = "body") %>% font(fontname = "Cormorant",  part = "header") %>% bold(part = "header") %>% padding(padding = 3, part = "all") %>% autofit()
```

We see that elo difference after 100 games is around 17 but with large error bars (SE=70.15). I wonder how the elo difference will play out with larger sample size.

```{r}
data2 <- data %>%
  mutate(ECO2 = str_extract(ECO, "[A-Z]+")) %>%
  group_by(ECO2) %>%
  mutate(ECO2.Score.Leela = cumsum(points.Leela)) %>%
  mutate(ECO2.Score.SF = cumsum(points.SF)) %>%
  mutate(ECO2.total = row_number()) %>%
  mutate(ECO2.draw_ratio = cumsum(points.Leela == points.SF)/ECO2.total) %>%
  mutate(ECO2.wins.Leela = cumsum(results.Leela=="Win")) %>%
  mutate(ECO2.losses.Leela = cumsum(results.Leela=="Loss")) %>%
  mutate(ECO2.wins.SF = cumsum(results.SF=="Win")) %>%
  mutate(ECO2.losses.SF = cumsum(results.SF=="Loss")) %>%
  mutate(ECO2.Draws = cumsum(results.Leela=="Draw")) %>%
  mutate(ECO2.win_rate.Leela = ECO2.Score.Leela/ECO2.total) %>%
  mutate(ECO2.elodiff = elo(ECO2.win_rate.Leela)) %>%
  mutate(ECO2.SE = elo(ECO2.win_rate.Leela + denom95(ECO2.win_rate.Leela, ECO2.total))-ECO2.elodiff) %>%
  mutate(ECO2.LOS = LOS(ECO2.total*(1-ECO2.draw_ratio), ECO2.total)) #%>%
  #mutate(ECO2.LOS2 = LOS2(wins.Leela, losses.Leela)) %>%
  #mutate(ECO2.LOS3 = LOS3(wins.Leela, losses.Leela, Draws)) 
```

We can now see the estimated ELO differences at the last of game of each ECO group of openings.

```{r}
data2 %>%
  slice(n()) %>% select(starts_with("ECO2")) %>% 
    rename_all(.funs=funs(sub("ECO2[.]", "", .))) %>%
  flextable() %>% autofit()
```


We see that Leela racked up the lead through the A and E openings in this season.

Looking at the opening evaluations by ECO family of codes, we can see that the opening evaluations do not differ when Leela played white and SF played black. But the opening evaluations differed a lot when SF played black. Notice though that D openings were evaluated almost similarly by Stockfish and Leela.

<center>
```{r, fig.height=8, fig.width=6}
library(stickylabeller)
data_df %>% 
  mutate(White=rep_len(c("SF", "Lc0"), 100)) %>%
  mutate(ECO = str_extract(ECO,"[A-Z]+")) %>%
  pivot_longer(cols = c("Lc0", "SF"), names_to = "Engine", values_to = "Opening Evaluation") %>%
  ggplot(aes(`Opening Evaluation`, color = Engine)) + geom_density() + facet_wrap(.~ECO+White, labeller = label_glue("({.l}) White={White}\nECO={ECO}"), nrow=5)
```
</center>
Quite interesting too is the number of moves in each game (mean =`r round(mean(sufi17$NMoves),2)`, sd =`r round(sd(sufi17$NMoves),2)`). Games were considerably quite shorter if Stockfish was playing white (mean =`r round(mean(sufi17$NMoves[sufi17$White=="Stockfish 20200407DC"]),2)`, sd =`r round(sd(sufi17$NMoves[sufi17$White=="Stockfish 20200407DC"]),2)`), specially when it was winning (mean = `r round(mean(sufi17$NMoves[sufi17$White=="Stockfish 20200407DC" & sufi17$Result=="1-0"]),2)`, sd = `r round(sd(sufi17$NMoves[sufi17$White=="Stockfish 20200407DC" & sufi17$Result=="1-0"]),2)`). Games took a while to finish when Leela was playing white (mean = `r round(mean(sufi17$NMoves[sufi17$White=="LCZero v0.24-sv-t60-3010" ]),2)`, sd = `r round(sd(sufi17$NMoves[sufi17$White=="LCZero v0.24-sv-t60-3010"]),2)`), specially when it was winning (mean = `r round(mean(sufi17$NMoves[sufi17$White=="LCZero v0.24-sv-t60-3010" & sufi17$Result=="1-0"]),2)`, sd = `r round(sd(sufi17$NMoves[sufi17$White=="LCZero v0.24-sv-t60-3010" & sufi17$Result=="1-0"]),2)`). However, SF lost as white (game 95, 93 moves) in much shorter time than it did winning as black (game 16, 196 moves, coming via a long series of high level shuffling from a fortress-y position, after Leela pressed for activity as discussed above).

<center>
```{r, fig.cap="Distribution of number of moves based on results and\neach engine playing as white."}
library(ggstance)
p2 <- sufi17 %>%
  ggplot(aes(x=Result, y=NMoves, color = Result)) +
  geom_boxplot() +
  facet_wrap(~White) +
  labs(x=NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y=NULL)
ggplotly(p2)
```
</center>

We also see that for this SuFi, the rooks and the king moved the most, perhaps due to many pawn and rook endings.

<center>
```{r}
# cols = c("W_B_moves", "W_K_moves", "W_N_moves", "W_O_moves", "W_Q_moves", "W_R_moves", "B_B_moves", "B_K_moves", "B_N_moves", "B_O_moves", "B_Q_moves", "B_R_moves", "B_moves", "K_moves", "N_moves", "O_moves", "Q_moves", "R_moves"
p4 <- sufi17 %>% 
  gather(key="Piece", value="NMoves", B_moves:N_moves, Q_moves:R_moves) %>%
  mutate(Piece=str_remove(Piece, "_moves")) %>%
  ggplot(aes(x=Piece, y = NMoves, color = Piece)) + 
  geom_boxplot() +
  coord_flip() +
  ggtitle("Distribution of number of piece moves.") +
  labs(x=NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y=NULL)
ggplotly(p4)
```
</center>

Finally, Leela's evaluations seemed to agree with SF's evaluation up to a certain centipawn value only, around $(-3,3)$. The reason was that TCEC had yet to update the system to reflect correct centipawn evaluation. This resulted in a lot of mates during the competition. Here is an attempt to model Leela's centipawn evaluation based on SF's evaluation. I have stored the matched evaluations throughout the games [here](https://raw.githubusercontent.com/josephviruses/josephviruses.github.io/master/content/post//evals_tcec.csv). This CSV file contains all of the evaluations in all games for which Leela's evaluation is in $(-4,4)$ and Stockfish's evaluation is in $(-20,20)$.  The reason for the choice of limits is nothing special--one engine evaluation seem to be well-behaved with respect to the other engine evaluation. I have also removed the last move of the engine with the greater number of moves so that the number of evaluations will match.

```{r}
evals_comp <- evals_nona %>% select(-plies, -Color) %>% pivot_wider(names_from = c("engine"), values_from = "evals") %>% mutate(
  nLc0 = unlist(map(Lc0, length)),
  nSF = unlist(map(SF, length))
) %>%
  rename(Lc0b=Lc0, SFb=SF) %>%
  mutate(
    Lc0 = case_when(nLc0 > nSF ~ map(Lc0b, function(x) {x=x[-length(x)]}),
                    TRUE~ map(Lc0b, function(x) x)),
    SF = case_when(nSF > nLc0 ~ map(SFb, function(x) {x=x[-length(x)]}),
                    TRUE~ map(SFb, function(x) x))
  ) %>% select(-SFb, -Lc0b) %>%
  unnest(keep_empty = T) %>%
  filter(Lc0 < 20, Lc0 >-20, SF < 20, SF > -20)
evals_comp %>% ggplot(aes(SF, Lc0)) + geom_point()
```

I fitted a logistic curve to the evaluations. To do this, I added 20 to SF's evals and 4 to Leela's evals and proceeded to fit the model in R.

```{r, echo=TRUE, eval=FALSE}
evals_comp <- read.csv("evals_tcec.csv")
```


```{r, echo=TRUE}
x <- evals_comp$SF + 20
y <- evals_comp$Lc0 + 4
data.df <- data.frame(x=x, y=y)
max(y)
logit <- qlogis
model.0 <- lm(logit(y/12) ~ x, data=data.df) 
summary(model.0)
phi1 <- 12
phi2 <- coef(model.0)[1]
phi3 <- coef(model.0)[2]
model<-nls(y~phi1/(1+exp(-(phi2+phi3*x))),
 start=list(phi1=phi1,phi2=phi2,phi3=phi3),data=data.df,trace=TRUE)
summary(model)
```

```{r, echo=TRUE}
#set parameters
phi1<-coef(model)[1]
phi2<-coef(model)[2]
phi3<-coef(model)[3]
x<-c(min(data.df$x):max(data.df$x)) #construct a range of x values bounded by the data
y<-phi1/(1+exp(-(phi2+phi3*(x)))) #predicted SF's evals
predict<-data.frame(x=x-20,y=y-4) #create the prediction data frame
# create a plot of actual values and the predictions from fitted model
ggplot(data=evals_comp,aes(x=SF,y=Lc0))+
geom_point(size=1)+theme_bw()+
labs(x='SF',y='Lc0')+
geom_line(data=predict,aes(x=x,y=y), size=1, color = "blue")
```


