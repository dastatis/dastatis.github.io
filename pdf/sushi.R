# 環境をクリア
rm(list=ls())
# 必要なパッケージの読み込み
library(tidyverse)
library(ggrepel)
library(cmdstanr)

setwd("~/Downloads/chapter01_19/chapter01_19/chapter07小杉/mac")

# library(rstan)
# options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)

# 文字化けを防ぐコード(Mac用呪文)
old = theme_set(theme_gray(base_family = "HiraKakuProN-W3"))

dat_i <- read.table("data/sushi3.idata",header = FALSE) %>% 
  rename(
    sushiID = V1,
    name = V2,
    style = V3,
    type1 = V4,
    type2 = V5,
    kotteri = V6,
    freq_eat = V7,
    price = V8,
    freq_sell = V9
  )

head(dat_i)

dat_u <- read.table("data/sushi3.udata",header = FALSE) %>% 
  rename(
    userID = V1,
    sex = V2,
    age = V3,
    rt = V4,
    V_15pref = V5,
    V_15region = V6,
    V_15ew = V7,
    V_Npref = V8,
    V_Nregion = V9,
    V_New = V10,
    V_all = V11
  )

dat_order10 <- read.table("data/sushi3a.5000.10.order",header = FALSE)
dat_score100 <- read.table("data/sushi3b.5000.10.score",header = FALSE) %>% 
  dplyr::select(V1,V3,V9,V13,V30) %>% 
  mutate(Gid=dat_u$V_15ew+1) %>% 
  filter_all(all_vars(.!=-1))
  # dplyr::select(V1,V2,V3,V4,V5,V7,V8,V9,V27,V30) 
  # rename(
  #   えび = V1,
  #   穴子 = V2,
  #   まぐろ = V3,
  #   いか = V4,
  #   うに = V5,
  #   いくら = V7,
  #   玉子 = V8,
  #   とろ = V9,
  #   鉄火巻 = V27,
  #   かっぱ巻 = V30
  # )


# dat_score100[dat_score100==-1] <- 100

dat_score100 %>% drop_na() %>%  arrange()

demo <- 
dat_score100 %>% 
  mutate(
    C_1_9 = V1 - V9 + 5,
    C_1_30 = V1 - V30 + 5,
    C_1_3 = V1 - V3 + 5,
    C_1_13 = V1 - V13 + 5,
    
    C_9_30 = V9 - V30 + 5,
    C_9_3 = V9 - V3 + 5,
    C_9_13 = V9 - V13 + 5,
    
    C_30_3 = V30 - V3 + 5,
    C_30_13 = V30 - V13 + 5,
  
    C_3_13 = V3 - V13 + 5
  ) %>% 
  dplyr::select(starts_with("C_"),Gid)

# demo[is.na(demo)] <- 999

# demo %>% 
#   gather("pair","score") %>% 
#   drop_na()

image.set <- demo

# Bayesian INDSCAL --------------------------------------------------------

# ファイルからデータを読み込む
# image.set <- read.csv("distance_set.csv",head=F)
# Bayesian INDSCAL
## 被験者の数
N <- nrow(image.set)
## 刺激の数
I <- 5
## 刺激のペアの数
P <- (I*(I-1))/2
## データセット
Y <- image.set[,1:P]
# ## 世代のID
Gid <- image.set$Gid
# ## 世代の総数
G <- max(Gid)

# Stanに渡すデータセットに組み上げる
dataset <- list(N=N,I=I,P=P,Y=as.matrix(Y),G=G,Gid=Gid)
# モデルのコンパイル
# model <- cmdstanr::cmdstan_model("demo2.stan")
model <- cmdstanr::cmdstan_model("b_indscal.stan")
# model <- stan_model("model.stan")
# サンプリング
fit <- model$sample(dataset,iter_sampling=1000,iter_warmup=1000,
                    save_warmup = TRUE,
                    output_dir = "~/Downloads/chapter01_19/chapter01_19/chapter07小杉/mac/csv",parallel_chains = 4)
fit

fit1 <- rstan::read_stan_csv(fit$output_files())
fit1

# 重みの推定値出力
print(fit1,pars="w")
# 座標の推定値出力
print(fit1,pars="lambda")


# 作業1:50%確信区間を伴う布置図の描画準備 --------------------------------------------------------

## Stanfit objectからMCMCサンプルを取り出してデータフレームに 
fit1 %>% rstan::extract() %>% data.frame() %>% 
  ## 座標パラメータだけ取り出す
  dplyr::select(starts_with("lambda")) %>% 
  ## ロング型データに
  tidyr::gather(key,val) %>% 
  ## 次元，対象変数を変数名から作成，不要な列の削除
  mutate(dim=str_sub(key,start=8,end=8),
         target=str_sub(key,start=10),
         key=NULL) %>% 
  ## 対象，次元でグループ化，ネスト
  dplyr::group_by(target,dim) %>% nest %>% 
  ## 次元を横に広げ，ネスト解除
  tidyr::spread(dim,data) %>% unnest() %>% 
  ## 改めて対象でグループ化
  group_by(target) %>% 
  ## 変数名をわかりやすく
  rename(X=val,Y=val1) %>% 
  # 要約統計量の算出
  dplyr::summarise_all(funs(EAP=mean,
                            lower=quantile(.,0.25),
                            upper=quantile(.,0.75))) %>% 
  ## 対象名を因子型にしラベルをつける
  mutate(target=factor(1:5,
                       labels=c(
                         "えび",
                         "はまち",
                         "かっぱ巻",
                         "まぐろ",
                         "とろ"
                       ))) -> plot.df


# 作業2:雲を纏わせるプロットの準備 --------------------------------------------------------------

## MCMCサンプルの中から一部を抽出
## 抽出するサンプル数
nsamp <- 100
## Stanfit objectからMCMCサンプルを取り出してデータフレームに 
fit1 %>% rstan::extract() %>% data.frame %>% 
  ## 座標パラメータだけ取り出す
  dplyr::select(starts_with("lambda")) %>% 
  ## MCMCサンプルからサンプリング
  sample_n(.,size=nsamp) %>% 
  ## 変数名を列名から取り込む
  tibble::rownames_to_column() %>% 
  ## ロング型データに
  tidyr::gather(key,val,-rowname) %>% 
  ## 対象名を変数につける
  mutate(label=rep(rep(c(                         "えび",
                                                  "はまち",
                                                  "かっぱ巻",
                                                  "まぐろ",
                                                  "とろ"), 
                       each=nsamp*2))) %>% 
  ## 次元変数を作成
  mutate(dim=paste0("dim",str_sub(key,start=8,end=8))) %>% 
  ## 不要な変数を削除
  mutate(key=NULL) %>%
  ## 次元を横に並べる
  tidyr::spread(dim,value=val) -> cloud.df


# 描画 ----------------------------------------------------------------------

## 元になる座標は作業1の座標
ggplot(data=plot.df,aes(x=X_EAP,y=Y_EAP)) + 
  ## 対象のプロット
  geom_point(size=3) +  labs(title="",x="dim1",y="dim2")  +
  ## エラーバーをX,Y軸に
  geom_errorbar(data=plot.df,mapping=aes(ymin=Y_lower,ymax=Y_upper),width=0.1,alpha=0.5) +
  geom_errorbarh(data=plot.df,mapping=aes(xmin=X_lower,xmax=X_upper),height=0.1,alpha=0.5) +
  ## 対象名をプロット(Mac用にフォントファミリを指定)
  geom_text_repel(data=plot.df,aes(x=X_EAP,y=Y_EAP,label=target),
                  family = "HiraKakuPro-W3",size=5) +
  ## 雲データをプロット
  geom_point(data=cloud.df,aes(x=dim1,y=dim2,shape=label),alpha=0.5,size=4) +
  ## シェイプの種類がデフォルトを超えるのでマニュアルで指定
  scale_shape_manual(values=1:5)


# 雲に色をつける
## 元になる座標は作業1の座標
ggplot(data=plot.df,aes(x=X_EAP,y=Y_EAP)) + 
  ## 色付きの雲を纏わせる
  geom_point(data=cloud.df,aes(x=dim1,y=dim2,shape=label,color=label),alpha=1) + 
  ## シェイプの種類がデフォルトを超えるのでマニュアルで指定
  scale_shape_manual(values=1:5) + 
  ## 対象名をプロット(Mac用にフォントファミリを指定)
  geom_text_repel(data=plot.df,aes(x=X_EAP,y=Y_EAP,label=target),
                  family = "HiraKakuPro-W3",size=5) +
  ## X,Y軸に名前をつける
  labs(title="",x="dim1",y="dim2") +
  theme_bw()

