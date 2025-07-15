#── ベースイメージ ──────────────────────────────────
FROM rocker/verse:4.5.1

# 1) renv 本体をインストール
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org'); \
          remotes::install_version('renv', version='1.1.4', repos='https://cloud.r-project.org')"
          
# 2) ロックファイルとオートローダーをコピー
COPY renv.lock .Rprofile activate.R ./

# 3) パッケージを復元
#    並列インストール数は利用可能コア数に合わせる
# 3) 一行でオプション設定＋restore
RUN R -e 'options(renv.consent = TRUE, \
                  Ncpus = parallel::detectCores()); \
         renv::restore()'
