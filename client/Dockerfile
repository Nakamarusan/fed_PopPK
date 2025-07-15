# 変更後 (renv.lockのRバージョンに合わせる)
FROM rocker/verse:4.5.1

# renvが内部でpakを使うように環境変数を設定
ENV RENV_CONFIG_PAK_ENABLED=TRUE

# 最初にrenvとpak自体をインストール
RUN R -e "install.packages(c('renv', 'pak'))"

# コンテナ内の作業ディレクトリを作成・指定
WORKDIR /app

# 1. ロックファイルと関連ファイルのみを先にコピー
COPY renv.lock .
COPY .Rprofile .
COPY renv/activate.R renv/

# 2. renv::restore()でロックファイルからパッケージを復元
# これで renv.lock に記載された全パッケージがインストールされる
RUN R -e "renv::restore()"

# 3. アプリケーションのソースコードをコピー
COPY . .

# コンテナ起動時に実行するデフォルトコマンドを指定
CMD ["Rscript", "client.R"]