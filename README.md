# www.syake.co.jp

Syake株式会社公式HP -> <https://www.syake.co.jp>

## 入れとく

- yarn
- [tidy-html5](https://github.com/htacg/tidy-html5)

## 動かす

- `stack build`
- `stack exec site build`
- `stack exec site rebuild` -- 全てのファイルが再度buildされる
- `stack exec site watch`

# デプロイ

1. `s3-redirect-rule.xml` の中身をS3の「Static website hosting」のリダイレクトルールにコピー (初回のみ)
1. `www.syake.co.jp` バケットの中身を空にする
1. build結果の `_site` 以下をバケットに投げ込む
1. cloudfrontのキャッシュが残っているのでInvalidationする
