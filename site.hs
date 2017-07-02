--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set         as S
import           Data.Time
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           Hakyll
import           System.FilePath
import           Text.Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateCompiler

    match "image/**" $ do
        route idRoute
        compile copyFileCompiler

    -- ニュースリリースの各記事にマッチ
    match "release/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/release.html" releaseCtx
            >>= loadAndApplyTemplate "templates/default.html" releaseCtx
            >>= loadAndApplyTemplate "templates/wrapper.html" releaseCtx
            >>= indentHtml

    -- ニュースリリース一覧にマッチ
    match "release/index.html" $ do
        route idRoute
        compile $ do
            release <- reverse <$> loadAll "release/*.md"
            let indexCtx = listField "release" releaseCtx (return release) `mappend`
                           constField "title" "ニュースリリース" `mappend`
                           syakeDefaultCtx
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= loadAndApplyTemplate "templates/wrapper.html" indexCtx
                >>= relativizeUrls

    -- GHOSTUS系記事にマッチ
    match "game/ghostus/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/ghostus.html" ghostusDefaultCtx
            >>= loadAndApplyTemplate "templates/wrapper.html" ghostusDefaultCtx
            >>= indentHtml

    -- 会社概要・errorページ等にマッチ。README.mdは除外
    match ("*.md" .&&. complement "README.md") $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/default.html" syakeDefaultCtx
            >>= loadAndApplyTemplate "templates/wrapper.html" syakeDefaultCtx
            >>= indentHtml

    -- HOMEにマッチ
    match "index.html" $ do
        route idRoute
        compile $ do
            release <- reverse <$> loadAll "release/*.md"
            let indexCtx = listField "release" releaseCtx (return release) <>
                           constField "title" "Syake株式会社" <>
                           constField "ogType" "website" <>
                           syakeDefaultCtx
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= loadAndApplyTemplate "templates/wrapper.html" indexCtx
                >>= indentHtml

    -- scss群にマッチ
    match "scss/*.scss" $ do
        compile $ do
            path <- getResourceFilePath
            unixFilter "npm" [ "run"
                             , "-s"
                             , "node-sass"
                             , path
                             , flip replaceDirectory "./_site/css/" $ replaceExtension path ".css"
                             ] "" >>= makeItem

    -- ニュースリリース一覧にマッチ
    match "favicon/**" $ do
        route idRoute
        compile copyFileCompiler

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            news <- take 10 . reverse <$> loadAllSnapshots "release/*.md" "content"
            renderAtom releaseFeedConfiguration (releaseCtx <> bodyField "description") news >>=
                indentXml

--------------------------------------------------------------------------------

-- | ニュースリリースのContext
releaseCtx :: Context String
releaseCtx = syakeDefaultCtx

-- | Syake系ページのContext
syakeDefaultCtx :: Context String
syakeDefaultCtx = descriptionField "description" syakeDefualtDescription <>
                  imageField "image" syakeDefaultImage <>
                  constField "css" "/css/default.css" <>
                  constField "keywords" "Syake株式会社,Syake,任価,ninka,ゲーム,鮭,シャケ" <>
                  defaultContext

-- | GHOSTUS系ページのContext
ghostusDefaultCtx :: Context String
ghostusDefaultCtx = descriptionField "description" ghostusDefualtDescription <>
                    imageField "image" ghostusDefaultImage <>
                    constField "css" "/css/ghostus.css" <>
                    constField "keywords" "GHOSTUS,ゴースタス,ゲーム,STG,SHMUP,puzzle,PZL,シューティングパズル,Syake" <>
                    defaultContext

-- | description, og:descriptionに写される記事の概要
-- key と descriptionが指定されていなかった場合のデフォルトの文字列を指定する
descriptionField :: String -> String -> Context String
descriptionField key defualtDescription = field key $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe defualtDescription $ lookupString key metadata

-- | Syake系記事のデフォルトのdescription, og:description
syakeDefualtDescription :: String
syakeDefualtDescription = "Syake株式会社は「任価」(任意の時期・金額・回数による支払い)による投稿型PCゲーム販売サイト「SYAKERAKE」の運営及びゲーム開発を行っています"

-- | GHOSTUS系記事のデフォルトのdescription, og:description
ghostusDefualtDescription :: String
ghostusDefualtDescription = "時間を繰り返し、積み重なる自身のリプレイ(GHOST)と共闘する、超時空多重リプレイSTGパズルゲーム"

-- | og:imageや記事一覧のサムネイルに使用する画像
-- keyとデフォルトの画像のpathを指定する
imageField :: String -> String -> Context String
imageField key defaultImage = field key $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe defaultImage $ lookupString key metadata

-- | Syake系記事のデフォルトのog:image
syakeDefaultImage :: String
syakeDefaultImage = "/image/logo/syake-ogp.png"

-- | GHOSTUS系記事のデフォルトのog:image
ghostusDefaultImage :: String
ghostusDefaultImage = "/image/logo/ghostus-ogp.png"

releaseFeedConfiguration :: FeedConfiguration
releaseFeedConfiguration = FeedConfiguration
    { feedTitle       = "Syake株式会社ニュースリリース"
    , feedDescription = "ニュースリリース"
    , feedAuthorName  = "Syake"
    , feedAuthorEmail = "info@syake.co.jp"
    , feedRoot        = "https://www.syake.co.jp"
    }

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom = pandocCompilerWith
    defaultHakyllReaderOptions { readerExtensions = S.insert Ext_ignore_line_breaks $
                                                    S.delete Ext_implicit_figures $
                                                    readerExtensions defaultHakyllReaderOptions
                               }
    defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax ""
                               , writerSectionDivs = True
                               , writerExtensions = S.insert Ext_ignore_line_breaks $
                                   writerExtensions defaultHakyllWriterOptions
                               , writerHtml5 = True
                               }

indentHtml :: Item String -> Compiler (Item String)
indentHtml = withItemBody (\bo -> unixFilter "tidy"
                              [ "--drop-empty-elements", "n"
                              , "--tidy-mark", "n"
                              , "--wrap", "0"
                              , "-indent"
                              ] bo)

indentXml :: Item String -> Compiler (Item String)
indentXml = withItemBody (\bo -> unixFilter "tidy" [ "--indent-cdata" , "y"
                                                   , "--wrap", "0"
                                                   , "-quiet"
                                                   , "-xml"
                                                   , "-indent"
                                                   ] bo)
