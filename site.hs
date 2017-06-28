--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set         as S
import           Data.Time
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           Hakyll
import           System.FilePath  (takeBaseName)
import           Text.Pandoc
import           Text.Regex


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateCompiler

    match "image/**" $ do
        route idRoute
        compile copyFileCompiler

    -- ニュースリリース
    match "release/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/release.html" releaseCtx
            >>= loadAndApplyTemplate "templates/default.html" releaseCtx
            >>= indentHtml

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
                >>= relativizeUrls

    match "game/ghostus/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/ghostus.html" ghostusDefaultCtx
            >>= indentHtml

    match "*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/default.html" syakeDefaultCtx
            >>= indentHtml

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
                >>= indentHtml

    -- scss/default.scssがあれば、同階層scssもまとめてbuildする
    match "scss/default.scss" $ do
        route $ setExtension "css"
        compile $ unixFilter "npm" ["run", "-s", "build:scss"] "" >>= makeItem

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            news <- take 10 . reverse <$> loadAllSnapshots "release/*.md" "content"
            renderAtom releaseFeedConfiguration (releaseCtx <> bodyField "description") news >>=
                indentXml

--------------------------------------------------------------------------------

releaseCtx :: Context String
releaseCtx = syakeDefaultCtx

syakeDefaultCtx :: Context String
syakeDefaultCtx = descriptionField "description" syakeDefualtDescription <> defaultContext

ghostusDefaultCtx :: Context String
ghostusDefaultCtx = descriptionField "description" ghostusDefualtDescription <> defaultContext

descriptionField :: String -> String -> Context String
descriptionField key defualtDescription = field key $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe defualtDescription $ lookupString key metadata

syakeDefualtDescription :: String
syakeDefualtDescription = "Syake株式会社は「任価」(任意の時期・金額・回数による支払い)による投稿型PCゲーム販売サイト「SYAKERAKE」の運営及びゲーム開発を行っています"

ghostusDefualtDescription :: String
ghostusDefualtDescription = "時間を繰り返し、積み重なる自身のリプレイ(GHOST)と共闘する、超時空多重リプレイSTGパズルゲーム"

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
