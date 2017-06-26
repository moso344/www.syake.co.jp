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
                           defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "game/ghostus/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/ghostus.html" releaseCtx
            >>= indentHtml

    match "*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= indentHtml

    match "index.html" $ do
        route idRoute
        compile $ do
            release <- reverse <$> loadAll "release/*.md"
            let indexCtx = listField "release" releaseCtx (return release) <>
                           constField "title" "Syake株式会社" <>
                           constField "ogType" "website" <>
                           defaultContext
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
releaseCtx = teaserField' "teaser" "content" <> defaultContext
  where teaserField' key snapshot = field key $ \item -> take 100 . stripTags . trans . itemBody <$> loadSnapshot (itemIdentifier item) snapshot
        trans h = either (error . show) id (writePlain def <$> readHtml def h)

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
                                   readerExtensions defaultHakyllReaderOptions }
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
