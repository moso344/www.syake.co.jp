--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import qualified Data.Set    as S
import           Hakyll
import           Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "image/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- ニュースリリース
    match "release/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/release.html" releaseCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" releaseCtx
            >>= relativizeUrls

    match "game/ghostus/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/ghostus.html" releaseCtx
            >>= relativizeUrls

    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            release <- recentFirst =<< loadAll "release/*"
            let indexCtx =
                    listField "release" releaseCtx (return release) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- scss/default.scssがあれば、同階層scssもまとめてbuildする
    match "scss/default.scss" $ do
        route $ setExtension "css"
        compile $ unixFilter "npm" ["run", "-s", "build:scss"] "" >>= makeItem

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            let feedCtx = releaseCtx `mappend`
                          bodyField "description"
            news <- fmap (take 10) . recentFirst =<< loadAllSnapshots "release/*.md" "content"
            renderAtom releaseFeedConfiguration feedCtx news


--------------------------------------------------------------------------------
releaseCtx :: Context String
releaseCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

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
