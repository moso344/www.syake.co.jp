--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


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
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/release.html"    releaseCtx
            >>= loadAndApplyTemplate "templates/default.html" releaseCtx
            >>= relativizeUrls

    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
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


--------------------------------------------------------------------------------
releaseCtx :: Context String
releaseCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

