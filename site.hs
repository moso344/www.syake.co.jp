--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe
import           Data.Monoid      (mappend)
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
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/release.html" releaseCtx
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
            release <- reverse <$> loadAll "release/*.md"
            let indexCtx =
                    listField "release" releaseCtx (return release) `mappend`
                    constField "title" "Syake株式会社" `mappend`
                    constField "ogType" "website" `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "release/index.html" $ do
        route idRoute
        compile $ do
            release <- reverse <$> loadAll "release/*.md"
            let indexCtx =
                    listField "release" releaseCtx (return release) `mappend`
                    constField "title" "ニュースリリース" `mappend`
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
            news <- take 10 . reverse <$> loadAllSnapshots "release/*.md" "content"
            renderAtom releaseFeedConfiguration feedCtx news


--------------------------------------------------------------------------------
releaseCtx :: Context String
releaseCtx = mconcat [ dateFiled' "date" "%Y-%m-%d"
                     , dateFiled' "published" "%Y-%m-%d"
                     , dateFiled' "updated" "%Y-%m-%d"
                     , teaserField "teaser" "content"
                     , teaserField' "description" "content"
                     , defaultContext
                     ]
  where
    teaserField' key snapShot  = mapContext (\s -> subRegex reg s "") $ teaserField key snapShot
    reg = mkRegex "<(\"[^\"]*\"|\'[^\']*\'|[^\'\">])*>"

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

dateFiled' :: String -> String -> Context String
dateFiled' key format = field key $ \item -> do
    let name = toFilePath $ itemIdentifier item
        basename = takeBaseName name
        dateString = take 10 basename
        time :: UTCTime
        time = fromMaybe
            (error $ "file `" ++ dateString ++ "` doesn't match to format `%Y/%m%d*`")
            (parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString)
    return $ formatTime defaultTimeLocale format time
