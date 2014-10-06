--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
import qualified Data.Set as S
import           Text.Pandoc.Options

import Agda.Interaction.Options (CommandLineOptions(..), defaultOptions)
import Hakyll.Web.Agda

pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    let agdaComp = pandocAgdaCompilerWith defaultHakyllReaderOptions
                                          writerOpts agdaOpts

    match ("posts/*.md" .||. "posts/*.lagda" .||. "posts/*.lhs") $ do
        route $ setExtension "html"
        compile $ agdaComp
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
          let feedCtx = postCtx `mappend`
                  constField "description" "This is the post description"
  
          posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
          renderAtom myFeedConfiguration feedCtx posts


--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Means to a coend"
    , feedDescription = "Types and terms"
    , feedAuthorName  = "Gabe Dijkstra"
    , feedAuthorEmail = "gabe.dijkstra@example.com"
    , feedRoot        = "localhost:8000"
    }


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

writerOpts :: WriterOptions
writerOpts = defaultHakyllWriterOptions {writerTableOfContents = True}

-- TODO: Some standard library might be included here.
agdaOpts :: CommandLineOptions
agdaOpts = defaultOptions {optIncludeDirs = Left ["."]}
