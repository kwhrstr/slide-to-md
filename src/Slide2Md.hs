module Slide2Md where

import RIO
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (head, replace, tail)
import Network.Google.Resource.Slides.Presentations.Get
import Network.Google.Slides
import Network.Google
import Network.Google.Auth.ServiceAccount
import Network.Google.Auth
import Control.Lens ((.~))
import RIO.Char
import RIO.List.Partial ((!!), head)
import System.Environment


type PresentationID = Text
data MdContent = ContShape Shape | ContTable Table

run :: IO ()
run = do
  args <- getArgs
  when (length args < 2) $ throwString "args must be longer than 2"
  runConvert (head args) (T.pack $ args !! 1)


runConvert :: FilePath -> PresentationID -> IO ()
runConvert testJson testID = do
  p <- getPresentation testJson testID
  let maybeTitle = p ^. preTitle
      mdTexts = p ^. preSlides
             & map (view pPageElements)
             & map (mapMaybe toMdContent)
             & concatMap (map toMdTexts)
  writeFileUtf8 "test.md"  $ T.unlines mdTexts


getPresentation :: FilePath -> PresentationID -> IO Presentation
getPresentation f pId = do
  s <- fromFilePath f
  lgr <- newLogger Debug stdout
  m <- newManager tlsManagerSettings
  env <- newEnvWith s lgr m <&> (envLogger .~ lgr) . (envScopes .~ presentationsScope)
  runResourceT . runGoogle env . send $ presentationsGet pId


toMdContent :: PageElement -> Maybe MdContent
toMdContent pe = fmap ContShape (pe ^. peShape) <|> fmap ContTable (pe ^. peTable)


toMdTexts :: MdContent -> Text
toMdTexts (ContShape sh) =
  let tc = sh ^. sText
  in case sh ^. sPlaceholder >>= view pType of
    Nothing -> ""
    Just PTTitle -> "## " <> tcToMdText tc
    Just _ -> tcToMdText tc
toMdTexts (ContTable tb) = "unimplrement"


tcToMdText :: Maybe TextContent  ->Text
tcToMdText Nothing  = ""
tcToMdText (Just tc)  =
  let tes = tc ^. tcTextElements
      f te = fromMaybe "" (prefixMd te <|> textRunMd te)
  in foldl' (\acc a -> acc <> f a) "" tes
  

textRunMd :: TextElement -> Maybe Text
textRunMd te = do
  tr <- te ^. teTextRun
  t <- tr ^. trContent
  pure $ trace t  $ T.replace "\n" "  \n"  t


prefixMd :: TextElement  -> Maybe Text
prefixMd te = do
  b <- te ^. teParagraphMarker >>= (^. pmBullet)
  let nest = fromMaybe 0 (view bNestingLevel b)
  glyph <- b ^. bGlyph
  let mdBullet = if isAlphaNum (T.head glyph) then "1. " else "+ "
  pure $ T.replicate (4 * fromIntegral nest) " " <> mdBullet


