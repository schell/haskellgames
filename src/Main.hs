{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import           Audax.KVStore.S3             (AWSCredentials (..))
import           Audax.Options                (descHeader, getYamlFile)
import           Control.Applicative          ((<|>))
import           Control.Lens                 (set, (&), (.~), (<&>), (?~))
import           Control.Monad                (join, void)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.AWS      (AWST', ChunkSize, Env, Region,
                                               chunkedFile, envRegion, newEnv,
                                               runAWST, send)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   (FromJSON)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.IO                 as T
import           GHC.Generics                 (Generic)
import           Milkshake                    (milkshakeCommand)
import           Network.AWS.Data             (toText)
import           Network.AWS.S3               (BucketName (..),
                                               ObjectCannedACL (OPublicRead),
                                               ObjectKey (..), poACL,
                                               poContentType, putObject)
import           Network.Mime                 (defaultMimeLookup)
import           Options.Applicative          (InfoMod, Parser, ParserInfo,
                                               command, execParser, fullDesc,
                                               header, help, helper, info,
                                               metavar, strArgument, subparser)
import           System.Directory             (doesDirectoryExist,
                                               listDirectory)
import           System.FilePath              (joinPath, splitPath, (</>))


data Config
  = Config
  { configBucket :: BucketName
  , configRegion :: Region
  , configCreds  :: AWSCredentials
  } deriving (Generic)


instance FromJSON Config


runS3 :: Config -> AWST' Env (ResourceT IO) b -> IO b
runS3 config f = do
  let creds = unAWSCredentials $ configCreds config
  -- get the region string out of the environment
  env <-
    liftIO
    $ newEnv creds
    <&> set envRegion (configRegion config)
  runResourceT $ runAWST env f


say :: MonadIO m => Text -> m ()
say = liftIO . T.putStrLn


putChunkedFile
  :: BucketName -- ^ The bucket to store the file in.
  -> ObjectKey  -- ^ The destination object key.
  -> ChunkSize  -- ^ The chunk size to send.
  -> FilePath   -- ^ The source file to upload.
  -> Maybe Text -- ^ The mimetype, if available.
  -> AWST' Env (ResourceT IO) ()
putChunkedFile b k c f mime = do
  bdy <- chunkedFile c f
  void . send
    $ putObject b k bdy
    & poContentType .~ mime
    & poACL ?~ OPublicRead
  say $ "Successfully Uploaded: " <> toText f
                                  <> " to "
                                  <> toText b
                                  <> " - "
                                  <> toText k
                                  <> " ("
                                  <> T.pack (show mime)
                                  <> ")"


listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  contents <- listDirectory path
  fmap concat <$> sequence $ flip map contents
                           $ \file -> do
    let filepath = path </> file
    doesDirectoryExist filepath >>= \case
      False -> return [filepath]
      True -> listDirectoryRecursive filepath


-- | Splits a filename and removes the slashes.
splitWithoutSlashes
  :: FilePath
  -- ^ The path.
  -> [String]
  -- ^ A list of folders without slashes
splitWithoutSlashes =
  map (filter (/= '/'))
  . splitPath


-- | Finds a directory prefix in a filename and removes everything before the
-- prefix and removes the prefix.
--
-- >>> cropFilenameByPrefix "docs/markdown" "/Users/schell/docs/markdown/yo/index.md"
-- "yo/index.md"
cropFilenameByPrefix
  :: FilePath
  -- ^ The prefix to find and remove.
  -> FilePath
  -- ^ The original filepath.
  -> FilePath
  -- ^ The modified filepath.
cropFilenameByPrefix prefix =
  joinPath
  . splitWithoutSlashes
  . maybe prefix T.unpack
  . T.stripPrefix (T.pack prefix)
  . T.pack


fileWithPrefixToKey :: FilePath -> FilePath -> ObjectKey
fileWithPrefixToKey = ((ObjectKey . T.pack) .) . cropFilenameByPrefix


copyAsset :: Config -> FilePath -> FilePath -> IO ()
copyAsset config prefix file = do
  let key = fileWithPrefixToKey prefix file
      mim = T.decodeUtf8 $ defaultMimeLookup $ T.pack file
  liftIO (runS3 config $ putChunkedFile (configBucket config) key 8192 file $ Just mim)


siteFolder :: FilePath
siteFolder = "build" </> "site"


-- | Recursively descend into the given directory, deploying
-- all the assets found within to S3.
deployIO :: FilePath -> IO ()
deployIO cfgFile = do
  config <- getYamlFile cfgFile
  assets <- liftIO $ listDirectoryRecursive siteFolder
  mapM_ (copyAsset config siteFolder) assets


parseConfig :: Parser FilePath
parseConfig =
  strArgument
    ( help "config file"
      <> metavar "FILE"
    )


deployCommand :: ParserInfo (IO ())
deployCommand = info (helper <*> cmd) deployInfo
  where
    deployInfo :: InfoMod (IO ())
    deployInfo =
      fullDesc
      <> descHeader "Deploy the site folder"
    cmd = deployIO <$> parseConfig


main :: IO ()
main = join $ execParser opts
  where
    opts   = info (helper <*> deploy <|> build) desc
    deploy = subparser $ command "deploy" deployCommand
    build  = subparser $ command "build" milkshakeCommand
    desc :: InfoMod a
    desc =
      fullDesc
      <> header "haskell-games - your source for haskell implementations of games"
