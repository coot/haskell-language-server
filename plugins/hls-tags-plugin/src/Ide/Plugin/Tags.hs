{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Tags where

import           Development.IDE.GHC.Compat

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe (fromMaybe)
import           Ide.Types
import           Ide.Plugin.Properties

import qualified Plugin.GhcTags as GhcTags


descriptor :: PluginId -> PluginDescriptor st
descriptor pid = (defaultPluginDescriptor pid)
  { pluginModifyGhcOpts = \config -> mempty { staticPlugins = [ StaticPlugin PluginWithArgs { paPlugin    = GhcTags.plugin,
                                                                                              paArguments = configToArgs (plcConfig config) }
                                                         ]},
    pluginConfigDescriptor = defaultConfigDescriptor { configCustomConfig = mkCustomConfig $
                                                          defineBooleanProperty (KeyNameProxy :: KeyNameProxy "etags")
                                                                               "If False create CTAGS (default), if True create ETAGS"
                                                                               False
                                                        . defineBooleanProperty (KeyNameProxy :: KeyNameProxy "stream") 
                                                                               "Use stream parser"
                                                                               False
                                                        . defineStringProperty (KeyNameProxy :: KeyNameProxy "tagsfile")
                                                                               "tags file path"
                                                                               "tags"
                                                        . defineBooleanProperty (KeyNameProxy :: KeyNameProxy "debug") 
                                                                               "enable debug output of ghc-tags-plugin"
                                                                               False
                                                        $ emptyProperties }
  }


data TagsPluginConfig = TagsPluginConfig {
      cfgEtags    :: Bool,
      cfgStream   :: Bool,
      cfgFilePath :: String,
      cfgDebug    :: Bool
    }

-- TODO: `configCustomConfig` suggests that the library has already a parser somewhere.
instance FromJSON TagsPluginConfig where
    parseJSON = withObject "TagsPluginConfig" $ \o ->
      TagsPluginConfig <$> (fromMaybe False <$> (o .:? "etags"))
                       <*> (fromMaybe False <$> (o .:? "stream"))
                       <*> o .: "tagsfile" 
                       <*> (fromMaybe False <$> (o .: "debug"))


tagsPluginConfigArgs :: TagsPluginConfig -> [CommandLineOption]
tagsPluginConfigArgs TagsPluginConfig { cfgEtags, cfgStream, cfgFilePath, cfgDebug } =
       if cfgEtags  then ["--etags"]  else []
    <> if cfgStream then ["--stream"] else []
    <> if cfgDebug  then ["--debug"]  else []
    <> [cfgFilePath]


configToArgs :: Object -> [CommandLineOption]
configToArgs o =
  case parse parseJSON (Object o) of
    Error err   -> error err
    Success cfg -> tagsPluginConfigArgs cfg
