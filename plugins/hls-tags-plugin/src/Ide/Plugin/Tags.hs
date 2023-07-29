module Ide.Plugin.Tags where

import Development.IDE.GHC.Compat

import           Ide.Types
import qualified Plugin.GhcTags as GhcTags


descriptor :: PluginId -> PluginDescriptor st
descriptor pid = (defaultPluginDescriptor pid)
  { pluginModifyGhcOpts = mempty { staticPlugins = [ StaticPlugin PluginWithArgs { paPlugin    = GhcTags.plugin
                                                                                 -- TODO arguments!
                                                                                 , paArguments = []
                                                                                 }
                                                   ]}}
