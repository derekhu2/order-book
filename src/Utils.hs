module Utils (getToken, getGuildId) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

import Discord.Types

getToken :: IO T.Text
getToken = TIO.readFile "auth-token.secret"

getGuildId :: IO GuildId
getGuildId = do
    gids <- readFile "guildid.secret"
    case readMaybe gids of
        Just g -> pure g
        Nothing -> error "invalid guild ID in guildid.secret"