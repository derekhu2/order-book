module Utils where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

import Discord
import Discord.Types
import Discord.Requests as R

getToken :: IO T.Text
getToken = TIO.readFile "auth-token.secret"

getGuildId :: IO GuildId
getGuildId = do
    gids <- readFile "guildid.secret"
    case readMaybe gids of
        Just g -> pure g
        Nothing -> error "invalid guild ID in guildid.secret"