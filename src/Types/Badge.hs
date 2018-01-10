module Types.Badge where

import Data.Text (Text)

data Badge = Badge { badgeLabel :: Text, badgeCount :: Int }
