{-# LANGUAGE CPP #-}

-- | Module providing standard Haskell vocabulary
--
-- This provides an extended "Prelude" re-exporting the most common
-- @base@ symbols to reduce the @import@-boilerplate as well as
-- unused-import warnings.
module Distribution.Server.Prelude
    ( module X
    , parseTimeMaybe
    , parseUTCTime
    , sortOn
    , isLeft
    ) where

import           Control.Applicative as X
import           Control.Monad       as X
import           Data.Int            as X
import           Data.List           as X (sortBy)
import           Data.Maybe          as X
import           Data.Ord            as X (comparing)
import           Data.Semigroup      as X
import           Data.Typeable       as X (Typeable)
import           Data.Time (UTCTime)
import           Data.Word           as X
import           Prelude             as X
import           Control.Monad.IO.Class as X (MonadIO(liftIO))

#if MIN_VERSION_base(4,8,0)
import           Data.List (sortOn)
#endif

#if MIN_VERSION_base(4,7,0)
import           Data.Either (isLeft)
#endif

-- TODO: move somewhere else
import Data.Time.Locale.Compat (defaultTimeLocale)
-- import Text.ParserCombinators.ReadP (ReadP)
import Distribution.Parsec
import Data.Time.Format (ParseTime, parseTimeM)
parseTimeMaybe :: ParseTime t => String -> String -> Maybe t
#if MIN_VERSION_time(1,5,0)
parseTimeMaybe = parseTimeM True defaultTimeLocale
#else
parseTimeMaybe = parseTime defaultTimeLocale
#endif

-- This looks dumb, but is there a better way to implement this? @fumieval
parseUTCTime :: CabalParsing m => m UTCTime
parseUTCTime = do
  doW <- parsecToken
  moY <- parsecToken
  doM <- parsecToken
  hms <- parsecToken
  zone <- parsecToken
  year <- parsecToken
  parseTimeM True defaultTimeLocale "%c" $ unwords [doW, moY, doM, hms, zone, year]

#if !MIN_VERSION_base(4,8,0)
-- | See "Data.List" starting with @base-4.8.0.0@
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
#endif

#if !MIN_VERSION_base(4,7,0)
-- | See "Data.Either" starting with @base-4.7.0.0@
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False
#endif
