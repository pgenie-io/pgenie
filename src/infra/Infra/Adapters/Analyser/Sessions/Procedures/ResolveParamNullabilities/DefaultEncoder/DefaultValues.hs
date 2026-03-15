module Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities.DefaultEncoder.DefaultValues where

import Data.Aeson qualified as Aeson
import Data.IP qualified as Iproute
import Utils.Prelude

netAddr :: Iproute.IPRange
netAddr = read "192.0.2.1/24"

ipV4 :: Iproute.IPv4
ipV4 = "127.0.0.1"

ipV6 :: Iproute.IPv6
ipV6 = "2001:db8:00:00:00:00:00:01"

day :: Day
day = read "2000-01-01"

json :: Aeson.Value
json = Aeson.Null

timetz :: (TimeOfDay, TimeZone)
timetz = (TimeOfDay 0 0 0, utc)
