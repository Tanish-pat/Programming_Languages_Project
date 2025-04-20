module CouponRoutes (getAllCoupons, getByCode, createCoupon, updateCoupon, deleteCoupon, getActiveCoupons) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

basePath :: Text
basePath = "/api/coupon"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> decodeUtf8 (urlEncode (encodeUtf8 v)) | (k, v) <- ps]

getAllCoupons :: Text
getAllCoupons = path ["getAll"]

getByCode :: Text -> Text
getByCode couponCode = path ["getByCode", couponCode]

createCoupon :: Text
createCoupon = path ["create"]

updateCoupon :: Text -> Text
updateCoupon couponCode = path ["update", couponCode]

deleteCoupon :: Text -> Text
deleteCoupon couponCode = path ["delete", couponCode]

getActiveCoupons :: Text
getActiveCoupons = path ["active"]

