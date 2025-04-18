module CouponRoutes (getAllCoupons, getByCode, createCoupon, updateCoupon, deleteCoupon, getActiveCoupons) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/coupon"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

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

