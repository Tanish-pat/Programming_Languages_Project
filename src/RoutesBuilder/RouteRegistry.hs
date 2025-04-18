module RouteRegistry where

-- | A single segment: literal or dynamic parameter
data RoutePathPart
  = Static  String
  | Dynamic String
  deriving (Show)

-- | One route function spec
data RouteSpecExtended = RouteSpecExtended
  { functionName :: String
  , httpVerb     :: String        -- e.g. "GET" (unused here)
  , pathParts    :: [RoutePathPart]
  , queryParams  :: [String]
  } deriving (Show)

-- | Group of routes under one model
data RouteGroup = RouteGroup
  { modelName :: String
  , routes    :: [RouteSpecExtended]
  } deriving (Show)

-- | Your route registry
routeRegistry :: [RouteGroup]
routeRegistry =
    [
    RouteGroup "User"
        [ RouteSpecExtended "getAllUsers"     "GET"    [Static "getAll"]                  []
        , RouteSpecExtended "getById"         "GET"    [Static "getById", Dynamic "id"]   []
        , RouteSpecExtended "createUser"      "POST"   [Static "create"]                  []
        , RouteSpecExtended "updateUser"      "PUT"    [Static "update", Dynamic "id"]    []
        , RouteSpecExtended "deleteUser"      "DELETE" [Static "delete", Dynamic "id"]    []
        , RouteSpecExtended "searchUsers"     "GET"    [Static "search"]                  ["name", "age"]
        ],

    RouteGroup "Product"
        [ RouteSpecExtended "getAllProducts"  "GET"    [Static "getAll"]                     []
        , RouteSpecExtended "getBySku"        "GET"    [Static "getBySku", Dynamic "sku"]    []
        , RouteSpecExtended "createProduct"   "POST"   [Static "create"]                     []
        , RouteSpecExtended "updateProduct"   "PUT"    [Static "update", Dynamic "sku"]      []
        , RouteSpecExtended "deleteProduct"   "DELETE" [Static "delete", Dynamic "sku"]      []
        , RouteSpecExtended "searchProducts"  "GET"    [Static "search"]                     ["name", "tags"]
        ],

    RouteGroup "Order"
        [ RouteSpecExtended "getAllOrders"    "GET"    [Static "getAll"]                        []
        , RouteSpecExtended "getByOrderId"    "GET"    [Static "getById", Dynamic "orderId"]     []
        , RouteSpecExtended "createOrder"     "POST"   [Static "create"]                         []
        , RouteSpecExtended "updateOrder"     "PUT"    [Static "update", Dynamic "orderId"]      []
        , RouteSpecExtended "deleteOrder"     "DELETE" [Static "delete", Dynamic "orderId"]      []
        , RouteSpecExtended "getOrdersByUser" "GET"    [Static "byUser", Dynamic "userId"]       []
        ],

    RouteGroup "Review"
        [ RouteSpecExtended "getAllReviews"   "GET"    [Static "getAll"]                           []
        , RouteSpecExtended "getByReviewId"   "GET"    [Static "getById", Dynamic "reviewId"]       []
        , RouteSpecExtended "createReview"    "POST"   [Static "create"]                            []
        , RouteSpecExtended "updateReview"    "PUT"    [Static "update", Dynamic "reviewId"]        []
        , RouteSpecExtended "deleteReview"    "DELETE" [Static "delete", Dynamic "reviewId"]        []
        , RouteSpecExtended "getReviewsByProduct" "GET" [Static "product", Dynamic "productId"]     ["minRating"]
        ],

    RouteGroup "Address"
        [ RouteSpecExtended "getAllAddresses" "GET"    [Static "getAll"]                             []
        , RouteSpecExtended "getByAddressId"  "GET"    [Static "getById", Dynamic "addressId"]       []
        , RouteSpecExtended "createAddress"   "POST"   [Static "create"]                             []
        , RouteSpecExtended "updateAddress"   "PUT"    [Static "update", Dynamic "addressId"]        []
        , RouteSpecExtended "deleteAddress"   "DELETE" [Static "delete", Dynamic "addressId"]        []
        , RouteSpecExtended "getByUserId"     "GET"    [Static "byUser", Dynamic "userId"]           []
        ],

    RouteGroup "Inventory"
        [ RouteSpecExtended "getAllInventory" "GET"    [Static "getAll"]                             []
        , RouteSpecExtended "getByProductId"  "GET"    [Static "getByProduct", Dynamic "productId"]  []
        , RouteSpecExtended "updateInventory" "PUT"    [Static "update", Dynamic "productId"]        []
        , RouteSpecExtended "checkAvailability" "GET"  [Static "availability"]                       ["location"]
        ],

    RouteGroup "Payment"
        [ RouteSpecExtended "getAllPayments"  "GET"    [Static "getAll"]                             []
        , RouteSpecExtended "getByPaymentId"  "GET"    [Static "getById", Dynamic "paymentId"]       []
        , RouteSpecExtended "createPayment"   "POST"   [Static "create"]                             []
        , RouteSpecExtended "updatePayment"   "PUT"    [Static "update", Dynamic "paymentId"]        []
        , RouteSpecExtended "deletePayment"   "DELETE" [Static "delete", Dynamic "paymentId"]        []
        , RouteSpecExtended "getPaymentsByOrder" "GET" [Static "byOrder", Dynamic "orderId"]         []
        ],

    RouteGroup "Coupon"
        [ RouteSpecExtended "getAllCoupons"   "GET"    [Static "getAll"]                             []
        , RouteSpecExtended "getByCode"       "GET"    [Static "getByCode", Dynamic "couponCode"]    []
        , RouteSpecExtended "createCoupon"    "POST"   [Static "create"]                             []
        , RouteSpecExtended "updateCoupon"    "PUT"    [Static "update", Dynamic "couponCode"]       []
        , RouteSpecExtended "deleteCoupon"    "DELETE" [Static "delete", Dynamic "couponCode"]       []
        , RouteSpecExtended "getActiveCoupons" "GET"   [Static "active"]                             []
        ],

    RouteGroup "Category"
        [ RouteSpecExtended "getAllCategories" "GET"    [Static "getAll"]                            []
        , RouteSpecExtended "getByCategoryId"  "GET"    [Static "getById", Dynamic "categoryId"]     []
        , RouteSpecExtended "createCategory"   "POST"   [Static "create"]                            []
        , RouteSpecExtended "updateCategory"   "PUT"    [Static "update", Dynamic "categoryId"]      []
        , RouteSpecExtended "deleteCategory"   "DELETE" [Static "delete", Dynamic "categoryId"]      []
        ],

    RouteGroup "ProductCategory"
        [ RouteSpecExtended "getAllMappings"      "GET"    [Static "getAll"]                                         []
        , RouteSpecExtended "getByProductAndCat"  "GET"    [Static "get", Dynamic "productId", Dynamic "categoryId"] []
        , RouteSpecExtended "createMapping"       "POST"   [Static "create"]                                          []
        , RouteSpecExtended "deleteMapping"       "DELETE" [Static "delete", Dynamic "productId", Dynamic "categoryId"] []
        , RouteSpecExtended "getByProduct"        "GET"    [Static "byProduct", Dynamic "productId"]                 []
        , RouteSpecExtended "getByCategory"       "GET"    [Static "byCategory", Dynamic "categoryId"]               []
        ]
    ]

