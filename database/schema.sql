CREATE TABLE IF NOT EXISTS "Customer" (
  id INTEGER,
  name TEXT,
  email TEXT,
  age INTEGER,
  isActive BOOLEAN,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS "Product" (
  sku TEXT,
  name TEXT,
  description TEXT,
  price REAL,
  tags TEXT,
  PRIMARY KEY (sku)
);

CREATE TABLE IF NOT EXISTS "Review" (
  reviewId INTEGER,
  customerId INTEGER,
  productId TEXT,
  rating INTEGER,
  comment TEXT,
  PRIMARY KEY (reviewId),
  FOREIGN KEY (customerId) REFERENCES Customer(id),
  FOREIGN KEY (productId) REFERENCES Product(sku)
);

CREATE TABLE IF NOT EXISTS "Address" (
  addressId INTEGER,
  customerId INTEGER,
  line1 TEXT,
  line2 TEXT,
  city TEXT,
  state TEXT,
  zipCode TEXT,
  PRIMARY KEY (addressId),
  FOREIGN KEY (customerId) REFERENCES Customer(id)
);

CREATE TABLE IF NOT EXISTS "Inventory" (
  productId TEXT,
  quantity INTEGER,
  location TEXT,
  PRIMARY KEY (productId),
  FOREIGN KEY (productId) REFERENCES Product(sku)
);

CREATE TABLE IF NOT EXISTS "Payment" (
  paymentId INTEGER,
  amount REAL,
  method TEXT,
  status TEXT,
  PRIMARY KEY (paymentId)
);

CREATE TABLE IF NOT EXISTS "Coupon" (
  couponCode TEXT,
  discount REAL,
  isActive BOOLEAN,
  PRIMARY KEY (couponCode)
);

CREATE TABLE IF NOT EXISTS "Category" (
  categoryId INTEGER,
  name TEXT,
  description TEXT,
  PRIMARY KEY (categoryId)
);

CREATE TABLE IF NOT EXISTS "ProductCategory" (
  productId TEXT,
  categoryId INTEGER,
  PRIMARY KEY (productId, categoryId),
  FOREIGN KEY (productId) REFERENCES Product(sku),
  FOREIGN KEY (categoryId) REFERENCES Category(categoryId)
);

