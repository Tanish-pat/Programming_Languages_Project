CREATE TABLE User (
  id INTEGER,
  name TEXT,
  email TEXT,
  age INTEGER,
  isActive BOOLEAN,
  PRIMARY KEY (id)
);

CREATE TABLE Product (
  sku TEXT,
  name TEXT,
  description TEXT,
  price REAL,
  tags TEXT,
  PRIMARY KEY (sku)
);

CREATE TABLE "Order" (
  orderId INTEGER,
  userId INTEGER,
  total REAL,
  status TEXT,
  PRIMARY KEY (orderId)
  FOREIGN KEY (userId) REFERENCES User(id)
);

CREATE TABLE Review (
  reviewId INTEGER,
  userId INTEGER,
  productId TEXT,
  rating INTEGER,
  comment TEXT,
  PRIMARY KEY (reviewId)
  FOREIGN KEY (userId) REFERENCES User(id)
  FOREIGN KEY (productId) REFERENCES Product(sku)
);

CREATE TABLE Address (
  addressId INTEGER,
  userId INTEGER,
  line1 TEXT,
  line2 TEXT,
  city TEXT,
  state TEXT,
  zipCode TEXT,
  PRIMARY KEY (addressId)
  FOREIGN KEY (userId) REFERENCES User(id)
);

CREATE TABLE Inventory (
  productId TEXT,
  quantity INTEGER,
  location TEXT,
  PRIMARY KEY (productId)
  FOREIGN KEY (productId) REFERENCES Product(sku)
);

CREATE TABLE Payment (
  paymentId INTEGER,
  orderId INTEGER,
  amount REAL,
  method TEXT,
  status TEXT,
  PRIMARY KEY (paymentId)
  FOREIGN KEY (orderId) REFERENCES "Order"(orderId)
);

CREATE TABLE Coupon (
  couponCode TEXT,
  discount REAL,
  isActive BOOLEAN,
  PRIMARY KEY (couponCode)
);

CREATE TABLE Category (
  categoryId INTEGER,
  name TEXT,
  description TEXT,
  PRIMARY KEY (categoryId)
);

CREATE TABLE ProductCategory (
  productId TEXT,
  categoryId INTEGER,
  PRIMARY KEY (productId, categoryId)
  FOREIGN KEY (productId) REFERENCES Product(sku)
  FOREIGN KEY (categoryId) REFERENCES Category(categoryId)
);

