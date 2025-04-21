CREATE TABLE Customer (
  id INTEGER,
  name VARCHAR(255),
  email VARCHAR(255),
  age INTEGER,
  isActive BOOLEAN,
  PRIMARY KEY (id)
);

CREATE TABLE Product (
  sku VARCHAR(255),
  name VARCHAR(255),
  description VARCHAR(255),
  price REAL,
  tags VARCHAR(255),
  PRIMARY KEY (sku)
);

CREATE TABLE `Transaction` (
  transactionId INTEGER,
  customerId INTEGER,
  total REAL,
  status VARCHAR(255),
  PRIMARY KEY (TransactionId),
  FOREIGN KEY (customerId) REFERENCES Customer(id)
);

CREATE TABLE Review (
  reviewId INTEGER,
  customerId INTEGER,
  productId VARCHAR(255),
  rating INTEGER,
  comment VARCHAR(255),
  PRIMARY KEY (reviewId),
  FOREIGN KEY (customerId) REFERENCES Customer(id),
  FOREIGN KEY (productId) REFERENCES Product(sku)
);

CREATE TABLE Address (
  addressId INTEGER,
  customerId INTEGER,
  line1 VARCHAR(255),
  line2 VARCHAR(255),
  city VARCHAR(255),
  state VARCHAR(255),
  zipCode VARCHAR(255),
  PRIMARY KEY (addressId),
  FOREIGN KEY (customerId) REFERENCES Customer(id)
);

CREATE TABLE Inventory (
  productId VARCHAR(255),
  quantity INTEGER,
  location VARCHAR(255),
  PRIMARY KEY (productId),
  FOREIGN KEY (productId) REFERENCES Product(sku)
);

CREATE TABLE Payment (
  paymentId INTEGER,
  transactionId INTEGER,
  amount REAL,
  method VARCHAR(255),
  status VARCHAR(255),
  PRIMARY KEY (paymentId),
  FOREIGN KEY (TransactionId) REFERENCES `Transaction`(TransactionId)
);

CREATE TABLE Coupon (
  couponCode VARCHAR(255),
  discount REAL,
  isActive BOOLEAN,
  PRIMARY KEY (couponCode)
);

CREATE TABLE Category (
  categoryId INTEGER,
  name VARCHAR(255),
  description VARCHAR(255),
  PRIMARY KEY (categoryId)
);

CREATE TABLE ProductCategory (
  productId VARCHAR(255),
  categoryId INTEGER,
  PRIMARY KEY (productId, categoryId),
  FOREIGN KEY (productId) REFERENCES Product(sku),
  FOREIGN KEY (categoryId) REFERENCES Category(categoryId)
);

