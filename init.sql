CREATE TABLE users(
  id TEXT PRIMARY KEY,
  name TEXT,
  age INT,
  email TEXT UNIQUE NOT NULL, 
  password TEXT NOT NULL,
  created_at TIMESTAMP,
  updated_at TIMESTAMP
);

CREATE TABLE accounts(
  id TEXT PRIMARY KEY,
  name TEXT,
  total_expense DECIMAL(10,2),
  user_id TEXT,
  created_at TIMESTAMP,
  updated_at TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE TABLE transactions(
  id TEXT PRIMARY KEY,
  name TEXT,
  type VARCHAR(8) CHECK (type IN ('cash_out', 'cash_in')),
  acc_id TEXT,
  user_id TEXT,
  amount DECIMAL(10,2),
  created_at TIMESTAMP,
  updated_at TIMESTAMP,
  FOREIGN KEY (acc_id) REFERENCES accounts(id) ON DELETE CASCADE,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE TABLE user_tokens(
  id TEXT PRIMARY KEY,
  user_id TEXT,
  token TEXT,
  created_at TIMESTAMP,
  updated_at TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);