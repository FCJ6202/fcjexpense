# Use the official Haskell image
FROM haskell:9.6.4

# Set the working directory inside the container
WORKDIR /app

# Install system dependencies (zlib is commonly needed)
RUN apt-get update && apt-get install -y zlib1g-dev

# Copy cabal file and source code
COPY fcjexpense.cabal .
COPY app/ app/

# Download and install dependencies
RUN cabal update && cabal build --only-dependencies

# Copy the rest of your project files (if any)
COPY . .

# Build the project
RUN cabal build

# Expose the port your app will run on
EXPOSE 8080

# Run the application
CMD ["cabal", "run"]