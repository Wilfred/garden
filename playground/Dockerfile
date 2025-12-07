# Separate stage for building the Garden binary.
FROM rust:1.85 AS garden-builder
WORKDIR /build
COPY Cargo.toml Cargo.lock build.rs ./
COPY src/ ./src/
RUN cargo build --release

# The stage for running the playground backend.
FROM node:20-slim

WORKDIR /app

COPY --from=garden-builder /build/target/release/garden /usr/local/bin/garden

COPY playground/package*.json ./
RUN npm install --production
COPY playground/index.js ./

EXPOSE 3000
CMD ["node", "index.js"]
