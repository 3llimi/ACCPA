# Build the Stella Rust typechecker
# Run from Submissions/a.alimi: docker build -f Dockerfile .
FROM rust:1-bookworm AS builder

WORKDIR /app
COPY Cargo.toml Cargo.lock* ./
COPY src/ ./src/

RUN cargo build --release --bin typechecker

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/typechecker /usr/local/bin/typechecker
ENTRYPOINT ["/usr/local/bin/typechecker"]
