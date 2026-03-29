FROM haskell:9.6 AS builder

WORKDIR /app
COPY . .

RUN cabal update
RUN cabal build
RUN cp "$(cabal list-bin exe:fish-project)" /tmp/fish-project

FROM debian:bookworm-slim

WORKDIR /app

RUN apt-get update && apt-get install -y libgmp10 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /tmp/fish-project /usr/local/bin/fish-project
COPY --from=builder /app/data /app/data

EXPOSE 3000

CMD ["fish-project"]