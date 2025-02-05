# Use Debian-based Elixir image
FROM elixir:1.15.7-slim

# Install dependencies
RUN apt-get update 
RUN apt-get install -y \
    build-essential \
    bash \
    openssh-client \
    postgresql-client \
    git \
 && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /app

# Install Hex and Rebar
RUN mix local.hex --force && mix local.rebar --force

# Copy the mix files and install dependencies
COPY mix.exs mix.lock ./
RUN mix deps.get 
RUN mix deps.compile


# Copy the rest of the application
COPY . .

# Compile the application
RUN mix compile

# Expose the Phoenix server port
EXPOSE 4000

# Start the Phoenix application
CMD ["mix", "phx.server"]
