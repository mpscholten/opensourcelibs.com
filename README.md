# opensourcelibs.com
An index of cool open source libraries for your projects

# Running Local
1. Clone the repository
2. Inside the repository run `nix-shell` (requires the nix package manager) to get a shell with everything ready
3. Run `make` to build
4. Start the application via `dist/main`. By default the web server starts listening on port 8080. You can specify a custom port via the `PORT` environment variable.
