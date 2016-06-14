# opensourcelibs.com
An index of cool open source libraries for your projects.

When looking for a library to solve a specific problem, I usually google for "<the platform> <the problem>", e.g. "php validation". The issue is that unless a library is called like "awesome-validator-php", I will probably not find it. Especially when a library is new, it's hard to find it via google. This website is trying to solve that.

# Running Local
1. Clone the repository
2. Inside the repository run `nix-shell` (requires the nix package manager) to get a shell with everything ready
3. Run `make` to build
4. Start the application via `dist/main`. By default the web server starts listening on port 8080. You can specify a custom port via the `PORT` environment variable.

# Adding A Library
To add a library, [just add it to the `Library.hs` file](https://github.com/mpscholten/opensourcelibs.com/edit/master/src/Library.hs#L31). Then just create a new pull request :)

<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
