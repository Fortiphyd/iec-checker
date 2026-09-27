# Releasing

Releases are built by GitHub Actions when a version tag is pushed.

1. Set the version in `dune-project` (`(version X.Y.Z)`) and run `dune build`,
   which updates `iec_checker.opam`. `iec_checker --version` prints it.
2. Move the entries under *Unreleased* in `CHANGES.md` to a section for the
   version.
3. Commit, and push to `master`.
4. Tag the commit and push the tag:

   ```bash
   git tag vX.Y.Z
   git push origin vX.Y.Z
   ```

The tag must match the version in `dune-project`, or the build fails.

Pushing the tag runs two workflows:

- **Release** (`.github/workflows/release.yml`) builds the binaries for
  Linux x86_64 (static) and macOS arm64, checks that each runs, and publishes a GitHub release with an archive for each platform and
  their `SHA256SUMS`. Each archive has the binary, the license, the README,
  the changelog, the example configuration and these docs. The release notes
  are generated from the commits since the previous release; edit them on
  GitHub afterwards if needed.
- **Docker release** publishes `ghcr.io/fortiphyd/iec-checker:vX.Y.Z` and
  `:latest`. **Docker nightly** publishes `:nightly` every Monday, or when run
  by hand.

The Release workflow also runs, without publishing, when a push changes the
build (`dune-project`, `dune` files or the workflow itself), and it can be
run by hand from the Actions tab. The binaries of such runs are artifacts of
the run.

The first image pushed to the registry is private. To let anyone pull it, set
its visibility to public in the package settings on GitHub.
