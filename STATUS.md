# miso-bulma — status

## Where things stand

Everything compiles, links, and has been visually verified running in a
real (headless) browser — **without jsaddle**. Uncommitted on `master`:

- `lib/Bulma.hs` — the component library
- `app/Main.hs` — a demo app exercising the library
- `app.cabal` — dependency/module-visibility fixes needed to actually build
- `cabal.project` — miso pin bumped from an old dev commit (`3c3c359`,
  resolving as `miso-1.9.0.0`) to the `1.14.0` release tag
  (`5e3d64c70251dace9eb5f78e1a49f3f4b448871a`); dropped the
  `flags: +template-haskell` override (was only ever needed to build
  jsaddle-wasm's TH-based iserv, not for anything we use)
- `Makefile` — fixes so `make build`/`make serve` work end-to-end; the
  jsaddle `initialSyncDepth` sed patch (see below) has been removed —
  it's no longer needed
- `.gitignore` — ignore the `public/` build output (mirrors `make clean`)

Nothing has been committed yet.

## Why the miso pin was bumped: jsaddle is gone

User asked to check whether miso still needs jsaddle for its WASM
backend, suspecting dmjio had moved to GHC's built-in wasm JS FFI
directly. Confirmed: current miso (`1.14.0.0`, latest release as of
2026-09) has **zero** jsaddle/jsaddle-wasm dependency anywhere —
checked both `miso.cabal` and `README.md` at `master`, neither
mentions jsaddle. It relies entirely on GHC's own wasm JSFFI
(`foreign import/export javascript`, `hs_start`, `post-link.mjs`, all
from the GHC toolchain itself). Our old pin (`3c3c359`, pre-1.14) was
from before that removal, which is exactly why the previous build
pulled in `jsaddle-0.9.9.4` / `jsaddle-wasm-0.1.2.1` and hit the
`initialSyncDepth` `ReferenceError` bug documented in the previous
version of this file.

Verified after the bump: `wasm32-wasi-cabal build --dry-run`'s
resolved plan contains no `jsaddle*` package at all — just `miso`,
`aeson` (ours, for `camelTo2`), `QuickCheck`, `text-iso8601`. The
`initialSyncDepth` string doesn't appear anywhere in the freshly
generated `public/ghc_wasm_jsffi.js` any more, confirming the old
Makefile sed-patch is dead weight now (removed).

## miso 1.9 → 1.14 API changes that needed code updates

All mechanical/systematic except the JSON `Value` and `textarea_`
changes:

1. **`Attribute`/`View` gained type parameters.** `Attribute action` →
   `Attribute model action`; `View model action` →
   `View context props model action` (the `context`/`props` params
   come from a new typesafe-React-props feature added in 1.11; we
   don't use it, so they're just left polymorphic). This is a purely
   mechanical, systematic rename — every one of ~130 `Attribute`
   occurrences and ~270 `View` occurrences across `lib/Bulma.hs` and
   `app/Main.hs` needed it, done via a scripted regex substitution,
   not by hand.
2. **`Href` gained a `CacheBust :: Bool` argument.** `Href url` →
   `Href url False` (our CDN URLs are already versioned, no need to
   cache-bust) in `bulmaStylesheet`.
3. **`Property`'s `Value` is now `Miso.JSON.Types.Value`, not
   aeson's.** `addClasses`'s `Property "class" (String v)` pattern
   match needed the import switched from `Data.Aeson.Types (Value(..))`
   to `Miso.JSON.Types (Value(..))` (kept `Data.Aeson.Types (camelTo2)`
   for the modifier-name case conversion, that's still aeson's). Since
   `Miso.JSON.Types.Value`'s `String` constructor holds a `MisoString`
   (not `Text`), `addClasses`/`bulmaToText` were also switched from
   `Data.Text`/`T.unwords`/`T.pack` to `Miso.String`'s `MisoString`/
   `unwords`/`pack` — this is actually *more* correct than before,
   since `MisoString` is `Text` on native/server builds but `JSString`
   under the wasm/JS backend, so hardcoding `Text` was already latently
   wrong for a wasm target even before this bump.
4. **`textarea_` is now a void element** (no children list), matching
   `input_`/`checkbox_`/`radio_`'s existing shape. Fixed `textarea` in
   `Bulma.hs` and its one call site in `Main.hs` (dropped the trailing
   `[]`).
5. **`Transition model action` renamed to `Effect context props model
   action`.** `updateModel :: Action -> Transition Model Action` →
   `Action -> Effect context props Model Action`. The lens-based update
   operators (`+=`, `-=`, `%=`) are unaffected — same API, just a
   different monad name/shape underneath.
6. **`startApp` needs an explicit `Events` argument, and the `run`
   wrapper is gone.** `main = run $ startApp app` → `main = startApp
   defaultEvents app` (`defaultEvents` comes from `Miso.Event.Types`,
   re-exported through `Miso`).

## Verification performed

- `nix develop --command cabal build all` — library and executable
  both build clean natively against `miso-1.14.0.0` (one harmless
  `-Wunused-imports` on `Miso.String`, since `Miso` already re-exports
  `ms`).
- `cabal build all --dry-run` plan grepped for `jsaddle` — zero
  matches, confirmed on both native and `wasm32-wasi-cabal`.
- `nix develop .#wasm --command make build && make optim` — full wasm
  cross-build succeeds, `public/app.wasm` + `public/ghc_wasm_jsffi.js`
  produced, no jsaddle in the build plan.
- Served `public/` with `http-server` and drove it with a headless
  Playwright Chromium:
  - Page loads with **zero console/page errors** — no patch needed
    this time.
  - All sections from `example.html`'s menu render with real Bulma
    styling (navbar, hero, typography, buttons, form, table, tags,
    breadcrumb, dropdown, card, level, media object, menu, message,
    modal, pagination, panel, tabs).
  - Clicked `+` three times → live counter went 0 → 3.
  - Clicked "Open modal" → modal opened (`.modal.is-active`).
  - Both match the pre-upgrade behavior exactly — the upgrade is a
    clean swap, not a behavior change.

## Next steps

Nothing outstanding from a correctness standpoint. Remaining is just:

1. Review the diff (it's larger this round — mechanical Attribute/View
   signature churn across both files, plus the jsaddle-removal-driven
   miso bump).
2. Commit `lib/Bulma.hs`, `app/Main.hs`, `app.cabal`, `cabal.project`,
   `Makefile`, `.gitignore` (and decide what if anything to do with
   this file).
