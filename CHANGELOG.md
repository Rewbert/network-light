# Revision history for network-light

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- YYYY-mm-dd

* Just cleanups

## 0.1.0.2 -- 2025-09-30

* Add README with example
* Small changes to make it compile with both mhs and ghc

## 0.1.0.4 -- 2026-04-22

* Previous version only worked non-blockingly with GHC, not MHS (as MHS did not implement anything to combat non-blocking IO). I have added simple support for non-blocking IO in MHS, whereas a green thread may continue running while another is blocking on a call in this file.
* Refactoring work.
* Small changes to README

## 0.1.0.5 -- 2026-07-16

* Made it work with GHC again, and added some tests.
* Worked on the Haddock documentation, before eventually putting it on Hackage.

## 0.1.0.6 -- 2026-08-06

* Verified that it works with a bunch more GHC cmopilers.