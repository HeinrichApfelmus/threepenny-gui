IF EXIST "../.cabal-sandbox" (
  ghc -i../src ^
    -no-user-package-db ^
    -package-db ../.cabal-sandbox/*-packages.conf.d ^
    -DSAMPLES "%* -e main
) ELSE (
  IF EXIST ../stack.yaml (
  stack --stack-yaml "../stack.yaml" exec ^
    ghc -- -i../src ^
    -DSAMPLES "%*" -e main
  ) ELSE (
  ghc -DSAMPLES "%*" -e main
  )
)
