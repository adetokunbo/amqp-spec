# amqp-compiled

[![GitHub CI](https://github.com/adetokunbo/amqp-compiled/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/amqp-compiled/actions)
[![Stackage Nightly](http://stackage.org/package/amqp-compiled/badge/nightly)](http://stackage.org/nightly/package/amqp-compiled)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/amqp-compiled/blob/master/LICENSE)

`amqp-compiled` derives types from the AMQP XML specification using [Template Haskell]

I.e the types are not defined in haskell source code, but are generated during
haskell compilation via applying TemplateHaskell macros to the AMQP XML source.


In addition generating the types, also generated are typeclass instances that
allow the types to be encoded and decoded from a byte stream as indicated in the
XML specification.


```

[Template Haskell]:   <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/template_haskell.html>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/amqp-compiled.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=amqp-compiled>
[hackage-badge]:      <https://img.shields.io/hackage/v/amqp-compiled.svg>
[hackage]:            <https://hackage.haskell.org/package/amqp-compiled>
