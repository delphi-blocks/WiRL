# WiRL: Delphi RESTful Library

<br />
<p align="center">
  <img src="./media/logo.png" alt="Delphi RESTful Library" width="300" />
</p>

![Top language](https://img.shields.io/github/languages/top/delphi-blocks/WiRL)
[![GitHub license](https://img.shields.io/github/license/delphi-blocks/WiRL)](https://github.com/delphi-blocks/WiRL/blob/master/LICENSE)
[![GitHub issues](https://img.shields.io/github/issues/delphi-blocks/WiRL)](https://github.com/delphi-blocks/WiRL/issues)
[![GitHub PR](https://img.shields.io/github/issues-pr/delphi-blocks/WiRL)](https://github.comdelphi-blocks/WiRL/pulls)
[![GitHub release](https://img.shields.io/github/release/delphi-blocks/WiRL)](https://github.com/delphi-blocks/WiRL/release)
![GitHub commit activity](https://img.shields.io/github/commit-activity/m/delphi-blocks/WiRL)
![GitHub last commit](https://img.shields.io/github/last-commit/delphi-blocks/WiRL)
![GitHub contributors](https://img.shields.io/github/contributors-anon/delphi-blocks/WiRL)

## Getting Started

Please follow the documentation at [wirl.delphiblocks.dev](https://wirl.delphiblocks.dev/)!

## What is WiRL?

**WiRL** was created to simplify RESTful service implementation in Delphi but, more importantly, to enable maximum interoperability with REST clients written in other languages and tools.

WiRL takes after Java JAX-RS specifications and tries to be compliant with the 6 REST constraints.

WiRL is a high-level REST framework exposing plain Delphi objects (PODO) as RESTful web resources by applying [attributes](http://docwiki.embarcadero.com/RADStudio/Seattle/en/Overview_of_Attributes) to these classes.

```Delphi
[Path('customers')]
TCustomerResource = class
public
  [GET]
  [Produces('TMediaType.APPLICATION_JSON')]
  function SelectCustomers: TCustomerList;

  [POST]
  [Consumes('TMediaType.APPLICATION_JSON')]
  [Produces('TMediaType.APPLICATION_JSON')]
  function InsertCustomer(ACustomer: TCustomer): TCustomer;
end;
```
WiRL has a strong HTTP content negotiation and  defines attributes to bind specific URI patterns and HTTP operations to individual methods of your Delphi class. It has parameter injection attributes so that you can easily pull in information from the HTTP request. It has message body readers and writers that allow you to decouple data format marshalling and unmarshalling from your Delphi objects. It has exception mappers that can map an exception to an HTTP response code and message.

WiRL uses 3 submodules:
1. [Delphi JOSE and JWT Library](https://github.com/paolo-rossi/delphi-jose-jwt) for the JSON Web Token creation and validation
2. [Neon - Serialization Library for Delphi](https://github.com/paolo-rossi/delphi-neon) to convert Delphi simple types, objects, records, arrays, etc... to the JSON format
3. [OpenAPI 3 for Delphi](https://github.com/paolo-rossi/OpenAPI-Delphi) for the OpenAPI documentation generation
