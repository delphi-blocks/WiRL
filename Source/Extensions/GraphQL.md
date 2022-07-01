# GraphQL

The `WiRL.GraphQL.Base` unit provides basic support for *GraphQL* for WiRL.

## Installation

You need to download the last release of  [GraphQL for Delphi](https://github.com/lminuti/graphql) and add the source directory to the search path of your application.

## Configuration

Then you need to configure GraphQL with your resolvers (see GraphQL for Delphi documentation) and tell WiRL where the `TGraphQLQuery` object is:

```pascal

  // Create the TGraphQLQuery instance
  FQuery := TGraphQLQuery.Create;

  // Register the resolvers
  FQuery.RegisterResolver(TGraphQLRttiResolver.Create(TTestApi, True));

  RESTServer := TWiRLServer.Create(Self);

  // Init WiRL server

  RESTServer.AddEngine<TWiRLEngine>('/rest')
    .SetEngineName('RESTEngine')
    .AddApplication('/app')
      .SetResources('*')
      .SetFilters('*')
      .SetReaders('*')

      // Tell WiRL where the TGraphQLQuery object is
      .Plugin.Configure<IWiRLGraphQLSetting>
        .SetGraphQLQuery(FQuery)
        .BackToApp

```

## Create the resource

In order to use GraphQL you need a resource that can handle GraphQL query. You can use *context injection* to get the `TGraphQLQuery` object and `TGraphQLPostQuery` to get the query:

```pascal

type
  [Path('/hellographql')]
  THelloWorldResource = class
  private
    [Context][Singleton] FQuery: TGraphQLQuery;
  public
    [POST, Path('/graphql')]
    [Consumes(TMediaType.APPLICATION_JSON)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function PostGraphQL([BodyParam] APostQuery: TGraphQLPostQuery): string;
  end;

implementation

function THelloWorldResource.PostGraphQL(APostQuery: TGraphQLPostQuery): string;
var
  LGraphQL: IGraphQL;
begin
  LGraphQL := FQuery.Parse(APostQuery.Query);

  Result := FQuery.Run(LGraphQL, APostQuery.Variables);
end;


```