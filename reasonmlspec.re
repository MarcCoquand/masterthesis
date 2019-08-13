open Result;
//------------------------------------------------------------------------------
// SPEC
//
// Spec creates a pipeline for request to response. If any of the computations
// it will throw the correct error response.
type encoded = string;
type response = Response.content(encoded);
type responseBuilder('a) = Response.t('a);
type server = Request.t => response;
type t('input, 'output) =
  | Top: t(('a, 'a), ('a, 'a))
  | Exact(string): t(('a, 'b), ('a, 'b))
  | Custom(string => option('a)): t(('a => 'b, 'c), ('b, 'c))
  | Query(string, string => option('a))
    : t((option('a) => 'b, 'c), ('b, 'c))
  | Slash(t(('a, 'b), ('c, 'd)), t(('c, 'd), ('e, 'f)))
    : t(('a, 'b), ('e, 'f))
  | Integer: t((int => 'a, 'b), ('a, 'b))
  | Path(t(('a, 'b), ('c, 'd))): t(('a, 'b), ('c, 'd))
  | Method(HttpMethod.t): t(('a, 'b), ('a, 'b))
  | Accept(list((MediaType.t, 'b => string))): t(('a, string), ('a, 'b))
  | ContentType(list((MediaType.t, string => option('a))))
    : t(('a => 'b, 'c), ('b, 'c))
  | Map(Status.code, 'a, t(('a, string), (Result.t('c), 'c)))
    : t(Request.t, Result.t(response))
  | OneOf(list(t(Request.t, Result.t(response)))): t(Request.t, response);

type part('handlerArguments, 'unencodedResponse) = (
  'handlerArguments,
  responseBuilder('unencodedResponse),
  Request.t,
);

let apply = ((arg, res, req): part('a => 'b, 'c), a): part('b, 'c) => (
  arg(a),
  res,
  req,
);

let setReq = (reqChanger, (arg, res, req): part('a, 'b)): part('a, 'b) => (
  arg,
  res,
  reqChanger(req),
);

let id: type a. a => a = x => x;

/**
* A spec is a function that takes a request and returns an encoded response. The
* best way to create these with automatic error handling is by composing parts
* together and merge them using the success function.
*/
module Accept = {
  type decoder('a) = string => option('a);
  /**
  * Accept a request body of type application/json by providing a way to decode
  * it.
  */
  let json = (decoder: Json.Decode.decoder('a)): (MediaType.t, decoder('a)) => (
    MediaType.Json,
    a =>
      Json.parse(a)
      ->Belt.Option.flatMap(value =>
          try (Some(decoder(value))) {
          | Json.Decode.DecodeError(s) => None
          }
        ),
  );
};

module Contenttype = {
  type encoder('a) = 'a => string;
  /**
  * Serialize the body to a text and set content type to text/plain.
  */
  let plain = (encoder: encoder('a)) => (MediaType.Plain, a => encoder(a));

  /**
  * Serialize the response body to application/json and set the content type to
  * applicaiton/json.
  */
  let json = (jsonEncoder: 'a => Js.Json.t): (MediaType.t, encoder('a)) => (
    MediaType.Json,
    a => a |> jsonEncoder |> Json.stringify,
  );
};

/**
* Reads accept header of request and sets the encoder to the matching content
* type. If content type is not found response is set to Unsupported Media Type
* with status code 415.
*/
let accept:
  type a handler unencoded.
    (list((MediaType.t, unencoded => string)), part(handler, string)) =>
    Result.t(part(handler, unencoded)) =
  (contentTypes, builder) => {
    let makeList = x =>
      Belt.Map.fromArray(
        Array.of_list(x),
        ~id=(module Request.MediaComparer),
      );
    let (h, res, req) = builder;
    let makeEncoder = (req: Request.t) =>
      Result.attempt(
        ~message="Unsupported Media Type: " ++ MediaType.toString(req.accept),
        ~code=Status.UnsupportedMediaType415,
        ~contenttype=MediaType.Plain,
        Belt.Map.get(makeList(contentTypes)),
        req.accept,
      );
    //           --- (encoder(req))--------
    //          /                           \
    // (req) =>                              => (handler,encoded(response))
    //          \                           /
    //           --- response -------------

    let maybeEncoder = makeEncoder(req);
    Result.map(
      encoder =>
        (
          h,
          Response.contramap(encoder, res)
          |> Response.setContentType(req.accept),
          req,
        ),
      maybeEncoder,
    );
  };

/**
 * Parses a query parameter from URL using a given parser and feeds it as an
 * optional parameter into the handler.
 * <pre><code>
 * // Notice that it requires the parameter published!
 * let books = (published: option(int)): list(book) => ...
 *
 * let api = Spec.query("published", Optional.int) |> Spec.handler(books)
 * </code></pre>
 */
let query =
    (parameter, parser, builder: part(option('a) => 'handler, 'response))
    : part('handler, 'response) => {
  let (h, res, req) = setReq(Request.parseQueries, builder);

  (h(Request.query(parameter, parser, req)), res, req);
};

/**
 * Takes a list of content types to decode the body of the request.
 * The decoded body is then feeded as an argument into the handler.
 */
let contentType:
  type a b c.
    (list((MediaType.t, string => option(a))), part(a => b, c)) =>
    Result.t(part(b, c)) =
  (contentTypes, builder) => {
    let makeMap = x =>
      Belt.Map.fromArray(
        Array.of_list(x),
        ~id=(module Request.MediaComparer),
      );
    let acceptsMap = makeMap(contentTypes);
    let decodeBody = (req: Request.t) =>
      Result.attempt(
        ~message=
          "Could not parse body with content type: "
          ++ MediaType.toString(req.contentType),
        ~code=Status.BadRequest400,
        ~contenttype=MediaType.Plain,
        Request.decodeBody(acceptsMap),
        req,
      );
    let (h, res, req) = builder;
    decodeBody(req) |> Result.map(body => (h(body), res, req));
  };

//------------------------------------------------------------------------------
// PARSING
let rec attempt:
  type a b c d. (t((a, b), (c, d)), part(a, b)) => Result.t(part(c, d)) =
  (route, state) => {
    let (handler, response, request) = state;
    switch (route) {
    | Top => Ok(state)

    | Exact(str) =>
      parseExact(str, request)
      |> Result.map(newReq => (handler, response, newReq))
    | Integer =>
      parseInt(request)
      |> Result.map(((i, newReq)) => (handler(i), response, newReq))
    | Custom(checker) =>
      parseCustom(checker, request)
      |> Result.map(((v, newReq)) => (handler(v), response, newReq))

    | Path(thePath) => attempt(thePath, state)

    | Query(param, decoder) =>
      query(param, decoder, state) |> (res => Result.Ok(res))

    | Slash(before, after) => attempt(before, state) >>= attempt(after)

    | Method(ofType) =>
      requestUsesMethod(ofType, request)
        ? Result.Ok(state) : Result.invalidParse
    | Accept(l) => accept(l, state)
    | ContentType(l) => contentType(l, state)
    };
  };

let rec build: type a. (t(Request.t, a), Request.t) => a =
  (route, request) =>
    switch (route) {
    | Map(code, subValue, subParser) =>
      attempt(subParser, (subValue, Response.default, request))
      |> Result.map(((value, response, _)) =>
           response
           |> Response.setCode(code)
           |> Response.fromResult
           |> Response.encode(value)
         )
    | OneOf(list) =>
      switch (list) {
      | [route, ...rest] =>
        build(route, request)
        |> (
          fun
          | Ok(response) => response
          | Failed(_, _, _) => build(OneOf(rest), request)
        )
      | [] =>
        Response.error(
          ~message="Not found",
          ~code=Status.NotFound404,
          ~method=MediaType.Plain,
        )
      }
    };

// Recall that responseBuilder(Request.t) = Request.t => response.
let (>-) = (a, b) => Slash(a, b);
let endpoint = (~handler, ~success, ~spec) => Map(success, handler, spec);
let oneOf = routes => List.map(r => r, routes) |> (r => OneOf(r));
let single = route => OneOf([route]);
let contenttype = l => ContentType(l);
let accept = l => Accept(l);

let uri = u => Path(u);

module Path = {
  let top = Top;
  let is = (str: string) => Exact(str);
  let takeInt = Integer;
  let takeText = Custom(value => Some(value));
  let takeCustom = (f: string => option('a)) => Custom(f);
  let query = (~parameter, ~decoder) => Query(parameter, decoder);
};

module Method = {
  let get = Method(HttpMethod.GET);
  let post = Method(HttpMethod.POST);
  let delete = Method(HttpMethod.DELETE);
  let put = Method(HttpMethod.PUT);
  let update = Method(HttpMethod.UPDATE);
  let head = Method(HttpMethod.HEAD);
  let option = Method(HttpMethod.OPTION);
  let connect = Method(HttpMethod.CONNECT);
  let trace = Method(HttpMethod.TRACE);
  let patch = Method(HttpMethod.PATCH);
};

let make: t(Request.t, response) => server =
  (specification, request) => {
    let firstDropped =
      String.sub(request.url, 1, String.length(request.url) - 1);
    let requestPrim = {
      ...request,
      url: firstDropped,
      length: request.length - 1,
    };
    build(specification, requestPrim);
  };