type book = {
  title: string,
  author: string,
  year: int,
  id: int,
};

module Encoders = {
  let json = myBook => {
    open! Json.Encode;
    Json.Encode.(
      object_([
        ("title", string(myBook.title)),
        ("year", int(myBook.year)),
        ("author", string(myBook.author)),
        ("id", int(myBook.id)),
      ])
    );
  };
  let jsonList = Json.Encode.list(json);
  let plain = book => book.title;
  let plainList = books =>
    List.fold_right((book, str) => str ++ plain(book), books, "");
};
module Decoders = {
  let json = json => {
    open! Json.Decode;
    Json.Decode.{
      title: json |> field("title", string),
      year: json |> field("year", int),
      author: json |> field("author", string),
      id: json |> field("id", int),
    };
  };
  let safeJson = myBook =>
    try (Some(json(myBook))) {
    | Json.Decode.DecodeError(s) => None
    };
  let int = s =>
    try (Some(int_of_string(s))) {
    | Failure("int_of_string") => None
    };
  let jsonWithKey = Json.Decode.tuple2(Json.Decode.int, json);
};

let deleteFromDatabase = key => {
  let success = Database.delete(key);

  if (success) {
    Result.Ok("Deleted");
  } else {
    Result.Failed(
      "Entry does not exist",
      Status.BadRequest400,
      MediaType.Plain,
    );
  };
};

let replace = (key, book) => {
  let success = Database.replace(key, Encoders.json(book));
  if (success) {
    Result.Ok("Added");
  } else {
    Result.Failed("Database failure", Status.Error500, MediaType.Plain);
  };
};

let insert = book => {
  let success = Database.insert(Encoders.json(book));
  if (success) {
    Result.Ok("Added");
  } else {
    Result.Failed("Database failure", Status.Error500, MediaType.Plain);
  };
};

let getFromDatabase = (queryAuthor, queryReleased) => {
  let cmpAuthor = (author, book) => book.author == author;
  let cmpReleased = (released, book) => book.year == released;
  switch (queryAuthor, queryReleased) {
  | (Some(author), Some(released)) =>
    Result.Ok(
      Database.filter(Decoders.safeJson, book =>
        cmpAuthor(author, book) && cmpReleased(released, book)
      ),
    )
  | (Some(author), None) =>
    Result.Ok(Database.filter(Decoders.safeJson, cmpAuthor(author)))
  | (None, Some(released)) =>
    Result.Ok(Database.filter(Decoders.safeJson, cmpReleased(released)))
  | (None, None) => Result.Ok(Database.get(Decoders.safeJson))
  };
};

module Endpoint = {
  open Spec.Router;

  let replaceId: type a. route(a) =
    endpoint(
      ~handler=replace,
      ~success=Status.Ok200,
      ~spec=
        Method.post
        >- Path.is("api")
        >- Path.is("books")
        >- Path.takeInt
        >- contenttype([Spec.Accept.json(Decoders.json)]),
    );

  let post: type a. route(a) =
    endpoint(
      ~handler=insert,
      ~success=Status.Created201,
      ~spec=
        Method.post
        >- Path.is("api")
        >- Path.is("books")
        >- contenttype([Spec.Accept.json(Decoders.json)]),
    );

  let get: type a. route(a) =
    endpoint(
      ~handler=getFromDatabase,
      ~success=Status.Ok200,
      ~spec=
        Method.get
        >- Path.is("api")
        >- Path.is("books")
        >- Path.query(~parameter="author", ~decoder=s => Some(s))
        >- Path.query(~parameter="released", ~decoder=Decoders.int)
        >- accept([
             Spec.Contenttype.json(Encoders.jsonList),
             Spec.Contenttype.plain(Encoders.plainList),
           ]),
    );

  let delete: type a. route(a) =
    endpoint(
      ~handler=deleteFromDatabase,
      ~success=Status.Ok200,
      ~spec=
        Method.delete >- Path.is("api") >- Path.is("books") >- Path.takeInt,
    );
  let router = oneOf([get, post, replaceId, delete]);
};

Spec.listen(3000, Endpoint.router);
