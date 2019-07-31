const express = require("express");
const bodyParser = require("body-parser");
const app = express();
const database = require("database");
app.use(bodyParser.json()); // support json encoded bodies
app.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies

const toString = book => book.author + " " + book.released + " " + book.name;

app.get("api/books", (req, res) => {
  const released = parseInt(req.query.released);
  const author = req.query.author;
  var books;
  if (released && author) {
    books = database.getByReleaseAndAuthor(released, author);
  } else if (released) {
    books = database.getByRelease(released);
  } else if (author) {
    books = database.getByAuthor(released);
  } else {
    books = database.get();
  }
  switch (req.header) {
    case "text/plain":
      res.send(books.map(toString).join(", "));
      break;

    case "application/json":
      res.send(books);
      break;

    default:
      res.status(405);
      res.send("Unsupported media type");
  }
});

app.delete("api/books/:bookId", (req, res) => {
  const id = parseInt(req.params.bookId);

  var success;
  if (id) {
    success = database.remove(id);
  } else {
    success = false;
  }

  if (success) {
    res.status(200);
    res.send("Deleted");
  } else {
    res.status(400);
  }
});

app.post("api/books", (req, res) => {
  const name = req.body.name;
  const released = parseInt(req.body.released);
  const author = req.body.author;
  const id = parseInt(req.body.id);
  if (name && released && author) {
    if (id) {
      database.update({
        key: id,
        book: {
          author: author,
          released: released,
          name: name
        }
      });
    } else {
      database.add({
        author: author,
        released: released,
        name: name
      });
    }
    res.status(201);
    res.send("success");
  } else {
    res.status(400);
    res.send("Bad format");
  }
});

app.listen(3000, function() {
  console.log("listening on 3000");
});
