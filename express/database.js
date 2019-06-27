var database = new Map();
var index = 0;

export const add = obj => {
  database.set(index, obj);
  index++;
};

//Initialize with some mock data
add({
  author: "J.K Rowling",
  released: 1996,
  name: "The  Philosopher's Stone"
});
add({
  author: "J.K Rowling",
  released: 1997,
  name: "The Chamber of Secrets"
});
add({
  author: "J.K Rowling",
  released: 1998,
  name: "The Prisoner of Azkaban"
});
add({
  author: "George RR Martin",
  released: 1996,
  name: "A Game of Thrones"
});
add({
  author: "George RR Martin",
  released: 1998,
  name: "A Clash of Kings"
});

// Returns true if successful
export const remove = database.delete;

export const getById = database.get;

// [id, obj]
export const update = val => {
  const { key, book } = val;
  database.set(key, book);
};

const filterByKey = boolFunc => {
  var values = [];
  const f = (value, key, map) => {
    if (boolFunc(key)) {
      values.push([key, value]);
    }
  };
  database.forEach(f);
  return values;
};

const filterByValue = boolFunc => {
  var values = [];
  const f = (value, key, map) => {
    if (boolFunc(value)) {
      values.push([value]);
    }
  };
  database.forEach(f);
  return values;
};

export const get = () => database.entries();

export const getByAuthor = author => {
  const comparison = a => a.author === author;
  filterByValue(comparison);
};

export const getByRelease = year => {
  const comparison = a => a.released === year;
  filterByValue(comparison);
};

export const getByReleaseAndAuthor = (year, author) => {
  const comparison = a => a.released === year && a.author === author;
  filterByValue(comparison);
};
