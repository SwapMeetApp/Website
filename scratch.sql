select *
from books;
select books.*,
    array_agg(authors.name)
from books
    inner join authors on books.id = authors.bid
group by books.id;
select *
from books
    inner join authors on books.id = authors.bid
    and authors.name = 'Rupi Kaur';
-- testing out a cross join
select books.isbn,
    authors.name,
    books.title,
    books.self_link,
    books.id,
    authors.bid
from authors
    cross join books
group by books.isbn,
    authors.name,
    books.title,
    books.self_link,
    books.id,
    authors.bid;
-- testing out GROUPING
select array_agg(books.id) as book_id,
    array_agg(books.title) as book_title,
    array_agg(authors.name) as author_name,
    books.isbn
from books
    inner join authors on books.id = authors.bid
group by books.isbn;