\connect harlan_whatifcsh;

drop table if exists suggestions;

create table suggestions (
    id bigserial primary key,
    description varchar(140) not null,
    upvotes integer not null,
    downvotes integer not null
);

alter table suggestions owner to harlan_whatifcsh;
