\connect harlan_whatifcsh;

drop table if exists suggestion cascade;

create table suggestion (
    id bigserial primary key,
    description varchar(140) not null,
    created_at timestamptz default current_timestamp,
    active boolean not null,
    submitter varchar(100) not null
);

alter table suggestion owner to harlan_whatifcsh;

drop type if exists vote_t cascade;
create type vote_t as enum (
    'upvote',
    'downvote'
);

drop table if exists vote cascade;
create table vote (
    suggestion_id bigint not null references suggestion (id),
    vote vote_t not null,
    member_uuid varchar(100) default 'nobody' not null
);
alter table vote owner to harlan_whatifcsh;
