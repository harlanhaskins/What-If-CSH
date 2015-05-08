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

drop table if exists vote cascade;
create table vote (
    suggestion_id bigint not null references suggestion (id),
    vote integer not null CHECK (vote = -1 or vote = 1),
    member varchar(100) default 'nobody' not null
);
alter table vote owner to harlan_whatifcsh;

create index "suggestion_id_index" on suggestion (id);
create index "suggestion_submitter_index" on suggestion (submitter);
create index "vote_member_index" on vote (member);
create index "vote_suggestion_id_index" on vote (suggestion_id);
