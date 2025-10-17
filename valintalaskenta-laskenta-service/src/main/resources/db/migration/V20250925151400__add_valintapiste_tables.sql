create type osallistumistieto as enum (
    'EI_OSALLISTUNUT',
    'OSALLISTUI',
    'MERKITSEMATTA',
    'EI_VAADITA'
);

create table valintapiste (
    hakemus_oid varchar not null,
    tunniste varchar not null,
    arvo varchar,
    osallistuminen osallistumistieto,
    tallettaja varchar not null,
    system_time tstzrange not null default tstzrange(now(), null, '[)'),
    transaction_id bigint not null default txid_current(),
    primary key (hakemus_oid, tunniste)
);

create table valintapiste_history (like valintapiste);

create function update_valintapiste_history() returns trigger
    language plpgsql
as $$
begin
insert into valintapiste_history (
    hakemus_oid,
    tunniste,
    arvo,
    osallistuminen,
    tallettaja,
    system_time,
    transaction_id
) values (
    old.hakemus_oid,
    old.tunniste,
    old.arvo,
    old.osallistuminen,
    old.tallettaja,
    tstzrange(lower(old.system_time), now(), '[)'),
    old.transaction_id
);
return null;
end;
$$;

create trigger delete_valintapiste_history
    after delete
    on valintapiste
    for each row
    execute procedure update_valintapiste_history();

create trigger update_valintapiste_history
    after update
    on valintapiste
    for each row
    execute procedure update_valintapiste_history();

CREATE INDEX IF NOT EXISTS valintapiste_modified_at_idx ON valintapiste (lower(system_time));
CREATE INDEX IF NOT EXISTS valintapiste_history_modified_at_idx ON valintapiste_history (upper(system_time));

comment on table valintapiste is 'Hakijan valintakokeiden osallistumis- ja pistetiedot';
comment on column valintapiste.hakemus_oid is 'Hakemuksen OID';
comment on column valintapiste.tunniste is 'Valintakokeen tunniste (syötettävä arvo)';
comment on column valintapiste.arvo is 'Valintakokeen pisteet';
comment on column valintapiste.osallistuminen is 'Valintakokeen osallistumistieto';
comment on column valintapiste.tallettaja is 'Tallentajan OID';
comment on column valintapiste.system_time is 'Tallennuksen aikaleima';
comment on column valintapiste.transaction_id is 'Tallennuksen transaktio-id';
