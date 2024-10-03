create index if not exists valinnanvaihe_history_systime_idx ON valinnanvaihe_history (upper(system_time));
create index if not exists valintatapajono_history_systime_idx ON valintatapajono_history (upper(system_time));
create index if not exists jonosija_history_systime_idx ON jonosija_history (upper(system_time));
create index if not exists valintakoe_osallistuminen_history_systime_idx ON valintakoe_osallistuminen_history (upper(system_time));
create index if not exists hakutoive_history_systime_idx ON hakutoive_history (upper(system_time));
create index if not exists valintakoe_valinnanvaihe_history_systime_idx ON valintakoe_valinnanvaihe_history (upper(system_time));
create index if not exists valintakoe_history_systime_idx ON valintakoe_history (upper(system_time));
