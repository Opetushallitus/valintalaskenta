CREATE TABLE jarjestyskriteerihistoria (
    id BIGSERIAL PRIMARY KEY,
    tunniste uuid NOT NULL DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    historia text NOT NULL,
    laskettu_uudelleen boolean NOT NULL DEFAULT false
);
