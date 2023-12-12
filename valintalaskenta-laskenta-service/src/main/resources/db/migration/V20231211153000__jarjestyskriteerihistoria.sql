CREATE TABLE jarjestyskriteerihistoria (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    historia text NOT NULL
);
