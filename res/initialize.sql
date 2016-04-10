--create types and tables

CREATE TYPE card_value AS ENUM ('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K');
CREATE TYPE card_suit AS ENUM ('Clubs', 'Diamonds', 'Hearts', 'Spades');
CREATE TYPE card AS (value card_value, suit card_suit, visible bool);

CREATE TABLE IF NOT EXISTS runs (
                                run_id bigserial NOT NULL PRIMARY KEY,
                                run_date timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP);

CREATE TABLE IF NOT EXISTS players (
                                run_id bigint NOT NULL REFERENCES runs,
                                player_id int NOT NULL, -- 0 can be assumed as always the dealer
                                ai_type text NOT NULL,
                                PRIMARY KEY (run_id, player_id));

CREATE TABLE IF NOT EXISTS matches (
                                run_id bigint NOT NULL REFERENCES runs,
                                match_id int NOT NULL,
                                PRIMARY KEY (run_id, match_id));

CREATE TABLE IF NOT EXISTS hands (
                                hand_id bigserial NOT NULL PRIMARY KEY,
                                run_id bigint NOT NULL REFERENCES runs,
                                match_id int NOT NULL,
                                cards card ARRAY NOT NULL,
                                player_id int NOT NULL,
                                victor int NOT NULL, --the player ID (in the current run) of the victor.  
                                                     --Will be either player_id or 0
                                UNIQUE (run_id, match_id, cards),
                                FOREIGN KEY (run_id, match_id) REFERENCES matches,
                                FOREIGN KEY (run_id, player_id) REFERENCES players,
                                FOREIGN KEY (run_id, victor) REFERENCES players);

