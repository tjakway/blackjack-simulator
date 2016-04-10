--create types and tables

CREATE TYPE card_value AS ENUM ('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K');
CREATE TYPE card_suit AS ENUM ('Clubs', 'Diamonds', 'Hearts', 'Spades');
CREATE TYPE card AS (value card_value, suit card_suit, visible bool);

CREATE TABLE IF NOT EXISTS runs (
                                run_id bigserial NOT NULL PRIMARY KEY,
                                run_date timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP);

CREATE TABLE IF NOT EXISTS players (
                                run_id bigint NOT NULL REFERENCES runs,
                                player_no int NOT NULL, -- 0 can be assumed as always the dealer
                                ai_type text NOT NULL,
                                PRIMARY KEY (run_id, player_no));

CREATE TABLE IF NOT EXISTS matches (
                                run_id bigint NOT NULL REFERENCES runs,
                                match_no int NOT NULL,
                                PRIMARY KEY (run_id, match_no));

CREATE TABLE IF NOT EXISTS hands (
                                hand_id bigserial NOT NULL PRIMARY KEY,
                                run_id bigint NOT NULL REFERENCES runs,
                                match_no int NOT NULL,
                                cards card ARRAY NOT NULL,
                                player_no int NOT NULL,
                                victor int NOT NULL, --the player ID (in the current run) of the victor.  
                                                     --Will be either player_no or 0
                                UNIQUE (run_id, match_no, cards),
                                FOREIGN KEY (run_id, match_no) REFERENCES matches,
                                FOREIGN KEY (run_id, player_no) REFERENCES players,
                                FOREIGN KEY (run_id, victor) REFERENCES players);

