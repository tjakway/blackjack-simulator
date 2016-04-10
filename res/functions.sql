--create functions

CREATE OR REPLACE FUNCTION players(int) RETURNS SETOF players AS $$
  SELECT * FROM players WHERE run_id = $1 ORDER BY player_no;
  $$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION matches(int) RETURNS SETOF matches AS $$
  SELECT * FROM matches WHERE run_id = $1 ORDER BY match_no;
  $$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION hands(int) RETURNS SETOF hands AS $$
  SELECT * FROM hands WHERE run_id = $1 ORDER BY match_no, cards;
  $$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION hands(int, int) RETURNS SETOF hands AS $$
  SELECT * FROM hands WHERE run_id = $1 AND match_no = $2 ORDER BY cards;
  $$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION next_player_no (int) RETURNS int AS $$
  SELECT CASE WHEN EXISTS (SELECT 1 FROM runs WHERE run_id = $1) THEN
           COALESCE((SELECT MAX(player_no) FROM players WHERE run_id = $1), 0) + 1 END
           $$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION next_match_no (int) RETURNS int AS $$
  SELECT CASE WHEN EXISTS (SELECT 1 FROM runs WHERE run_id = $1) THEN
           COALESCE((SELECT MAX(match_no) FROM matches WHERE run_id = $1), 0) + 1 END
           $$ LANGUAGE SQL;

--TODO: rewrite these functions:
CREATE OR REPLACE FUNCTION card_to_int (card) RETURNS int AS $$
  SELECT ((SELECT enumsortorder::int-1 FROM pg_enum WHERE enumtypid = 'card_suit'::regtype AND enumlabel = ($1).suit::name) * 13 +
            (SELECT enumsortorder::int-1 FROM pg_enum WHERE enumtypid = 'card_value'::regtype AND enumlabel = ($1).value::name) + 1) *
                     CASE WHEN ($1).visible THEN 2 ELSE 1 END
                     $$ LANGUAGE SQL; -- SELECT card_to_int(('3', 'Spades', false))

CREATE OR REPLACE FUNCTION int_to_card (int) RETURNS card AS $$
  SELECT ((SELECT enumlabel::card_value FROM pg_enum WHERE enumtypid = 'card_value'::regtype AND enumsortorder = ((($1-1)%13)+1)::real),
            (SELECT enumlabel::card_suit  FROM pg_enum WHERE enumtypid = 'card_suit'::regtype  AND enumsortorder = (((($1-1)/13)::int%4)+1)::real),
                      $1 > (13*4))::card
                      $$ LANGUAGE SQL; -- SELECT i, int_to_card(i) FROM generate_series(1, 13*4*2) i
