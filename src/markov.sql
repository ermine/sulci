CREATE TABLE IF NOT EXISTS words (word1 varchar(256), word2 varchar(256), counter int);
CREATE INDEX IF NOT EXISTS word1word2 ON words (word1, word2);

-- @check_pair
SELECT 1 FROM words WHERE word1=@word1 AND word2=@word2 LIMIT 1;
-- @add_pair
INSERT INTO words (word1, word2, counter) VALUES;
-- @update_pair
UPDATE words SET counter=counter+1 WHERE word1=@word1 AND word2=@word2;

-- @get_sum
SELECT coalesce(sum(counter),0) FROM words WHERE word1=@word1 LIMIT 1;
-- @get_pair
SELECT word2, counter FROM words WHERE word1=@word1;

-- @count
SELECT COUNT(*) FROM words;

-- @get_top
SELECT word1, word2, counter FROM words WHERE word1!='' AND word2!='' ORDER BY counter DESC LIMIT 10;
