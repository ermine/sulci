CREATE TABLE IF NOT EXISTS talkers (jid varchar, nick varchar, room varchar, words int, me int, sentences int);
CREATE INDEX IF NOT EXISTS talkers_idx ON talkers (jid, room);
CREATE INDEX IF NOT EXISTS words_idx ON talkers (words);
-- @test_nick
SELECT 1 FROM talkers WHERE nick=@nick AND room=@room LIMIT 1;
-- @test_jid
SELECT 1 FROM talkers WHERE jid=@jid AND room=@room limit 1;
-- @update_by_nick
UPDATE talkers SET words=words+@words, sentences=sentences+@sentences, me=me+@me WHERE nick=@nick AND room=@room;
-- @update_by_jid
UPDATE talkers SET words=words+@words, sentences=sentences+@sentences, me=me+@me WHERE jid=@jid AND room=@room;
-- @insert_new
INSERT INTO talkers (jid, nick, room, words, me, sentences) VALUES;
--@select_talkers_by_nick
SELECT nick, words, me, sentences FROM talkers WHERE room=@room AND nick LIKE @nick ORDER BY words DESC, sentences ASC;
-- @select_talkers_limit
SELECT nick, words, me, sentences FROM talkers WHERE room=@room ORDER BY words DESC, sentences ASC LIMIT 10;

           



