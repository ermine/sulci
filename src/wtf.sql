CREATE TABLE IF NOT EXISTS wtf (stamp int, nick varchar, luser varchar, lserver varchar, "key" varchar, "value" varchar);
CREATE INDEX IF NOT EXISTS dfnidx ON wtf ("key");
CREATE INDEX IF NOT EXISTS dfncheck ON wtf ("key", luser, lserver);
-- @total
SELECT COUNT(*) FROM wtf;

-- @dfn_new
INSERT INTO wtf (stamp, nick, luser, lserver, "key", "value") VALUES;

-- @dfn_check_by_jid
SELECT "value" FROM wtf WHERE "key"=@key AND luser=@luser AND lserver=@lserver LIMIT 1;
-- @dfn_delete_by_jid
DELETE FROM wtf WHERE "key"=@key AND luser=@luser AND lserver=@lserver;
-- @dfn_update_by_jid
UPDATE wtf SET stamp=@stamp, nick=@nick, "value"=@value WHERE "key"=@key AND luser=@luser AND lserver=@lserver;

-- @dfn_check_by_occupant
SELECT "value" FROM wtf WHERE "key"=@key AND nick=@nick AND luser=@luser AND lserver=@lserver LIMIT 1;
-- @dfn_delete_by_occupant
DELETE FROM wtf WHERE "key"=@key AND nick=@nick AND luser=@luser AND lserver=@lserver;
-- @dfn_update_by_occupant
UPDATE wtf SET stamp=@stamp, "value"=@value WHERE "key"=@key AND nick=@nick AND luser=@luser AND lserver=@lserver;


-- @get_wtf_one
SELECT nick, "value" FROM wtf WHERE "key"=@key ORDER BY stamp DESC LIMIT 1;
-- @get_wtf_all
SELECT nick, "key", "value" FROM wtf WHERE "key"=@key ORDER BY stamp;
-- @wtf_count
SELECT count(*) FROM wtf WHERE "key"=@key;
-- @wtffind
SELECT nick, "key", "value" FROM wtf WHERE "key" LIKE @key OR "value" LIKE @value;

-- @get_rand
SELECT nick, "key", "value" FROM wtf LIMIT @rand,1;
-- @get_key_total
SELECT count(*) FROM wtf WHERE "key"=@key;
-- @get_rand_key
SELECT nick, "value" FROM wtf WHERE "key"=@key LIMIT @rand,1;

-- @delete_key
DELETE FROM wtf WHERE "key"=@key;
--@delete_key_value
DELETE FROM wtf WHERE "key"=@key AND "value"=@value;