CREATE TABLE IF NOT EXISTS greeting (jid varchar, room varchar, msg varchar);
CREATE INDEX IF NOT EXISTS gr_index ON greeting (jid, room);
-- @check_greet
SELECT 1 FROM greeting WHERE jid=@jid AND room=@room LIMIT 1;
-- @add_greet
INSERT INTO greeting (jid, room, msg) VALUES;
-- @update_greet
UPDATE greeting SET msg=@msg WHERE jid=@jid AND room=@room;
--@get_greet
SELECT msg FROM greeting WHERE jid=@jid AND room=@room LIMIT 1;

CREATE TABLE IF NOT EXISTS users (jid varchar, room varchar, nick varchar, last integer, action varchar, reason varchar);
CREATE INDEX IF NOT EXISTS users_index ON users (jid, room);
CREATE INDEX IF NOT EXISTS users_nicks ON users (nick, room);

-- @check_user_by_nick
SELECT 1 FROM users WHERE nick=@nick AND room=@room LIMIT 1;
-- @update_user_by_nick
UPDATE users SET last=@last, action=@action, reason=@reason WHERE nick=@nick AND room=@room;
-- @add_user_by_nick
INSERT INTO users (jid, room, nick, last, action, reason) VALUES;

-- @check_user_by_jid
SELECT 1 FROM users WHERE jid=@jid AND room=@room LIMIT 1;
-- @update_user_by_jid
UPDATE users SET last=@last, action=@action, reason=@reason WHERE jid=@jid AND room=@room;
-- @add_user_by_jid
INSERT INTO users (jid, room, nick, last, action, reason) VALUES;

-- @seen_by_nick
SELECT jid, last, action, reason FROM users WHERE nick=@nick AND room=@room ORDER BY last DESC LIMIT 1;
