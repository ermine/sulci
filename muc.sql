CREATE TABLE IF NOT EXISTS muc (room text, nick text, lang text, chatlog char(1));
-- @select_rooms
SELECT room, nick, lang, chatlog FROM muc;