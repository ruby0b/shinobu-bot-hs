-- name: voiceIDToAssociatedTextID :: (Snowflake TextChannel)
-- :voiceID :: Snowflake VoiceChannel
SELECT voice_id, text_id
FROM voice_to_text
WHERE voice_id = :voiceID
;;;

-- name: makeNewUser :: ()
-- :userID :: ID GachaUser
INSERT OR IGNORE INTO user (id) VALUES (:userID)
;;;

-- name: getBirthdayPeople :: [GachaUser]
SELECT *
FROM user
WHERE birthday == DATE('now', 'localtime')
;;;

-- name: rewardAndUpdateBirthday :: ()
-- :userID :: ID GachaUser
-- :giftAmount :: Money
UPDATE user
SET balance = balance + :giftAmount
,   birthday = DATE(birthday, '+1 years')
WHERE id = :userID
;;;

-- name: getAvailablePacks :: [Pack]
SELECT *
FROM pack
WHERE (pack.start_date <= DATE('NOW', 'LOCALTIME'))
AND (pack.end_date IS NULL OR pack.end_date >= DATE('NOW', 'LOCALTIME'))
;;;

-- name: getAvailablePackByName :: Pack
-- :packName :: Text
SELECT *
FROM pack
WHERE (pack.start_date <= DATE('NOW', 'LOCALTIME'))
AND (pack.end_date IS NULL OR pack.end_date >= DATE('NOW', 'LOCALTIME'))
AND name LIKE :packName
;;;

-- name: getUserByID :: User
-- :userID :: ID User
SELECT * FROM user WHERE id = :userID
;;;

-- name: listRarities :: [Rarity]
SELECT * FROM rarity
;;;

-- name: listWeightedCharacterIDs :: [(ID Character, Double)]
-- :packID :: ID Pack
-- :rarityID :: ID Rarity
SELECT character.id, MAX(batch_in_pack.weight)
FROM character
JOIN batch_in_pack      ON batch_in_pack.batch = character.batch
JOIN rarity             ON rarity.value >= character.rarity
WHERE batch_in_pack.pack = :packID AND rarity.value = :rarityID
GROUP BY character.id
;;;

-- name: getCharacterByID :: Character
-- :charID :: ID Character
SELECT * FROM character WHERE id = :charID
;;;

-- name: getOwnedWaifu :: OwnedWaifu
-- :userID :: ID GachaUser
-- :charID :: ID Character
SELECT * FROM waifu
WHERE user = :userID AND character = :charID
;;;

-- TODO migrate from shinobu-bot-py:
-- * add_characters.py 
-- * utils/waifus.py (after line 80)
-- * utils/trade.py
