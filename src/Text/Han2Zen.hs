module Text.Han2Zen
    ( han2zen
    , han2zenSafe
    ) where

-- | テキスト内の半角カタカナを全角にに変える

import Data.Maybe

han2zen :: String -> String
han2zen x = map hanKanaToZenKana' (chars x)

hanKanaToZenKana' :: String -> Char
hanKanaToZenKana' x = fromMaybe (error $ "Syntax Error: " ++ x) $ hanKanaToZenKana x

han2zenSafe :: String -> Maybe String
han2zenSafe x = mapM hanKanaToZenKana (chars x)

-- | 半角の濁点の有無で文字ごとに分ける
chars :: String -> [String]
chars [] = []
chars [x] = [[x]]
chars txt
  | isDakuten (txt !! 1) = take 2 txt : chars (drop 2 txt)
  | otherwise            = take 1 txt : chars (drop 1 txt)

-- | 濁点または半濁点を判別する
isDakuten :: Char -> Bool
isDakuten 'ﾞ' = True
isDakuten 'ﾟ' = True
isDakuten _   = False

-- | 半角を全角文字に
hanKanaToZenKana :: String -> Maybe Char
hanKanaToZenKana "｡" = Just '。'
hanKanaToZenKana "｢" = Just '「'
hanKanaToZenKana "｣" = Just '」'
hanKanaToZenKana "､" = Just '、'
hanKanaToZenKana "･" = Just '・'
hanKanaToZenKana "ｦ" = Just 'ヲ'
hanKanaToZenKana "ｧ" = Just 'ァ'
hanKanaToZenKana "ｨ" = Just 'ィ'
hanKanaToZenKana "ｩ" = Just 'ゥ'
hanKanaToZenKana "ｪ" = Just 'ェ'
hanKanaToZenKana "ｫ" = Just 'ォ'
hanKanaToZenKana "ｬ" = Just 'ャ'
hanKanaToZenKana "ｭ" = Just 'ュ'
hanKanaToZenKana "ｮ" = Just 'ョ'
hanKanaToZenKana "ｯ" = Just 'ッ'
hanKanaToZenKana "ｰ" = Just 'ー'
hanKanaToZenKana "ｱ" = Just 'ア'
hanKanaToZenKana "ｲ" = Just 'イ'
hanKanaToZenKana "ｳ" = Just 'ウ'
hanKanaToZenKana "ｴ" = Just 'エ'
hanKanaToZenKana "ｵ" = Just 'オ'
hanKanaToZenKana "ｶ" = Just 'カ'
hanKanaToZenKana "ｷ" = Just 'キ'
hanKanaToZenKana "ｸ" = Just 'ク'
hanKanaToZenKana "ｹ" = Just 'ケ'
hanKanaToZenKana "ｺ" = Just 'コ'
hanKanaToZenKana "ｻ" = Just 'サ'
hanKanaToZenKana "ｼ" = Just 'シ'
hanKanaToZenKana "ｽ" = Just 'ス'
hanKanaToZenKana "ｾ" = Just 'セ'
hanKanaToZenKana "ｿ" = Just 'ソ'
hanKanaToZenKana "ﾀ" = Just 'タ'
hanKanaToZenKana "ﾁ" = Just 'チ'
hanKanaToZenKana "ﾂ" = Just 'ツ'
hanKanaToZenKana "ﾃ" = Just 'テ'
hanKanaToZenKana "ﾄ" = Just 'ト'
hanKanaToZenKana "ﾅ" = Just 'ナ'
hanKanaToZenKana "ﾆ" = Just 'ニ'
hanKanaToZenKana "ﾇ" = Just 'ヌ'
hanKanaToZenKana "ﾈ" = Just 'ネ'
hanKanaToZenKana "ﾉ" = Just 'ノ'
hanKanaToZenKana "ﾊ" = Just 'ハ'
hanKanaToZenKana "ﾋ" = Just 'ヒ'
hanKanaToZenKana "ﾌ" = Just 'フ'
hanKanaToZenKana "ﾍ" = Just 'ヘ'
hanKanaToZenKana "ﾎ" = Just 'ホ'
hanKanaToZenKana "ﾏ" = Just 'マ'
hanKanaToZenKana "ﾐ" = Just 'ミ'
hanKanaToZenKana "ﾑ" = Just 'ム'
hanKanaToZenKana "ﾒ" = Just 'メ'
hanKanaToZenKana "ﾓ" = Just 'モ'
hanKanaToZenKana "ﾔ" = Just 'ヤ'
hanKanaToZenKana "ﾕ" = Just 'ユ'
hanKanaToZenKana "ﾖ" = Just 'ヨ'
hanKanaToZenKana "ﾗ" = Just 'ラ'
hanKanaToZenKana "ﾘ" = Just 'リ'
hanKanaToZenKana "ﾙ" = Just 'ル'
hanKanaToZenKana "ﾚ" = Just 'レ'
hanKanaToZenKana "ﾛ" = Just 'ロ'
hanKanaToZenKana "ﾜ" = Just 'ワ'
hanKanaToZenKana "ﾝ" = Just 'ン'
hanKanaToZenKana "ﾞ" = Just '゛' -- これいる？
hanKanaToZenKana "ﾟ" = Just '゜' -- これいる？
hanKanaToZenKana "ｶﾞ" = Just 'ガ'
hanKanaToZenKana "ｷﾞ" = Just 'ギ'
hanKanaToZenKana "ｸﾞ" = Just 'グ'
hanKanaToZenKana "ｹﾞ" = Just 'ゲ'
hanKanaToZenKana "ｺﾞ" = Just 'ゴ'
hanKanaToZenKana "ｻﾞ" = Just 'ザ'
hanKanaToZenKana "ｼﾞ" = Just 'ジ'
hanKanaToZenKana "ｽﾞ" = Just 'ズ'
hanKanaToZenKana "ｾﾞ" = Just 'ゼ'
hanKanaToZenKana "ｿﾞ" = Just 'ゾ'
hanKanaToZenKana "ﾀﾞ" = Just 'ダ'
hanKanaToZenKana "ﾁﾞ" = Just 'ヂ'
hanKanaToZenKana "ﾂﾞ" = Just 'ヅ'
hanKanaToZenKana "ﾃﾞ" = Just 'デ'
hanKanaToZenKana "ﾄﾞ" = Just 'ド'
hanKanaToZenKana "ﾊﾞ" = Just 'バ'
hanKanaToZenKana "ﾋﾞ" = Just 'ビ'
hanKanaToZenKana "ﾌﾞ" = Just 'ブ'
hanKanaToZenKana "ﾍﾞ" = Just 'ベ'
hanKanaToZenKana "ﾎﾞ" = Just 'ボ'
hanKanaToZenKana "ﾊﾟ" = Just 'パ'
hanKanaToZenKana "ﾋﾟ" = Just 'ピ'
hanKanaToZenKana "ﾌﾟ" = Just 'プ'
hanKanaToZenKana "ﾍﾟ" = Just 'ペ'
hanKanaToZenKana "ﾎﾟ" = Just 'ポ'
hanKanaToZenKana "ｳﾞ" = Just 'ヴ'
hanKanaToZenKana [c] = Just c
hanKanaToZenKana _ = Nothing -- error ("syntax error:" ++ xs)

hanKana :: String
hanKana = "｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝﾞﾟ"

zenKana :: String
zenKana = "。「」、・ヲァィゥェォャュョッーアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜"

daku :: String
daku = "ｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾊﾋﾌﾍﾎ"

dakuzen :: String
dakuzen = "ガギグゲゴザジズゼゾダヂヅデドバビブベボ"

handaku :: String
handaku = "ﾊﾋﾌﾍﾎ"

handakuzen :: String
handakuzen = "パピプペポ"
