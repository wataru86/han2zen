module Text.Han2Zen
    ( han2zen
    , han2zenSafe
    , han2zenSafe'
    ) where

import Data.Maybe

-- | テキスト内の半角カタカナを全角にに変える
han2zen :: String -> String
han2zen x = map hanKanaToZenKana' (chars x)

hanKanaToZenKana' :: String -> Char
hanKanaToZenKana' x = either (error $ "Syntax Error: " ++ x) id $ hanKanaToZenKana x

han2zenSafe :: String -> Either String String
han2zenSafe x = mapM hanKanaToZenKana (chars x)

han2zenSafe' :: String -> [Either String Char]
han2zenSafe' x = map hanKanaToZenKana (chars x)

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
hanKanaToZenKana :: String -> Either String Char
hanKanaToZenKana "｡" = Right '。'
hanKanaToZenKana "｢" = Right '「'
hanKanaToZenKana "｣" = Right '」'
hanKanaToZenKana "､" = Right '、'
hanKanaToZenKana "･" = Right '・'
hanKanaToZenKana "ｦ" = Right 'ヲ'
hanKanaToZenKana "ｧ" = Right 'ァ'
hanKanaToZenKana "ｨ" = Right 'ィ'
hanKanaToZenKana "ｩ" = Right 'ゥ'
hanKanaToZenKana "ｪ" = Right 'ェ'
hanKanaToZenKana "ｫ" = Right 'ォ'
hanKanaToZenKana "ｬ" = Right 'ャ'
hanKanaToZenKana "ｭ" = Right 'ュ'
hanKanaToZenKana "ｮ" = Right 'ョ'
hanKanaToZenKana "ｯ" = Right 'ッ'
hanKanaToZenKana "ｰ" = Right 'ー'
hanKanaToZenKana "ｱ" = Right 'ア'
hanKanaToZenKana "ｲ" = Right 'イ'
hanKanaToZenKana "ｳ" = Right 'ウ'
hanKanaToZenKana "ｴ" = Right 'エ'
hanKanaToZenKana "ｵ" = Right 'オ'
hanKanaToZenKana "ｶ" = Right 'カ'
hanKanaToZenKana "ｷ" = Right 'キ'
hanKanaToZenKana "ｸ" = Right 'ク'
hanKanaToZenKana "ｹ" = Right 'ケ'
hanKanaToZenKana "ｺ" = Right 'コ'
hanKanaToZenKana "ｻ" = Right 'サ'
hanKanaToZenKana "ｼ" = Right 'シ'
hanKanaToZenKana "ｽ" = Right 'ス'
hanKanaToZenKana "ｾ" = Right 'セ'
hanKanaToZenKana "ｿ" = Right 'ソ'
hanKanaToZenKana "ﾀ" = Right 'タ'
hanKanaToZenKana "ﾁ" = Right 'チ'
hanKanaToZenKana "ﾂ" = Right 'ツ'
hanKanaToZenKana "ﾃ" = Right 'テ'
hanKanaToZenKana "ﾄ" = Right 'ト'
hanKanaToZenKana "ﾅ" = Right 'ナ'
hanKanaToZenKana "ﾆ" = Right 'ニ'
hanKanaToZenKana "ﾇ" = Right 'ヌ'
hanKanaToZenKana "ﾈ" = Right 'ネ'
hanKanaToZenKana "ﾉ" = Right 'ノ'
hanKanaToZenKana "ﾊ" = Right 'ハ'
hanKanaToZenKana "ﾋ" = Right 'ヒ'
hanKanaToZenKana "ﾌ" = Right 'フ'
hanKanaToZenKana "ﾍ" = Right 'ヘ'
hanKanaToZenKana "ﾎ" = Right 'ホ'
hanKanaToZenKana "ﾏ" = Right 'マ'
hanKanaToZenKana "ﾐ" = Right 'ミ'
hanKanaToZenKana "ﾑ" = Right 'ム'
hanKanaToZenKana "ﾒ" = Right 'メ'
hanKanaToZenKana "ﾓ" = Right 'モ'
hanKanaToZenKana "ﾔ" = Right 'ヤ'
hanKanaToZenKana "ﾕ" = Right 'ユ'
hanKanaToZenKana "ﾖ" = Right 'ヨ'
hanKanaToZenKana "ﾗ" = Right 'ラ'
hanKanaToZenKana "ﾘ" = Right 'リ'
hanKanaToZenKana "ﾙ" = Right 'ル'
hanKanaToZenKana "ﾚ" = Right 'レ'
hanKanaToZenKana "ﾛ" = Right 'ロ'
hanKanaToZenKana "ﾜ" = Right 'ワ'
hanKanaToZenKana "ﾝ" = Right 'ン'
hanKanaToZenKana "ﾞ" = Right '゛' -- これいる？
hanKanaToZenKana "ﾟ" = Right '゜' -- これいる？
hanKanaToZenKana "ｶﾞ" = Right 'ガ'
hanKanaToZenKana "ｷﾞ" = Right 'ギ'
hanKanaToZenKana "ｸﾞ" = Right 'グ'
hanKanaToZenKana "ｹﾞ" = Right 'ゲ'
hanKanaToZenKana "ｺﾞ" = Right 'ゴ'
hanKanaToZenKana "ｻﾞ" = Right 'ザ'
hanKanaToZenKana "ｼﾞ" = Right 'ジ'
hanKanaToZenKana "ｽﾞ" = Right 'ズ'
hanKanaToZenKana "ｾﾞ" = Right 'ゼ'
hanKanaToZenKana "ｿﾞ" = Right 'ゾ'
hanKanaToZenKana "ﾀﾞ" = Right 'ダ'
hanKanaToZenKana "ﾁﾞ" = Right 'ヂ'
hanKanaToZenKana "ﾂﾞ" = Right 'ヅ'
hanKanaToZenKana "ﾃﾞ" = Right 'デ'
hanKanaToZenKana "ﾄﾞ" = Right 'ド'
hanKanaToZenKana "ﾊﾞ" = Right 'バ'
hanKanaToZenKana "ﾋﾞ" = Right 'ビ'
hanKanaToZenKana "ﾌﾞ" = Right 'ブ'
hanKanaToZenKana "ﾍﾞ" = Right 'ベ'
hanKanaToZenKana "ﾎﾞ" = Right 'ボ'
hanKanaToZenKana "ﾊﾟ" = Right 'パ'
hanKanaToZenKana "ﾋﾟ" = Right 'ピ'
hanKanaToZenKana "ﾌﾟ" = Right 'プ'
hanKanaToZenKana "ﾍﾟ" = Right 'ペ'
hanKanaToZenKana "ﾎﾟ" = Right 'ポ'
hanKanaToZenKana "ｳﾞ" = Right 'ヴ'
hanKanaToZenKana [c] = Right c
hanKanaToZenKana xs = Left $ "Syntax Error: " ++ xs

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
