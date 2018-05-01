module Text.Han2Zen
    ( han2zen
    ) where

-- | テキスト内の半角カタカナを全角にに変える
han2zen :: String -> String
han2zen x = map hanKanaToZenKana (chars x)

-- | 半角の濁点の有無で文字ごとに分ける
chars :: String -> [String]
chars [] = []
chars [x] = [[x]]
chars txt
  | isDakuten (txt !! 1) = take 2 txt : chars (drop 2 txt)
  | otherwise                 = take 1 txt : chars (drop 1 txt)

-- | 濁点または半濁点を判別する
isDakuten :: Char -> Bool
isDakuten 'ﾞ' = True
isDakuten 'ﾟ' = True
isDakuten _   = False

-- | 半角を全角文字に
hanKanaToZenKana :: String -> Char
hanKanaToZenKana "｡" = '。'
hanKanaToZenKana "｢" = '「'
hanKanaToZenKana "｣" = '」'
hanKanaToZenKana "､" = '、'
hanKanaToZenKana "･" = '・'
hanKanaToZenKana "ｦ" = 'ヲ'
hanKanaToZenKana "ｧ" = 'ァ'
hanKanaToZenKana "ｨ" = 'ィ'
hanKanaToZenKana "ｩ" = 'ゥ'
hanKanaToZenKana "ｪ" = 'ェ'
hanKanaToZenKana "ｫ" = 'ォ'
hanKanaToZenKana "ｬ" = 'ャ'
hanKanaToZenKana "ｭ" = 'ュ'
hanKanaToZenKana "ｮ" = 'ョ'
hanKanaToZenKana "ｯ" = 'ッ'
hanKanaToZenKana "ｰ" = 'ー'
hanKanaToZenKana "ｱ" = 'ア'
hanKanaToZenKana "ｲ" = 'イ'
hanKanaToZenKana "ｳ" = 'ウ'
hanKanaToZenKana "ｴ" = 'エ'
hanKanaToZenKana "ｵ" = 'オ'
hanKanaToZenKana "ｶ" = 'カ'
hanKanaToZenKana "ｷ" = 'キ'
hanKanaToZenKana "ｸ" = 'ク'
hanKanaToZenKana "ｹ" = 'ケ'
hanKanaToZenKana "ｺ" = 'コ'
hanKanaToZenKana "ｻ" = 'サ'
hanKanaToZenKana "ｼ" = 'シ'
hanKanaToZenKana "ｽ" = 'ス'
hanKanaToZenKana "ｾ" = 'セ'
hanKanaToZenKana "ｿ" = 'ソ'
hanKanaToZenKana "ﾀ" = 'タ'
hanKanaToZenKana "ﾁ" = 'チ'
hanKanaToZenKana "ﾂ" = 'ツ'
hanKanaToZenKana "ﾃ" = 'テ'
hanKanaToZenKana "ﾄ" = 'ト'
hanKanaToZenKana "ﾅ" = 'ナ'
hanKanaToZenKana "ﾆ" = 'ニ'
hanKanaToZenKana "ﾇ" = 'ヌ'
hanKanaToZenKana "ﾈ" = 'ネ'
hanKanaToZenKana "ﾉ" = 'ノ'
hanKanaToZenKana "ﾊ" = 'ハ'
hanKanaToZenKana "ﾋ" = 'ヒ'
hanKanaToZenKana "ﾌ" = 'フ'
hanKanaToZenKana "ﾍ" = 'ヘ'
hanKanaToZenKana "ﾎ" = 'ホ'
hanKanaToZenKana "ﾏ" = 'マ'
hanKanaToZenKana "ﾐ" = 'ミ'
hanKanaToZenKana "ﾑ" = 'ム'
hanKanaToZenKana "ﾒ" = 'メ'
hanKanaToZenKana "ﾓ" = 'モ'
hanKanaToZenKana "ﾔ" = 'ヤ'
hanKanaToZenKana "ﾕ" = 'ユ'
hanKanaToZenKana "ﾖ" = 'ヨ'
hanKanaToZenKana "ﾗ" = 'ラ'
hanKanaToZenKana "ﾘ" = 'リ'
hanKanaToZenKana "ﾙ" = 'ル'
hanKanaToZenKana "ﾚ" = 'レ'
hanKanaToZenKana "ﾛ" = 'ロ'
hanKanaToZenKana "ﾜ" = 'ワ'
hanKanaToZenKana "ﾝ" = 'ン'
hanKanaToZenKana "ﾞ" = '゛' -- これいる？
hanKanaToZenKana "ﾟ" = '゜' -- これいる？
hanKanaToZenKana "ｶﾞ" = 'ガ'
hanKanaToZenKana "ｷﾞ" = 'ギ'
hanKanaToZenKana "ｸﾞ" = 'グ'
hanKanaToZenKana "ｹﾞ" = 'ゲ'
hanKanaToZenKana "ｺﾞ" = 'ゴ'
hanKanaToZenKana "ｻﾞ" = 'ザ'
hanKanaToZenKana "ｼﾞ" = 'ジ'
hanKanaToZenKana "ｽﾞ" = 'ズ'
hanKanaToZenKana "ｾﾞ" = 'ゼ'
hanKanaToZenKana "ｿﾞ" = 'ゾ'
hanKanaToZenKana "ﾀﾞ" = 'ダ'
hanKanaToZenKana "ﾁﾞ" = 'ヂ'
hanKanaToZenKana "ﾂﾞ" = 'ヅ'
hanKanaToZenKana "ﾃﾞ" = 'デ'
hanKanaToZenKana "ﾄﾞ" = 'ド'
hanKanaToZenKana "ﾊﾞ" = 'バ'
hanKanaToZenKana "ﾋﾞ" = 'ビ'
hanKanaToZenKana "ﾌﾞ" = 'ブ'
hanKanaToZenKana "ﾍﾞ" = 'ベ'
hanKanaToZenKana "ﾎﾞ" = 'ボ'
hanKanaToZenKana "ﾊﾟ" = 'パ'
hanKanaToZenKana "ﾋﾟ" = 'ピ'
hanKanaToZenKana "ﾌﾟ" = 'プ'
hanKanaToZenKana "ﾍﾟ" = 'ペ'
hanKanaToZenKana "ﾎﾟ" = 'ポ'
hanKanaToZenKana "ｳﾞ" = 'ヴ'
hanKanaToZenKana [c] = c
hanKanaToZenKana xs = error ("syntax error:" ++ xs)

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
