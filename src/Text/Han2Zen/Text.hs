{-# LANGUAGE OverloadedStrings#-}
module Text.Han2Zen.Text
    ( han2zen
    ) where

import qualified Data.Text as T

-- | テキスト内の半角カタカナを全角にに変える
han2zen :: T.Text -> T.Text
han2zen x = T.concat $ map (T.singleton.hanKanaToZenKana) (chars x)

-- | 半角の濁点の有無で文字ごとに分ける
chars :: T.Text -> [T.Text]
chars txt
  | txt == T.empty            = []
  | T.length txt == 1         = [txt]
  | isDakuten (T.index txt 1) = T.take 2 txt : chars (T.drop 2 txt)
  | otherwise                 = T.take 1 txt : chars (T.drop 1 txt)

-- | 濁点または半濁点を判別する
isDakuten :: Char -> Bool
isDakuten 'ﾞ' = True
isDakuten 'ﾟ' = True
isDakuten _   = False

-- | 半角を全角文字に
hanKanaToZenKana :: T.Text -> Char
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
hanKanaToZenKana cs | T.length cs == 1 = (\(Just (c,_)) -> c) $ T.uncons cs
hanKanaToZenKana xs = error . T.unpack $ T.append "syntax error:" xs

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
