const starlink = {
  "delux": {
    "starship": [
      "アーウィン",
      "ゼニス",
      "ネプチューン",
      "パルス",
      "ランス",
      "ナディア"
    ],
    "pilot": [
      "フォックス・マクラウド",
      "メイソン",
      "ジャッジ",
      "チェイス",
      "ハンター",
      "シェイド",
      "リーヴァイ",
      "レイザー",
      "イーライ",
      "カール"
    ],
    "weapon": [
      "フレームスロアー",
      "フロストバラージ",
      "シュレッダー",
      "フローター",
      "ボルケーノ",
      "インプローダー",
      "イレイサー",
      "アイアンフィスト",
      "フリーズレイMk. 2",
      "クラッシャー",
      "シュレッダーMk. 2",
      "ショックウェーブ",
      "ガウスガンMk. 2",
      "ヘイルストーム",
      "メテオールMk. 2"
    ]
  },
  "standard": {
    "starship": [
      "アーウィン",
      "ゼニス",
      "ネプチューン",
      "パルス",
      "ランス"
    ],
    "pilot": [
      "フォックス・マクラウド",
      "メイソン",
      "ジャッジ",
      "チェイス",
      "ハンター",
      "リーヴァイ",
      "レイザー"
    ],
    "weapon": [
      "フレームスロアー",
      "フロストバラージ",
      "シュレッダー",
      "フローター",
      "ボルケーノ",
      "インプローダー",
      "アイアンフィスト",
      "フリーズレイ",
      "クラッシャー",
      "シュレッダーMk. 2",
      "ショックウェーブ",
      "ガウスガンMk. 2"
    ]
  },
  "collection1pack": {
    "starship": [
      "ネプチューン",
      "パルス",
      "ランス",
      "ナディア"
    ],
    "pilot": [
      "リーヴァイ",
      "レイザー",
      "イーライ",
      "カール"
    ],
    "weapon": [
      "フローター",
      "ボルケーノ",
      "インプローダー",
      "イレイサー",
      "アイアンフィスト",
      "フリーズレイMk. 2",
      "クラッシャー",
      "シュレッダーMk. 2",
      "ショックウェーブ",
      "ガウスガンMk. 2",
      "ヘイルストーム",
      "メテオールMk. 2"
    ]
  },
  "collection2pack": {
    "starship": [
      "スカルスクリーム",
      "ヴァンテージ",
      "ヴィジランス"
    ],
    "pilot": [
      "ランコア",
      "ファーン・ワイルダー",
      "ガーラ・ジャウスト",
      "スターテイル",
      "ヘイワイヤー"
    ],
    "weapon": [
      "タイダルウェーブ",
      "アイスマイン",
      "ジャーント",
      "ガウスガン",
      "フューリーキャノン",
      "フリーズレイ",
      "メテオール",
      "フローター Mk.2",
      "ボルケーノ Mk.2",
      "インプローダー Mk.2",
      "イレイサー Mk.2"
    ]
  },
  "pilotpack": {
    "starship": [],
    "pilot": [
      "ペッピー",
      "ファルコ",
      "スリッピー"
    ],
    "weapon": []
  }
}

const make_dlx2 = () => {

  const ret = {};
  const concat2 = (arr1, arr2) => { return Array.from(new Set(arr1.concat(arr2))).sort() }

  ret.starship = concat2(starlink.delux.starship, starlink.collection2pack.starship);
  ret.pilot = concat2(starlink.delux.pilot, starlink.collection2pack.pilot);
  ret.weapon = concat2(starlink.delux.weapon, starlink.collection2pack.weapon);

  return ret;
}

const make_std12 = () => {

  const ret = {};
  const concat3 = (arr1, arr2, arr3) => { return Array.from(new Set(arr1.concat(arr2).concat(arr3))).sort() }

  ret.starship = concat3(starlink.standard.starship, starlink.collection1pack.starship, starlink.collection2pack.starship);
  ret.pilot = concat3(starlink.standard.pilot, starlink.collection1pack.pilot, starlink.collection2pack.pilot);
  ret.weapon = concat3(starlink.standard.weapon, starlink.collection1pack.weapon, starlink.collection2pack.weapon);

  return ret;

}

const std12 = make_std12();
const dlx2 = make_dlx2();

console.log(std12.starship);
console.log(dlx2.starship);

console.log(std12.pilot);
console.log(dlx2.pilot);

console.log(std12.weapon);
console.log(dlx2.weapon);
