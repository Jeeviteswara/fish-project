{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord(..), (.:), Header, decodeByName)
import qualified Data.Vector as V
import Data.List (sort, nub, sortOn)
import Data.Ord (Down(..))
import Data.Char (isSpace)
import Text.Printf (printf)
import Data.IORef
import Data.Unique (newUnique, hashUnique)
import System.Environment (lookupEnv)

data FishRecord = FishRecord
  { species  :: String
  , area     :: String
  , year     :: Int
  , catchAmt :: Double
  } deriving (Show)

instance FromNamedRecord FishRecord where
    parseNamedRecord r =
        FishRecord <$> r .: "Species"
                   <*> r .: "Area"
                   <*> r .: "Year"
                   <*> r .: "Catch"

trimSpaces :: String -> String
trimSpaces = f . f
  where
    f = reverse . dropWhile isSpace

isValidName :: String -> Bool
isValidName s = length (trimSpaces s) >= 2

isValidEmail :: String -> Bool
isValidEmail s =
    let t = trimSpaces s
        (left, rest) = break (=='@') t
    in not (null left)
       && not (null rest)
       && length (filter (=='@') t) == 1
       && let domain = drop 1 rest
          in not (null domain)
             && '.' `elem` domain
             && head domain /= '.'
             && last domain /= '.'

isValidPassword :: String -> Bool
isValidPassword s = length (trimSpaces s) >= 6

formatDouble :: Double -> String
formatDouble x = printf "%.2f" x

meanList :: [Double] -> Double
meanList [] = 0
meanList xs = sum xs / fromIntegral (length xs)

safePercentChange :: Double -> Double -> Double
safePercentChange old newVal =
    if old == 0 then 0 else ((newVal - old) / old) * 100

filterRecords :: String -> String -> [FishRecord] -> [FishRecord]
filterRecords _ _ [] = []
filterRecords sp ar (x:xs)
    | species x == sp && area x == ar = x : filterRecords sp ar xs
    | otherwise = filterRecords sp ar xs

getCatchValues :: [FishRecord] -> [Double]
getCatchValues [] = []
getCatchValues (x:xs) = catchAmt x : getCatchValues xs

getYears :: [FishRecord] -> [Int]
getYears [] = []
getYears (x:xs) = year x : getYears xs

yearlyDiff :: [Double] -> [Double]
yearlyDiff [] = []
yearlyDiff [_] = []
yearlyDiff (x:y:xs) = (y - x) : yearlyDiff (y:xs)

countDeclines :: [Double] -> Int
countDeclines [] = 0
countDeclines (x:xs)
    | x < 0     = 1 + countDeclines xs
    | otherwise = countDeclines xs

sustainabilityStatus :: Int -> Int -> String
sustainabilityStatus declineCount totalChanges
    | totalChanges == 0 = "Not enough data"
    | declineCount > totalChanges `div` 2 = "Declining trend - possible sustainability risk"
    | declineCount == totalChanges `div` 2 = "Mixed trend - needs monitoring"
    | otherwise = "Stable or improving trend"

buildOptions :: [String] -> TL.Text
buildOptions [] = ""
buildOptions (x:xs) =
    "<option value=\"" <> TL.pack x <> "\">" <> TL.pack x <> "</option>" <> buildOptions xs

changeCell :: Maybe Double -> TL.Text
changeCell Nothing = "<td>-</td>"
changeCell (Just d)
    | d < 0     = "<td class='neg'>" <> TL.pack (formatDouble d) <> "</td>"
    | d > 0     = "<td class='pos'>+" <> TL.pack (formatDouble d) <> "</td>"
    | otherwise = "<td>0.00</td>"

buildRows :: [Int] -> [Double] -> [Maybe Double] -> TL.Text
buildRows [] [] [] = ""
buildRows (y:ys) (c:cs) (d:ds) =
    "<tr><td>" <> TL.pack (show y) <> "</td><td>" <> TL.pack (formatDouble c) <> "</td>"
    <> changeCell d <> "</tr>" <> buildRows ys cs ds
buildRows _ _ _ = ""

buildDatasetRows :: [FishRecord] -> TL.Text
buildDatasetRows [] = ""
buildDatasetRows (r:rs) =
    "<tr><td>" <> TL.pack (species r) <> "</td><td>" <> TL.pack (area r) <> "</td><td>"
    <> TL.pack (show (year r)) <> "</td><td>" <> TL.pack (formatDouble (catchAmt r)) <> "</td></tr>"
    <> buildDatasetRows rs

speciesAreaMapJS :: [FishRecord] -> TL.Text
speciesAreaMapJS records =
    let speciesList = sort (nub (map species records))
        go [] = ""
        go (s:ss) =
            let areasForS = sort (nub [ area r | r <- records, species r == s ])
                one =
                    "\"" <> TL.pack s <> "\":["
                    <> TL.intercalate "," (map (\a -> "\"" <> TL.pack a <> "\"") areasForS)
                    <> "]"
            in one <> (if null ss then "" else ",") <> go ss
    in "{" <> go speciesList <> "}"

pageShell :: Bool -> TL.Text -> TL.Text -> TL.Text
pageShell loggedIn displayName body =
    let navLinks =
            if loggedIn
            then mconcat
                [ "<a href='/dashboard'>Dashboard</a>"
                , "<a href='/dataset'>Dataset Overview</a>"
                , "<a href='/methodology'>Methodology</a>"
                , "<a href='/compare-species'>Species Compare</a>"
                , "<a href='/compare-areas'>Area Compare</a>"
                , "<a href='/risk-ranking'>Risk Ranking</a>"
                , "<a href='/calculator'>Catch Calculator</a>"
                , "<a href='/logout'>Sign Out</a>"
                ]
            else mconcat
                [ "<a href='/'>Home</a>"
                , "<a href='/login'>Login</a>"
                ]
        signedBlock =
            if loggedIn
            then "<div class='signed'>Signed in as: <span>" <> displayName <> "</span></div>"
            else ""
    in mconcat
        [ "<!DOCTYPE html>"
        , "<html><head><meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1.0'>"
        , "<title>MarineStock AI</title><style>"
        , "body{margin:0;background:linear-gradient(120deg,#15171d,#1b1d23,#16181e);color:white;font-family:'Segoe UI',sans-serif;}"
        , ".container{width:92%;max-width:1320px;margin:30px auto;}"
        , ".top-actions{display:flex;gap:14px;flex-wrap:wrap;margin-bottom:24px;}"
        , ".top-actions a{padding:10px 16px;border-radius:999px;background:rgba(255,255,255,0.05);color:#d3d7dd;text-decoration:none;font-size:14px;transition:0.2s ease;}"
        , ".top-actions a:hover{background:rgba(255,255,255,0.09);transform:translateY(-2px);}"
        , ".card{background:linear-gradient(180deg,#20232a 0%, #16181d 100%);border:1px solid rgba(255,255,255,0.05);border-radius:18px;padding:24px;box-shadow:0 18px 38px rgba(0,0,0,0.28);}"
        , ".page-title{font-size:52px;line-height:0.95;letter-spacing:-2px;margin:0 0 14px 0;}"
        , ".page-sub{color:#a9afb7;font-size:18px;line-height:1.7;margin:0 0 26px 0;max-width:900px;}"
        , "table{width:100%;border-collapse:collapse;background:#111318;border-radius:12px;overflow:hidden;}"
        , "th,td{padding:14px 16px;text-align:left;border-bottom:1px solid rgba(255,255,255,0.07);}"
        , "th{background:#171920;color:#aab0b8;font-size:14px;letter-spacing:1px;text-transform:uppercase;}"
        , "td{color:#eceef2;}"
        , "tr:hover{background:rgba(255,255,255,0.02);}"
        , "input,select{width:100%;padding:16px 16px;font-size:17px;border-radius:12px;border:1px solid rgba(255,255,255,0.08);background:#111318;color:white;outline:none;box-sizing:border-box;margin-top:8px;margin-bottom:16px;}"
        , "button,.btn{display:inline-block;padding:14px 22px;background:#f3f4f6;color:#15171d;border:none;border-radius:12px;font-size:16px;font-weight:700;text-decoration:none;cursor:pointer;transition:0.2s ease;}"
        , "button:hover,.btn:hover{transform:translateY(-2px);}"
        , ".grid-2{display:grid;grid-template-columns:1fr 1fr;gap:18px;}"
        , ".grid-3{display:grid;grid-template-columns:1fr 1fr 1fr;gap:18px;}"
        , ".mini-card{background:linear-gradient(180deg,#1d2027 0%, #171920 100%);border:1px solid rgba(255,255,255,0.06);border-radius:16px;padding:20px;}"
        , ".mini-label{color:#aab0b8;font-size:13px;letter-spacing:1px;text-transform:uppercase;}"
        , ".mini-value{font-size:30px;margin-top:10px;color:#f3f4f6;font-weight:700;}"
        , ".section-title{font-size:28px;margin:0 0 16px 0;}"
        , ".muted{color:#8f96a0;font-size:14px;line-height:1.7;}"
        , ".neg{color:#ff8b8b;font-weight:700;}"
        , ".pos{color:#7ee7a2;font-weight:700;}"
        , ".danger{color:#ff8b8b;font-size:26px;font-weight:700;line-height:1.2;}"
        , ".warning{color:#f0d27a;font-size:26px;font-weight:700;line-height:1.2;}"
        , ".good{color:#7ee7a2;font-size:26px;font-weight:700;line-height:1.2;}"
        , ".compare-box{display:grid;grid-template-columns:1fr 1fr;gap:18px;}"
        , ".signed{margin-top:18px;color:#d6d9de;font-size:15px;}"
        , ".signed span{color:#f4c38c;font-weight:700;}"
        , "canvas{background:#111318;border-radius:12px;padding:10px;}"
        , "@media (max-width:900px){.grid-2,.grid-3,.compare-box{grid-template-columns:1fr;}.page-title{font-size:40px;}}"
        , "</style></head><body><div class='container'>"
        , "<div class='top-actions'>", navLinks, "</div>"
        , body
        , signedBlock
        , "</div></body></html>"
        ]

landingPage :: TL.Text
landingPage =
    mconcat
        [ "<!DOCTYPE html><html><head><meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1.0'>"
        , "<title>MarineStock AI</title><style>"
        , "body{margin:0;font-family:'Segoe UI',sans-serif;background:linear-gradient(120deg,#15171d,#1b1d23,#16181e);color:white;min-height:100vh;}"
        , ".page-wrap{width:94%;max-width:1460px;margin:26px auto;background:linear-gradient(180deg,#1c1f26 0%, #171920 100%);border:1px solid rgba(255,255,255,0.06);box-shadow:0 10px 30px rgba(0,0,0,0.45);overflow:hidden;}"
        , ".hero{display:grid;grid-template-columns:1.1fr 1.9fr;gap:34px;padding:42px 46px 46px 46px;}"
        , ".left{display:flex;flex-direction:column;justify-content:center;min-height:640px;}"
        , ".left h1{font-size:78px;line-height:0.95;letter-spacing:-2px;margin:0 0 28px 0;color:#f3f4f6;font-weight:700;}"
        , ".left p{color:#a9afb7;font-size:20px;line-height:1.7;max-width:500px;margin:0 0 34px 0;}"
        , ".project-tag{font-size:13px;letter-spacing:2px;color:#c2c5cb;text-transform:uppercase;margin-bottom:18px;}"
        , ".cta-row{display:flex;gap:18px;align-items:center;flex-wrap:wrap;}"
        , ".btn-main,.btn-ghost{display:inline-block;padding:14px 26px;border-radius:12px;text-decoration:none;font-weight:700;transition:0.2s ease;}"
        , ".btn-main{background:#f3f4f6;color:#15171d;}"
        , ".btn-main:hover,.btn-ghost:hover{transform:translateY(-2px);}"
        , ".btn-ghost{border:1px solid rgba(214,160,98,0.7);color:#d6a062;}"
        , ".right{display:grid;grid-template-columns:1fr 1fr 1fr;gap:28px;align-items:stretch;}"
        , ".panel{position:relative;min-height:640px;border-radius:14px;overflow:hidden;background:linear-gradient(180deg,#24272f 0%, #191b21 100%);border:1px solid rgba(255,255,255,0.04);text-decoration:none;transition:all 0.3s ease;}"
        , ".panel:hover{transform:translateY(-10px) scale(1.02);box-shadow:0 24px 40px rgba(0,0,0,0.45);border-color:rgba(214,160,98,0.22);}"
        , ".panel:hover .visual-label{color:#f4c38c;}"
        , ".panel-note{position:absolute;right:18px;top:18px;padding:8px 12px;border-radius:999px;background:rgba(255,255,255,0.06);color:#cfd3d8;font-size:12px;letter-spacing:1.5px;text-transform:uppercase;}"
        , ".visual-label{position:absolute;left:18px;bottom:18px;color:#e5e7eb;font-size:20px;transition:0.3s ease;}"
        , ".visual-1::after{content:'';position:absolute;left:18%;top:14%;width:48%;height:58%;background:linear-gradient(180deg,#30343b,#181b20);transform:skewY(-8deg);transition:0.3s ease;}"
        , ".panel:hover.visual-1::after{transform:skewY(-8deg) scale(1.04);}"
        , ".visual-2{background:radial-gradient(circle at 50% 25%, #2b2e34 0%, #191b20 42%, #111318 100%);}"
        , ".visual-2::before{content:'';position:absolute;left:18%;right:18%;bottom:18%;height:26%;border-radius:50%;background:linear-gradient(180deg,#0f1114,#1b1d22);transition:0.3s ease;}"
        , ".visual-2::after{content:'';position:absolute;left:44%;top:30%;width:16px;height:110px;border-radius:10px;background:linear-gradient(180deg,#0f1012,#24262c);transition:0.3s ease;}"
        , ".panel:hover.visual-2::before{transform:scale(1.04);}"
        , ".panel:hover.visual-2::after{transform:translateY(-8px);}"
        , ".visual-3 .lamp{position:absolute;width:2px;background:#343840;top:0;transition:0.3s ease;}"
        , ".visual-3 .lamp::after{content:'';position:absolute;left:-10px;bottom:-54px;width:22px;height:54px;border-radius:3px;background:linear-gradient(180deg,#3d4148,#1b1d22);}"
        , ".visual-3 .lamp.l1{left:24%;height:62%;}.visual-3 .lamp.l2{left:54%;height:48%;}.visual-3 .lamp.l3{left:86%;height:76%;}"
        , ".panel:hover .lamp.l1{transform:translateY(8px);}.panel:hover .lamp.l2{transform:translateY(-4px);}.panel:hover .lamp.l3{transform:translateY(12px);}"
        , "@media (max-width:1180px){.hero{grid-template-columns:1fr;}.right{grid-template-columns:1fr;}.panel{min-height:320px;}.left h1{font-size:58px;}}"
        , "</style></head><body><div class='page-wrap'><div class='hero'><div class='left'>"
        , "<div class='project-tag'>Marine Stock Analysis</div>"
        , "<h1>Minimalism<br>at its finest.</h1>"
        , "<p>Analyze marine fish sustainability using a clean Haskell-powered dashboard with recursive trend analysis and professional visualization.</p>"
        , "<div class='cta-row'><a class='btn-main' href='/login'>Enter Dashboard</a><a class='btn-ghost' href='/login'>Login to Continue</a></div>"
        , "</div><div class='right'>"
        , "<a class='panel visual-1' href='/login'><div class='panel-note'>Interactive</div><div class='visual-label'>Dataset</div></a>"
        , "<a class='panel visual-2' href='/login'><div class='panel-note'>Interactive</div><div class='visual-label'>Recursive Model</div></a>"
        , "<a class='panel visual-3' href='/login'><div class='panel-note'>Interactive</div><div class='lamp l1'></div><div class='lamp l2'></div><div class='lamp l3'></div><div class='visual-label'>Dashboard</div></a>"
        , "</div></div></div></body></html>"
        ]

loginPage :: TL.Text -> TL.Text
loginPage errMsg =
    mconcat
        [ "<!DOCTYPE html><html><head><meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1.0'>"
        , "<title>Enter Dashboard</title><style>"
        , "body{margin:0;font-family:'Segoe UI',sans-serif;background:linear-gradient(120deg,#15171d,#1b1d23,#16181e);color:white;min-height:100vh;}"
        , ".page-wrap{width:94%;max-width:1460px;margin:26px auto;background:linear-gradient(180deg,#1c1f26 0%, #171920 100%);border:1px solid rgba(255,255,255,0.06);box-shadow:0 10px 30px rgba(0,0,0,0.45);overflow:hidden;}"
        , ".hero{display:grid;grid-template-columns:1.05fr 0.95fr;gap:34px;padding:42px 46px 46px 46px;}"
        , ".left{display:flex;flex-direction:column;justify-content:center;min-height:640px;}"
        , ".left h1{font-size:72px;line-height:0.95;letter-spacing:-2px;margin:0 0 26px 0;color:#f3f4f6;font-weight:700;}"
        , ".left p{color:#a9afb7;font-size:19px;line-height:1.7;max-width:520px;margin:0 0 26px 0;}"
        , ".left-bullets{display:grid;gap:12px;margin-top:12px;color:#c9cdd3;font-size:16px;}"
        , ".left-bullets div{padding:12px 16px;border-radius:12px;background:rgba(255,255,255,0.04);border:1px solid rgba(255,255,255,0.05);max-width:430px;}"
        , ".card{align-self:center;background:linear-gradient(180deg,#20232a 0%, #16181d 100%);border:1px solid rgba(255,255,255,0.05);border-radius:20px;padding:36px;box-shadow:0 20px 40px rgba(0,0,0,0.45);}"
        , ".card h2{margin:0 0 8px 0;font-size:42px;}.card p{color:#a9afb7;margin:0 0 24px 0;line-height:1.6;}"
        , ".error{margin:0 0 18px 0;padding:12px 14px;border-radius:12px;background:rgba(255,107,107,0.12);border:1px solid rgba(255,107,107,0.35);color:#ffb4b4;font-size:14px;}"
        , "label{display:block;margin:12px 0 8px 0;font-size:13px;letter-spacing:2px;color:#9ca3af;text-transform:uppercase;}"
        , "input{width:100%;padding:15px 14px;font-size:16px;border-radius:12px;border:1px solid rgba(255,255,255,0.08);background:#111318;color:white;outline:none;box-sizing:border-box;}"
        , "button{width:100%;padding:16px 18px;margin-top:18px;background:#f3f4f6;color:#15171d;border:none;border-radius:12px;font-size:17px;font-weight:700;cursor:pointer;}"
        , ".back{display:inline-block;margin-top:18px;color:#d6a062;text-decoration:none;}.hint{margin-top:14px;color:#8f96a0;font-size:13px;line-height:1.6;}"
        , "@media (max-width:1100px){.hero{grid-template-columns:1fr;}.left{min-height:auto;}}"
        , "</style></head><body><div class='page-wrap'><div class='hero'><div class='left'>"
        , "<h1>Enter the<br>dashboard.</h1>"
        , "<p>Use your name, a valid email address, and a password to continue to the search workspace.</p>"
        , "<div class='left-bullets'><div>Recursive backend filtering for species-area matching</div><div>Year-wise change computation and sustainability insights</div><div>Professional results dashboard with graph and table</div></div>"
        , "</div><div class='card'><h2>Sign In</h2><p>Login is required before using any function in the project.</p>"
        , if TL.null errMsg then "" else "<div class='error'>" <> errMsg <> "</div>"
        , "<form method='post' action='/login'>"
        , "<label>Name</label><input type='text' name='fullname' placeholder='Enter your name' required>"
        , "<label>Email</label><input type='email' name='email' placeholder='Enter valid email' required>"
        , "<label>Password</label><input type='password' name='password' placeholder='Enter password' required>"
        , "<button type='submit'>Continue</button></form>"
        , "<div class='hint'>Email must be in valid format. Password must be at least 6 characters.</div>"
        , "<a class='back' href='/'>Back to Home</a>"
        , "</div></div></div></body></html>"
        ]

unauthorizedPage :: TL.Text
unauthorizedPage =
    mconcat
        [ "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Login Required</title><style>"
        , "body{margin:0;background:linear-gradient(120deg,#15171d,#1b1d23,#16181e);color:white;font-family:'Segoe UI',sans-serif;text-align:center;padding:60px;}"
        , ".card{max-width:700px;margin:auto;background:linear-gradient(180deg,#20232a 0%, #16181d 100%);border:1px solid rgba(255,255,255,0.05);border-radius:18px;padding:30px;}"
        , "a{display:inline-block;margin-top:18px;padding:12px 22px;background:#f3f4f6;color:#15171d;text-decoration:none;border-radius:12px;font-weight:700;}"
        , "</style></head><body><div class='card'><h1>Login Required</h1><p>You must log in before using this function.</p><a href='/login'>Go to Login</a></div></body></html>"
        ]

datasetPage :: TL.Text -> [FishRecord] -> TL.Text
datasetPage displayName records =
    let body =
            "<h1 class='page-title'>Dataset Overview</h1>\
            \<p class='page-sub'>This page displays your CSV dataset directly inside the project interface.</p>\
            \<div class='card'><div class='section-title'>Dataset Table</div><table>\
            \<thead><tr><th>Species</th><th>Area</th><th>Year</th><th>Catch</th></tr></thead>\
            \<tbody>" <> buildDatasetRows records <> "</tbody></table>\
            \<div class='muted' style='margin-top:18px;'>Total records loaded: " <> TL.pack (show (length records)) <> "</div></div>"
    in pageShell True displayName body

methodologyPage :: TL.Text -> TL.Text
methodologyPage displayName =
    let body =
            "<h1 class='page-title'>Recursive Methodology</h1>\
            \<p class='page-sub'>This project uses a Haskell backend to parse CSV data, filter records recursively, compute yearly changes, count declining years, and generate sustainability insights.</p>\
            \<div class='grid-2'>\
            \<div class='card'><div class='section-title'>Dataset Parsing</div><p class='muted'>The system reads CSV input and converts each row into a structured FishRecord containing species, area, year, and catch values.</p></div>\
            \<div class='card'><div class='section-title'>Recursive Filtering</div><p class='muted'>The backend uses recursive functions to filter records based on the selected species and matching area.</p></div>\
            \<div class='card'><div class='section-title'>Trend Computation</div><p class='muted'>Yearly changes are computed recursively to detect decline and improvement across the available years.</p></div>\
            \<div class='card'><div class='section-title'>Decision Logic</div><p class='muted'>The dashboard classifies the result as declining, mixed, or stable based on how many yearly changes are negative.</p></div>\
            \</div>"
    in pageShell True displayName body

searchPage :: [FishRecord] -> TL.Text -> TL.Text
searchPage records displayName =
    let speciesList = sort (nub (map species records))
        buildMap [] = ""
        buildMap (s:ss) =
            let areas = sort (nub [ area r | r <- records, species r == s ])
                one = "\"" <> TL.pack s <> "\":[" <> TL.intercalate "," (map (\a -> "\"" <> TL.pack a <> "\"") areas) <> "]"
            in one <> (if null ss then "" else ",") <> buildMap ss
        body =
            "<h1 class='page-title'>Search your dataset.</h1>\
            \<p class='page-sub'>Select a species and a matching area to generate your sustainability analysis dashboard.</p>\
            \<div class='card' style='max-width:960px;'><div class='section-title'>Fish Sustainability Analyzer</div>\
            \<form method='post' action='/analyze'>\
            \<label>Search Species</label>\
            \<input type='text' id='searchBox' placeholder='Type to search species...' onkeyup='filterSpecies()'>\
            \<label>Species</label>\
            \<select id='species' name='species' onchange='updateAreas()' required>\
            \<option value=''>Select Species</option>" <> buildOptions speciesList <> "</select>\
            \<label>Area</label>\
            \<select id='area' name='area' required><option value=''>Select Area</option></select>\
            \<button type='submit'>Analyze</button></form></div>\
            \<script>\
            \const mapping = {" <> buildMap speciesList <> "};\
            \function updateAreas(){\
            \ const s=document.getElementById('species').value;\
            \ const a=document.getElementById('area');\
            \ a.innerHTML=\"<option value=''>Select Area</option>\";\
            \ if(mapping[s]){ mapping[s].forEach(x=>{ let o=document.createElement('option'); o.value=x; o.text=x; a.appendChild(o); }); }\
            \}\
            \function filterSpecies(){\
            \ let input=document.getElementById('searchBox').value.toLowerCase();\
            \ let options=document.getElementById('species').options;\
            \ for(let i=0;i<options.length;i++){\
            \  let txt=options[i].text.toLowerCase();\
            \  options[i].style.display=txt.includes(input)?'':'none';\
            \ }\
            \}\
            \</script>"
    in pageShell True displayName body

resultsPage :: TL.Text -> TL.Text -> [Int] -> [Double] -> Int -> String -> TL.Text -> TL.Text
resultsPage sp ar yearsList catches declineCount status displayName =
    let diffs = yearlyDiff catches
        statusClass =
            if "Declining" `elem` words status then "danger"
            else if "Mixed" `elem` words status then "warning"
            else "good"
        yearsJs = TL.pack (show yearsList)
        catchesJs = TL.pack (show catches)
        changeList = Nothing : map Just diffs
        tableRows = buildRows yearsList catches changeList
        body =
            "<h1 class='page-title'>Fish Sustainability Results</h1>\
            \<div class='grid-2'>\
            \<div class='mini-card'><div class='mini-label'>Species</div><div class='mini-value'>" <> sp <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Area</div><div class='mini-value'>" <> ar <> "</div></div>\
            \</div><div style='height:18px'></div>\
            \<div class='grid-3'>\
            \<div class='mini-card'><div class='mini-label'>Years of Data</div><div class='mini-value'>" <> TL.pack (show (length yearsList)) <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Declining Years</div><div class='mini-value'>" <> TL.pack (show declineCount) <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Status</div><div class='" <> TL.pack statusClass <> "'>" <> TL.pack status <> "</div></div>\
            \</div><div style='height:20px'></div>\
            \<div class='card'><div class='section-title'>Catch Trend Graph</div><canvas id='catchChart' height='110'></canvas></div>\
            \<div class='card'><div class='section-title'>Year-wise Analysis Table</div>\
            \<table><thead><tr><th>Year</th><th>Catch</th><th>Yearly Change</th></tr></thead><tbody>" <> tableRows <> "</tbody></table>\
            \<div style='margin-top:20px;'><a class='btn' href='/dashboard'>Analyze Again</a></div></div>\
            \<script src='https://cdn.jsdelivr.net/npm/chart.js'></script>\
            \<script>\
            \const years = " <> yearsJs <> ";\
            \const catches = " <> catchesJs <> ";\
            \const ctx = document.getElementById('catchChart').getContext('2d');\
            \new Chart(ctx,{type:'line',data:{labels:years,datasets:[{label:'Catch',data:catches,borderColor:'#d6a062',backgroundColor:'rgba(214,160,98,0.12)',tension:0.35,fill:true,pointBackgroundColor:'#f4c38c',pointBorderColor:'#ffffff',pointRadius:4}]},options:{responsive:true,plugins:{legend:{labels:{color:'white'}}},scales:{x:{ticks:{color:'white'},grid:{color:'rgba(255,255,255,0.08)'}},y:{ticks:{color:'white'},grid:{color:'rgba(255,255,255,0.08)'}}}}});\
            \</script>"
    in pageShell True displayName body

compareSpeciesBody :: TL.Text -> [FishRecord] -> TL.Text
compareSpeciesBody displayName records =
    let speciesList = sort (nub (map species records))
        areaList = sort (nub (map area records))
        body =
            "<h1 class='page-title'>Species Compare</h1>\
            \<p class='page-sub'>Compare two different species in the same area using your dataset.</p>\
            \<div class='card' style='max-width:980px;'><div class='section-title'>Compare Species</div>\
            \<form method='post' action='/compare-species'>\
            \<div class='compare-box'>\
            \<div><label>Species 1</label><select name='species1' required><option value=''>Select Species 1</option>" <> buildOptions speciesList <> "</select></div>\
            \<div><label>Species 2</label><select name='species2' required><option value=''>Select Species 2</option>" <> buildOptions speciesList <> "</select></div>\
            \</div>\
            \<label style='margin-top:18px;'>Common Area</label>\
            \<select name='area' required><option value=''>Select Area</option>" <> buildOptions areaList <> "</select>\
            \<button type='submit' style='margin-top:18px;'>Compare</button>\
            \</form></div>"
    in pageShell True displayName body

compareSpeciesResultPage :: TL.Text -> String -> String -> String -> [FishRecord] -> [FishRecord] -> TL.Text
compareSpeciesResultPage displayName sp1 sp2 ar rs1 rs2 =
    let c1 = getCatchValues rs1
        c2 = getCatchValues rs2
        d1 = countDeclines (yearlyDiff c1)
        d2 = countDeclines (yearlyDiff c2)
        m1 = meanList c1
        m2 = meanList c2
        years1 = TL.pack (show (getYears rs1))
        vals1  = TL.pack (show c1)
        years2 = TL.pack (show (getYears rs2))
        vals2  = TL.pack (show c2)
        body =
            "<h1 class='page-title'>Species Compare Result</h1>\
            \<p class='page-sub'>Comparing <b>" <> TL.pack sp1 <> "</b> and <b>" <> TL.pack sp2 <> "</b> in <b>" <> TL.pack ar <> "</b>.</p>\
            \<div class='grid-2'>\
            \<div class='mini-card'><div class='mini-label'>Species 1 Average Catch</div><div class='mini-value'>" <> TL.pack (formatDouble m1) <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Species 2 Average Catch</div><div class='mini-value'>" <> TL.pack (formatDouble m2) <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Species 1 Declining Years</div><div class='mini-value'>" <> TL.pack (show d1) <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Species 2 Declining Years</div><div class='mini-value'>" <> TL.pack (show d2) <> "</div></div>\
            \</div><div style='height:20px'></div>\
            \<div class='card'><div class='section-title'>Comparison Graph</div><canvas id='compareChart' height='110'></canvas></div>\
            \<script src='https://cdn.jsdelivr.net/npm/chart.js'></script>\
            \<script>\
            \const labels1 = " <> years1 <> ";\
            \const data1 = " <> vals1 <> ";\
            \const labels2 = " <> years2 <> ";\
            \const data2 = " <> vals2 <> ";\
            \const allLabels = [...new Set(labels1.concat(labels2))].sort();\
            \function align(labels, vals){ let map = {}; for(let i=0;i<labels.length;i++){ map[labels[i]] = vals[i]; } return allLabels.map(x => map[x] ?? null); }\
            \const ctx = document.getElementById('compareChart').getContext('2d');\
            \new Chart(ctx,{type:'line',data:{labels:allLabels,datasets:[{label:'" <> TL.pack sp1 <> "',data:align(labels1,data1),borderColor:'#d6a062',backgroundColor:'rgba(214,160,98,0.12)',tension:0.35,fill:false},{label:'" <> TL.pack sp2 <> "',data:align(labels2,data2),borderColor:'#7ee7a2',backgroundColor:'rgba(126,231,162,0.12)',tension:0.35,fill:false}]},options:{responsive:true,plugins:{legend:{labels:{color:'white'}}},scales:{x:{ticks:{color:'white'},grid:{color:'rgba(255,255,255,0.08)'}},y:{ticks:{color:'white'},grid:{color:'rgba(255,255,255,0.08)'}}}}});\
            \</script>\
            \<div style='margin-top:20px;'><a class='btn' href='/compare-species'>Compare Again</a></div>"
    in pageShell True displayName body

compareAreasBody :: TL.Text -> [FishRecord] -> TL.Text
compareAreasBody displayName records =
    let speciesList = sort (nub (map species records))
        areaList = sort (nub (map area records))
        body =
            "<h1 class='page-title'>Area Compare</h1>\
            \<p class='page-sub'>Compare the same species across two different areas.</p>\
            \<div class='card' style='max-width:980px;'><div class='section-title'>Compare Areas</div>\
            \<form method='post' action='/compare-areas'>\
            \<label>Species</label>\
            \<select name='species' required><option value=''>Select Species</option>" <> buildOptions speciesList <> "</select>\
            \<div class='compare-box' style='margin-top:18px;'>\
            \<div><label>Area 1</label><select name='area1' required><option value=''>Select Area 1</option>" <> buildOptions areaList <> "</select></div>\
            \<div><label>Area 2</label><select name='area2' required><option value=''>Select Area 2</option>" <> buildOptions areaList <> "</select></div>\
            \</div>\
            \<button type='submit' style='margin-top:18px;'>Compare</button>\
            \</form></div>"
    in pageShell True displayName body

compareAreasResultPage :: TL.Text -> String -> String -> String -> [FishRecord] -> [FishRecord] -> TL.Text
compareAreasResultPage displayName sp ar1 ar2 rs1 rs2 =
    let c1 = getCatchValues rs1
        c2 = getCatchValues rs2
        d1 = countDeclines (yearlyDiff c1)
        d2 = countDeclines (yearlyDiff c2)
        m1 = meanList c1
        m2 = meanList c2
        years1 = TL.pack (show (getYears rs1))
        vals1  = TL.pack (show c1)
        years2 = TL.pack (show (getYears rs2))
        vals2  = TL.pack (show c2)
        body =
            "<h1 class='page-title'>Area Compare Result</h1>\
            \<p class='page-sub'>Comparing <b>" <> TL.pack sp <> "</b> across <b>" <> TL.pack ar1 <> "</b> and <b>" <> TL.pack ar2 <> "</b>.</p>\
            \<div class='grid-2'>\
            \<div class='mini-card'><div class='mini-label'>Area 1 Average Catch</div><div class='mini-value'>" <> TL.pack (formatDouble m1) <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Area 2 Average Catch</div><div class='mini-value'>" <> TL.pack (formatDouble m2) <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Area 1 Declining Years</div><div class='mini-value'>" <> TL.pack (show d1) <> "</div></div>\
            \<div class='mini-card'><div class='mini-label'>Area 2 Declining Years</div><div class='mini-value'>" <> TL.pack (show d2) <> "</div></div>\
            \</div><div style='height:20px'></div>\
            \<div class='card'><div class='section-title'>Comparison Graph</div><canvas id='areaCompareChart' height='110'></canvas></div>\
            \<script src='https://cdn.jsdelivr.net/npm/chart.js'></script>\
            \<script>\
            \const labels1 = " <> years1 <> ";\
            \const data1 = " <> vals1 <> ";\
            \const labels2 = " <> years2 <> ";\
            \const data2 = " <> vals2 <> ";\
            \const allLabels = [...new Set(labels1.concat(labels2))].sort();\
            \function align(labels, vals){ let map = {}; for(let i=0;i<labels.length;i++){ map[labels[i]] = vals[i]; } return allLabels.map(x => map[x] ?? null); }\
            \const ctx = document.getElementById('areaCompareChart').getContext('2d');\
            \new Chart(ctx,{type:'line',data:{labels:allLabels,datasets:[{label:'" <> TL.pack ar1 <> "',data:align(labels1,data1),borderColor:'#d6a062',backgroundColor:'rgba(214,160,98,0.12)',tension:0.35,fill:false},{label:'" <> TL.pack ar2 <> "',data:align(labels2,data2),borderColor:'#7ee7a2',backgroundColor:'rgba(126,231,162,0.12)',tension:0.35,fill:false}]},options:{responsive:true,plugins:{legend:{labels:{color:'white'}}},scales:{x:{ticks:{color:'white'},grid:{color:'rgba(255,255,255,0.08)'}},y:{ticks:{color:'white'},grid:{color:'rgba(255,255,255,0.08)'}}}}});\
            \</script>\
            \<div style='margin-top:20px;'><a class='btn' href='/compare-areas'>Compare Again</a></div>"
    in pageShell True displayName body

riskRankingRows :: [(String, Int, String)] -> TL.Text
riskRankingRows [] = ""
riskRankingRows ((label, declines, status):xs) =
    "<tr><td>" <> TL.pack label <> "</td><td>" <> TL.pack (show declines) <> "</td><td>" <> TL.pack status <> "</td></tr>"
    <> riskRankingRows xs

riskRankingPage :: TL.Text -> [FishRecord] -> TL.Text
riskRankingPage displayName records =
    let pairs = nub [(species r, area r) | r <- records]
        rankOne (sp, ar) =
            let fs = sortOn year (filterRecords sp ar records)
                diffs = yearlyDiff (getCatchValues fs)
                declines = countDeclines diffs
                status = sustainabilityStatus declines (length diffs)
            in (sp ++ " | " ++ ar, declines, status)
        ranked = sortOn (\(_, d, _) -> Down d) (map rankOne pairs)
        body =
            "<h1 class='page-title'>Risk Ranking</h1>\
            \<p class='page-sub'>Species-area pairs ranked by number of declining years.</p>\
            \<div class='card'><div class='section-title'>Ranking Table</div>\
            \<table><thead><tr><th>Species | Area</th><th>Declining Years</th><th>Status</th></tr></thead><tbody>"
            <> riskRankingRows ranked <> "</tbody></table></div>"
    in pageShell True displayName body

calculatorPage :: TL.Text -> [FishRecord] -> Maybe (String, String, Double, Double, Double, Double) -> TL.Text
calculatorPage displayName records mResult =
    let speciesList = sort (nub (map species records))
        mapping = speciesAreaMapJS records
        resultHtml =
            case mResult of
                Nothing -> ""
                Just (sp, ar, currentTons, prevAvg, lastKnown, pctFromLast) ->
                    "<div class='card' style='margin-top:20px;'>\
                    \<div class='section-title'>Result</div>\
                    \<p class='muted'><b>Species:</b> " <> TL.pack sp <> "</p>\
                    \<p class='muted'><b>Area:</b> " <> TL.pack ar <> "</p>\
                    \<p class='muted'><b>Entered Current Tons:</b> " <> TL.pack (formatDouble currentTons) <> "</p>\
                    \<p class='muted'><b>Previous Average Catch:</b> " <> TL.pack (formatDouble prevAvg) <> "</p>\
                    \<p class='muted'><b>Last Known Catch:</b> " <> TL.pack (formatDouble lastKnown) <> "</p>\
                    \<p class='muted'><b>Difference from Previous Average:</b> " <> TL.pack (formatDouble (currentTons - prevAvg)) <> "</p>\
                    \<p class='muted'><b>Percentage Change from Last Known Catch:</b> " <> TL.pack (formatDouble pctFromLast) <> "%</p>\
                    \</div>"
        body =
            "<h1 class='page-title'>Catch Calculator</h1>\
            \<p class='page-sub'>Select fish name and area, then enter current tons. The system uses previous years in the dataset to generate the result.</p>\
            \<div class='card' style='max-width:820px;'>\
            \<form method='post' action='/calculator'>\
            \<label>Fish Name</label>\
            \<select id='calcSpecies' name='species' onchange='updateCalcAreas()' required>\
            \<option value=''>Select Fish</option>" <> buildOptions speciesList <> "</select>\
            \<label>Area</label>\
            \<select id='calcArea' name='area' required><option value=''>Select Area</option></select>\
            \<label>Current Tons</label>\
            \<input type='number' step='any' name='current' required>\
            \<button type='submit' style='margin-top:18px;'>Calculate</button>\
            \</form></div>"
            <> resultHtml <>
            "<script>\
            \const calcMapping = " <> mapping <> ";\
            \function updateCalcAreas(){\
            \ const s = document.getElementById('calcSpecies').value;\
            \ const a = document.getElementById('calcArea');\
            \ a.innerHTML = \"<option value=''>Select Area</option>\";\
            \ if(calcMapping[s]){\
            \   calcMapping[s].forEach(x => { let o = document.createElement('option'); o.value = x; o.text = x; a.appendChild(o); });\
            \ }\
            \}\
            \</script>"
    in pageShell True displayName body

makeSessionToken :: IO TL.Text
makeSessionToken = do
    u <- newUnique
    pure (TL.pack (show (hashUnique u)))

setSessionCookie :: TL.Text -> ActionM ()
setSessionCookie token =
    setHeader "Set-Cookie" ("session=" <> token <> "; Path=/; HttpOnly")

clearSessionCookie :: ActionM ()
clearSessionCookie =
    setHeader "Set-Cookie" "session=; Path=/; Max-Age=0; HttpOnly"

extractSessionToken :: TL.Text -> Maybe TL.Text
extractSessionToken cookieText =
    let parts = T.splitOn "; " (TL.toStrict cookieText)
        pick [] = Nothing
        pick (p:ps) =
            if "session=" `T.isPrefixOf` p
            then Just (TL.fromStrict (T.drop 8 p))
            else pick ps
    in pick parts

getLoggedInUser :: IORef [(TL.Text, TL.Text)] -> ActionM (Maybe TL.Text)
getLoggedInUser sessionsRef = do
    mcookie <- Web.Scotty.header "Cookie"
    case mcookie of
        Nothing -> pure Nothing
        Just cookieText ->
            case extractSessionToken cookieText of
                Nothing -> pure Nothing
                Just tok -> do
                    sessions <- liftIO (readIORef sessionsRef)
                    pure (Prelude.lookup tok sessions)

requireLogin :: IORef [(TL.Text, TL.Text)] -> ActionM TL.Text
requireLogin sessionsRef = do
    muser <- getLoggedInUser sessionsRef
    case muser of
        Just name -> pure name
        Nothing -> do
            html unauthorizedPage
            finish

main :: IO ()
main = do
    putStrLn "SERVER STARTING..."

    mPort <- lookupEnv "PORT"
    let port = maybe 3000 read mPort

    putStrLn ("Running on port: " ++ show port)

    scotty port $ do
        get "/" $ do
            text "Server is running"