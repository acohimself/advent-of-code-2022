{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2022/day/6

-}


module Main exposing (main)

import Browser
import Html exposing (Html, a, div, form, h2, text, textarea)
import Html.Attributes exposing (action, class, placeholder, required, rows, style)
import Html.Events exposing (onClick, onInput)
import Set
import String exposing (left, toList)


type alias Model =
    { day : Int
    , title : String
    , inputString : String
    , solution1 : Int
    , solution2 : Int
    }


initialModel : Model
initialModel =
    { day = 6
    , title = "Tuning Trouble"
    , inputString = ""
    , solution1 = 0
    , solution2 = 0
    }


type Msg
    = LoadInputFromForm String
    | LoadInputFromCache
    | FindSolutions


processSignal : (String -> Bool) -> Int -> String -> String -> Int
processSignal detection counter scan signal =
    case detection scan of
        True ->
            counter

        False ->
            let
                next =
                    String.dropLeft 1 scan ++ left 1 signal

                remainingSignal =
                    String.dropLeft 1 signal
            in
            processSignal detection (1 + counter) next remainingSignal


findStartOfPacket : String -> Int
findStartOfPacket s =
    let
        detectStartOfPacket : String -> Bool
        detectStartOfPacket packet =
            case toList packet of
                x1 :: x2 :: x3 :: x4 :: _ ->
                    x1 /= x2 && x1 /= x3 && x1 /= x4 && x2 /= x3 && x2 /= x4 && x3 /= x4

                _ ->
                    False
    in
    processSignal detectStartOfPacket 4 (left 4 s) (String.dropLeft 4 s)


findStartOfMessage : String -> Int
findStartOfMessage s =
    let
        detectStartOfMessage : String -> Bool
        detectStartOfMessage message =
            let
                uniqueSize =
                    Set.size <| Set.fromList <| toList message
            in
            uniqueSize == 14
    in
    processSignal detectStartOfMessage 14 (left 14 s) (String.dropLeft 14 s)


update : Msg -> Model -> Model
update msg model =
    case msg of
        LoadInputFromForm text ->
            { model | inputString = text }

        LoadInputFromCache ->
            { model | inputString = inputString }

        FindSolutions ->
            { model
                | solution1 = findStartOfPacket model.inputString
                , solution2 = findStartOfMessage model.inputString
            }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            [ div [ class "w-1/2 mx-auto" ]
                [ h2 [ class "text-xl" ] [ text <| "Day" ++ String.fromInt model.day ++ ": " ++ model.title ]
                ]
            , div [ class "flex space-x-8 justify-center" ]
                [ a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick LoadInputFromCache
                    ]
                    [ text "Load data" ]
                , a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick FindSolutions
                    ]
                    [ text "Find solutions" ]
                ]
            , form [ action "#" ]
                [ div [ class "mdl-textfield mdl-js-textfield", style "padding" "16px" ]
                    [ textarea
                        [ class "form-control block w-full px-3 py-1.5 text-base font-normal text-gray-700 bg-white bg-clip-padding border border-solid border-gray-300 rounded transition ease-in-out m-0 focus:text-gray-700 focus:bg-white focus:border-blue-600 focus:outline-none"
                        , rows 3
                        , placeholder "Paste input text here"
                        , required True
                        , onInput LoadInputFromForm
                        ]
                        [ text model.inputString ]
                    ]
                , div [ class "flex space-x-8 justify-center" ]
                    [ div [ class "textarea_label" ] [ text "First task solution: " ]
                    , text <| String.fromInt model.solution1
                    , div [ class "textarea_label" ] [ text "Second task solution: " ]
                    , text <| String.fromInt model.solution2
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


inputString : String
inputString =
    """rdzrddbgdbbqtbqbrrznznjzjjctcbtttrvrwwsvwssjfjcjnjzzqgzgzsggfddvnnrbbwgwfwgfgpgrprgrprmmqjmqmvmlmmgjmmgqmqppqgqlqmllszsmzzwfzfqzzgcccmggvfgvvzlvzlznnjfjsscrsrprcrscsqsfqfggrrhccpnpwphwhchfhtfhttzpzwzjzqzjjqjdqqnvqnqjqbqcbqccdllbcblbjljgjbgbhhgphghjhrrvbrvbrvvfggwbwnnghgwhhpbpgphpnpgpqqzvqzqtzzngzngnjnnffgbgbvvjvqvwwqnwqnnvrnnfgfttcftcfczzpbbdlbdbhbssrggtfgfbgfgfnffqsfqfcqqqwvvqpphfpfcfrrjmjhjrjbblcbbznzwwpbwwsvvwnvvzjvzzmllrpllqffplpwlpwwlpwwmnmrrlggvssjggdjjsffbtthlhsswvwjjbtbjbsswqsshppddcjjlrlttwgtwwcnwcncmcpmpnnhndnccqscqqzqpplglvvpggcvvhnhmhqqsvvhcvvjpphbhnndpdbdndjnnmccmjmhmrrlcrrfzfnndldffstffhqffhcffhbhwhzhnhrhprhhtqtztntnptnppjdpdqdzqqpzprrqbqmqhmqqmhqqjttlrrqbqnqdnnrqqtqfqjjdfjfdjjsnspptltlgttmctcgttcthcttdpppzphpnhhvthtccbvccpplpwwfgwfffhfbbrsbbgppnhpnhhwnnnrprcpcncffrqqpjprpddjnddnccqtcthhfqfnnnlrrzhhmvvczzlbzbrzbbjpjpbbjttmwtwbtbqqtssgdsdsmsmbsmsfftltmtwwsswbwdbdwbbblgbbqpbqbmbgmbgmbbvcvbbmrrldlpdpqqnvvrzzcddjsdjjfvjvvndnvvrsvrsvrsrmsswlswlssqllfmlffmwmttjgttzgtggghrhprppbgbdgdtgddvmdvvbmmchhjppczzglzglzglzgggsmsrrsttgddmvvhddvdhvvcfcvvglvlmvmdvmdvdcdvvgfftccljllmssrvvqhvvnlnplnlglrlvrvfvrrgjjzqjjzcjcvcsvvbppgrrbdrdsrddqsspwssdlltlcttgngzglgfgzfzhfzhfhnffmsspvsscjjrbbpgbbfgbffhggzpznzmnmcmzmdzmzjzddcsscgssdnddjbdjdtdzdwwshwhfhtthjhsjjfjvvdnnmzmtmmlrlblzzmmspsnpnbnvvczvzsszrrsgsbsrrlprrzfrrjdrdbdhdvvwrvvrfvrrvwrrqlqwwqjjpwwffdvffwfbwwdrwdrddhhhjghjhvhccgbcbvblllbwlwtlldbdzbbdpbbqmmglmllvwwzmzhhfnfcnfcfddvrvhvbvzvbvhvthtrtppsccggdgpghgfhhhnwnmmwfwtffpjfppmdpdhppvwvfvvhdhzhtzhtzzplpspmpnpzpgzgqgvvqcqcqmqffzszwzfwfzzcmchhmgmppgwwvffnnvhhttvtjvjgjljmmfcmmzmvvjfjggppqmpqqswsnwwdvwdvwdwjwrrrtprpqrrsddsshgssbbpbgpbbprprzppsnsbszszccddfcftcfczffchfcfjjffjwwsbbvfvpfprpbczqdnvrqrlhsrzvlvgzbqpwgpmgftzfvtlnlqrtmpjfstmsrbfjldtmhvvqwznrcflbvmsnbzcrjvbzgvvmwlzcwggwvhnpscnwhjldzjvmtfdgvptfhjtdtwfjntzqtswqgwvnfgqhqpdvdchfqjzfhgjmdstmchppcvgpdfrqbrhrvdvzbtnnrpqsnmnljjzgzqfnzmlvbzmwzfbfszvfsqdnqfstpjglwtcdjpmdfqzfcsngbcvvjvdzlnzndcggcdtdjpwgvfzlfdqpgbgfjjfvgtwwrgtrfcpfvmzvrwrftpfdprzmcmrpnjpfrdvbmlrfzrjcdzhvwbpgmbpdnqggjdpqttgnqbtjfmrglqbvcfjwghrqtcgjddhwpntjgtghmfgvjdhbzzgmfnrrrgvdmsfbhndfcwlbbtsdcbpgfpbvnwmpwdpjlvcbcgnwjzftsfqhvwdshzltrmqpcsngfzrvfwhffrcjzlfqjqdbcntdwhfrfnrzwftqhlfjpjqpngjqjcnfnrpmmbprdtzgsvsdjpzsnzzmzdfjplfhzqrnrwggcvrrmqzwlvslwvtvhbgwmjmnzftrbfhdrzszcvmdhgfvwgwgsgqtpwgvpvfrszczmsmstwhnhtnftmmmjblpchrnrdwlnhrvqtbwqwdchfjbcldmjmjzwlcngfgfvmblgmnwtwvjmzswzmpvdjtcfgcpvvqhrfnwcczrrrhwwvfrngwmlstqcvvrqrshwdvrgtgbffvlwtvwhvlcwwqgspnfmndbsbpvdbjwdlfntwrrmtgsdtwbfmjjcjfvsvltwdjmvswwpchpdtsjmbbgcgtddcbprwznsldmwflrgcrgfflzwllrzcrgdgqgsvrqmspqzsrzvppztrhlpsnqdlhmghdcrppvbljsnrjgcwhpvmlgcnswrjjbjltwnctqbqsffbcfpclhcbrnsjlmrstpngbvcfcbjstgvzwfgcsqbwgqqblsnmfddprrqmpqgfrjbhfptrdvltcrmtrcqgcdfpjhjptzngzqdghqhpsfwlmrwgfhldzpfrbtzzgsblcfmwztjmtjzgwrpttvwfhbntvsgnfvbpfpnscdspmcvncsqltzqnwczvdbtphwrdtjcszmhbpcfnvbrbgfpnrrhvhzlrglthpldrlfscpvpvttvjtfdrqpjvnwvmscdvclbnzfvppslzgmglrvvdvpsbsfhflnjbdnpqzjzlrfgwgjvwvpthjptftlzppmnmrrpfvdhgwfzfdnnbhqpwrzvvdgtcrflwfjgbmvbgsnqzqtmsvrdlfmlbqqdnfftljbtphnpqqrgbrlzrbbhlgvjpcdbmtvzlpqhbvjpmhpdmtrmjwvzrbfrdrdfmwsfvllljdwmnqlgcnzvpphwmlmstcsljvmljcjprtgmzfssgbtjlttssfzbcqbgnjnvrwzchtwtwdtfwngdflwzjrhzdlrrqfnsztvdbzqfwfzppqrghrhzsqtwqstsfspddpjrpffhgqvspzwmlzwhtzqqldqbwlsrqhlmvhhmzjpdsrgdcvqnfpldnmblgvvssrcdnjqvcgwtwmhqdcwtsqqhbntvjnlbljlqjqglggvpqncpdzztlvhhlghtrncfcdhjtzwjqdhlntjfrzccbnmglmnzwvplclvcmnsppqjhbggbzncqlcfnzbbzdjrvcdthcqwjzjvdbjddcbjchwfgbjhwqpzbpgsdtlwlphtvhwddjdbwbpsnqhnffqptcrljcqzzhszdfpdfsgflhwwsgbfcnwrbdqflnrcwddwwmfztlbswlzhtzzcllnvbtqgjsdzmhcpnnqpdpqgdntlfwgvddgqvhqhrbvstsmzrmgwslpdjlsbgthfhgnlftbqdnzsvcrrmllcjvdlqrzbvrbrjcbpttsvwcrnlvvbnjvfgldzmtflvmzqdgbnjcgctllrldgzltwlswmfbbwjmcqpldhhdsmqpbvnjprdqnvrbjhrjzqwqfrfqwngwtwjjmzdbqmpmvqrprjhnhnrlmlgpfwwzjhlgmbzdlpshcqpnlgrqvprbspmdznzzsvhdlzwmttpdnlrlqjllqnshjllvvsrblscjcmbcqlsgpcjlmmpgwrvjnjzvzfgvgghwqfjswjbjghmzcgdpsjwhbnzmbhtzgnchpbrmnfdbfscgzldpqmvprjpvcwtwdfjblfshffwqctdphhnhngsjlrqtqprpjhwqcbmhctqbpdtpzvbbfncfrcvmbfvqmbmqjvgtdvspfqfbqnmjwhzbcpcfgbhtllbsssqntfbmsmlwhjchgcsrvsfznbmspwwszqfnwzzljfnvcwnwmgfzqfvmwwwdjd"""
