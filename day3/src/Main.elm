{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2022/day/3

-}


module Main exposing (main)

import Browser
import Char exposing (isUpper, toCode)
import Debug exposing (toString)
import Html exposing (Html, a, button, div, form, h2, input, label, text, textarea)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (concatMap, drop, foldl, length, map, member, sum, take)
import Maybe exposing (Maybe, withDefault)
import String exposing (contains, fromChar, fromList, split, toList, uncons)


type alias Model =
    { inputString : String
    , solution1 : Int
    , solution2 : Int
    }


initialModel : Model
initialModel =
    { inputString = ""
    , solution1 = 0
    , solution2 = 0
    }


type Msg
    = LoadInputFromForm String
    | LoadInputFromCache
    | FindSolutions


type alias Rucksack =
    ( List Char, List Char )


priority : Char -> Int
priority c =
    case isUpper c of
        True ->
            toCode c - 38

        False ->
            toCode c - 96


parseInput : String -> List Rucksack
parseInput input =
    split "\n" input
        |> map toList
        |> map (\l -> ( take (length l // 2) l, drop (length l // 2) l ))


findDisplacedItem : Rucksack -> List Char
findDisplacedItem ru =
    case ru of
        ( l :: ls, rs ) ->
            case contains (fromChar l) (fromList rs) of
                True ->
                    [ l ]

                _ ->
                    findDisplacedItem ( ls, rs )

        ( [], _ ) ->
            []


priorityOfDisplaced : List Rucksack -> Int
priorityOfDisplaced rs =
    concatMap findDisplacedItem rs
        |> map priority
        |> sum


priorityOfBadges : List String -> Int
priorityOfBadges ru =
    case ru of
        x1 :: x2 :: x3 :: rs ->
            priorityOfBadge x1 x2 x3 + priorityOfBadges rs

        _ ->
            0


priorityOfBadge : String -> String -> String -> Int
priorityOfBadge x1 x2 x3 =
    case uncons x1 of
        Just ( l, ls ) ->
            case contains (fromChar l) x2 of
                True ->
                    case contains (fromChar l) x3 of
                        True ->
                            priority l

                        False ->
                            priorityOfBadge ls x2 x3

                _ ->
                    priorityOfBadge ls x2 x3

        _ ->
            0


update : Msg -> Model -> Model
update msg model =
    case msg of
        LoadInputFromForm text ->
            { model | inputString = text }

        LoadInputFromCache ->
            { model | inputString = inputString }

        FindSolutions ->
            { model
                | solution1 = priorityOfDisplaced (parseInput model.inputString)
                , solution2 = priorityOfBadges (split "\n" model.inputString)
            }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            [ div [ class "w-1/2 mx-auto" ]
                [ h2 [ class "text-xl" ] [ text "Day2: Rock Paper Scissors" ]
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
    """QJRBMDMtRDCtJzBtJMfjNjhwvmNDvwjLVVgh
TPSNNPZGTjgmSmvfjL
bPlpZZbpsTlTsWprpGFCJtRtzMNdMMBBcWnJQB
tppvbQBhpQQdrzMMcLwhMc
gZnWRccRNgFGRGRFRNNgZgJMddddLLLMCPqwLCNPwqPJ
nRRmFSnWmlgZlTlTllSlSWWWTsfvfDQpBfBcpQvpVQpTfQQf
lRlsVFgTlMgRNsSNTlFgmbWnMPppPnMqWZMWPPWW
fDjgBJdCfCHHBnfLWpqnmnpZmf
GjQHHcdvJHQBHSSNsFQFslwwRg
NPwDLDHNwjLLHWjbdSbDfJJQTZsZDS
BcFBcvgFvghnFLrBpvrgcgrJSZJpQdfSTZbCsSdfZZfbCf
VrngVFRmrVWHLGVMlL
SNBBBDlfZDLqNGmgFjjmBsQgCFtF
VPPVbhpbhMhRhncnScRncbrQtCgQQFmjjjsgtRtQHmFQ
nhWcPJVhpbvMvwvwllvSlGlD
wNlNNqtqHHHPhqCz
MMMMcQSWSpQCWFnRRPchLVvPLLzhmhLzhh
CrgRSWrnrQpppRQrCTnRTRtGtBDBfbNBllbTJlZtfNBN
QNbbNrnNnCwHmNPQmzqQNPsCCfBFFGtsBBddBDtCJDJd
gvVgpZWgTWvRvlvLPDDJjGBfdsdpDDJGdd
ZRMWWRMVgRZghTggPSMZzQwwnqwmnzhNnNwHcQHm
VmPHzBmpmQHbVHSpNHBVQCtRPPCPvFFMqqntZCZqMR
dWlDcfcfcjcfDWjlsZfjJhdGvFLGnLsLqsRnvRvRGGRttC
wfJhZTllcfdZdfjJfjdmQzHVSzHzgHQTpHpmpV
qNnqmzmCBfvmDvBm
HcdhtQdttbbhtVcrVVDMfZvdMBTqsWZMBsWZ
HQGtctRblwqpNwRN
SBtBLBMZzPDDNFFDQnVVVnnDmf
dgCjblRdgRvrbwjJGzQQQzwJVJ
WpWbCWWvlgrcCHdvvCdvWbSLZzhhZhtLBPPStSPhMSpM
PlPnGGGzCqqlrqTRsbTmFRWgDPmR
wwpLtjwpzjDwFWRsWTWW
NZtJjHNNhHfnCBcJMBlCSz
wSrwggPrhJhCdddw
tLMNvMTFhDZdhTBh
LtMvFttGbNcWRsLFLsccRRgfnSrPjPnfljSfPWlnPhrS
TSZlwZSSccSHZLHVcllSvmDLmJhjDDffJmGjQjgQQJ
sdBdzsNnBMBstNNMFhNPNbPzgGfDgJrtrfjCjDrCfJmmDQJf
BnnBznRsFRFBsspzzbZpSTqVTpHVhpTvlqVW
VtVjjhdFmCCfhRRzzSDbDzpmgzmvgb
CHJqrswsWvbvJbpD
CqCPcZHGHTcsCBQsBrTGHMFnLVQjMjLVVhdhnFQVRL
tvlPSrlNNvtglTtPccldQdhbQbZdcqqZ
mRmBGHWmDFRsZqHrfbdhqhZZ
jjMGjWrJpttNjtgg
HPtCMJNjvJLMDZRdBgLSBSfsWBgG
VmnrhwwqhbbzrwnDrqpdWBgfdSdfBGgffGWRdh
qmnTFbVnpqVzpnvlDFJZDClNZPZN
NNRFQfzbNWhLHTVh
dGjptnrPqgvqjccvndnnPPhlHrVVTHLWMHlwmrlHMmVTWm
tDggGnPqDcPPpPpddjGhggtJCCSssfJbQsDfbZsbsbRZFQ
bqZWhbvvvqfvhqvQCChhZlllGwlwGjNRrNGrwGwNRQ
PmspSscJVJStzSVzWJlgwlwNlGRLDGrPgNwN
pdHmWMVStWJWFBBCCbMhCfbC
wtwbctGLwGWhGwfWwhNrnLrlrQFNmPNNVrrl
CSdqZRsMStdJMMSZqPnFmVqPQlnjNjqj
TMtsTBSSRZBCJStMJSZTHtfvpgvzzWwhbpwhggbzHpbW
HncMbCwCncHlcbMDMnMFGsNsJVFJGchVTTcmcG
RRfBRNjRLLJTLTThsq
zpBRjWRrRvBpNtRWrgwbrwQPDPMDwCnn
TDcPLTVRjntFwDwDnb
SJJhffHqHZZgHGSFFbdrGTGnGv
NQHWZgJQHNHgHQhlLLLBjpRTjLjMNNLM
sMNnNRNrlGlsZBrGsrFQpclWlWLfpWjtzTfDtpzj
gvhPgwTgdSHtHDtpDPLp
gwhSwdvTSTbSgRrZNrrNFFNBGb
rtZnDHJrrDtGtGHvGHDWfdfwCjcBhjBCffwwLv
lzVlzsTRsmzVNTspVsMMsmwCLcmjmcdbBBChwfBbCW
sVTMpTpppsVMsPRPVzMNFqMFwZtQrHZDGqgHZrSQQrQQJDGn
wGQQMMQvCTPPQnHPBS
FsWdJddszWrrRRJRTmRmpppRHBNPBppNHp
rWdFWlFJzbzzMTwcvvMbGMgc
WTnnTpqSnCLmjGgSgjztgg
rQRHQvbNLwrgtGtrmDglJt
PwHRNvQPsvHvPTTpLTcCLVnq
qTsqJDJHjjfMCSDj
RnGGNFGznzGVnBCWmfSMSLwWRwSj
NnBbVQFVCClctHQc
BHzmfDHfJLGcQBGgQLDcstNttlZgdlltldshgZZg
PwPPSJwPvSNZlvSl
CJwwjnJFFnWRMcMzcHMHRzGL
rmZpvcZcqccsqmqzzzcBRLBZbBBRLBlRGVdfZR
PwjFggwMDgNFwPgTwNFgtFJjfGLhBLsGRGbfBfBLbbTVLbdf
DWJwgDWMJJDWCCNHmrnscmqqcnpWSQ
bsRlVgMhtzHvhRvpzcLSZcTWLGzTTrGc
QJnDjmqjJdmDqqrGWWsZsZTc
nQPsnwCJdBDJDDJvhHhpMRCVlhlgRV
NBNwMCtNgqCHClHClq
JpQmFrQQfHfWjJTfLTjfLRRFRvnvhvnDGDcRcvVGGV
HTzzpzzdHgbBZZtdMB
SWcVvBFBVBjShWhGQtZnHFDHRGGQsR
pMZpmmJPbwbTTQttrDrRrttT
mZflqdlbMVcNjdWLSj
tvjdccdbLjhvhlcjRMvRTCQJmBPBCFRG
qgnqZfHpZDVnCpZzZJQFQBgmmPFJBmRQJQ
SHDZDDzpNVpfsNsHqpDSjLwCbbWLChwtwjtCWc
FsWTbcwmGfFFFrpl
LMhzdfqjLdHQQnSvldGvnS
VZjVNzfNLtjzDMhVDNtqDqwJwRmmmZJgmgcbWgRRwCbJ
ZJbPwwfJcGlwCrrZrMMddMMMtt
pTNvvSSHmmnbpFRp
SLSjSLDSQNLHNDbJbcfJclBzjGsz
WSQCWQWstCWCCgNNsDCZMZDBjjlLPnHMMLPrHlcrcLHHTjTh
bVFJFwfdRFFgjTPgnc
GmzRRqvRddbdRRdfJRJfsqsSSsZDsDBQZtCSgpgt
FPjprPpPCCFpFPHWsWvqnnllQsdLQMMlLtslLQMc
wmzJgzRSRRJghBbwGBBSbtGfLfGlcNnlltdddQtrMd
zDmRBmwDrpVFDTVDVp
FPGqjsZGlDJmzsHcTcTMMs
SQNLSvdbvVbrSbHcftGcrpHGfMmf
CNNGSCCdSCZqjqljZF
GvqpqrpqdqdsdGshSMhhRsSMhhlSlJ
DLCzzjzBwCbQWtQlRRFRRJFptfffgM
WzpLbLDbBcLPjWQWDBzzmnvNndHNqZqZNZNvcrNT
scHCGfWHsvWHVfGsggHfgvVcSLwLLPRwwDLPLllRPDzlPr
tbjqqNNTlPDTTSrD
QntmNbNnnddqJqqbFJHZWWHWJWvZHGVJsSsp
WZjpjwwGBGZQsqBLBHLHSRLP
mJhtdfVtDVJtvVLSmNRSccPPPlNHcH
JJLCFDhLCfVGGwbGGCwrGC
nBnsGSCrptmsLWGhWRvVRJVJ
rllMZZbcWWLJvhTl
MHwzczHwwHqZcdzMdbqSmwsssmtNCrBmtrnQNB
LzwrZNrNzBMrJBzJsfqqntMlVlSfhnhb
HTDPWDHPTgGHWTGcPFRgFpPPtfqmsfqlccmlSmnblbshqnmm
jWGgpRGPFRHjzdBBsrBJvj
hjNghjlwqjzGhwhGwLrMMrsMdsMfczPfsr
ZJQSFZFZpCTQSZHTTFbcWWPbWsWrdLVmrMWMfr
tttHSCpFQBQQpJZSJLgBNNDhqhBqvBvvRq
hLLJJJLcLPLfLwcJDchfhpSmqGbmdQGmGSdbqdbmqGGGdG
zgCCVVvVCNVssdbqmtMWvbnndD
rCCZZCVTjVZNzFZJBlflBLccDhBFFB
wwPPHfCMHQsrcwPbMPMcvQFJvqWgFTZgDFJltgZt
jRBVLhpNqpBmRhhRdNJZJgWTBBtgZWltZJJJ
mSjndhSzphjLRVqmhphNShrGMGrcbGbnGCHGwrwGfbbG
PVBRhBdlwRtRhRBwtBlVzDcGpVcZnggGzGMMsg
fFFWQqbFbLWCWvvFbTjjGnsZMfgsZcZzSZGMpSgD
QJTCCLFFLjFqFbHTbbltmhBNwwcNmthNhlHr
qwPJJsJdbPdwJddQCRCgCTMTRGGwMG
cLFcFBZNWWQLSQRfZjpljTGRCgGR
cFvrcNBFJDhzdQzv
zTsVTqDqQNtNwwMVmN
pHpSzPbRrvbRrGzGMwZwlBJmNtclwJpB
SjHRPfRbffPHqzCCCdTsTzqj
jnbMBnPjjjFtBtMjFPRtGfvvfzgWWHMfWHTlGgHH
dCpdqrVrmdpHfTJTCWGJgG
qVdrppqSTddqNwZcDPPPhZRBPBLBRLjF
VbHqLlGQlgjLjjQsNvCZTsNjMtCZvT
SJtttppwwpwBwdPvsvCvBZrvNrTrvM
JDnWJpDSSpmSwmpPzSwznhDlqGqqtqqHGHLlhblGbR
RqRJJVMPdRVVpqMdFwmvnSMwZcfCGfDSZc
CssQgjssvZvjffmS
zNlbbWTBLWCbCPPFPbVH
nvQsHSsGvNvnQghTRMrrjpjM
ttlLDlzPtGDcRRtpZTFjtgMj
PBLBwPPDzzLwblzffzLlVHHsCCHqsfvCCSsGSNWC
jHrTrThrtHgttThgHTtfgTgsmZZmBSZGSGsSGfZBZFFmQs
qCCPdbcCJddbRcsQSGhFzmZqZGmq
VVNNdVvclDcPbMWMwnnlwhphjp
ZdBgJqFWNNNqnZZNGsBCCCRvrCwCjCssCB
htDPMSPtMPzPTLMzMTMbRRbTbvwRCjfRfsbWWs
LhMmtMDWmHlpppplJZJgNd
mhtsjtbChcpLqmpmzL
DPlPprrfBrpGHHVGNVHRqcNvvLLqLcvJzzTvLc
VFfVPrrBQFPlDDwDwBpBtSgQjnghMhCdbSnnhtMM
DPDMpbsHPDPNtdtrgMtdnQ
WShWlSCJVlzccSBvBvhVZZWlgTNTrNrrQTjQjjjjgDSgSdNt
cvmCDvCJCcsRbmpFmqms
sSfFssmLnLwPtrrmttsFbDvWgCvddVgfgWdRDWlChD
nnGnHBzqHjqBJGChlRClhvghJWDd
jNzNcczMcGntPMwwSsSr
GGPCThCCvCTVWBCBGMVMsTgZJsrZtHNNtrsHJrgH
zjRwcwwfvSjmwznfzQSHDJtgrNrRNrLDsRrHtD
fjvzmcfSlSznwcnmnSQnhdlhWBpGpdBqhGhqhVPd
sHGGqpRqfNRVbDDtVwwzWf
CCLQZllTQLTcSShTQvjhQLnnWrDzVpwtDDwVDnczwMwM
vggZLZTldlhpCTlZlZCRRPNRmqdmGBHPFqsGqN
wwFDFLMDjjCNgNwNlwwgvR
frPbSJMSSPBqrfppSqrBZqMQhHlmNsRZmmslvghsmhsgggtZ
TPSPfBQrdJSfTTqSbbBfTfdcGWjFWWFDWnGMjLjGVFCj
LZRZbHtqnVztHTTTjMBQjQHH
rJcDGpwwgDwCCWFGSFMSffVWfF
cNNNgvhNglDnhdzsbLbmVs
RwmrGVPmNLzdmVpmrVtHDjjgDHHRqjFtngFt
CBlWhQWlTWshsblFGntjHtGbHG
WsTSGZSTQZZJpPNdzSrzwvpr
CVsggSgdwSwghVSTCgVZjJlRvlQNJHJGZVvjvj
qrrnzrrpDFMzbDbbzrMbBcNjRBHHQHGRRllHHPBNBljl
rnFppcpWcqnWMLDNsggSmWmsWfggdg
wjQzPjJcplwmDDBL
vghWhhnfWqzhftWtfnbFBmnGDnLGDbDmmC
zZNvZrNsWfgVftNZhQcSdPHPTcPHQQTTJV
WjvPVbWnbbFvjfLlcplQvLQvCwCl
sJhmrrTRTDDJHhhsmJhmrNDdQwLQQlHllHwwLpCLclBBlcPC
RJTRDdmPmmzNTDhnWtzMfMWtqjqqWM
vvpjqtllDMlHDtDBsPSSfBJFlSffNS
gwTmJrTcJWrNSmsNBBPfmf
VzzJzgTnddzWrwngnWqbHqbtLqjqvpvqhbMd
TlpzwGZGGFmZJdPpRtpHPrpcPs
CMJCMgQjMQvrfMHtMfHv
DjnNjCBqCCNnWWgDBQQDnCZwFJwmwwTznmFVwFmzTJJm
CcDPppDCFdDrFcFsMsdlLVjjLsMHvM
fqSmmtNGqLNffhHHbsMsbjbjNjbv
SthSGmLnmfwfWGWhSQGSQRnGpDpJPCDJrBPTcPrDwPzFcpFT
FdqjDtPWzqPdnPPtPFbssllqLJlqNppsJGppLp
TwfrcvwRgvfTBWRgBssJhspHfffJHlHNGh
MMZCQrrRBwQCCZMQwcTMwPztnFZSDWVWPttPSZzdzd
prHlrpJbdccllrrPbFdrgPzZfZhZVhRZVScNRNWtSZjWRW
LmwCCnvqwGCLMnsWtGRZWVfbfbftRW
bwnvBnLBvbsBvszHzpgBlPzHHlzg
grSJNTSgBHgpqhvCGbbZddGCGbbT
nDLMssQMRLwMtMWRWCZdQfqjfGvZQfCjCc
PqsDWPMLnwlRllJzghmgmSNhpgrl
TQGcWQBDnSzzsBSL
mJJlqJwVJdbSrhlrlhhsLL
JPtwMtdPbJbVqVNpPtmbpwZcQDFFcCccFjCQjpQWSWZg
JfbfpZJmzffmpZnZZwsrwDFvwHPP
RDdQtWTWQQSTGNRhsFsjnvjwrhPjtH
QccddTVQQldcGGRdGlgmVmBzfVpDmbgggmpL
HVnhVcHvpVFWDpmP
QswNZblTTwmqlntDPdqD
sGZzNwsGNThhMrhBBhzn
fQllBlVQncgwLlfWwWDvppZZggZqGpZgpGdvGG
shPTRsFbNFJmvqpGjrpvPDdr
RNFDtRRRssRTStRmTlnzwSVQlVVWfWzcQc
WmCpPCWTjQPCWWSjSTmrqRLGDRFGrTFDRFDLDD
gJnVcnVzdfnZgchvrslMDZGlRRDZLR
fdHhfncwfbfzJbnJzJfcczhhSmLCCNBjSpjmpjHjBQjpmpNW
BDvDPGRwRvCmLssGLmsL
frRjjlldrqtNspLWpqFcCmzm
ndSnVNtllldrdfSjfNvgVRHBwbbVMRbVPJgH
PpgjhpVLghPZhSgZVVzzcJWccPNCrcJzrFsJ
BdBNNMqMdfDnDNTFHHJCqHrJHzrFzF
wfMNtMndlBTlmTBndRpgghhjZRjvSZVjRw
ZQnQMWMcjHDHrWNF
TvtCvvBVgdRdmvBVNzDHlGFjFHjfRfDD
dvtCCbdJmhvhhhhbhVBPMwqZswnZqZjjMccsZJ
DDMzRBBSzRDTMQRZsbvssCbhZtCDtP
dLmwNplnmmwjGvPVCRtVVvVd
NNmjLmqWJjFRwFSrgcrSHBzcTz
TwTwTMBWcWBJJBtTWHddCmfgzlCzClsvmfsM
PPLDnNqPRLQNVnGNVsDQnNmzdhvdddlvdlqgqmdlrfvv
SQQsjPPLGLbDSnGLLNnWTFZJHbcpFctHZpwJWB
FzMltgtMzFpZtmzdjPpnvRTQTvRWTDfnnTlvwW
JcbVcBrqLCVJHJSNCcZVqVqqTRQRWWfNsTfTvDfsWvwTsnwv
rcZqVJVhmhgPmhmd
ttvSnlWvWWgcScMDsHHMPMjPmH
pzLGLfNRpJsvmmfvMDfs
GhpzRqqpZppNrhvFgwSlWnnBFn
sbQcDJQJJDbQhwchSctVnVnqTMvMWSqTMPSMlP
jtjCtNRLNCRgRnlTPPWg
pzpHdLtFNdJbDhJHsQhs
pSqnfqDnWPHNPCCHCp
GdJZQdgZbBvgQLcCZZCCZlPLRH
PzBgQggbvBthtMdMvbzvVfFfzTWqDmWDqzqWrfff
nnJdrfgfrdMCMdgrqMnWdgwNTTTzFhPSSHfSHhllzjzNFT
vBRvmvGZsLZZsHFNFFzTNPzb
LZVRmcDRvpQLmvvVGDGmpntJJwCWCnCPJwgJDrPDqM
QddMvdzlVfvdSQmGhmwLbGbmzbns
JtCCWqqZDsLpGhbGjD
FNrhqCTWMSRSrQQg
ZsBZJFsZSmmJsJSmrJrJrvsrdGdCQGQphMGwRMGQRGdbBChM
FlgfqNNNWnNnHfVnnHdbGwpwGWQhGdRMMdRM
LFnggHlDqDLvjDmZPcPmvP
CRHJWfvJvrQfrCsDlGGBszQBjjGB
LmPHVnMmpLlPssBPlDtd
MmMSZmVnncMFcmSVHvfSrffCwSvfbHWv
wsrJrpdJLsMCZDWL
BbLtGGbNmLQggqgQQtGgMmDCTnWZCZWZTmMmCZnT
qNBGNNgQcbbtGbbFBLVjfcfwHvrHHJHJcr
pCZCpdjBljhjBlpVccCpbDDwRWDsLhLbwDsDwsDw
HNgFSSNvSmdqwsFLFWLGttbw
gMMndNrzNHnzJZVlMCMCTcpc
CfsFNszCrrGzrsggsPfPVNVlqTdSjSqMTdSVTdLL
vRhcHllwJDmnJmDMMdhqSqpVMhdjdp
cvHRvwQBPZZlrQgz
TsFhCtQtQsBBLtBLPvgz
jjWZZjZSMNlNNjljNnlmjjfJLMBGGLvBdzPQpggJJLQzpg
wjbcmmlnQZmlrTsCFVwshwTr
nRGFnFjcdlwLSHSpNNnBfWHN
TgQvPbCMPRhbMPQvtQPvMCRBSHNQHBrQSNfWqpHHrWNWSf
PCgMbPvTZVDgtPRggtCCbgmmFJJLmcGFLjdmJFcDwJmm
dgWPssfdvQCLPLhL
pMtSMtpSmpMpFSMMFZjQCLbLQZZbVbVhNTLblZ
mpqcpzncfWwhzfRf
vntvVnRCsvpBpMjCpTpj
rQdZfhzczNzWcNLTpWgSvjjjpGpMSB
ZqNDQhfcNchLchQqcDqRHJtHVwnwbtvHsbVs
qtJGQgTrqtqQdQDgbGjPzZHWWzVjslPZlG
vBShwRRvvSRSvFvwLSvfcnfBWmHZHVWWHPzlNPWVWjZsWnWV
cLBFBFhCBLlwpFccFBFftqJDQdgdTDJJCbJgCCdg
wfmsPvPwNfvmfLNFvzzJbRMnllhlnLhRLC
gjtqDDTtjgpJcbnMTzCRnCCWhC
SDqtpGSStVtdqpgBVjBGZmFPJNJmffvfPsHZPZQd
HQMBBWrQQmPBvmBWnvrTnMSsbFfcfwgfCgscsmGgwgcJGg
NzzlJLthtlgswGFcwGst
JqNNRqpzhVRWTSQrrvSQ
mFpDZjvmtPPGvFjmmGTzTcFRbHczHTbzQgRS
fNdqhJsNrnnVNhwNVdrdsVczQCcwCMHSTCHgHCRzHgcM
JlgnNhsqVqNqNpPlvZvDDDGlZZ"""
