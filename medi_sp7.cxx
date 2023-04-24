/*                                SPECTRUM: C++
 * Copyright (C) Siemens AG 1992-2017 All Rights Reserved.Confidential
 * Message Display
 *
 * $NewRev
 * 171114 Mayh sp7-scada_v02.00_df_ems00091456_medi_background_update_vie
 * 170503 Mayh sp7-scada_v02.00_df_ems00085487_coeldeta_320infos_vie
 * 170110 Mayh sp7-scada_v02.00_df_ems00083561_medi_detacheck_vie
 * 160524 Mayh sp7-scada_v02.00_df_ems00074805_idads_crewmember_vie
 *   pVarDisplay: additional check for decision table types
 * 160330 Mayh sp7-scada_v02.00_df_ems00070990_mepr_nstatchg_vie
 *   cMaxSelChanEntries defined
 * 160223 Mayh sp7-scada_v02.00_df_ems00070945_idsd_minweeks_vie
 *   cppcheck correction
 * 151216 Mayh sp7-scada_v02.00_df_ems00068028_medi_delchan_vie
 * 151111 zeis sp7-any_v02.00_fu_ems00066953_rhel7: RHEL7 & ASAN fixes
 * 151009 Mayh sp7-scada_v02.00_df_ems00066875_medi_odvupdate_vie
 * 150826 gess ems00065348: correction of key for ActfileC
 * 150612 Mayh sp7-scada_v01.30_fu_ems00060139_linestat_v3_vie
 * 150505 gess sp7-ui_v01.30_df_ems00061766_medi_vie:
 *   Do not check for cMaxGrupp
 * 150212 tens sp7-scada_v01.20_df_ems00059067_lidi_vie
 * 150119 Mayh sp7-scada_v01.20_df_ems00058701_medi_vie
 *   endless loop in getUpdateVipShort corrected (in case of n x maxDataL
 *   variables in a display)
 * 141208 dfg sp7-dots_v01.20_df_ems00058027_dots_msp
 *            fixed check for dots configuration
 * 141117 mrn sp7-dots_v01.20_df_ems00056765_toev_msp
 *            added code to send req r_OTS_MEDI to dsots address and obj if medi
 *            runs in a D-OTS CCM server
 * 140925 z00018si ems00048503_rm_mip_vie: deleted mimic board relevant/dependent code
 * 140219 borko ems49164: removed JGCD config
 * 140122 Mayh sp7-scada_v01.10_df_ems00043559_medi_mph_corr01_vie
 * 131106 jane: fix compile error
 * 131024 Mayh sp7-scada_v01.10_fu_ems00033748_cuman_aor_corr05_vie
 *   131024 Mayh sp7-ui_v01.00_df_ems00043994_ots_medi_vie
 *     OTS: reset UI update admin in case of snapshot restore
 *   131014 Mayh sp7-ui_v01.00_df_ems00044673_medi_his_corr01_vie
 *   re-design of SHHIS update
 * 131007 Mayh sp7-scada_v01.10_fu_ems00033748_cuman_aor_corr04_vie
 *   131007 Mayh sp7-ui_v01.00_df_ems00044673_medi_his_vie:
 *     correction of flash status message to SHHIS
 * 130919 Mayh sp7-scada_v01.10_df_ems00043559_medi_mph_switching_vie
 *   supply phase attributs in case of switching (flashing)
 *   130207 palc sp7-scada_v01.00_df_ems00038810_vie
 *     added preset phase attributes in detAlt
 * 130910 jane: rename womanSPEC/womanODV to odvgate/odvserver
 * 130904 vkrishna : ems42509 - OLM change. Fix did 0 being sent to htopr.
 * 130820 gess: Process topo changes for dynamic registered variables
 * 130807 gess: DynVarProcVipPosition added.
 * 130619 ppoh: Adopting to tGID
 * 131024 Mayh sp7-ui_v01.00_df_ems00043994_ots_medi_vie
 *   OTS: reset UI update admin in case of snapshot restore
 * 131014 Mayh sp7-ui_v01.00_df_ems00044673_medi_his_corr01_vie
 *   re-design of SHHIS update
 * 131007 Mayh sp7-ui_v01.00_df_ems00044673_medi_his_vie:
 *   correction of flash status message to SHHIS
 * 130806 tens EMS41992 correct type for computer id
 * 130207 palc sp7-scada_v01.00_df_ems00038810_vie
 *   added preset phase attributes in detAlt
 * 130326 Mayh sp7-scada_v01.00_df_ems00038808_medi_timestamp_vie
 *   correct time stamp for htopr send
 * 121211 gess sp7-ui_v01.10_fu_ems00034622_medi_tne_interface_vie:
 *   introduce register/deregister of nonpersitent variables
 * 121226 xiao sp7-any_v01.10_fu_ems00033766_tne4study_vie
 *             use DispDes/InputVar instead of JGDispD/JGInputV;
 *             build actFile/meDisp for study case by outDisp
 * 121203 borko odb_fx_relations_adms_vie: use tSegId for segment id
 * 121023 Mayh sp7-scada_v01.00_fu_ems00000562_cuman_corr03_vie:
 *   correction in pRawInMeTopo (buffer overwriting corrected)
 * 120802 mayh sp7-scada_v01.00_fu_ems562_cuman_vie: register for console
 *   data update, don't get updates from idsd, use TeArActiA (Ack)
 *   additionally store TeArActiV (view)
 * 120619 palc bof-ems784_HIS_replay_medi: corr. in pVipHIS
 * 120531 Mayh bof-scada_v01.01_df_ems1384_medi_mph_corr_vie:
 *   for neSwitch there is no assym bit for topology defined, thus
 *   phase processing will always be done.
 * 120530 suet/mure bf-ui_43644_47 acknowledge to toev also on FaiOvCC
 * 120308 palc bof-ems791_OTS_medi : corr. in trainingcheck
 * 111222 palc bof-SCADA-TechArea: DETEAREA not used anymore
 * 120229 palc bof-ems784_HIS_replay_medi: takeover from Power3
 * 111222 palc bof-SCADA-TechArea: DETEAREA not used anymore
 * 111121 Team bof-any_v01.00_fu_ems500_base_111111_vie: P470 merge
 * 110913 slav bf-ui_41581_CoDeTaEl_256_47: Rule256
 * 110901 tens: BestOf OTS support ems162 trainingInit, trainingcheck,..
 * 110922 Mayh bof-scada_v01.00_fu_ems182_mph_step2_vie:
 *   multi phase extensions
 * 110801 allhu bof_mph_step2: multi phase extension
 * 110111 Team feat-P4onX86_basics_2nd: PE78 (Com/XId), chr8 names, fixes
 * 101010 Team feat-P4onX86_basics: X86/64bit/p2c clean up
 * 100315 zeis bf-T5000_upgrade_7th_46: clean up for full byte ComputId,
 *   switch MEDISP back to copy 0 (raise condition if DbcOpen failed)
 * 090904 Vojtech Kovacs: SPR Spec400041161 : integration project specific solution     VK01
 * 080714 Mayh bf-UI_40283:
 *   update to woman only once per computId if channel 0 (pVipChan)
 * 080222 rybar feat-P2CXX_ISOCPP_3rd: longjump -> exception handling
 * 080111 team feat-P2CXX_ISOCPP: p2c conversion
 * 070327 Mayh: bf-ui_38033_SigStayEventDBAChange_45: delete bStay
 *              entries for DId at DBA activation (disposebDId)
 * 061009 fich: bf-ui_supply_37278_45: do NIMORIG always in mainloop !
 *   (avoids NIM relation open for dismounted cases an wrong copy access)
 * 060727 Mayh: bf-ui_36186_medi_Info0_45: avoid core if Info = 0
 * 060301 Mayh: bf-ui_36036_MediPosit_45: check sb-Size at pActPos
 * 050915 dorr: spr 32739 corr in pDbcOpen
 * 050915 dorr: spr 34659;medi endless loop if DISPDES record missing at ack
 * 050707 sche spr-32117: CCAcknowledge: Transmit info type.
 * 050414 mani: feat-dms-spr-29800: cJGC_B1Name processing removed
 * 050207 dorr: spr 32978 core for chan>127
 * 040419 dorr: spr 32138 neg sign conters in seds
 * 040920 roman feat-P2C_prog_ui_spl1: P2C conversion
 * 040907 haim: spr-31039_final: Support of multiple JGC segments
 * 040616 mure feat-P2C_odb_syntax1: modify->DBAord_modify
 * 040419 dorr: define pRDispDes as function
 * 040324 dorr: spr 31762: set AcknBit in NIM
 * 040317 fich: spr 31734: use function "addr" instead of AddObjectIdP,
 *   because AddObjectIdP does not work for compiler mode "quick"
 *   outDisp record declared global to avoid compiler warnings
 * 040305 dorr: spr 31497: NoelTy in mixed DeTa's
 * 040205 Mayh: bf-uis_spr_31249: bf-cama_30310_corr_v2/v3 corrections
 * 040130 Mayh: bf-cama_30310_corr_v3 - corrections
 * 040126 fich/Mayh: bf-cama_30310_corr_v2
 * 031215 dorr: spr 30310: Jumpers, Cuts & Grounds in Study Mode
 * 031104 dorr: toev_opt:  RawInMeTopo
 * 031028 dorr: spr 30573: sizeof(ActFileC) corrected
 * 021217 dorr: spr 28873: err 2001 suppressed in case of jgcd changes
 * 021028 dorr: spr 28847: dId relelection for jDMSDId
 * 011010 dorr: spr 27178: vipShL durch maxDataL ersetzen
 * 010824 dorr: spr 26737: saveNextBDId in pUpdateCAF
 * 010821 dorr: spr 26697: acknowl.: test if achnInfo is valid for signal.
 * 010511 dorr: spr 25206: TopIfMedi error handling
 * 010117 Majoor protokolierung alarm-quitierung
 * 000929 dorr: spr 24597: ActFileC kein DbcOpen
 * 000807 dorr: spr 23542: ActFileC-Overflow
 * 991029 dorr: spr 21847: McPTran in pApiAck
 * 991005 dorr: spr 21811: corr NIMRTY in pApiAck
 * 990921 dorr: spr 21598: nil-pointer in pcheckAf (B123Medisp[iop].rp)
 * 990831 dorr: spr 21485: conChan -> ShortInt
 * 990831 dorr: spr 21399: pcheckAF: pointer to vMeDispL
 * 990823 dorr: spr 19962: check malloc
.* 990819 dorr: spr 20068: plausibility check meDisp
 * 990722 dorr: spr 21081; pRnewUpd (Fehlerkorrektur XTerminal)
 * 990422 kopp: spr 16737 / change priority position <=> destination flashing
 *              bf-bbchsel / nrecessary for selective busbar change
 * 981117 dorr: spr 19263
 * 981117 dorr: spr 19093
 * 981116 dorr: spr 19034, 19069
 * 981002 dorr: spr 18221, 18222, 18359, 18492, 18512
 * 981009 dorr: spr 18365
 * 981009 dorr: spr 18446
 * 980921 dorr: spr 18198
 * 980824 dorr: spr 14801
 * 980813 dorr: spr 17712
 * 980805 dorr: spr 15061, 16055, 17389, 17438
 * $NewRevEnd$
 *
 * $NewRev$
 * $NewRevEnd$
 */

#include <fstream>
#include <iostream>

#include <base/syxp2c_c.h>

#include <base/erx_c.h>
#include <base/sbx_c.h>
#include <base/dbxa_c.h>
#include <base/dbxfus_c.h>
#include <base/lixtra_c.h>
#include <base/syxot_c.h>
#include <base/syxtim_c.h>
#include <im_odb/erxdat_c.h>
#include <ui_supply/dix_c.h>
#include <ui_supply/uixcoi_c.h>
#include <ssi/sixud_c.h>
#include <baps/tex_c.h>
#include <baps/texnim_c.h>
#include <baps/textif_c.h>
#include <im_odb/dox_c.h>
#include <na/DEF/nac_c.h>
#include <ui_graph/DEF/ui_graphConst.h>
#include <ssi/DEF/sittext_c.h>
#include <baps/DEF/tettyde_c.h>
#include <ots/DEF/Ots_m.h>

#include <baps/SBO/teb_c.h>
#include <baps/SBO/tebrawinf_c.h>
#include <baps/SBO/tebtop_c.h>
#include <ui_supply/SBO/dibdi_c.h>
#include <ui_supply/SBO/dibme_c.h>
#include <ui_supply/SBO/dibacop_c.h>
#include <ui_supply/SBO/dibse_c.h>
#include <ui_graph/SBO/dibfulb_c.h>
#include <ssi/SBO/siback_c.h>
#include <im_odb/SBO/dobuser_c.h>

#include <ui_supply/REL/didme_c.h>
#include <baps/REL/tednim_c.h>
#include <baps/REL/tedmp_c.h>
#include <baps/REL/teddt_c.h>
#include <ssi/sixud_c.h>
#include "mediproto_c.h"

using namespace std;

//Labels
//exception raise

static const int source_version_p2c = 1;

//Constants

static const int Priority = 1;    //Softbus priority

static const int Slice = 0;       //Softbus slice
static const int version = 21;    //program version  MEDI

static const int cMaxChan = cMaxChannelPerServer;
/*=cMaxConsolePerMMI*
                                   cMaxChannelPerConsole+ MIP-Channel*/
static const int checkDispD = 1111;  //used by medi for internal check`s

static const int C_SET_ALT_VIPSH = 1;  //set ttVipShortP (set_aAltNorm_mph, set_aAltFl_mph)
static const int C_SET_ALT_DISPI = 2;  //set DispI       (set_aAltNorm_mph, set_aAltFl_mph)

//Types


union tReceive {
  //Receipt record for SB-jobs
  tSboHead      SboH;
  struct {
    ObjectIdB     Header1;
    tMediJob      Header2;
    tRelCopKey    RelCopKey;
    tAckPara      AckPara;
  } U1;
  toutDisp      outDisp;
  tRawInMe      rawInMe;
  tFlash        flash;
  tSingAck      singAck;
  tVipDispAck   vDispAck;
  tMeCoAck      meCoAck;
  tCheckAf      checkAf;
  tmePosit      mePosit;
  tStrtUpd      strtUpd;
  tDMSDId       DMSDId;
  tDMSAFGr      DMSAFGr;
  ttotalAck     vtotalAck;
  tmmiDId       mmiDId;
  tCTII         ApiAck;
  tDataMod      DmsDataP;
  tMeTrace      rTrace;
  tRawInMeTopo  rawInMeTopo;
  tMedispC      rMedispC;
  tUpdConsoleAccessData vUpdCoAcD;
  tSBMeVarRegister rMeVarRegister;
  char          IPCbuffer[cMaxLanData];
};

struct tDtPara {
  // Parameter for processing of
  // decision tables
  short         DtNr;             // Number of decision table
  tTA           TA;               // Technological address
  short         NrOkRu;           // Number of fulilled rules
  short         AcEx;             // Actions to be processed
};


struct tAreaMedi {
  //Selected displays in area
  tSegId        dId;              //Display identification
  tSegId        segId;            //*t*elim    Segment identification
  bool          segmented;
};

struct tSegmMedi {
  tSegId        dId;              //Remaining selected display segments
};

struct tRecsegm {
  //Segmented displays (f. one channel)
  short         nSeg;             //Number of picture segments
  tSegmMedi     segments[cMaxSeg-1];
};

struct trChanDir {
  //Channel directory (f. one channel)
  short         channel;          //Channel identification
  ComputId      Comput;           //computer for updating
  short         digr;             //Display group
  tSMod         sMod;             //Modification of display selection
  tRelCopKey    RelCopKey;        //Copy
  bool          ackPosit;         //Acknow. variable is positionable
  tAreaMedi     area[4];
  tRecsegm      segDisp;
};


typedef enum {
   doupdate
  ,noUpdate
  ,updateOk
  ,lastof_tmmiState = updateOk
} tmmiState_enum;
typedef uint8_t tmmiState;
//Update status of mmi

typedef CL_SETDEF(tActFileSet, maxDId);

//Numbers of updated displays

struct trMmiDir {
  //Channel directory (for one channel)
  ComputId      Comput;           //computer for updating
  tmmiState     mmiState;         //Update status of mmi
  short         nActivChan;       //number of activ channels
  trChanDir*    VChan[cMaxChan];
  tActFileSet   mmiUpdtSet;       //meVar dId`s, to be updated on mmi
  tActFileSet   mmiActFileSet;    //meVar dId`s, updated
  alltMeClS     mmiAMeClS;        //actual message classes
  alltTeArS     mmiATeArS;        //actual technological areas
};

struct _REC_tSelChan {
  //*t*const mit cMaxComput*cMaxChan
  /*Array with Comput,chan where
  dId/RelCopKey is selected*/
  ComputId      sComput;          //computer with dId/RelCopKey
  short         sChan;            //Channel  with dId/RelCopKey
  short         sArea;            //area     with dId/RelCopKey
};


static const int cMaxSelChanEntries = 140;
typedef _REC_tSelChan tSelChan[cMaxSelChanEntries];

typedef short tmaxVar;
typedef uint16_t tVarSet[vipShL/16+1];  //set of varables in one segment

struct tbTA {
  //TA entry in bFlash
  tTA           ta;               //TA of message with flashing
  bool          acknFlash;        //acknowledge flashing y/n
  bool          desFlash;         //desired state flashing y/n
  bool          posFlash;         //positioned  state flashing y/n
  ComputId      comp;             //computer ID
  short         conChan;          //System service channel
  short         siChan;           //channel with siFlash
  int           InfVal;           //Desired state of info
  struct tbTA*  nextBTA;          //next TA entry in bFlash
};

struct tbDispI {
  //dispI entry in bFlash
  tSegId        AFDispI;          //Index of variable in dispDes
  // TODO 2: [ppoh] check
  tGID          gid;			 //identifier
  short         fAltNorm;         //figure alternative; normal
  short         aAltNorm;         //attribute alternative; normal
  short         aAltNormA;        //attribute alternative; normal - phase A
  short         aAltNormB;        //attribute alternative; normal - phase B
  short         aAltNormC;        //attribute alternative; normal - phase C
  short         fAltFl;           //figure alternative; ackn+desFlash
  short         aAltFl;           //attribute alternative; ackn+desFlash
  short         aAltFlA;          //attribute alternative; ackn+desFlash (phase A)
  short         aAltFlB;          //attribute alternative; ackn+desFlash (phase B)
  short         aAltFlC;          //attribute alternative; ackn+desFlash (phase C)
  short         saveAGroup;       //for desFlash
  short         aAltPosFl;        //attribute alternative; posFlash
  int           iValNorm;         //integer value:normal
  float         rValNorm;         //real    value:normal
  int           iValDes;          //integer value:desState
  float         rValDes;          //real    value:desState
  struct tbDispI* nextBDispI;     //next dispIentry in bFlash
  tbTA*         nextBTA;          //next TA entry in bFlash
};

struct tbDId {
  tSegId        AFDId;            //Display identification
  tRelCopKey    RelCopKey;        //Copy
  struct tbDId* nextBDId;         //next dId entry in bFlash
  tbDispI*      nextBDispI;       //next dispI entry in bFlash
  uint16_t*     VarSet;           //set of variables to be updated
};

typedef ComputId tmmi;            //type for mmiSet
typedef SetOfComputer tmmiSet;    //type for mmiSet

struct tbVipSH {
  //VipShort entry in bVipSH
  tmmiSet       mmiSet;           //mmiSet
  struct tbVipSH* nextVipSH;      //pointer to next buffer entry
  tVipShortP    VipSH;            //VipShort
};

struct tPtB {
  //record with buffer pointers
  tbDId*        bSelect;          //pointer to buffer node
  tbDId*        precBDId;         //precursor dId entry in bFlash
  tbDispI*      precBDispI;       //precursor dispI entry in bFlash
  tbTA*         precBTA;          //precursor TA entry in bFlash
  tbDId*        nextBDId;         //next dId entry in bFlash
  tbDispI*      nextBDispI;       //next dispI entry in bFlash
  tbTA*         nextBTA;          //next TA entry in bFlash
};

union tB123 {
  struct {
    //Search Key of B1 B2 B3
    short         B1;
    short         B2;
    short         B3;
    bool          MdspChg;        //meDisp record has been changed
  } U1;
  struct {
    int           B1B2;
    short         d1;
    bool          d2;
  } U0;
};


struct tB123Rec {
  tB123         B123Key;          //Search Key of B1 B2 B3
  struct tB123Rec* nextB123DId;   //pointer to next tB123Rec record
};

struct tchangeAF {
  tSegId        AFDId;            //Display identification
  tSegId        AFDispI;          //Index of variable in dispDes
  tRelCopKey    RelCopKey;        //Copy
  tSecs         timeSec;          //Time - milliseconds (for htopr)
  tMilS         timemSec;         //Time - seconds (for htopr)
};

/* multi phase info of a norm element */
struct tMultiPhaseInfo {
  tInfo     NetwStat;             // network status info
  tInfo     NetwGrp;              // network group info
  tInfo     NeStPhA;              // network status info Phase A
  tInfo     NeGrPhA;              // network group info  Phase A
  tInfo     NeStPhB;              // network status info Phase B
  tInfo     NeGrPhB;              // network group info  Phase B
  tInfo     NeStPhC;              // network status info Phase C
  tInfo     NeGrPhC;              // network group info  Phase C
  tInfo     NeStAssym;            // network status info assymetric
  tInfo     NeGrAssym;            // network group info  assymetric
};
typedef tMultiPhaseInfo   tNoElMultiPhaseInfo[neDim+1];

/* multi phase processing of one element */
struct tDoMPHproc {
  tSwPhase  swPh;                 // element type phase info
  bool      deTaNetwGr;           // NetwGrNo is combined in deta
  bool      detaNetwSt;           // NetwStat is combined in deta
  bool      doNGr_A;              // network group A
  bool      doNGr_B;              // network group B
  bool      doNGr_C;              // network group C
  bool      doNSt_A;              // network status A
  bool      doNSt_B;              // network status B
  bool      doNSt_C;              // network status C
};

typedef CL_SETDEF(tCoDetaS,cDTKeyMax);
/*set of 1..cDTKeyMax             Set for mph relevant CoDetaEl       */

// open line stat infos for NoEl neTopElem
struct tOpenLineStatInfo {
  tInfo     LineStat;             // open line stat main info
  tInfo     LineStatA;            // open line stat main info Phase A
  tInfo     LineStatB;            // open line stat main info Phase B
  tInfo     LineStatC;            // open line stat main info Phase C
};

//Variables

struct _REC_B123Medisp {
  tB123         B123;             //search key B1,B2,B3
  meDisp*       rp;
};
struct _REC_bTA {
  //Buffer for optimization of beBau
  tTA           TA;
  NormElem      NoElTy;
  tInterStr     InfStr;
  tinfoSet      infoSet;
};
struct tDeTaEntry {
  bool          ElDeTa;           //Pointer to DT`s of job
  union {
    CoDeTaEl*     El;
    DeTa*         TY;
  } UU;
  inline bool notEl() {           //not a DtEl data
    if (!ElDeTa) return true;
    if (UU.El == NULL) return true;
    return false;
  }
  inline bool notTy() {           //not a DtTy data
    if (ElDeTa) return true;
    if (UU.TY == NULL) return true;
    return false;
  }
};
struct _REC_varList {
  tB1           B1;               //Bereichsnummer B1
  tB2           B2;               //Bereichsnummer B2
  tB3           B3;               //Bereichsnummer B3
  tVarList      V;
};
struct _REC_bTAcrmd {
  //Zwischenpuffer fuer ta`s
  tTA           TA;
  bool          bDSG;
  bool          bAckP;
  tiinfoSet     bInfoSet;
};
short         i;
bool          markbcaf;           //marker for pUpdateD
tSegId        bDispI;             //key3: Index of variable in dispDes
bool          varFound;           //Search indicator
tSegId        newDId;
bool          writeDispdes;       //Entry in dispdes
bool          writeActFileC;      //Entry in actFileC
bool          relOpen;            //database relations opened
bool          firstSoftInit;      //waiting for first SoftInit
bool          meSoftInit;         //parameter for pWrActFile
bool          corrSoftInit;       //parameter for pWrActFile
bool          createNew;          //parameter for pWrActFile
bool          initialUpdate;      //true until run-up pass is done
bool          isRTSBrunup;        //true during SB runup
int           compElem;           //temporary compare for Element
int           IRDispDO;           //counter dbRead DispDes
int           IMeVarO;            //counter var RawInMe
int           NMediGenLcheck;     //counter meDispGen(false) in
//pMeSoftInit

//SoftBus
toutDisp      SoutDisp;
tbVipSH*      tempbVipSHLoc;      //work around compiler warning

int           NB123Medisp;        //counter of entries in B123Medisp
_REC_B123Medisp B123Medisp[cNB123Medisp+1];
bool          B123MdspChgOK;      //no meDisp record changed
meDisp*       pmeDisp;            //Pointer to meDisp record in Heap
bool          checkBFl;           //check of bFlash is active
bool          PCMedi;             //medi on PC: Updating the mmi`s
bool          PCNormstatus;       //medi on PC
tRelCopKey    lastRelCopKey;      //Copy number of last job
tRelCopKey    current_RelCopKey;  //Current number of job

tSegId        dId;                //Display identification
tSegId        segId;              //*t*elim    Segment identification
struct tvdispDes  {
  //Display description:
  //Dummy-buffer for dbread
  tSegId        dId;
}             vdispDes;
struct trActFileC {
  //Copy of updating file:
  //Dummy-buffer for dbread
  tSegId        dId;
  tRelCopKey    RelCopKey;
}             rActFileC;
struct trInputVar {
  //Variable description for input:
  //Dummy-buffer for dbread
  tSegId        dId;
}             rInputVar;
tVipInVar     rinVar;             //Input record of one variable
NIMAD         rNimad;             //Technological oriented service data

int           varInd;             //Index in medisp
tVipShortP*   pVipShort;          //Pointer to actfile
tMeOut*       pMeOut;             //Pointer to variable description
tMeVar*       pmeVar;             //Pointer to description of one var.
ByteInt*      pInfoStr;           //Pointer to info string in NIM
tinfoS        infoS;              //Info number compressed to one byte

tVipHIS       bVipHIS;            //Buffer for HTOPR / r_TOPO_data_change
tDIdHIS       bDIdHIS;            //Buffer for HTOPR / r_TOPO_display_change

tVipShortP    bVipShort;          //Buffer for VipShort
tVipFunct     bVipFunct;          //Buffer for VipFunct
ActSign       bActSign;           //Buffer for ActSign
tLiUAckNow    bLiUAcknow;         //Buffer for common acknowledgement
tchangeAF     bChangeAF[bCAFMax];
//Buffer for changes of selected AFs
short         nbChangeAF;         //Number of entries in bChangeAF
tchangeAF*    pChangeAF;          //buffer for changed variables
tB123Rec*     pDIdTbl[maxDId];
//DId Table: pointer to tB123Rec

void*         addrVipShort;
tTA           TA;                 //TA for NimRMp
short         nbTA;               //Number of entries in bTA
_REC_bTA       bTA[Condmax];
int           sbF;                //Flag for sb procedures
int           sbFRec;             //Flag for sbReceive
int           nimF;               //Flag for nimOrig
bool          diTransmOk;         //sbTransmit to workstation ok
int           Flag;               //For coDtPrEl
bool          RunOK;              //Notification to Softbus is ok.
short         AcEx;               //Flag pDeTaEl: Alternative
tDeTaEntry    DeTaPointer[cDTKeyMax];
tDtPara       DtPara;             //Parameter for processing DT
int           iRVarDes;           //Reading index in variable descript.
int           iWVipShort;         //Writing index in VipShort
int           iRawInMe;           //Reading index in RawInMe
int           arInd;              //Area index
short         chan;               //Channel
SbAddressS    diAddr;             //Softbus address on data receipt
tReceive      receive;            //Receipt record for SB in MEDI
int           DataSize;           //Data length of softbus job (bytes)
size_t        lVipS;              //Length of vipShort
tNimVar       NimVar;             //IO-parameter variable
tNimVarMp     NimVarMp;           //IO-parameter variable
tNimVarTy     NimVarTy;           //IO-parameter variable

tSboHead      OTS_AckSb;          //Ack. to OTS when load case is done
tSboHeadAck   TOEV_AckSb;         //askt TOEV to recalculate network
tFlash        saveFlash;
tAckPara      saveAckpara;
tDispAck      saveDispAck;
tVipDispAck   saveVipDispAck;
int           iVipDispAck;
tMeCoAck      saveMeCoAck;
ttVipShortP*  pttVipShort;
int           InfVal;             //Value of Info
tLength       Leng;               //Length of info read out of NIM
bool          changeFlash;
//*t* elim
//change of status bit is set
tbDId*        bFlash;             //Pointer to dId entry in bFlash
tbDId*        bCAF;               //Pointer to dId entry in bCAF
tbDId*        bStay;              //Pointer to dId entry in bStay
tbDId*        nextBDId;           //next dId entry in bFlash
tbDispI*      nextBDispI;         //next dispI entry in bFlash,bCAF,bStay
tbTA*         nextBTA;            //next TA entry in bFlash
tbTA          bFTA;               //TA entry for insertion into bFlash
int           nCAF;               //number of inserts into bCAF
int           nBVipSH;            //number of vipShort records in BVipSH
tbTA          bSTA;               //TA entry for insertion into bStay
tbVipSH*      bVipSH;             //VipShort entry in bVipSH
tB123Rec*     nextB123DId;        //Pointer to dId entry in pDIdTbl[]
bool          corrSSStart;        //Start of corrSpecialState
bool          makeDesState;       //Destination state flashing
tbVipSH*      bVipSHLast;         //last VipShort entry in bVipSH
tbVipSH*      newbVipSH;          //new VipShort entry in bVipSH
tmmiSet       mmiSet;             //set of mmis 1..cMaxComput
tPtB          PtB;                //record with buffer pointers

tMeClS        fullMmiAMeClS;      //set with all message classes
tTeArS        fullMmiATeArS;      //set with all technological areas

trMmiDir*     mmiDir[cMaxComput]; //array of mmi`s
trMmiDir*     restoreMMI[cMaxComput];

/**t*cMaxComput vgl.
  $SPECPATH/I/def/computid.h
  derzeit 27, max 127*/
tActFileSet   ActFileSet;         //meVar dId`s
tActFileSet   okActFileSet;       //meVar dId`s, updated
tActFileSet   AllDidSet;          //all dId`s for B1-B3-combination
tActFileSet   AllDidSetSave;      //all dId`s for B1-B3-combination
tActFileSet   ReSelDidSet;        //all dId`s for ReSelection (DMS)
tActFileSet   HISActFileSet;      //meVar dId`s, updated to SHHIS (correlate with ActFileSet)
bool          endOfJob;           //job is ended because of error
SbClock       ClockId;            //Clock-identification
tSyTim        Rastint;            //Grid interval
tSegId        dIdCheck;           //Last checked dId
tCheckAf      rCheckAF;           //Check actfile
SvcAck        SvcA;               //Acknowledgement to PSM
int           lSvcA;              //Length of SvcA
int           sOldRawInMe;        //Old value of statistic counter
ComputId      MyComputId;         //Own Computer Identification
tOpMode       MyOpMode;           //Own opmode
ComputId      PaComputId;         //Partner Computer Identification
sProMode      GrMode;             //Group tag positioning flags
sProMode      SelMode;            //Preselect positioning flags

dispDes*      pGDispDesRpP;
ComputId      ots_computer;       //Ots server Id

//Database file variables
tfDispDes     fDispDes;           //DispDes:       DB-file variable
tfActFileC    fActFileC;          //ActFileC:      DB-file variable
tfInputVarP   fInputVar;          //InputVarP       DB-file variable
tfmeDisp      fmeDisp;            //Medisp:        DB-file variable
tfCoDeTaEl    fCoDeTaEl;          //CoDeTaEl:      DB-file variable
tfDeTa        fDeTa;              //DeTa:          DB-file variable
tfnimad       fNimad;             //Nimad:         DB-file variable
tRlInfo       fCoDeTaElInfo;      //CoDeTaEl:      DB-Info variable
tRlInfo       fDeTaInfo;          //DeTa:          DB Info variable

CoDeTaEl      vCoDeTaElL;         //Decicion table elementar
DeTa          vDeTaL;             //Decicion table types

//Variables for creation of meDisp
meDisp        vMeDispL;           //List message-display
int           iMax;               //Index Max. in bMedisp
int           nVarOld;
int           iRMeDisp;           //Leseindex in Medisp
int           iWMeDisp;           //Schreibindex in Medisp
int           meVarInd;

struct tbMeDisp {
  //Zwischenpuffer
  short         nVar;             //Anzahl Variablen in Variablenliste
  _REC_varList   varList[cMaxMeDisp];
}             bMeDisp;

int           nB123;              //Anzahl Eintr.in bMedisp mit B1,B2,B3

_REC_bTAcrmd   bTAcrmd[Condmax];
short         DtNr;               //Entscheidungstabellennummer

tNoElMultiPhaseInfo NoElMPhInfo;  //norm element multi phase info
tCoDetaS  NetwStatCoDetaElS;      //CoDetaEl used for netwstat comb.
tCoDetaS  NetwGrpCoDetaElS;       //CoDetaEl used for netwgrp  comb.

typedef CL_SETDEF(tCoDetaS,cDTKeyMax);
/*set of 1..cDTKeyMax             Set for mph relevant CoDetaEl       */


bool IsMultiPhaseYes = false;     // multi phase switch

tOpenLineStatInfo OpenLineStatInfo;  // nede infos open line status

tSecs         gActSec;              //seconds
tMilS         gActMSec;             //milliseconds

FILE*         TestOut;
bool          medi_log1;
int           LTestOut;
tMeTrace      rTrace;             //trace selector
tSyTim        SyTim;              //system time
String40      rDATUM;
String20      dateTime;

//package UIPCOI
tIFCvar       UIcIntVar;          //interface variable

tSIuStruct*   gSIuHandle;         //SIPUD - API for User Data
static
void          pRRel();
static
void          pNextVar();
static
void          pDelChanDir();
static
void          pInfoType();
static
bool          pRDispDesC();
static
void          pWrActFile();
static
void          pFRel();
static
void          meDispGen(
  bool          insert);
static
bool          pSendSeds();
static
void          pWBChangeAf(
  tSegId        idId,
  tSegId        DispI,
  tRelCopKey    aRelCopKey,
  tSecs         iSec = gActSec,
  tMilS         iMSec = gActMSec);
static
void          pUpdateD();
static
void          sendBVipSH(
  tSegId        idId,
  tmmi          mmi);
static
void          delCAF(
  tbDId*        delBDId);
static
bool          findBDId(
  tSegId        bDId,
  tRelCopKey    bRelCopKey,
  tbDId*        bSelect);
static
void          delBVipSH();
static
bool          findB123DId(
  tB123         SearchB123,
  tSegId        DIdL);
static
void          prepB123DId(
  tB123         SearchB123,
  tSegId        DIdL);
static
bool          findB123MeDisp(
  tB123*        keyB123L,
  int*          iB123);
static
void pVipHIS
(        tVipShortP*  ip_vipShort
,  const bool         iCreate_bVipHIS
,  const bool         iSet_bVIPHIS_Time);
static
void pSndVipShort
(        tVipShortP*   ipVipSH
,  const SbAddressS    iAddress
,  const ObjectIdS     iObject
,  const ComputId      iComput
,  const short         iChan
,  const bool          iErrJmp);
static
tVipShortP * getUpdateVipShort
(        int          &iIdx);

//Functions
jmp_buf       _JL99;

static
void*         malloc_chk(
  int           Number)
{
//malloc is checked for nil-pointer

void*         dummyPointer;

dummyPointer = malloc(Number);
if (dummyPointer==0) {
  err(0,4101,"malloc  ",0.0,0.0,0.0,"BBB");
  endOfJob = true;
  longjmp(_JL99,1);               //end of job
  }

return dummyPointer;
}


//IntDifNulSet:=(ActFileSet*mmiUpdtSet)-mmiActFileSet=[]
static
bool          IntDifNulSet(
  tActFileSet   ActFileSet,
  tActFileSet   mmiUpdtSet,
  tActFileSet   mmiActFileSet)
{
tActFileSet   TmpSet;

IntSet(TmpSet,ActFileSet,mmiUpdtSet,tActFileSet);
return DifNulSet(TmpSet,mmiActFileSet,tActFileSet);
}

// ---------------------------------------------------------------------
// set aAltNorm for main and multiphase info
//
void set_aAltNorm_mph
(       ttVipShortP   *ip_ttVipShP    // I : pointer to ttVipShort
,       tbDispI       *ip_tbDispI     // I : pointer to tbDispI
, const int            iOpt           // I : C_SET_ALT_VIPSH, C_SET_ALT_DISPI
) {
  if (iOpt == C_SET_ALT_VIPSH) {
    ip_ttVipShP->aAlt = ip_tbDispI->aAltNorm;
    ip_ttVipShP->uvipA.svALG.aAltPhaseA = ip_tbDispI->aAltNormA;
    ip_ttVipShP->uvipA.svALG.aAltPhaseB = ip_tbDispI->aAltNormB;
    ip_ttVipShP->uvipA.svALG.aAltPhaseC = ip_tbDispI->aAltNormC;
  } else {
    ip_tbDispI->aAltNorm  = ip_ttVipShP->aAlt;
    ip_tbDispI->aAltNormA = ip_ttVipShP->uvipA.svALG.aAltPhaseA;
    ip_tbDispI->aAltNormB = ip_ttVipShP->uvipA.svALG.aAltPhaseB;
    ip_tbDispI->aAltNormC = ip_ttVipShP->uvipA.svALG.aAltPhaseC;
  }
  if (SosyData.sdDEBUGlevel == 49)
    errfmt(0,10,"s_aAltNo","GID - aAltNorm/aAltNormA/B/C - %d - %d/%d/%d/%d"
               ,ip_ttVipShP->gid,ip_ttVipShP->aAlt,ip_ttVipShP->uvipA.svALG.aAltPhaseA
               ,ip_ttVipShP->uvipA.svALG.aAltPhaseB,ip_ttVipShP->uvipA.svALG.aAltPhaseC);
} // set_aAltNorm_mph
// ---------------------------------------------------------------------
// set aAltFl for main and multiphase info
//
void set_aAltFl_mph
(       ttVipShortP   *ip_ttVipShP    // I : pointer to ttVipShort
,       tbDispI       *ip_tbDispI     // I : pointer to tbDispI
, const int            iOpt           // I : C_SET_ALT_VIPSH, C_SET_ALT_DISPI
) {
  if (iOpt == C_SET_ALT_VIPSH) {
    ip_ttVipShP->aAlt = ip_tbDispI->aAltFl;
    ip_ttVipShP->uvipA.svALG.aAltPhaseA = ip_tbDispI->aAltFlA;
    ip_ttVipShP->uvipA.svALG.aAltPhaseB = ip_tbDispI->aAltFlB;
    ip_ttVipShP->uvipA.svALG.aAltPhaseC = ip_tbDispI->aAltFlC;
  } else {
    ip_tbDispI->aAltFl  = ip_ttVipShP->aAlt;
    ip_tbDispI->aAltFlA = ip_ttVipShP->uvipA.svALG.aAltPhaseA;
    ip_tbDispI->aAltFlB = ip_ttVipShP->uvipA.svALG.aAltPhaseB;
    ip_tbDispI->aAltFlC = ip_ttVipShP->uvipA.svALG.aAltPhaseC;
  }
  if (SosyData.sdDEBUGlevel == 49)
    errfmt(0,10,"s_aAltFl","GID - aAltFl/aAltFlA/B/C - %d - %d/%d/%d/%d"
               ,ip_ttVipShP->gid,ip_ttVipShP->aAlt,ip_ttVipShP->uvipA.svALG.aAltPhaseA
               ,ip_ttVipShP->uvipA.svALG.aAltPhaseB,ip_ttVipShP->uvipA.svALG.aAltPhaseC);

} // set_aAltFl_mph
// ---------------------------------------------------------------------
static
void          wrOutDisp()
{
//write outdisp into trace file
int           i;
int           FORLIM;
if (!rTrace.wrOutDisp)
  return;
GetSyTim(&SyTim);
TimCoStr(&SyTim,rDATUM,dateTime,20);
fprintf(TestOut,"******* %.20s outDisp: ",dateTime);
fprintf(TestOut,
  "Comput=%-1d RelCopKey=%-1d chan=%-1d area=%-1d dId=%-1d digr=%-1d CondSel=%s newDisp=",
  receive.outDisp.AckPara.Comput,receive.outDisp.RelCopKey,
  receive.outDisp.chan,receive.outDisp.area,receive.outDisp.dId,
  receive.outDisp.digr,receive.outDisp.CondSel?"true":"false");
if (receive.outDisp.newDisp==true)
  fprintf(TestOut,"t");
else
  fprintf(TestOut,"f");
fprintf(TestOut," dSeg=");
FORLIM = receive.outDisp.dSeg[0]+1;
for (i = 2; i<=FORLIM; ++i)
  fprintf(TestOut,"%-1d ",receive.outDisp.dSeg[_AC(i-1,cMaxSeg+1)]);
fprintf(TestOut,"\n");
++LTestOut;
fflush(TestOut);
} // wrOutDisp
// ---------------------------------------------------------------------
// write RawInMe,RawInMeTopo infos
//
static void wrRawInMeTA
( tTA5*       ipTA                // I : TA pointer
) {

  int           i;
  int           j;
  String2       TEMP;
  NormElem      lNoEl;
  tLength       lLen;
  int           lInf;
  tInfo         varInfo;          //Info number out of variable description

  TA.B1 = ipTA->B1;
  TA.B2 = ipTA->B2;
  TA.B3 = ipTA->B3;
  TA.Elem = ipTA->Elem;
  TA.Info = ipTA->Info;
  varInfo = ipTA->Info;
  TA.Info = 0;
  if (!NIMRMP(&NimVarMp.FlagS,nTeAd,receive.U1.RelCopKey,0)) {
    fprintf(TestOut,"  error nimrp, Flag = %10d\n",NimVarMp.FlagS);
    ++LTestOut;
  } else {
  lNoEl = NimVarMp.NoElTy;
  j = NimVarMp.StrLeng[_AC(lNoEl,neDim)]-1;
    fprintf(TestOut," Nim : ");
    for (i = 0; i<=j; ++i)
      fprintf(TestOut,"%.2s ",
        Int2Hex_C(TEMP,NimVarMp.pInfStr->ByteStr[_AC(i,InfoStrMax-1)]));
  fprintf(TestOut,"\n");

  /* multiphase infos */
  if (IsMultiPhaseYes)
    if ((NoElMPhInfo[_AC(lNoEl,neDim)].NetwStat > 0) ||
        (NoElMPhInfo[lNoEl].NetwGrp  > 0)) {
      fprintf(TestOut,"      .. Multiphase: ");

      if (NoElMPhInfo[lNoEl].NetwStat > 0) {
        GETINFO(NoElMPhInfo[lNoEl].NetwStat,NimVarMp.NedePoi[lNoEl]
               ,(tInterStr*)NimVarMp.pInfStr
               ,&lInf,&lLen);
        fprintf(TestOut,"NetwStat: %d ",lInf);
        }
      if (NoElMPhInfo[lNoEl].NetwGrp > 0) {
        GETINFO(NoElMPhInfo[lNoEl].NetwGrp,NimVarMp.NedePoi[lNoEl]
               ,(tInterStr*)NimVarMp.pInfStr
               ,&lInf,&lLen);
        fprintf(TestOut,"NetwGrp: %d ",lInf);
        }
      if (NoElMPhInfo[lNoEl].NeStPhA > 0) {
        GETINFO(NoElMPhInfo[lNoEl].NeStPhA,NimVarMp.NedePoi[lNoEl]
               ,(tInterStr*)NimVarMp.pInfStr
               ,&lInf,&lLen);
        fprintf(TestOut,"NetwStatA : %d ",lInf);
        }
      if (NoElMPhInfo[lNoEl].NeGrPhA > 0) {
        GETINFO(NoElMPhInfo[lNoEl].NeGrPhA,NimVarMp.NedePoi[lNoEl]
               ,(tInterStr*)NimVarMp.pInfStr
               ,&lInf,&lLen);
        fprintf(TestOut,"NetwGrpA : %d ",lInf);
        }
      if (NoElMPhInfo[lNoEl].NeStPhB > 0) {
        GETINFO(NoElMPhInfo[lNoEl].NeStPhB,NimVarMp.NedePoi[lNoEl]
               ,(tInterStr*)NimVarMp.pInfStr
               ,&lInf,&lLen);
        fprintf(TestOut,"NetwStatB : %d ",lInf);
        }
      if (NoElMPhInfo[lNoEl].NeGrPhB > 0) {
        GETINFO(NoElMPhInfo[lNoEl].NeGrPhB,NimVarMp.NedePoi[lNoEl]
               ,(tInterStr*)NimVarMp.pInfStr
               ,&lInf,&lLen);
        fprintf(TestOut,"NetwGrpB : %d ",lInf);
        }
      if (NoElMPhInfo[lNoEl].NeStPhC > 0) {
        GETINFO(NoElMPhInfo[lNoEl].NeStPhC,NimVarMp.NedePoi[lNoEl]
               ,(tInterStr*)NimVarMp.pInfStr
               ,&lInf,&lLen);
        fprintf(TestOut,"NetwStatC : %d ",lInf);
        }
      if (NoElMPhInfo[lNoEl].NeGrPhC > 0) {
        GETINFO(NoElMPhInfo[lNoEl].NeGrPhC,NimVarMp.NedePoi[lNoEl]
               ,(tInterStr*)NimVarMp.pInfStr
               ,&lInf,&lLen);
        fprintf(TestOut,"NetwGrpC : %d ",lInf);
        }
      fprintf(TestOut,"\n");
      }
    }

    fflush(TestOut);
} // wrRawInMeTA
// ---------------------------------------------------------------------
static
void          wrRawInMe()
{
//write RawInMe into trace file

if (!rTrace.wrRawInMe) return;

GetSyTim(&SyTim);
TimCoStr(&SyTim,rDATUM,dateTime,20);
fprintf(TestOut,"******* %.20s RawInMe: nRIMe=%-1d",
        dateTime,receive.rawInMe.nRawInMe);
fprintf(TestOut,"  Sending Obj= %-1d\n",SosyData.sdSBRobj);
++LTestOut;
for (int iRI = 1; iRI<=receive.rawInMe.nRawInMe; ++iRI) {
  fprintf(TestOut,
    "  TA:/%-1d/%-1d/%-1d/%-1d/%-1d statinfo=%-1d MeCl=%-1d InTy=%-1d NoElTy=%-1d deSiFl=%-1d InfStr:",
    receive.rawInMe.Data[_AC(iRI-1,cMaxMess-1)].TA.B1,
    receive.rawInMe.Data[iRI-1].TA.B2,receive.rawInMe.Data[iRI-1].TA.B3,
    receive.rawInMe.Data[iRI-1].TA.Elem,receive.rawInMe.Data[iRI-1].TA.Info,
    (int)receive.rawInMe.Data[iRI-1].statinfo,
    receive.rawInMe.Data[iRI-1].meClass,receive.rawInMe.Data[iRI-1].InfoType,
    receive.rawInMe.Data[iRI-1].NoElTy,
    receive.rawInMe.Data[iRI-1].delSiFlash);

  if (rTrace.bitcmp && receive.U1.Header2==jrawInMe)
    wrRawInMeTA(&receive.rawInMe.Data[_AC(iRI-1,cMaxMess-1)].TA);
  else
  fprintf(TestOut,"\n");
  ++LTestOut;
  }  //for iRI...

fflush(TestOut);
}  //wrRawInMe
// ---------------------------------------------------------------------
// write RawInMeTopo into trace file
//
static void wrRawInMeTopo()
{

if (!rTrace.wrRawInMe) return;

GetSyTim(&SyTim);
TimCoStr(&SyTim,rDATUM,dateTime,20);
fprintf(TestOut,"******* %.20s RawInMeTopo: nRIMe=%-1d",
        dateTime,receive.rawInMeTopo.nRawInMe);
fprintf(TestOut,"  Sending Obj= %-1d\n",SosyData.sdSBRobj);
++LTestOut;
for (int i = 1; i<=receive.rawInMeTopo.nRawInMe; i++) {
  fprintf(TestOut,"  TA:/%-1d/%-1d/%-1d/%-1d/%-1d InfStr:"
         ,receive.rawInMeTopo.Data[_AC(i-1,cMaxMessTopo-1)].TA.B1
         ,receive.rawInMeTopo.Data[i-1].TA.B2
         ,receive.rawInMeTopo.Data[i-1].TA.B3
         ,receive.rawInMeTopo.Data[i-1].TA.Elem
         ,receive.rawInMeTopo.Data[i-1].TA.Info);

  if (rTrace.bitcmp && receive.U1.Header2==jrawInMeTopo)
    wrRawInMeTA(&receive.rawInMeTopo.Data[i-1].TA);
  else
    fprintf(TestOut,"\n");
  ++LTestOut;
}  //for i...

fflush(TestOut);
} // wrRawInMeTopo
// ---------------------------------------------------------------------
// write vipshort to logfile
//
static
void          wrVipShort(
  const char   *iMethod,      // I  calling methode
  tVipShortP*   VipShort,
  int           Computer)
{
int           i;
char          sBuf1[80];
char          sBuf2[80];

if (!rTrace.wrVipShort) return;

bool tr2err = (SosyData.sdDEBUGlevel == 9998);

// due to big data amount trace all dId, if rTrace.dId == 0
// or trace just rTrace.dId
if ((rTrace.dId != 0) && (rTrace.dId != VipShort->dId)) return;

GetSyTim(&SyTim);
TimCoStr(&SyTim,rDATUM,dateTime,20);
fprintf(TestOut,"\n******* %s vipShort - %s:",dateTime,iMethod);
//with VipShort..
fprintf(TestOut,
  " Comput=%-1d RelCopKey=%-1d Chan=%-1d Area=%-1d dId=%-1d dataL=%-1d\n",
  Computer,VipShort->RelCopKey,VipShort->chan,VipShort->area,VipShort->dId,
  VipShort->dataL);
for (i = 1; i<=VipShort->dataL; ++i) {
  sprintf(sBuf1,"gid=%8d aAlt=%-1d aGroup=%-1d Mark=%-1d",
          VipShort->Data[_AC(i-1,vipShL-1)].gid,
          VipShort->Data[i-1].aAlt,VipShort->Data[i-1].aGroup,
          VipShort->Data[i-1].Mark);


  if (VipShort->Data[i-1].vipAct==vipActLFig)
    sprintf(sBuf2,"lFig fAlt=%-1d fGroup=%-1d aAltOld,PhA,B,C %-1d,%-1d,%-1d,%-1d - LineSt/A/B/C: %-1d,%-1d,%-1d,%-1d",
            VipShort->Data[_AC(i-1,vipShL-1)].uvipA.svALG.fAlt,
            VipShort->Data[i-1].uvipA.svALG.fGroup,
            VipShort->Data[i-1].uvipA.svALG.aAltOld,
            VipShort->Data[i-1].uvipA.svALG.aAltPhaseA,
            VipShort->Data[i-1].uvipA.svALG.aAltPhaseB,
            VipShort->Data[i-1].uvipA.svALG.aAltPhaseC,
            VipShort->Data[i-1].uvipA.svALG.semiOpenCombi,
            VipShort->Data[i-1].uvipA.svALG.semiOpenPhase[0],
            VipShort->Data[i-1].uvipA.svALG.semiOpenPhase[1],
            VipShort->Data[i-1].uvipA.svALG.semiOpenPhase[2]);

  if (VipShort->Data[i-1].vipAct==vipActGFig)
    sprintf(sBuf2,"gFig fAlt=%-1d fGroup=%-1d aAltOld,PhA,B,C %-1d,%-1d,%-1d,%-1d - LineSt/A/B/C: %-1d,%-1d,%-1d,%-1d",
            VipShort->Data[_AC(i-1,vipShL-1)].uvipA.svALG.fAlt,
            VipShort->Data[i-1].uvipA.svALG.fGroup,
            VipShort->Data[i-1].uvipA.svALG.aAltOld,
            VipShort->Data[i-1].uvipA.svALG.aAltPhaseA,
            VipShort->Data[i-1].uvipA.svALG.aAltPhaseB,
            VipShort->Data[i-1].uvipA.svALG.aAltPhaseC,
            VipShort->Data[i-1].uvipA.svALG.semiOpenCombi,
            VipShort->Data[i-1].uvipA.svALG.semiOpenPhase[0],
            VipShort->Data[i-1].uvipA.svALG.semiOpenPhase[1],
            VipShort->Data[i-1].uvipA.svALG.semiOpenPhase[2]);

  if (VipShort->Data[i-1].vipAct==vipActINteger)
    sprintf(sBuf2,"Integer iOption=%-1d iNumSy=%-1d iValue=%-1d",
            VipShort->Data[_AC(i-1,vipShL-1)].uvipA.svAI.iOption,
            VipShort->Data[i-1].uvipA.svAI.iNumSy,
            VipShort->Data[i-1].uvipA.svAI.iValue);

  if (VipShort->Data[i-1].vipAct==vipActREal)
    sprintf(sBuf2,
      " vipAct= vipActREal --> Real rOption=%-1d rNumSy=%-1d rValue=%1.1e",
      VipShort->Data[_AC(i-1,vipShL-1)].uvipA.svAR.rOption,
      VipShort->Data[i-1].uvipA.svAR.rNumSy,
      VipShort->Data[i-1].uvipA.svAR.rValue);

  if (VipShort->Data[i-1].vipAct==vipActCHan) {
    sprintf(sBuf2,"change Option COption=%-1d",
            VipShort->Data[_AC(i-1,vipShL-1)].uvipA.svACh.COption);
  }

  fprintf(TestOut,"  %s %s\n",sBuf1,sBuf2);

  if (tr2err)
    errfmt(0,10,"wrVipSh ","dId: %d: %s %s"
               ,VipShort->dId,sBuf1,sBuf2);

  }  //for..
LTestOut+=VipShort->dataL+1;
fflush(TestOut);
}  //wrVipShort
// ---------------------------------------------------------------------
//list message variables
//
static
void          getTATrace()
{
dispDes*      pWTHpGDispDesRpP;
ttVipShortP*  pWTHmeVarInd;
//read invar -> determine TA

if (!INREAD(&fInputVar,pGDispDesRpP->dId,
            pVipShort->Data[_AC(iRVarDes-1,vipShL-1)].gid,
            &rinVar)) {
  pWTHpGDispDesRpP = pGDispDesRpP;
  pWTHmeVarInd = &pVipShort->Data[meVarInd-1];
  err(0,3364,"initBFEl",pWTHpGDispDesRpP->dId,pWTHmeVarInd->gid,
      0,"iib");
  goto _L0;
  }
if (rinVar.Info.vipVar!=meVar) {
  pWTHpGDispDesRpP = pGDispDesRpP;
  pWTHmeVarInd = &pVipShort->Data[meVarInd-1];
  err(0,3365,"initBFEl",pWTHpGDispDesRpP->dId,pWTHmeVarInd->gid,
      0,"iib");
  goto _L0;
  }

//provide TA for pInfoType
TA.B1 = rinVar.Info.uVipV.smeVa.meTa.B1;
TA.B2 = rinVar.Info.uVipV.smeVa.meTa.B2;
TA.B3 = rinVar.Info.uVipV.smeVa.meTa.B3;
TA.Elem = rinVar.Info.uVipV.smeVa.meTa.Elem;
TA.Info = rinVar.Info.uVipV.smeVa.meTa.Info;
_L0: ;
}  //getTAtrace


static
void          wrMeVar()
{
typedef tProMo tPosiddeTa11;
int           iR;
tPosiddeTa11  PosidHelp;
int           FORLIM;
tMeVar*       pWTHpmeVar;


fprintf(TestOut,"  number of variables: nMeOut = %-1d dataL = %-1d",
        pMeOut->nMeOut,pVipShort->dataL);
if (pMeOut->nMeOut!=pVipShort->dataL)
  fprintf(TestOut,"!!!!!!!!! different !!!!!!!!!\n");
else
  fprintf(TestOut,"\n");
FORLIM = pMeOut->nMeOut;

for (iR = 1; iR<=FORLIM; ++iR) {
  iRVarDes = iR;
  pmeVar = &pMeOut->varDes[_AC(iR-1,vipShL-1)];

  getTATrace();

  fprintf(TestOut,"    No: %3d",iR);

  fprintf(TestOut," proMode: ");
  for (PosidHelp = pSingleStep;
       PosidHelp<=deTa11;
       PosidHelp = (tPosiddeTa11)(PosidHelp+1)) {
    if (InSet(pmeVar->proMode,(int)PosidHelp,sProMode))
      fprintf(TestOut,"%-1d ",(int)PosidHelp);
    }

  pWTHpmeVar = pmeVar;
  //with pmeVar^

  fprintf(TestOut," ->");
  if (InSet(pWTHpmeVar->proMode,(int)ackPerm,sProMode))
    fprintf(TestOut," ackPerm");
  if (InSet(pWTHpmeVar->proMode,(int)dispSelG,sProMode))
    fprintf(TestOut," dispSelG");
  if (InSet(pWTHpmeVar->proMode,(int)netGrCol,sProMode))
    fprintf(TestOut," netGrCol");
  if (InSet(pWTHpmeVar->proMode,(int)deTaEl,sProMode))
    fprintf(TestOut," deTaEl");
  if (InSet(pWTHpmeVar->proMode,(int)deTaTyp,sProMode))
    fprintf(TestOut," deTaTyp");
  if (InSet(pWTHpmeVar->proMode,(int)deTa11,sProMode))
    fprintf(TestOut," deTa11");
  fprintf(TestOut," deTaNA: %-1d",pWTHpmeVar->deTaNA);
  fprintf(TestOut," deTaNF: %-1d",pWTHpmeVar->deTaNF);
  fprintf(TestOut," Info: %-1d",pWTHpmeVar->taMeVar.Info);
  fprintf(TestOut," TA:/%-1d/%-1d/%-1d/%-1d/%-1d ",
          TA.B1,TA.B2,TA.B3,TA.Elem,TA.Info);

  fprintf(TestOut," gid: %-1d",pVipShort->Data[_AC(iR-1,vipShL-1)].gid);
  fprintf(TestOut," aAlt: %-1d",pVipShort->Data[iR-1].aAlt);
  fprintf(TestOut," aGroup:  %-1d",pVipShort->Data[iR-1].aGroup);
  fprintf(TestOut," Mark: %-1d",pVipShort->Data[iR-1].Mark);


  if (pVipShort->Data[iR-1].vipAct==vipActLFig)
    fprintf(TestOut," lFig");
  if (pVipShort->Data[_AC(iR-1,vipShL-1)].vipAct==vipActGFig)
    fprintf(TestOut," gFig");

  if (pVipShort->Data[_AC(iR-1,vipShL-1)].vipAct==vipActLFig ||
      pVipShort->Data[iR-1].vipAct==vipActGFig) {
    fprintf(TestOut," fAlt: %-1d",
            pVipShort->Data[_AC(iR-1,vipShL-1)].uvipA.svALG.fAlt);
    fprintf(TestOut," fGroup: %-1d",pVipShort->Data[iR-1].uvipA.svALG.fGroup);
    fprintf(TestOut," aAltOld,PhA,B,C: %-1d,%-1d,%-1d,%-1d",
            pVipShort->Data[iR-1].uvipA.svALG.aAltOld,
            pVipShort->Data[iR-1].uvipA.svALG.aAltPhaseA,
            pVipShort->Data[iR-1].uvipA.svALG.aAltPhaseB,
            pVipShort->Data[iR-1].uvipA.svALG.aAltPhaseC);
    }

  if (pVipShort->Data[iR-1].vipAct==vipActREal) {
    fprintf(TestOut," Real");
    fprintf(TestOut," rOption: %-1d",
            pVipShort->Data[_AC(iR-1,vipShL-1)].uvipA.svAR.rOption);
    fprintf(TestOut," rNumSy:  %-1d",pVipShort->Data[iR-1].uvipA.svAR.rNumSy);
    fprintf(TestOut," rValue:  %1.1e",pVipShort->Data[iR-1].uvipA.svAR.rValue);
    }

  if (pVipShort->Data[iR-1].vipAct==vipActINteger) {
    fprintf(TestOut," Integer");
    fprintf(TestOut,"iOption: %-1d",
            pVipShort->Data[_AC(iR-1,vipShL-1)].uvipA.svAI.iOption);
    fprintf(TestOut,"iNumSy: %-1d",pVipShort->Data[iR-1].uvipA.svAI.iNumSy);
    fprintf(TestOut,"iValue: %-1d",pVipShort->Data[iR-1].uvipA.svAI.iValue);
    }

  fprintf(TestOut,"\n");
  }

LTestOut+=pMeOut->nMeOut+2;
}  //wrMeVar


static
void          wrdispDes()
{
//List all variables of selected dId
struct {
  //Display description:
  //Dummy-buffer for dbread
  tSegId        dId;
}             vdispDesTrace;

tfDispDes     fDispDesSave;       //DispDes:       DB-file variable
dispDes*      pWTHpGDispDesRpP;


fprintf(TestOut,"******* message variables of dId = %d\n",rTrace.dId);

fDispDesSave.Rp = fDispDes.Rp;
fDispDes.Rp = (dispDes*)(&vdispDesTrace.dId);

vdispDesTrace.dId = rTrace.dId;

if (vdispDesTrace.dId>maxDId) {
  fprintf(TestOut,"  ERROR: dId %-1d is greater than maxDId %-1d\n",
          rTrace.dId,maxDId);
  goto _L0;
  }

if (!DbRead(&fDispDes,RpOnPage)) {
  fprintf(TestOut,"  ERROR on Reading DispDes, dId = %3d flag: %3d\n",
          receive.outDisp.dId,fDispDes.Flag);
  goto _L0;
  }

pGDispDesRpP = fDispDes.RpP;
pWTHpGDispDesRpP = pGDispDesRpP;

if (!InSet(pWTHpGDispDesRpP->sDispVar,(int)meVar,pWTHpGDispDesRpP->sDispVar)) {
  fprintf(TestOut,"  *** no message variables\n");
  goto _L0;
  }
pVipShort = (tVipShortP*)(&pWTHpGDispDesRpP->information[_AC(
                  pWTHpGDispDesRpP->Directory.iVipShort-1,cMaxInf-1)]);
pMeOut = (tMeOut*)(&pWTHpGDispDesRpP->information[_AC(
                       pWTHpGDispDesRpP->Directory.iMeOut-1,cMaxInf-1)]);
wrMeVar();

_L0:

DbRcFree(&fDispDes,0);
fDispDes.Rp = fDispDesSave.Rp;
}  //wrdispDes
// ---------------------------------------------------------------------
static
void          wrMeDisp()
{
//write specified meDisp record from internal heap into trace file

int           i;
int           j;
int           M;
tB123         B123Key;
int           FORLIM;


fprintf(TestOut,"******* medisp record for B1=%-1d B2=%-1d B2=%-1d\n",
        rTrace.B1,rTrace.B2,rTrace.B3);

B123Key.U1.B1 = rTrace.B1;
B123Key.U1.B2 = rTrace.B2;
B123Key.U1.B3 = rTrace.B3;


if (!findB123MeDisp(&B123Key,&M))
  fprintf(TestOut,"  ***** no entries\n");
else {
  pmeDisp = B123Medisp[_AC(M-1,cNB123Medisp)].rp;

  fprintf(TestOut,"  nVar%5d\n",pmeDisp->nVar);

  FORLIM = pmeDisp->nVar;
  for (i = 1; i<=FORLIM; ++i) {
    fprintf(TestOut,"  Elem%-1d",pmeDisp->varList[_AC(i-1,cMaxMeDisp-1)].Elem);
    fprintf(TestOut," Info%-1d",pmeDisp->varList[i-1].Info);
    fprintf(TestOut," dId%-1d",pmeDisp->varList[i-1].dId);
    fprintf(TestOut," Ind %-1d",pmeDisp->varList[i-1].Ind);
    fprintf(TestOut," meDispI%-1d",pmeDisp->varList[i-1].meDispI);

    fprintf(TestOut," prM: ");
    if (InSet(pmeDisp->varList[_AC(i-1,cMaxMeDisp-1)].prM,(int)dSG,tPrMSet))
      fprintf(TestOut," dSG");
    if (InSet(pmeDisp->varList[_AC(i-1,cMaxMeDisp-1)].prM,(int)AckP,tPrMSet))
      fprintf(TestOut," AckP");

    fprintf(TestOut," tinfoSet: ");
    for (j = 1; j<=Infomax; ++j) {
      if (InSet(pmeDisp->varList[_AC(i-1,cMaxMeDisp-1)].infoSet,j,tinfoSet))
        fprintf(TestOut,"%-1d ",j);
      }
    fprintf(TestOut,"\n");
    }
  fprintf(TestOut,"\n");
  LTestOut+=pmeDisp->nVar+3;
  }

// find maximum VarList entries
fprintf(TestOut," Check max nVar entries search entries > 0.75 of cMaxMeDisp: %d \n"
       ,cMaxMeDisp);
int lMax_nVar = 0;
float lfl_nVar = 0.0;
B123Key.U1.B1 = 0; B123Key.U1.B2 = 0; B123Key.U1.B3 = 0;
for (int i=0; i < NB123Medisp; i++) {
  if (B123Medisp[i].rp->nVar > lMax_nVar) {
    lMax_nVar = B123Medisp[i].rp->nVar;
    B123Key.U1.B1 = B123Medisp[i].rp->B1;
    B123Key.U1.B2 = B123Medisp[i].rp->B2;
    B123Key.U1.B3 = B123Medisp[i].rp->B3;
  }
  lfl_nVar = B123Medisp[i].rp->nVar;
  if ((lfl_nVar/cMaxMeDisp) >= 0.75) {
    fprintf(TestOut," - nVar MeDisp entries %d - B1/B2/B3: %d/%d/%d\n"
           ,B123Medisp[i].rp->nVar,B123Medisp[i].rp->B1
           ,B123Medisp[i].rp->B2, B123Medisp[i].rp->B3);
    LTestOut++;
  }
}
fprintf(TestOut," Maximum nVar entries %d / B1/B2/B3: %d/%d/%d\n"
       ,lMax_nVar,B123Key.U1.B1,B123Key.U1.B2,B123Key.U1.B3);
fprintf(TestOut,"\n");
LTestOut+=3;
fflush(TestOut);
}  //wrMeDisp
// ---------------------------------------------------------------------
static
void          chgTraceFile()
{
//close tracefile and switch to alternate file

fflush(TestOut);
P_ioresult = 0;
if (TestOut!=0)
  fclose(TestOut);
TestOut = 0;
if (medi_log1) {
  TestOut = fopen("/home/logs/medi_log2","w");
  if (TestOut==0)
    _EscIO2(FileNotFound,"/home/logs/medi_log2");
  medi_log1 = false;
  LTestOut = 0;
} else {
  TestOut = fopen("/home/logs/medi_log1","w");
  if (TestOut==0)
    _EscIO2(FileNotFound,"/home/logs/medi_log1");
  medi_log1 = true;
  LTestOut = 0;
  }
}  //chgTraceFile
// ---------------------------------------------------------------------
static
void          pTrace()
{
bool wasOn = rTrace.On;

//set trace switches
rTrace = receive.rTrace;

if (rTrace.On) {
  if (wasOn) errfmt(0,10,"pTrace ","pTrace: trace parameter modified");
  else       errfmt(0,10,"pTrace ","pTrace: trace turned On");
} else {
  errfmt(0,10,"pTrace  ","pTrace: trace turned Off");
  rTrace.wrbFlash  = false;
  rTrace.wrChandir = false;
  rTrace.wrMeDisp  = false;
  rTrace.wrdispDes = false;
  rTrace.wrVipShort= false;
  rTrace.wrRawInMe = false;
  rTrace.bitcmp    = false;
  rTrace.wrOutDisp = false;
}

if ((wasOn) && (!rTrace.On)) {
  // trace has been turned off
  chgTraceFile();
  return;
}

// force a flush
if (wasOn && rTrace.On) {
  fflush(TestOut);
  return;
}

if (rTrace.wrbFlash) {
  fprintf(TestOut," *** bFlash trace shifted to medi_dump (bum obj medi ssig dumpvar)\n\n");
  ++LTestOut;
}
if (rTrace.wrChandir) {
  fprintf(TestOut," *** wrChandir trace shifted to medi_dump (bum obj medi ssig dumpvar)\n\n");
  ++LTestOut;
}

if (rTrace.wrMeDisp)
  wrMeDisp();
if (rTrace.wrdispDes)
  wrdispDes();

if (rTrace.On)
  fflush(TestOut);

}  //pTrace


//Handle change of database copy
static
void          pDbcOpen(
  tRelCopKey    iRelCopKey)       //I  new key for data base copy
{
int           DbcOpenFlag;
bool          DbcOpenOK;


DbcOpenFlag = 0;

switch (receive.SboH.Service) {
  case s_DIM_ME:
    switch (receive.SboH.Request) {
      case r_MeAcknOrd:
        // blank case
        break;
      case r_MeAcknMsg:
        // blank case
        break;
      default:
        return;
        break;
      }//case SvoH.Request
    break;


  default:
    //otherwise
    switch (receive.U1.Header1) {
      case obDIMA:
        switch (receive.outDisp.Header2) {
          case joutDisp:
            break;

          default:
            return;
            break;
          }//case header2
        break;
      case obMEDI:
        switch (receive.U1.Header2) {
          case jrawInMe:
            // blank case
            break;
          case jFlash:
            // blank case
            break;
          case jsingAck:
            // blank case
            break;
          case jmeCoAck:
            // blank case
            break;
          case jcheckAf:
            // blank case
            break;
          case jmePosit:
            // blank case
            break;
          default:
            return;
            break;
          }//case header2
        break;
      case obVIP:
        switch (receive.vDispAck.Header2) {
          case vipDispAck:
            // blank case
            break;
          default:
            return;
            break;
          }
        break;
      default:
        return;
        break;
      }//case header1
    break;
  }//case SboH.Service

if (iRelCopKey!=lastRelCopKey) {
  lastRelCopKey = iRelCopKey;

  DbcOpenOK = true;
  if (!DbcOpen(&fCoDeTaEl,iRelCopKey))
    DbcOpenOK = false;
  if (!DbcOpen(&fDeTa,iRelCopKey))
    DbcOpenOK = false;
  if (!DbcOpen(&fNimad,iRelCopKey))
    DbcOpenOK = false;
  if (!DbcOpen(&fDispDes,iRelCopKey))
    DbcOpenOK = false;
  if (!DbcOpen(&fInputVar,iRelCopKey))
    DbcOpenOK = false;
  if (iRelCopKey==0 && !DbcOpen(&fmeDisp,0))
    DbcOpenOK = false;

  if (!DbcOpenOK) {
    DbcOpenFlag = fCoDeTaEl.Flag+fDeTa.Flag+fNimad.Flag+fDispDes.Flag+
                  fInputVar.Flag;
    err(iRelCopKey,3304,"daba    ",rs("DbcO"),rs("pen "),DbcOpenFlag,"ZZI");
    endOfJob = true;
    longjmp(_JL99,1);             //end of job
    }
  }
}  //pDbcOpen


static
void          pMeStart()
{
//Start of MEDI: presettings
/*
in pMeStart some variables and records are set to their initial values.
*/

McNotifyOwnComStateObj(true);     //notify object for signal OwnComState
McNotifyPartnerComStateObj(true); //notify object for signal PartnerComState
McNotifyOtherComStateObj(true);   //notify object for signal OtherComState

//notification at dbam for data modifications
RunOK = DMSUSER(obMEDI,saMedi,2,ELEMENTS,DBA_BLOCK,minorder,minorder,minorder,
                minorder,minorder,minorder,minorder,minorder);
if (!RunOK)
  err(0,3202,"DmsUser ",rs("Err."),rs("Prog"),rs("Term"),"SSS");

markbcaf = true;                  //marker for pUpdateD
MyComputId = McI();               //own computer identification
PaComputId = McP();
MyOpMode = McOpMode(MyComputId);

nbChangeAF = 0;

#if SUBS_SHHIS
ById00(&bVipHIS,sizeof(tVipHIS));
bVipHIS.Service = s_HIS_TOPR;
bVipHIS.Request = r_TOPO_data_change;
bVipHIS.AckClass= AckNo;

ById00(&bDIdHIS,sizeof(tDIdHIS));
bDIdHIS.Service = s_HIS_TOPR;
bDIdHIS.Request = r_TOPO_display_change;
bDIdHIS.AckClass= AckNo;
#endif

bVipShort.Header1 = obVIP;
bVipShort.Header2 = vipShort;
bVipShort.AckPara.AckCont = AckNo;

bVipFunct.Header1 = obVIP;
bVipFunct.Header2 = vipFunct;
bVipFunct.AckPara.AckCont = AckNo;
bVipFunct.vipFunc = vipFuDIAlog;
bVipFunct.uviFu.svFHD.diControl = vipDmedi+vipPrio;

bActSign.Header1 = obSEDS;
bActSign.Header2 = sdActSign;
bActSign.ActSignNu = 0;

lSvcA = sizeof(SvcAck);
SvcA.sSAk0.AckMode = amNormInfo;
SvcA.sSAk0.Info = 0;

bLiUAcknow.Header1 = obLIDI;
bLiUAcknow.Header2 = LiUAckNow;
bLiUAcknow.AckPara.AckCont = AckNo;

//Presetting for pCheckAf
rCheckAF.Header1 = obMEDI;
rCheckAF.Header2 = jcheckAf;
rCheckAF.RelCopKey = 0;
dIdCheck = 0;

TA.B1 = 0;
TA.B2 = 0;
TA.B3 = 0;
TA.Elem = 0;
TA.Info = 0;

endOfJob = false;                 //job is ended because of error
PCMedi = false;
bFlash = 0;
bCAF = 0;
nCAF = 0;
nBVipSH = 0;
bStay = 0;
bVipSH = 0;
bVipSHLast = 0;
// Positionable id`s for multi element selection
CleSet(GrMode,sProMode);
AddSet(GrMode,(int)pTagCtlInh,sProMode);
AddSet(GrMode,(int)pTagWrkPrm,sProMode);
AddSet(GrMode,(int)pTagRemOpr,sProMode);
AddSet(GrMode,(int)pTagSwInProg,sProMode);
checkBFl = false;

PreRanSet(fullMmiAMeClS,1,cMaxMeCl,tMeClS);
PreRanSet(fullMmiATeArS,1,cMaxTear,tTeArS);
// Positionable id`s for preselection
CleSet(SelMode,sProMode);
AddSet(SelMode,(int)pManupdate,sProMode);
lastRelCopKey = 0;

if (TestOut!=0)
  TestOut = freopen("/home/logs/medi_log1","w",TestOut);
else
  TestOut = fopen("/home/logs/medi_log1","w");
if (TestOut==0)
  _EscIO2(FileNotFound,"/home/logs/medi_log1");
LTestOut = 0;
medi_log1 = true;
rTrace.On = false;
rTrace.wrbFlash = false;
rTrace.wrMeDisp = false;
rTrace.B1 = 0;
rTrace.B2 = 0;
rTrace.B3 = 0;
rTrace.wrChandir = false;
rTrace.wrdispDes = false;
rTrace.dId = 0;
rTrace.wrVipShort = false;
rTrace.wrRawInMe = false;
rTrace.bitcmp = false;
rTrace.wrOutDisp = false;
rTrace.LMaxTestO = 200000;
P2Ccps(rDATUM,"^YYY-^N-^D ^H:^M:^S",sizeof(rDATUM));

/* Initialize logging for dynamic variables */
InitLogging();


CleSet(ReSelDidSet,tActFileSet);

}  //pMeStart


static
void          pFillCoDeTa()
{
//read all CoDeTaEl and CoDeTa into Heap
int           NCoDeTaEl;
int           NDeTa;
DbInfo(&fCoDeTaEl,&fCoDeTaElInfo);
DbInfo(&fDeTa,&fDeTaInfo);

NCoDeTaEl = 0;
NDeTa = 0;
for (i = 1; i<=cDTKeyMax; ++i) {
  free(DeTaPointer[_AC(i-1,cDTKeyMax-1)].UU.El);
  DeTaPointer[i-1].UU.El = 0;
  DeTaPointer[i-1].ElDeTa = false;
  }

fCoDeTaEl.Rp->DtNr = 0;
DbPos(&fCoDeTaEl,NxKey);
while (DbRead(&fCoDeTaEl,NextRc)) {
  DeTaPointer[_AC(fCoDeTaEl.Rp->DtNr-1,cDTKeyMax-1)].UU.El =
    (CoDeTaEl*)malloc_chk(fCoDeTaEl.RcS);
  DeTaPointer[fCoDeTaEl.Rp->DtNr-1].ElDeTa = true;
  ByIdBy(DeTaPointer[fCoDeTaEl.Rp->DtNr-1].UU.El,&vCoDeTaElL,fCoDeTaEl.RcS);
  ++NCoDeTaEl;
  }
fDeTa.Rp->DtNr = 0;
DbPos(&fDeTa,NxKey);
while (DbRead(&fDeTa,NextRc)) {
  if (!DeTaPointer[_AC(fDeTa.Rp->DtNr-1,cDTKeyMax-1)].ElDeTa) {
    DeTaPointer[_AC(fDeTa.Rp->DtNr-1,cDTKeyMax-1)].UU.TY =
      (DeTa*)malloc_chk(fDeTa.RcS);
    ByIdBy(DeTaPointer[fDeTa.Rp->DtNr-1].UU.TY,&vDeTaL,fDeTa.RcS);
    ++NDeTa;
  } else {
    errfmt(0,3201,"FillDeTa","pFillCoDeTa: duplicate data base entry El/Ty: %d - DeTa not used!"
                 ,fDeTa.Rp->DtNr);
    free(DeTaPointer[fDeTa.Rp->DtNr-1].UU.El);
    DeTaPointer[fDeTa.Rp->DtNr-1].UU.El = NULL;
    DeTaPointer[fDeTa.Rp->DtNr-1].ElDeTa = false;
  }
}

// check element / info of deta
bool lDtErr;
DeTa* pDtTy;
for (int il=0; il < cDTKeyMax; ++il) {
  lDtErr = false;
  if (DeTaPointer[il].UU.El == NULL) continue;
  if (DeTaPointer[il].ElDeTa) {
    for (int nI=0; nI < DeTaPointer[il].UU.El->NrInfo; ++nI)
      if ((DeTaPointer[il].UU.El->Info[nI] < 0) ||
          (DeTaPointer[il].UU.El->Info[nI] > Infomax)) {
        lDtErr = true;
        break;
      }
  } else {
    pDtTy = DeTaPointer[il].UU.TY;
    for (int nI=0; nI < pDtTy->NrCond; ++nI)
      if ((pDtTy->uD.cl[nI].Elem < 0) ||
          (pDtTy->uD.cl[nI].Elem > Elmax) ||
          (pDtTy->uD.cl[nI].Info < 0) ||
          (pDtTy->uD.cl[nI].Info > Infomax)) {
        lDtErr = true;
        break;
      }
  }
  if (lDtErr) {
    errfmt(0,3202,"FillDeTa","pFillCoDeTa: wrong Elem/Info: %d (%s) - DeTa not used!"
                 ,(il+1),(DeTaPointer[il].ElDeTa ? "El" : "Ty"));
    free(DeTaPointer[il].UU.El);
    DeTaPointer[il].UU.El = NULL;
    DeTaPointer[il].ElDeTa = false;
  }
}

if (fCoDeTaElInfo.istEntA!=NCoDeTaEl || fDeTaInfo.istEntA!=NDeTa)
  errfmt(0,2317,"DT error","El:DbInfo/Read: %d/%d - Ty:DbInfo/Read: %d/%d"
               ,fCoDeTaElInfo.istEntA,NCoDeTaEl
               ,fDeTaInfo.istEntA,NDeTa);

CleSet(NetwStatCoDetaElS,tCoDetaS); // netwstat CoDetaEl set
CleSet(NetwGrpCoDetaElS, tCoDetaS); // netwgrp  CoDetaEl set

// check CoElDeTa Info and tInfoStr
size_t szInfoStr  = sizeof(tInfoStr);
size_t szRuOtherS = sizeof(tsRuOther);

if (szInfoStr != szRuOtherS)
  errfmt(0,2000,"DT error"
  ,"WARNING: nede != max. CoElDeTa Infos - check cRuleLastI/InfoStrMax");

if (!IsMultiPhaseYes) return;

/* build up NetwStat,NetwGrp Deta Set */

tNormElem lNoEl;
CoDeTaEl* pCoDt;
for (i=0; i < cDTKeyMax; i++) {
  if (DeTaPointer[i].notEl())
    continue;

  pCoDt = DeTaPointer[i].UU.El;
  lNoEl = pCoDt->NETyp;

  if (NoElMPhInfo[_AC(lNoEl,neDim)].NetwStat > 0)
    for (int j=0; j < pCoDt->NrInfo; j++)
      if (pCoDt->Info[j] == NoElMPhInfo[lNoEl].NetwStat) {
        AddSet(NetwStatCoDetaElS,pCoDt->DtNr,tCoDetaS);
        break;
      }

  if (NoElMPhInfo[lNoEl].NetwGrp > 0)
    for (int j=0; j < pCoDt->NrInfo; j++)
      if (pCoDt->Info[j] == NoElMPhInfo[lNoEl].NetwGrp) {
        AddSet(NetwGrpCoDetaElS,pCoDt->DtNr,tCoDetaS);
        break;
      }
}

}  //pFillCoDeTa


static
void          pClearCoDeTa()
{
//clear all CoDeTaEl and CoDeTa in Heap
for (i = 1; i<=cDTKeyMax; ++i) {
  if (DeTaPointer[_AC(i-1,cDTKeyMax-1)].ElDeTa) {
    free(DeTaPointer[_AC(i-1,cDTKeyMax-1)].UU.El);
    DeTaPointer[i-1].UU.El = 0;
    DeTaPointer[i-1].ElDeTa = false;
  } else {
    free(DeTaPointer[i-1].UU.TY);
    DeTaPointer[i-1].UU.TY = 0;
    }
  }
}  //pClearCoDeTa

// ---------------------------------------------------------------------
// Fill set of infos for given elemtary decision table
bool          GetInfoSetEl(short DtNr, tinfoSet* infoSet)
{
  if (DtNr>cDTKeyMax)
  {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    return false;
  }

  if (DeTaPointer[DtNr-1].notEl()) {
    DatErr(0,2315,"DT inval",0,0,DtNr,"III","CoDeTaEl",TA,0);
    errfmt(0,2315,"DT inval","GetInfoSetEl: no CoElDeTa: DtNr: %d",DtNr);
    return false;
  }

  for (int j = 1; j<=DeTaPointer[DtNr-1].UU.El->NrInfo; ++j)
    AddSet(*infoSet,DeTaPointer[DtNr-1].UU.El->Info[j-1],tinfoSet);

  return true;
}

static
void          pMeSoftInit()
{
//Start of MEDI: opening of database files
/*
pMeSoftInit opens the database files of all database relations of MEDI.
For each database relation there is a database file variable declared.
Only the originals of database files are opened.
*/

short         i;
short         iComput;
tSegId        tmpDId;
bool          meDispWrong;
short         FORLIM;
dispDes*      pWTHpGDispDesRpP;

if (!DbfOpen(&fDispDes,0,"DABA","DISPDES ",0,0,0,sizeof(dispDes))) {
  err(0,3310,"DBFOPEN ",rs("DABA"),rs("disp"),fDispDes.Flag,"SSI");
  RunOK = false;                  //end of program
  }
fDispDes.Rp = (dispDes*)(&vdispDes);

if (!DbfOpen(&fActFileC,0,"DABA","ACTFILEC",0,0,0,sizeof(ActFileC))) {
  err(0,3310,"DBFOPEN ",rs("DABA"),rs("AFC "),fActFileC.Flag,"SSI");
  RunOK = false;                  //end of program
  }
fActFileC.Rp = (ActFileC*)(&rActFileC);

DbDelete(&fActFileC,DelAllRc);
if (!DbfOpen(&fInputVar,0,"DABA","INPUTVAR",0,0,0,sizeof(InputVarP))) {
  err(0,3310,"DBFOPEN ",rs("DABA"),rs("inpv"),fInputVar.Flag,"SSI");
  RunOK = false;                  //end of program
  }
fInputVar.Rp = (InputVarP*)(&rInputVar);

if (!DbfOpen(&fmeDisp,0,"DABA","MEDISP  ",0,0,0,sizeof(meDisp))) {
  err(0,3310,"DBFOPEN ",rs("DABA"),rs("meDi"),fmeDisp.Flag,"SSI");
  RunOK = false;                  //end of program
  }
fmeDisp.Rp = &vMeDispL;

//Package UIPCOI
if (!UIcOpen(&UIcIntVar,saMedi,0,0))  //end of program
  RunOK = false;

//NIM-accesses
NIMINIT();                        //Creating of NIM-administration
NimVar.pTa = &TA;
CleSet(NimVar.RelSet,tsNimRfix);
AddSet(NimVar.RelSet,xNimAd,tsNimRfix);
AddSet(NimVar.RelSet,xAdNim,tsNimRfix);
AddSet(NimVar.RelSet,xNeDe,tsNimRfix);
if (!NIMOPEN(&NimVar.FlagS,0,3314))  //Opening NIMAD, ADNIM and NEDE
  RunOK = false;
//end of program
NimVarMp.pTyNu = (tTyDeNu*)malloc_chk(sizeof(tTyDeNu));
NimVarMp.pTa = &TA;
CleSet(NimVarMp.RelSet,tsNormElem);
if (!NIMOPMP(&NimVarMp.FlagS,0,3314)) {  //end of program
  /*Open  NIM-relations for message
                                        processing*/
  RunOK = false;
  }
NimVarTy.pTa = &TA;
NimVarTy.pTyNu = 0;
CleSet(NimVarTy.RelSet,tsNimRfix);
AddSet(NimVarTy.RelSet,xElTyDe ,tsNimRfix);
AddSet(NimVarTy.RelSet,xInfoDef,tsNimRfix);
AddSet(NimVarTy.RelSet,xInTyDe,tsNimRfix);
if (!NIMOPTY(&NimVarTy.FlagS,0,3314)) {  //end of program
  /*Opening of NIM-relations INFODEF
                                        and INTYDE*/
  RunOK = false;
  }

//Decision tables
fCoDeTaEl.Rp = &vCoDeTaElL;
if (!DbfOpen(&fCoDeTaEl,0,"DABA","CODETAEL",0,0,0,sizeof(CoDeTaEl))) {
  err(0,3310,"DBFOPEN ",rs("DABA"),rs("CoEl"),fCoDeTaEl.Flag,"SSI");
  RunOK = false;                  //end of program
  }
fDeTa.Rp = &vDeTaL;
if (!DbfOpen(&fDeTa,0,"DABA","CODETA  ",0,0,0,sizeof(DeTa))) {
  err(0,3310,"DBFOPEN ",rs("DABA"),rs("CoDe"),fDeTa.Flag,"SSI");
  RunOK = false;                  //end of program
  }

//Technological oriented service data
if (!DbfOpen(&fNimad,0,"DABA","NIMAD   ",0,0,0,sizeof(NIMAD))) {
  err(0,3310,"DBFOPEN ",rs("DABA"),rs("nima"),fNimad.Flag,"SSI");
  RunOK = false;                  //end of program
  }
fNimad.Rp = &rNimad;

DataSize = sizeof(tReceive);

//creation of mmi directory
for (iComput = 1; iComput<=cMaxComput; ++iComput) {
  mmiDir[_AC(iComput-1,cMaxComput-1)] = 0;
  restoreMMI[_AC(iComput-1,cMaxComput-1)] = 0;
  }

//Presetting of grid interval.
Rastint.Bin.TimOvlap = NoOvlap;
Rastint.Sys.Year = 0;
Rastint.Sys.Month = 0;
Rastint.Sys.Day = 0;
Rastint.Sys.Hour = 0;
Rastint.Sys.Min = minAlarm;
Rastint.Sys.Sec = 0;
Rastint.Sys.cSec = 0;
ClockId = SbClMEDI;

/*MyComputId (= number of own computer) -> job is written into saMeactF
after the selected time interval and performed after jobs of higher
priority.
OwnComput ->  performance of the job after the selected time
intervall.*/
if (!SbAlarm(&sbF,MyComputId,saMeactF,obMEDI,sizeof(tCheckAf),&rCheckAF,
             ClockId,&Rastint,SbAfter,SbIntern) && sbF!=SBeNew) {
  err(0,2217,"SBALARM1",sbF,0.0,0.0,"III");
  //Lock sb-addresses until mepr has finished runup operations
  }

SbModify(&sbF,saMedi,obMEDI,SbLockReceive);
SbModify(&sbF,saRawInMe,obMEDI,SbLockReceive);
SbModify(&sbF,saMeactF,obMEDI,SbLockReceive);

// register for cosole access update
if (!CSIxUD::RequestAuthorizationXid(MyComputId,saMedi,obMEDI
                                    ,sr_RegisterCoAcData,0,0)) {
  errfmt(0,2500,"pMeSoftI","Error at RequestAuthorizationXid");
}

writeDispdes = false;
writeActFileC = false;


//determine all dId`s with meVar for updating
meSoftInit = true;
corrSoftInit = false;
createNew = true;
initialUpdate = true;
isRTSBrunup = false;
for (i = 1; i<=maxDId; ++i)       //initialize DIdTable
  pDIdTbl[_AC(i-1,maxDId-1)] = 0;
NB123Medisp = 0;

if (!cUseMeDispOrig) {
  errfmt(0,100,"Init    ","pMeSoftInit: Don't use MeDisp, DelAllRc MeDisp");
  DbDelete(&fmeDisp,DelAllRc);
}

//loop through whole meDisp
vMeDispL.B1 = 0;
vMeDispL.B2 = 0;
vMeDispL.B3 = 0;

DbPos(&fmeDisp,NxKey);
while (DbRead(&fmeDisp,NextRc)) {
  if (fmeDisp.RcS!=
      sizeof(meDisp)-(cMaxMeDisp-fmeDisp.Rp->nVar)*sizeof(tVarList)) {
    //meDisp record inconsistent
    err(0,3369,"MeD wrng",vMeDispL.B1,vMeDispL.B2,0.0,"iih");
    DbDelete(&fmeDisp,0);
    DbPos(&fmeDisp,NextKey);
    continue;
    }

  //another plausibility check
  meDispWrong = false;
  FORLIM = fmeDisp.Rp->nVar;
  for (i = 1; i<=FORLIM; ++i) {
    if (fmeDisp.Rp->varList[_AC(i-1,cMaxMeDisp-1)].dId<=0) {
      //meDisp record wrong
      err(0,3369,"MeD wrng",vMeDispL.B1,vMeDispL.B2,0.0,"iih");
      DbDelete(&fmeDisp,0);
      DbPos(&fmeDisp,NextKey);
      meDispWrong = true;
      }
    }
  if (meDispWrong)
    continue;

  ++NB123Medisp;
  if (NB123Medisp>cNB123Medisp) {
    err(0,3363,"MeD Heap",EoD,vMeDispL.B1,vMeDispL.B2*1000.0+vMeDispL.B3,
        "iii");
    NB123Medisp = cNB123Medisp;
    }

  // allocate new buffer for meDisp record
  B123Medisp[_AC(NB123Medisp-1,cNB123Medisp)].rp =
    (meDisp*)malloc_chk(fmeDisp.RcS);

  // set key
  B123Medisp[NB123Medisp-1].B123.U1.B1 = vMeDispL.B1;
  B123Medisp[NB123Medisp-1].B123.U1.B2 = vMeDispL.B2;
  B123Medisp[NB123Medisp-1].B123.U1.B3 = vMeDispL.B3;

  // copy temp buffer to new buffer
  ByIdBy(B123Medisp[NB123Medisp-1].rp,&vMeDispL,fmeDisp.RcS);
  B123Medisp[NB123Medisp-1].B123.U1.MdspChg = false;
  tmpDId = 0;
  FORLIM = fmeDisp.Rp->nVar;
  for (i = 1; i<=FORLIM; ++i) {
    if (tmpDId!=fmeDisp.Rp->varList[_AC(i-1,cMaxMeDisp-1)].dId) {
      tmpDId = fmeDisp.Rp->varList[_AC(i-1,cMaxMeDisp-1)].dId;
      if (!findB123DId(B123Medisp[_AC(NB123Medisp-1,cNB123Medisp)].B123,
                       fmeDisp.Rp->varList[_AC(i-1,cMaxMeDisp-1)].dId))
        prepB123DId(B123Medisp[_AC(NB123Medisp-1,cNB123Medisp)].B123,
                    fmeDisp.Rp->varList[_AC(i-1,cMaxMeDisp-1)].dId);
      }
    }
  }

B123MdspChgOK = true;

receive.outDisp.RelCopKey = 0;
CleSet(ActFileSet,tActFileSet);
CleSet(okActFileSet,tActFileSet);
CleSet(HISActFileSet,tActFileSet);
vdispDes.dId = 1;
NMediGenLcheck = 0;

DbPos(&fDispDes,NxKey);
while (DbRead(&fDispDes,NextRc+RpOnPage)) {
  pGDispDesRpP = fDispDes.RpP;
  pWTHpGDispDesRpP = pGDispDesRpP;
  vdispDes.dId = pWTHpGDispDesRpP->dId;
  if (InSet(pWTHpGDispDesRpP->sDispVar,(int)meVar,pWTHpGDispDesRpP->sDispVar)) {
    if (vdispDes.dId>maxDId)
      err(0,2603,"maxDId ",vdispDes.dId,maxDId,0.0,"III");
    else
      AddSet(ActFileSet,vdispDes.dId,tActFileSet);
  } else {
    ++NMediGenLcheck;
    if (NMediGenLcheck==cNMediGenLcheck) {
      McAckLifeCheck();
      NMediGenLcheck = 0;
      }
    meDispGen(false);
        //Delete dId in all concerned medisp records
    }
  }

if (fDispDes.Flag<0)
  err(0,3336,"SoftIni1",fDispDes.Flag,fDispDes.Fv.FlagS,vdispDes.dId,"III");
else
  meSoftInit = false;
pFRel();                          //Release dispDes and actfile

McAckEndGC(&sbF,0);

if (McNormStatus(MyComputId)==swsPC)
  PCNormstatus = true;
else
  PCNormstatus = false;

#if MULTI_PHASE_YES
  IsMultiPhaseYes = true;
#endif

IpcGetTime(&gActSec,&gActMSec);
/*
 * build up multi phase info / open line stat infos
 */
OpenLineStatInfo.LineStat  = 0;
OpenLineStatInfo.LineStatA = 0;
OpenLineStatInfo.LineStatB = 0;
OpenLineStatInfo.LineStatC = 0;

NEDE*       pNede;
tInfoNorm*  pInfoNorm;
for (i=0; i<=neDim; i++) {

  // open line stat infos
  if ((i == neTopElem) && (NimVarMp.NedePoi[i]->NezuInf > 0)) {
    pNede = NimVarMp.NedePoi[i];
    pInfoNorm = &pNede->uStDc.InfoNorm[pNede->NezuInf-1];
    OpenLineStatInfo.LineStat  = pInfoNorm->sUni.LineSt;
    OpenLineStatInfo.LineStatA = pInfoNorm->sUni.LineStA;
    OpenLineStatInfo.LineStatB = pInfoNorm->sUni.LineStB;
    OpenLineStatInfo.LineStatC = pInfoNorm->sUni.LineStC;
  }

  if ((!IsMultiPhaseYes) || (!IsNoElMP(i))) {
    NoElMPhInfo[i].NetwStat  = 0; NoElMPhInfo[i].NetwGrp   = 0;
    NoElMPhInfo[i].NeStPhA   = 0; NoElMPhInfo[i].NeGrPhA   = 0;
    NoElMPhInfo[i].NeStPhB   = 0; NoElMPhInfo[i].NeGrPhB   = 0;
    NoElMPhInfo[i].NeStPhC   = 0; NoElMPhInfo[i].NeGrPhC   = 0;
    NoElMPhInfo[i].NeStAssym = 0; NoElMPhInfo[i].NeGrAssym = 0;
    continue;
    }

  pNede = NimVarMp.NedePoi[i];
  NoElMPhInfo[i].NetwStat = pNede->NezuInf;
  NoElMPhInfo[i].NetwGrp  = pNede->NeGrInf;

  if (NoElMPhInfo[i].NetwStat > 0) {
    pInfoNorm = &pNede->uStDc.InfoNorm[NoElMPhInfo[i].NetwStat-1];
    NoElMPhInfo[i].NeStPhA   = pInfoNorm->sUni.PhaseA;
    NoElMPhInfo[i].NeStPhB   = pInfoNorm->sUni.PhaseB;
    NoElMPhInfo[i].NeStPhC   = pInfoNorm->sUni.PhaseC;
    NoElMPhInfo[i].NeStAssym = pInfoNorm->sUni.Asymmetr;
  }

  if (NoElMPhInfo[i].NetwGrp  > 0) {
    pInfoNorm = &pNede->uStDc.InfoNorm[NoElMPhInfo[i].NetwGrp-1];
    NoElMPhInfo[i].NeGrPhA   = pInfoNorm->sUni.PhaseA;
    NoElMPhInfo[i].NeGrPhB   = pInfoNorm->sUni.PhaseB;
    NoElMPhInfo[i].NeGrPhC   = pInfoNorm->sUni.PhaseC;
    NoElMPhInfo[i].NeGrAssym = pInfoNorm->sUni.Asymmetr;
  }

} // build multi phase infos

pFillCoDeTa();                    //read elementary decicion table into heap

gSIuHandle = SIuNew();
// open SIu interface for  SIuGetTeArea
if (!SIuOpen(gSIuHandle,0)) {
  SIuDelete(gSIuHandle);
  gSIuHandle = NULL;
  err(0,3337,"SoftIni1",rs("SIuO"),rs("pen "),rs("fail"),"ZZS");
  RunOK = false;                  //end of program
  }

}  //pMeSoftInit


static
void          pORe()
{
//Open sb-addresses
/*
mepr sends signal SBSIGUSR4 to medi after having finisched runup
operations.
*/
SbModify(&sbF,saMedi,obMEDI,SbOpenReceive);
SbModify(&sbF,saRawInMe,obMEDI,SbOpenReceive);
SbModify(&sbF,saMeactF,obMEDI,SbOpenReceive);
}  //pORe
// ---------------------------------------------------------------------
// delete all active channel of one mmiDir entry
//
static void pDelActiveChannels
(       trMmiDir *     pMmiDirDC     // IO : pointer to channel directory
, const short          iComp         // I  : ComputId
) {
  int           lChan;
  int           lChanIdx;
  int           lArInd;

  if (pMmiDirDC == NULL) return;

  if (pMmiDirDC->nActivChan > 0) {
    //preset record for deselection
    receive.outDisp.RelCopKey = 0;
    receive.outDisp.AckPara.Comput = iComp;
    receive.outDisp.dId = 0;
    receive.outDisp.newDisp = true;
    lChan = 1;
    while ((lChan <= cMaxChan) && (pMmiDirDC->nActivChan>0)) {
      lChanIdx = _AC(lChan-1,cMaxChan-1);
      for (lArInd = 0; lArInd <= 3; ++lArInd) {
        if ((pMmiDirDC->VChan[lChanIdx] != NULL) &&
            (pMmiDirDC->VChan[lChanIdx]->area[_AC(lArInd,3)].dId != 0)) {
          receive.outDisp.area = lArInd;
          receive.outDisp.chan = lChan;
          pDelChanDir();        //delete display out of channel directory
        }
      }
      lChan++;
    }
  }  //if nActivChan

} // pDelActiveChannels
// ---------------------------------------------------------------------
static
void          pStrtUpd()
{
//Start updating of a mmi computer
/*
If background updating is needed, woman sends a message'StrtUpd' to
medi. Medi creates a record for the mmi directory.
From this time on this mmi is updated.
*/
int           chan;
int           j;
trMmiDir*     pWTHiComput;
PCMedi = true;                    //medi has to update the mmi`s
ComputId iComput = receive.strtUpd.Comput;
tOpMode  lUpdCompOpmode = McOpMode(iComput);
//
// check OpMode of updating computer
//
if (MyOpMode != lUpdCompOpmode) {
  errfmt(0,2000,"pStrtUpd","Wrong OpMode of updating computer - break!");
  err(0,2000,"pStrtUpd",MyOpMode,lUpdCompOpmode,MyComputId,"iic");
  return;
}

if (mmiDir[_AC(iComput-1,cMaxComput-1)]==0)
  {  //create a record for mmi directory
  mmiDir[_AC(iComput-1,cMaxComput-1)] =
    (trMmiDir*)malloc_chk(sizeof(trMmiDir));
  pWTHiComput = mmiDir[iComput-1];
  for (j = 1; j<=cMaxChannelPerMMI; ++j) {
    SetSet(pWTHiComput->mmiAMeClS[_AC(j-1,cMaxChannelPerMMI-1)],fullMmiAMeClS,
           tMeClS);
    SetSet(pWTHiComput->mmiATeArS[j-1],fullMmiATeArS,tTeArS);
    }
  }
pWTHiComput = mmiDir[_AC(iComput-1,cMaxComput-1)];
    //Start updating of new mmi

pWTHiComput->Comput = iComput;    //computer for updating
pWTHiComput->mmiState = doupdate;  //*t*Optimierung: mmi`s, die nicht
                                  //permanent aufgedatet werden: noUpdate.
                                  //Besser: dima sendet kein StrtUpd
pWTHiComput->nActivChan = 0;
CleSet(pWTHiComput->mmiActFileSet,tActFileSet);  //meVar dId`s, updated
PreRanSet(pWTHiComput->mmiUpdtSet,1,maxDId,tActFileSet);
for (chan = 1; chan<=cMaxChan; ++chan)
  pWTHiComput->VChan[_AC(chan-1,cMaxChan-1)] = 0;

if (DEBUGlevel(100))
  cp(0,100,"pStrtUpd",iComput,(int)pWTHiComput->mmiState,0.0,"cib");
Rastint.Sys.Min = 0;
Rastint.Sys.Sec = secAlarm;
if (!SbAlarm(&sbF,MyComputId,saMeactF,obMEDI,sizeof(tCheckAf),&rCheckAF,
             ClockId,&Rastint,SbAfter,SbIntern)) {
  if (sbF!=SBeNew)
    err(0,2217,"SBALARM2",sbF,0.0,0.0,"III");
  }

}  //pStrtUpd


static
void          pStopUpd()
{
//Stop updating of a mmi computer
/*
If background updating is stopped, woman sends 'StopUpd' to medi. Medi
clear a record for the mmi directory. From this time on this mmi
will not updated.
*/
short         iComput;
int           iChan;
int           iArInd;
int           chan;
trMmiDir*     pWTHiComput;
trChanDir*    pWTHiChan;
PCMedi = true;
iComput = receive.strtUpd.Comput;
if (mmiDir[_AC(iComput-1,cMaxComput-1)] != NULL) {
      //Deselect all dids of this mmi
        pWTHiComput = mmiDir[_AC(iComput-1,cMaxComput-1)];

  pDelActiveChannels(pWTHiComput,iComput);

  pWTHiComput->Comput = iComput;  //computer for updating
  pWTHiComput->mmiState = noUpdate;
  pWTHiComput->nActivChan = 0;
  CleSet(pWTHiComput->mmiActFileSet,tActFileSet);  //meVar dId`s, updated
  CleSet(pWTHiComput->mmiUpdtSet,tActFileSet);
  for (chan = 1; chan<=cMaxChan; ++chan)
    pWTHiComput->VChan[_AC(chan-1,cMaxChan-1)] = NULL;

  if (DEBUGlevel(100))
    cp(0,100,"pStopUpd",iComput,(int)pWTHiComput->mmiState,0.0,"cib");
  }
}  //pStopUpd


static
void          pRnewUpd()
{
//Renew updating of a mmi computer
//Sent by woman, when updates could have been lost

int           j;
int           chan;
trMmiDir*     pWTHiComput;

PCMedi = true;                    //medi has to update the mmi`s
ComputId iComput = receive.strtUpd.Comput;
tOpMode  lUpdCompOpmode = McOpMode(iComput);
//
// check OpMode of updating computer
//
if (MyOpMode != lUpdCompOpmode) {
  errfmt(0,2000,"pRnewUpd","Wrong OpMode of updating computer - break!");
  err(0,2000,"pRnewUpd",MyOpMode,lUpdCompOpmode,MyComputId,"iic");
  return;
}
if (mmiDir[_AC(iComput-1,cMaxComput-1)]==0)
  {  //create a record for mmi directory
  mmiDir[_AC(iComput-1,cMaxComput-1)] =
    (trMmiDir*)malloc_chk(sizeof(trMmiDir));
  pWTHiComput = mmiDir[iComput-1];
  for (j = 1; j<=cMaxChannelPerMMI; ++j) {
    SetSet(pWTHiComput->mmiAMeClS[_AC(j-1,cMaxChannelPerMMI-1)],fullMmiAMeClS,
           tMeClS);
    SetSet(pWTHiComput->mmiATeArS[j-1],fullMmiATeArS,tTeArS);
    }
  pWTHiComput = mmiDir[_AC(iComput-1,cMaxComput-1)];
  pWTHiComput->nActivChan = 0;
  PreRanSet(pWTHiComput->mmiUpdtSet,1,maxDId,tActFileSet);
  for (chan = 1; chan<=cMaxChan; ++chan)
    pWTHiComput->VChan[_AC(chan-1,cMaxChan-1)] = 0;
  }
pWTHiComput = mmiDir[_AC(iComput-1,cMaxComput-1)];

pWTHiComput->Comput = iComput;    //computer for updating
if (pWTHiComput->mmiState==updateOk)
  pWTHiComput->mmiState = doupdate;
CleSet(pWTHiComput->mmiActFileSet,tActFileSet);  //meVar dId`s, updated
if (DEBUGlevel(100))
  cp(0,100,"pRnewUpd",iComput,(int)pWTHiComput->mmiState,0.0,"cib");
//Start updating of new mmi
Rastint.Sys.Min = 0;
Rastint.Sys.Sec = secAlarm;
if (!SbAlarm(&sbF,MyComputId,saMeactF,obMEDI,sizeof(tCheckAf),&rCheckAF,
             ClockId,&Rastint,SbAfter,SbIntern)) {
  if (sbF!=SBeNew)
    err(0,2217,"SBALARM2",sbF,0.0,0.0,"III");
  }

}  //pRnewUpd
// ---------------------------------------------------------------------
// reset UI update admin, without timer (used for trainingcheck())
//
static
void          pResetUpd()
{
  // check all server
  trMmiDir*   pMmiDir;
  for (int j = 1; j <= cMaxComput; ++j)
    if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
      pMmiDir = mmiDir[j-1];
      if ((pMmiDir->Comput != MinComput) && (McOpMode(pMmiDir->Comput) == MyOpMode)) {
        pMmiDir->mmiState = doupdate;
        CleSet(pMmiDir->mmiActFileSet,tActFileSet);
        if (DEBUGlevel(100))
          cp(0,100,"pResUpd ",pMmiDir->Comput,0.0,0.0,"cbb");
      }
    }
} // pResetUpd
// ---------------------------------------------------------------------
static
void          pMmiDId()
{
//Start updating of a remote mmi
/*
After having run up wmupd sends a message 'mmiDId' to medi. Medi
creates a record for the mmi directory. From this time on this mmi
is updated with those dId`s that where sendt by wmupd.
*/
short         iComput;
int           i;
int           j;
int           chan;
trMmiDir*     pWTHiComput;
int           FORLIM;
PCMedi = true;                    //medi has to update the mmi`s
iComput = receive.mmiDId.Comput;
if (mmiDir[_AC(iComput-1,cMaxComput-1)]==0)
  {                               //create a record for mmi directory
  mmiDir[_AC(iComput-1,cMaxComput-1)] =
    (trMmiDir*)malloc_chk(sizeof(trMmiDir));

  pWTHiComput = mmiDir[iComput-1];
  pWTHiComput->Comput = iComput;  //computer for updating
  pWTHiComput->mmiState = doupdate;  //*t*Optimierung: mmi`s, die nicht
                                  //permanent aufgedatet werden: noUpdate.
                                  //Besser: dima sendet kein StrtUpd
  pWTHiComput->nActivChan = 0;
  CleSet(pWTHiComput->mmiActFileSet,tActFileSet);  //meVar dId`s, updated
  PreRanSet(pWTHiComput->mmiUpdtSet,1,maxDId,tActFileSet);
  for (j = 1; j<=cMaxChannelPerMMI; ++j) {
    SetSet(pWTHiComput->mmiAMeClS[_AC(j-1,cMaxChannelPerMMI-1)],fullMmiAMeClS,
           tMeClS);
    SetSet(pWTHiComput->mmiATeArS[j-1],fullMmiATeArS,tTeArS);
    }
  for (chan = 1; chan<=cMaxChan; ++chan)
    pWTHiComput->VChan[_AC(chan-1,cMaxChan-1)] = 0;

  if (DEBUGlevel(100))
    cp(0,100,"pMmiDId ",iComput,(int)pWTHiComput->mmiState,0.0,"cii");
  }

mmiDir[_AC(iComput-1,cMaxComput-1)]->mmiState = doupdate;

FORLIM = receive.mmiDId.nDID;
for (i = 1; i<=FORLIM; ++i) {     //Start updating of new mmi
  if (receive.mmiDId.dIdArr[_AC(i-1,cMaxSegDId-1)]==0)
    CleSet(mmiDir[_AC(iComput-1,cMaxComput-1)]->mmiUpdtSet,tActFileSet);
  else
    AddSet(mmiDir[iComput-1]->mmiUpdtSet,
           receive.mmiDId.dIdArr[_AC(i-1,cMaxSegDId-1)],tActFileSet);
  }


Rastint.Sys.Min = 0;
Rastint.Sys.Sec = secAlarm;
if (!SbAlarm(&sbF,MyComputId,saMeactF,obMEDI,sizeof(tCheckAf),&rCheckAF,
             ClockId,&Rastint,SbAfter,SbIntern)) {
  if (sbF!=SBeNew)
    err(0,2217,"SBALARM2",sbF,0.0,0.0,"III");
  }

}  //pMmiDId


static
void          pUpdResp
(      tConsoleAccess   *ipCoAc
) {
//update console responsibilities
trMmiDir*     pMmiDirComp;

if (ipCoAc->XT_Comput <= MinComput) return;
if ((ipCoAc->XT_ChanFrom == 0) || (ipCoAc->XT_ChanTo == 0)) return;

if (DEBUGlevel(100))
  errfmt(0,100,"pUpdResp","Update console access received - ComputID: %d, Chan %d - %d"
             ,ipCoAc->XT_Comput,ipCoAc->XT_ChanFrom,ipCoAc->XT_ChanTo);

int  compIdx = _AC(ipCoAc->XT_Comput-1,cMaxComput-1);
if (mmiDir[compIdx]==0) {
  //create a record for mmi directory
  mmiDir[compIdx] = (trMmiDir*)malloc_chk(sizeof(trMmiDir));
  pMmiDirComp = mmiDir[compIdx];
  pMmiDirComp->Comput = ipCoAc->XT_Comput;
  pMmiDirComp->mmiState = noUpdate;
  pMmiDirComp->nActivChan = 0;
  CleSet(pMmiDirComp->mmiActFileSet,tActFileSet);  //meVar dId`s, updated
  PreRanSet(pMmiDirComp->mmiUpdtSet,1,maxDId,tActFileSet);
  for (int chan = 1; chan<=cMaxChan; ++chan)
    pMmiDirComp->VChan[_AC(chan-1,cMaxChan-1)] = 0;
  }

pMmiDirComp = mmiDir[compIdx];
int chIdx = 0;
for (int i=ipCoAc->XT_ChanFrom; i <= ipCoAc->XT_ChanTo; i++) {
  chIdx = _AC(i-1,cMaxChannelPerMMI-1);
  SetSet(pMmiDirComp->mmiAMeClS[chIdx],ipCoAc->MeClActi,tMeClS);
  SetSet(pMmiDirComp->mmiATeArS[chIdx],ipCoAc->TeArActiA,tTeArS);
}

}  //pUpdResp


static
void          pSigOwnComState()
{
//reaction on change of own computer state

short         iComput;
int           iChan;
int           iArInd;
trMmiDir*     pWTHiComput;
trChanDir*    pWTHiChan;
if (McNormStatus(MyComputId)!=swsPC) {
  PCMedi = false;
  PCNormstatus = false;

  delBVipSH();                    //delete all entries of bVipSH

  //deselect all selected channels; set mmi`s to 'no update'
  for (iComput = 1; iComput<=cMaxComput; ++iComput) {
    if (mmiDir[_AC(iComput-1,cMaxComput-1)]!=0 && iComput!=MyComputId) {
          //mimic board not deselected
      //Deselect all dids of this mmi
      pWTHiComput = mmiDir[_AC(iComput-1,cMaxComput-1)];
      pWTHiComput->mmiState = noUpdate;

      pDelActiveChannels(pWTHiComput,iComput);
      }
    }
} else
  PCNormstatus = true;
}  //pSigOwnComState
// ---------------------------------------------------------------------
// reaction on change of state of partner computer
//
static void pSigPartnerComState()
{
  if (DEBUGlevel(100))
    errfmt(0,100,"pSigPart","pSigPartnerComState: isRTSBrunup: %d",isRTSBrunup);

  isRTSBrunup = false;
  if (McNormStatus(MyComputId) != swsPC) return;
  tActPhase PaActPhase = cMinActPhase;
  PaComputId = McP();
  if (PaComputId != MinComput) {
    PaActPhase = McActPhase(PaComputId);
    isRTSBrunup = (PaActPhase < rupEndOfPostRU);
  }

  if (DEBUGlevel(100))
    errfmt(0,100,"pSigPart","pSigPartnerComState: PartnerActPhase: %d - isRTSBrunup: %d"
               ,PaActPhase,isRTSBrunup);

} // pSigPartnerComState
// ---------------------------------------------------------------------

static
void          pSigOtherComState()
{
//reaction on change of state of another computer

short         iComput;
int           iChan;
int           iArInd;
trMmiDir*     pWTHiComput;
trChanDir*    pWTHiChan;
if (PCMedi) {
  for (iComput = 1; iComput<=cMaxComput; ++iComput) {
    if (mmiDir[_AC(iComput-1,cMaxComput-1)]!=0) {
      pWTHiComput = mmiDir[_AC(iComput-1,cMaxComput-1)];
      if (McNormStatus(pWTHiComput->Comput)!=swsPC &&
          pWTHiComput->Comput!=MyComputId)
        {                         //mimic board not deselected
        if (pWTHiComput->mmiState==doupdate || pWTHiComput->mmiState==updateOk) {
          pWTHiComput->mmiState = noUpdate;
          CleSet(pWTHiComput->mmiActFileSet,tActFileSet);
          CleSet(pWTHiComput->mmiUpdtSet,tActFileSet);
          }

        pDelActiveChannels(pWTHiComput,iComput);
        }
      //deselect all selected channels; set mmi`s to 'no update'
      }
    }  //for iComput
  DynVarProcComState(); // check registered dynamic variables
  }
}  //pSigOtherComState


static
void          pSigUsr2(
  tOpMode  i_opMode)
{
//create mmiDir for mmi`s

/*Send this signal after having ended and restarted medi for test. The
procedure simulates the jobs of all dima's of all mmi`s*/
short         iComput;
tBlFunction   iBlFunction;        //block utilization
iBlFunction = 0;                  //to avoid compiler warning



for (iComput = 1; iComput<=cLastComput; ++iComput) {
  if (McInqBlFunction(iComput,blfGRAFIC,true))
    if (McOpMode(iComput) == i_opMode) {
    receive.strtUpd.Comput = iComput;
    pStrtUpd();                   //Start updating of a mmi computer
    cp(1,1,"SigUs2  ",rs("comp"),iComput,0.0,"SII");
    }
  }
}  //pSigUsr2


static
void          pSigUsr3()
{
//Set updating files invalid

int           j;
trMmiDir*     pWTHj;

CleSet(okActFileSet,tActFileSet);
CleSet(HISActFileSet,tActFileSet);
createNew = true;
for (j = 1; j<=cMaxComput; ++j) {
  if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
    pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];
    CleSet(pWTHj->mmiActFileSet,tActFileSet);
    if (pWTHj->mmiState==updateOk)
      pWTHj->mmiState = doupdate;
    }
  }  //for j
}  //pSigUsr3

// ---------------------------------------------------------------------
// get multi phase switch phase
//   true: multi phase infos need to be processed
//
static bool getMPhSwitchPhase
( NormElem    iNoEl               // I : norm element type
, tNimSet     iNimSet             // I : nimset number
, tDoMPHproc* pSwPh               //  O: processing infos
) {


  /* 1st. check norm element */

  tMultiPhaseInfo* pNoElMPh = &NoElMPhInfo[_AC(iNoEl,neDim)];
  pSwPh->swPh       = ph_ABC;     // preset
  pSwPh->deTaNetwGr = false;      // preset
  pSwPh->detaNetwSt = false;      // preset
  pSwPh->doNGr_A = (pNoElMPh->NeGrPhA > 0);
  pSwPh->doNGr_B = (pNoElMPh->NeGrPhB > 0);
  pSwPh->doNGr_C = (pNoElMPh->NeGrPhC > 0);
  pSwPh->doNSt_A = (pNoElMPh->NeStPhA > 0);
  pSwPh->doNSt_B = (pNoElMPh->NeStPhB > 0);
  pSwPh->doNSt_C = (pNoElMPh->NeStPhC > 0);

  if (!(pSwPh->doNGr_A || pSwPh->doNGr_B || pSwPh->doNGr_C ||
        pSwPh->doNSt_A || pSwPh->doNSt_B || pSwPh->doNSt_C))
    return false;

  /* 2nd. read phase info of element type */

  /* read AdNim */
  NimVar.pfVari.pfAdNim->Rp->NoElType = iNoEl;
  NimVar.pfVari.pfAdNim->Rp->NimSatz  = iNimSet;
  NimVar.pfVari.pfAdNim->Rp->Dummy    = 0;
  NimVar.pfVari.pfAdNim->Rp->Dummz    = 0;

  if (!DbRead(&NimVar.pfVari.pfAdNim->Flag,0)) {
    errfmt(0,2351,"SwPhase "
          ,"getMPhSwitchPhase: AdNim: Flag/Noel/NimSet: %d/%d/%d"
          ,NimVar.pfVari.pfAdNim->Flag,iNoEl,iNimSet);
    return false;
  }

  /* read eltyde */
  NimVarTy.pfVari.pfElTyde->Rp->ElemType = NimVar.pfVari.pfAdNim->Rp->uAdni.sNeEl.ElemType;
  if (!DbRead(&NimVarTy.pfVari.pfElTyde->Flag,0)) {
    errfmt(0,2351,"SwPhase "
          ,"getMPhSwitchPhase: ElTyde: Flag/Noel/NimSet/ElemType: %d/%d/%d/%d"
          ,NimVarTy.pfVari.pfElTyde->Flag
          ,iNoEl,iNimSet
          ,NimVarTy.pfVari.pfElTyde->Rp->ElemType);
    return false;
  }

  pSwPh->swPh = NimVarTy.pfVari.pfElTyde->Rp->uZgr.SpName.sw_phase;

  /* 3rd. check element type phase info */
  switch (pSwPh->swPh) {
    case ph_ABC :
      break;
    case ph_A :
      pSwPh->doNGr_B = false; pSwPh->doNGr_C = false;
      pSwPh->doNSt_B = false; pSwPh->doNSt_C = false;
      break;
    case ph_B :
      pSwPh->doNGr_A = false; pSwPh->doNGr_C = false;
      pSwPh->doNSt_A = false; pSwPh->doNSt_C = false;
      break;
    case ph_C :
      pSwPh->doNGr_A = false; pSwPh->doNGr_B = false;
      pSwPh->doNSt_A = false; pSwPh->doNSt_B = false;
      break;
    case ph_AB :
      pSwPh->doNGr_C = false; pSwPh->doNSt_C = false;
      break;
    case ph_AC :
      pSwPh->doNGr_B = false; pSwPh->doNSt_B = false;
      break;
    case ph_BC :
      pSwPh->doNGr_A = false; pSwPh->doNSt_A = false;
      break;
    default:
      break;
  }

  return true;

} // getMPhSwitchPhase
// ---------------------------------------------------------------------
// check if NetwGrNo and NetwStat are used in elementary decison table
//
static void getNetGrStDeTaUsage
( short       ideTaNA             // I : attribut decison table
, NormElem    iNoEl               // I : norm element type
, tDoMPHproc* pSwPh               //  O: processing infos
) {

  if (ideTaNA <= 0) return;

  if (InSet(NetwStatCoDetaElS,ideTaNA,tCoDetaS))
    pSwPh->detaNetwSt = true;

  if (InSet(NetwGrpCoDetaElS,ideTaNA,tCoDetaS))
    pSwPh->deTaNetwGr = true;

//  if (!DeTaPointer[_AC(ideTaNA-1,cDTKeyMax-1)].ElDeTa) return;
//
//  CoDeTaEl* pCoDt = DeTaPointer[(ideTaNA-1)].UU.El;
//
//  for (int i=0; i < pCoDt->NrInfo; i++) {
//    if (pCoDt->Info[i] == NoElMPhInfo[_AC(iNoEl,neDim)].NetwStat)
//      pSwPh->detaNetwSt = true;
//    if (pCoDt->Info[i] == NoElMPhInfo[iNoEl].NetwGrp)
//      pSwPh->deTaNetwGr = true;
//  }

} // getNetGrStDeTaUsage
// ---------------------------------------------------------------------
//Execution of elementary decision tables for DIMS
static
bool          coDtPrEl(
  int*          Flag,             // O: Flags
  short*        AcEx,             // O: Action to be done
  const tInfoStr ist,             //I : NIM infostring
  short         iln,              //I : length of infostring [bytes]
  short         DtNr)             //I : Number of decision table
{
bool          coDtPrEl_RTN;

int           i;
bool          ContinueRu;
union {
  tInfoStr      ist;
  tsRu032       ist032;
  tsRu064       ist064;
  tsRu096       ist096;
  tsRu128       ist128;
  tsRuOther     istOth;
}             Re;
tsRu032       SET;
CoDeTaEl*     pWTHEl;
tRule032*     pWTHi;
tRule064*     pWTHi1;
tsRu064       SET1;
tRule096*     pWTHi2;
tsRu096       SET2;
tRule128*     pWTHi3;
tsRu128       SET3;
tRuleOther*   plRuOth;
tsRuOther     lSetOth;

CleSet(Re.ist032,tsRu032);
AddSet(Re.ist032,0,tsRu032);
CleSet(Re.ist064,tsRu064);
AddSet(Re.ist064,0,tsRu064);
CleSet(Re.ist096,tsRu096);
AddSet(Re.ist096,0,tsRu096);
CleSet(Re.ist128,tsRu128);
AddSet(Re.ist128,0,tsRu128);
CleSet(Re.istOth,tsRuOther);
AddSet(Re.istOth,0,tsRuOther);
if (DtNr>cDTKeyMax) {
  err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
  *Flag = 50;
  return false;
  }
if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notEl()) {
  DatErr(0,2315,"DT inval",fCoDeTaEl.Flag,pGDispDesRpP->dId,DtNr,"III",
         "CodeTaEl",TA,0);
  err(0,2609,"DT-type?",DtNr,pGDispDesRpP->dId,iWVipShort,"III");

  return false;
  }

coDtPrEl_RTN = true;
*Flag = 0;
i = 0;                            //Counter of rules
memcpy(Re.ist,ist,iln);
ContinueRu = true;

pWTHEl = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El;
switch (pWTHEl->CompLen) {
  case l032:
    while (i<pWTHEl->NrRu && ContinueRu)
      {  // 2             Loop over all rules
      ++i;                        //next rule
      pWTHi = &pWTHEl->uDEl.Ru032[i-1];
      //with Ru032
      IntSet(SET,pWTHi->Filter,Re.ist032,tsRu032);
      if (EquSet(pWTHi->Target,SET,tsRu032)) {
        *AcEx = pWTHi->Action;
        ContinueRu = false;
        }
      }  // 2
    break;
  case l064:
    while (i<pWTHEl->NrRu && ContinueRu)
      {  // 2             Loop over all rules
      ++i;                        //next rule
      pWTHi1 = &pWTHEl->uDEl.Ru064[i-1];
      //with Ru064
      IntSet(SET1,pWTHi1->Filter,Re.ist064,tsRu064);
      if (EquSet(pWTHi1->Target,SET1,tsRu064)) {
        *AcEx = pWTHi1->Action;
        ContinueRu = false;
        }
      }  // 2
    break;
  case l096:
    while (i<pWTHEl->NrRu && ContinueRu)
      {  // 2             Loop over all rules
      ++i;                        //next rule
      pWTHi2 = &pWTHEl->uDEl.Ru096[i-1];
      //with Ru096
      IntSet(SET2,pWTHi2->Filter,Re.ist096,tsRu096);
      if (EquSet(pWTHi2->Target,SET2,tsRu096)) {
        *AcEx = pWTHi2->Action;
        ContinueRu = false;
        }
      }  // 2
    break;
  case l128:
    while (i<pWTHEl->NrRu && ContinueRu)
      {  // 2             Loop over all rules
      ++i;                        //next rule
      pWTHi3 = &pWTHEl->uDEl.Ru128[i-1];
      //with Ru128
      IntSet(SET3,pWTHi3->Filter,Re.ist128,tsRu128);
      if (EquSet(pWTHi3->Target,SET3,tsRu128)) {
        *AcEx = pWTHi3->Action;
        ContinueRu = false;
        }
      }  // 2
    break;
  case lOther:
    while (i<pWTHEl->NrRu && ContinueRu)
      {  // 2             Loop over all rules
      ++i;                        //next rule
      plRuOth = &pWTHEl->uDEl.RuOther[i-1];
      //with Ru256
      IntSet(lSetOth,plRuOth->Filter,Re.istOth,tsRuOther);
      if (EquSet(plRuOth->Target,lSetOth,tsRuOther)) {
        *AcEx = plRuOth->Action;
        ContinueRu = false;
        }
      }  // 2
    break;
  default:
    *AcEx = 0;
    break;
  }
coDtPrEl_RTN = !ContinueRu;
if (*AcEx<=0 || ContinueRu) {
  coDtPrEl_RTN = false;
  DatErr(0,2337,"coDtPrEl",DtNr,*AcEx,*Flag,"III","        ",TA,0);
  }
return coDtPrEl_RTN;
}  // 1


//Calling supply procedures
static
void          beBau(
  int*          Flag,             // O: Flags
  tDtPara*      DtPara,           //I : Parameter for execution of DT
  CondLine*     CoLi,             //I : Condition text of decision table
  short*        Res)              // O: State of a NIM-info
{  // 1
int           i;
bool          notFound;
bool          Ok;
tLength       Leng;
int           InfValue;

Ok = false;

//Optimization: Define variables global in main program
*Flag = 0;
switch (CoLi->cowo) {
  case NIMacc:
    if (CoLi->B1!=0) {  //Any element
      TA.B1 = CoLi->B1;
      TA.B2 = CoLi->B2;
      TA.B3 = CoLi->B3;
      TA.Elem = CoLi->Elem;
      TA.Info = 0;
      }
    else {
      //Typified
      cop3ofTA5(&TA,DtPara->TA);
      TA.Elem = CoLi->Elem;
      TA.Info = 0;
      }

    i = 0;
    notFound = true;
    while (i<nbTA && notFound) {
      ++i;
      if (bTA[_AC(i-1,Condmax-1)].TA.Elem==TA.Elem && bTA[i-1].TA.B1==TA.B1 &&
          bTA[i-1].TA.B2==TA.B2 && bTA[i-1].TA.B3==TA.B3) {
        notFound = false;
        Ok = true;
        }
      }
    if (notFound) {
      if (!NIMRMP(&NimVarMp.FlagS,nTeAd,receive.outDisp.RelCopKey,0)) {
        DatErr(0,2311,"NIM     ",NimVarMp.FlagS,pGDispDesRpP->dId,iWVipShort,
               "III","        ",TA,0);
        *Res = irrelevant;
        *Flag = NimVarMp.FlagS;
        Ok = false;
      } else {
        ++nbTA;
        ++i;
        Ok = true;
        bTA[_AC(i-1,Condmax-1)].InfStr = *NimVarMp.pInfStr;
        bTA[i-1].TA = TA;
        bTA[i-1].NoElTy = NimVarMp.NoElTy;
        }
      }

    if (Ok) {
      GETINFO(CoLi->Info,
              NimVarMp.NedePoi[_AC(bTA[_AC(i-1,Condmax-1)].NoElTy,neDim)],
              &bTA[_AC(i-1,Condmax-1)].InfStr,&InfValue,&Leng);
      *Res = InfValue;
      }
    break;

  case DeBlink:
  case PoBlink:
    *Flag = -11;
    break;
  default:
    *Flag = -11;
    break;
  }
}  // 1


//Execution of a DT for display management
static
void          coDtPr(
  int*          Flag,             // O: Flags
  tDtPara*      DtPara)           //IO: Parameter for DT execution
{  // 1
int           i;
int           j;
int           l;
bool          Ok;
bool          ContinueRu;
bool          ContinueCond;
short         ergbau[Condmax];    //Output of function
bool          rufbau[Condmax];    //procedure called
int           FORLIM;
DeTa*         pWTHTY;

for (i = 1; i<=Condmax; ++i)
  ergbau[_AC(i-1,Condmax-1)] = 0;

*Flag = 0;
Ok = true;
AcEx = 0;
if (DtPara->DtNr>cDTKeyMax) {
  err(0,2601,"DTNR    ",DtPara->DtNr,cDTKeyMax,0.0,"III");
  *Flag = 50;
  return;
  }
// 2
if (DeTaPointer[_AC(DtPara->DtNr-1,cDTKeyMax-1)].notTy()) {
  DatErr(0,2316,"DT inva",fDeTa.Flag,pGDispDesRpP->dId,DtPara->DtNr,"III",
         "fDeTa   ",DtPara->TA,0);
  err(0,2609,"DT-type?",DtPara->DtNr,pGDispDesRpP->dId,iWVipShort,"III");
  *Flag = 50;
  Ok = false;
  }

pWTHTY = DeTaPointer[_AC(DtPara->DtNr-1,cDTKeyMax-1)].UU.TY;
if (Ok) {  // 3
  FORLIM = pWTHTY->NrCond;
  for (i = 1; i<=FORLIM; ++i)     //No procedure called yet
    rufbau[_AC(i-1,Condmax-1)] = false;
  i = 0;                          //Counter of rules
  DtPara->NrOkRu = 0;             //Number of fuilled rules
  ContinueRu = true;
  while (i<pWTHTY->NrRu && ContinueRu)  // 4
    {  // 4    Loop over all rules
    ++i;                          //Next rule
    ContinueCond = true;
    j = 0;                        //Counter of conditions
    l = (i-1)*pWTHTY->NrCond+pWTHTY->RuOffset;
    while (j<pWTHTY->NrCond && ContinueCond)  // 5
      {  // 5    Loop over all conditions
      ++j;                        //Next condition
      if (pWTHTY->uD.RU[_AC(l+j-1,DtLength-1)]!=irrelevant)
        {  // 6    Determine condition, compare
        if (!rufbau[_AC(j-1,Condmax-1)])  //Determine condition-pointer
          {  // 7    Call supply procedure
          beBau(Flag,DtPara,&pWTHTY->uD.cl[_AC(j-1,Condmax-1)],&ergbau[j-1]);
          if (*Flag!=0) {  // 8    Error in supply procedure
            ContinueRu = false;
            ContinueCond = false;
            //error
            }  // 8
          else
            rufbau[_AC(j-1,Condmax-1)] = true;
          }  // 7
        if (pWTHTY->uD.RU[_AC(l+j-1,DtLength-1)]!=ergbau[_AC(j-1,Condmax-1)])
              //If condition is not fuilled
                ContinueCond = false;
        //Then go on with next rule
        }  // 6
      }
    //Loop over all conditions
    if (ContinueCond) {           // 5
      ContinueRu = false;
      DtPara->NrOkRu = 1;         //Number of fuilled rules
      DtPara->AcEx = pWTHTY->uD.AC[_AC(i+pWTHTY->AcOffset-1,DtLength-1)];
          //Action connected with the rule
      }  // 5
    //Rule fuilled
    }
  //Loop ober all rules
  }  // 3
if (DtPara->AcEx==0) {
  *Flag = 50;
  DatErr(0,2337,"coDtPr  ",DtPara->DtNr,DtPara->AcEx,*Flag,"III","       ",
         DtPara->TA,0);
  }
}  // 1


//prepare and insert B123 into DIdL`s linked list
static
void          prepB123DId(
  tB123         SearchB123,
  tSegId        DIdL)
{
tB123Rec*     newb123DId;

newb123DId = (tB123Rec*)malloc_chk(sizeof(tB123Rec));
newb123DId->B123Key = SearchB123;
newb123DId->nextB123DId = 0;

if (nextB123DId==0) {
  newb123DId->nextB123DId = pDIdTbl[_AC(DIdL-1,maxDId-1)];
  pDIdTbl[DIdL-1] = newb123DId;
} else {
  newb123DId->nextB123DId = nextB123DId->nextB123DId;
  nextB123DId->nextB123DId = newb123DId;
  }
nextB123DId = newb123DId;
}


//less or equal
//greater than
static
bool          LE(
  tB123*        key1,
  tB123*        key2)
{
bool          LE_RTN;
LE_RTN = true;

#if L_ENDIAN
if (key1->U1.B1>key2->U1.B1 ||
    key1->U1.B1==key2->U1.B1 && key1->U1.B2>key2->U1.B2)
#else
if (key1->U0.B1B2>key2->U0.B1B2)
#endif
  return false;
if (key1->U0.B1B2==key2->U0.B1B2 && key1->U1.B3>key2->U1.B3)
  return false;

return true;
}

static
bool          GT(
  tB123*        key1,
  tB123*        key2)
{
return !LE(key1,key2);
}


//search for B123 in DIdL`s linked list
static
bool          findB123DId(
  tB123         SearchB123,
  tSegId        DIdL)
{
bool          findB123DId_RTN;



if (pDIdTbl[_AC(DIdL-1,maxDId-1)]==0 ||
    GT(&pDIdTbl[DIdL-1]->B123Key,&SearchB123))
  {                               //greater than
  nextB123DId = 0;
  findB123DId_RTN = false;
  goto _L0;
  }
nextB123DId = pDIdTbl[_AC(DIdL-1,maxDId-1)];
while (nextB123DId->nextB123DId!=0 &&
       LE(&nextB123DId->nextB123DId->B123Key,&SearchB123))
  nextB123DId = nextB123DId->nextB123DId;
if (memcmp(&nextB123DId->B123Key,&SearchB123,sizeof(tB123))==0)
  findB123DId_RTN = true;
else
  findB123DId_RTN = false;

_L0:
return findB123DId_RTN;
}  //findB123DId


//compare two B123 keys
static
int           CpB123(
  tB123*        key1,
  tB123*        key2)             //O: 0 if key1B123=key1B123
{
//   1 if key1B123>key1B123
//  -1 if key1B123<key1B123
#if L_ENDIAN
if (key1->U1.B1>key2->U1.B1 ||
    key1->U1.B1==key2->U1.B1 && key1->U1.B2>key2->U1.B2)
#else
if (key1->U0.B1B2>key2->U0.B1B2)
#endif
  return 1;
else {
#if L_ENDIAN
if (key1->U1.B1<key2->U1.B1 ||
    key1->U1.B1==key2->U1.B1 && key1->U1.B2<key2->U1.B2)
#else
  if (key1->U0.B1B2<key2->U0.B1B2)
#endif
    return -1;
  else {
    if (key1->U1.B3>key2->U1.B3)
      return 1;
    else {
      if (key1->U1.B3<key2->U1.B3)
        return -1;
      else {
        return 0;

        }
      }
    }
  }
}  //CpB123

// ---------------------------------------------------------------------
// get size of medisp record
//
static inline int getMeDispSize
( int         iEntries
) {

return (int)(sizeof(meDisp) -
             (cMaxMeDisp-iEntries)*sizeof(tVarList));
}
// ---------------------------------------------------------------------

//find B1, B2, B3 in B123Medisp
static
bool          findB123MeDisp(
  tB123*        keyB123L,
  int*          iB123)
{
bool          findB123MeDisp_RTN;


int           u;
int           o;
int           M;
int           CpResult;

M = 0;
//binary search for B123 record
*iB123 = 0;
CpResult = -99;
if (current_RelCopKey!=0 && keyB123L->U1.B1==B1_JGC) {
  if (DbcOpen(&fmeDisp,current_RelCopKey)) {
    vMeDispL.B1 = keyB123L->U1.B1;
    vMeDispL.B2 = keyB123L->U1.B2;
    vMeDispL.B3 = keyB123L->U1.B3;
    if (DbRead(&fmeDisp,0)) {
      M = cNB123Medisp+1;         // dummy for Study mode
      B123Medisp[_AC(M-1,cNB123Medisp)].rp = &vMeDispL;
      CpResult = 0;
    } else {
      if (fmeDisp.Flag!=NoK)
        err(current_RelCopKey,3399,"DBR_Medi",
            rii(fmeDisp.Flag,fmeDisp.Fv.FlagS),
            rii(keyB123L->U1.B1,keyB123L->U1.B2),
            rii(keyB123L->U1.B3,fmeDisp.Fv.Cop),"TTT");
      }
    if (!DbcOpen(&fmeDisp,0))
      err(0,3304,"DbcOpen ",rs("MEDI"),rs("SP  "),
          rii(fmeDisp.Flag,fmeDisp.Fv.FlagS),"ZZT");
  } else
    err(current_RelCopKey,3304,"DbcOpen ",rs("MEDI"),rs("SP  "),
        rii(fmeDisp.Flag,fmeDisp.Fv.FlagS),"ZZT");
} else {
  if (NB123Medisp==0) {
    findB123MeDisp_RTN = false;
    goto _L0;
    }

  u = 1;
  o = NB123Medisp;
  do {
    M = (u+o)/2;
    CpResult = CpB123(keyB123L,&B123Medisp[_AC(M-1,cNB123Medisp)].B123);
        //compare two B123 keys

    if (CpResult>0)               //keyB123L > B123Medisp[M].B123
      u = M+1;
    else {
      o = M-1;
      //keys equal
      }
    } while (CpResult!=0 && u<=o);
  }

if (CpResult==0 && B123Medisp[_AC(M-1,cNB123Medisp)].rp!=0)  //keys equal
  findB123MeDisp_RTN = true;
else
  findB123MeDisp_RTN = false;
*iB123 = M;
_L0:
return findB123MeDisp_RTN;
}  //findB123MeDisp
// Local variables for meDispGen:
struct LCL_meDispGen {
  bool          insert;
  bool          funcErr;
};


static
void          pWMeDisp(
  struct LCL_meDispGen* LNK_meDispGen)
{
//finish and store old meDisp-record

int           i;
int           j;
int           k;
int           iB123;
int           iMe;
tB123         keyB123;
int           FORLIM;
tB123*        pWTHB123;

if (vMeDispL.B1!=0) {
  keyB123.U1.B1 = vMeDispL.B1;
  keyB123.U1.B2 = vMeDispL.B2;
  keyB123.U1.B3 = vMeDispL.B3;
  j = 0;
  k = 1;
  fmeDisp.Rp->varList[0].Ind = 0;
  FORLIM = fmeDisp.Rp->nVar;
  for (i = 2; i<=FORLIM; ++i) {
    if (fmeDisp.Rp->varList[_AC(i-1,cMaxMeDisp-1)].Elem==
        fmeDisp.Rp->varList[_AC(k-1,cMaxMeDisp-1)].Elem)
      ++j;
    else {
      j = 0;
      k = i;
      }
    fmeDisp.Rp->varList[_AC(i-1,cMaxMeDisp-1)].Ind = j;
    }  //for i

  cp(1,1,"pWMeDisp",rs("B123"),vMeDispL.B1*1000.0+vMeDispL.B2,vMeDispL.B3,
     "SII");
  cp(1,1,"pWMeDisp",rs("nVar"),rs("New "),fmeDisp.Rp->nVar-nVarOld,"SSI");
  cp(1,1,"pWMeDisp",rs("nVar"),rs("    "),fmeDisp.Rp->nVar,"SSI");

  fmeDisp.RcS = getMeDispSize(fmeDisp.Rp->nVar);

  if (fmeDisp.Rp->nVar==0) {
    if (findB123MeDisp(&keyB123,&iB123)) {
      free(B123Medisp[_AC(iB123-1,cNB123Medisp)].rp);
      B123Medisp[iB123-1].rp = 0;
      if (cUseMeDispOrig) {
      B123Medisp[iB123-1].B123.U1.MdspChg = true;
      B123MdspChgOK = false;
      } else {
        B123Medisp[iB123-1].B123.U1.MdspChg = false;
        B123MdspChgOK = true;
      }
    } else
      err(0,3368,"MeDispGe",Flag,vMeDispL.B1,vMeDispL.B2,"iii");
  } else {
    if (findB123MeDisp(&keyB123,&iB123)) {
      free(B123Medisp[_AC(iB123-1,cNB123Medisp)].rp);
      B123Medisp[iB123-1].rp = 0;
    } else {
      if (NB123Medisp==0 || iB123==0 ||
          B123Medisp[iB123-1].B123.U0.B1B2!=keyB123.U0.B1B2 ||
          B123Medisp[iB123-1].B123.U1.B3!=keyB123.U1.B3) {
        //insert new B123 into B123Medisp
        ++NB123Medisp;
        if (NB123Medisp>cNB123Medisp) {
          err(0,3360,"MeD Heap",EoD,vMeDispL.B1,
              vMeDispL.B2*1000.0+vMeDispL.B3,"iii");
              //Achtung: eigene err-Nummer
          NB123Medisp = cNB123Medisp;
          }
        iMe = NB123Medisp;
#if L_ENDIAN
        while (iMe>1 && (B123Medisp[iMe-2].B123.U1.B1>keyB123.U1.B1 ||
                         (B123Medisp[iMe-2].B123.U1.B1==keyB123.U1.B1 &&
                          B123Medisp[iMe-2].B123.U1.B2>keyB123.U1.B2) ||
#else
        while (iMe>1 && (B123Medisp[iMe-2].B123.U0.B1B2>keyB123.U0.B1B2 ||
#endif
                         (B123Medisp[iMe-2].B123.U0.B1B2==keyB123.U0.B1B2 &&
                          B123Medisp[iMe-2].B123.U1.B3>keyB123.U1.B3))) {
          B123Medisp[_AC(iMe-1,cNB123Medisp)] = B123Medisp[_AC(iMe-2,
                                                               cNB123Medisp)];
          --iMe;
          }
        B123Medisp[_AC(iMe-1,cNB123Medisp)].B123 = keyB123;
        iB123 = iMe;
        }
      }

    // allocate new buffer for meDisp record
    B123Medisp[_AC(iB123-1,cNB123Medisp)].rp =
      (meDisp*)malloc_chk(fmeDisp.RcS);

    // copy temp buffer to new buffer
    ByIdBy(B123Medisp[iB123-1].rp,fmeDisp.Rp,fmeDisp.RcS);
    if (cUseMeDispOrig) {
    B123Medisp[iB123-1].B123.U1.MdspChg = true;
    B123MdspChgOK = false;
    } else {
      B123Medisp[iB123-1].B123.U1.MdspChg = false;
      B123MdspChgOK = true;
    }
    if (B123Medisp[_AC(iB123-1,cNB123Medisp)].B123.U1.B1!=
        B123Medisp[iB123-1].rp->B1 ||
        B123Medisp[iB123-1].B123.U1.B2!=B123Medisp[iB123-1].rp->B2 ||
        B123Medisp[iB123-1].B123.U1.B3!=B123Medisp[iB123-1].rp->B3) {
      pWTHB123 = &B123Medisp[_AC(iB123-1,cNB123Medisp)].B123;
      err(0,3370,"wrB123  ",pWTHB123->U1.B1,pWTHB123->U1.B2,pWTHB123->U1.B3,
          "iii");
      }

    //update pDIdTbl[]
    if (LNK_meDispGen->insert) {
      if (!findB123DId(B123Medisp[_AC(iB123-1,cNB123Medisp)].B123,
                       vdispDes.dId))
        prepB123DId(B123Medisp[_AC(iB123-1,cNB123Medisp)].B123,vdispDes.dId);
      }
    }
  }  //B1 <> 0
}  //pWMeDisp


static
void          pbMeDispMax(
  struct LCL_meDispGen* LNK_meDispGen)
{
//search greatest entry in bMedisp

bool          greater;
int           j;
int           FORLIM;

iMax = 1;
greater = false;
FORLIM = bMeDisp.nVar;
for (j = 2; j<=FORLIM; ++j) {
  greater = false;
  if (bMeDisp.varList[_AC(j-1,cMaxMeDisp-1)].B1>
      bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B1)
    greater = true;
  else {
    if (bMeDisp.varList[_AC(j-1,cMaxMeDisp-1)].B1==
        bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B1) {
      if (bMeDisp.varList[_AC(j-1,cMaxMeDisp-1)].B2>
          bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B2)
        greater = true;
      else {
        if (bMeDisp.varList[_AC(j-1,cMaxMeDisp-1)].B2==
            bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B2) {
          if (bMeDisp.varList[_AC(j-1,cMaxMeDisp-1)].B3>
              bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B3)
            greater = true;
          else {
            if (bMeDisp.varList[_AC(j-1,cMaxMeDisp-1)].B3==
                bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B3) {
              if (bMeDisp.varList[_AC(j-1,cMaxMeDisp-1)].V.Elem>
                  bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].V.Elem)
                greater = true;
              }
            }
          }
        }
      }
    }
  if (greater)
    iMax = j;
  }  //for j
}  //pbMeDispMax


static
void          pnB123(
  struct LCL_meDispGen* LNK_meDispGen)
{
//Number of entries with B1,B2,B3 in bMeDisp

int           j;
int           FORLIM;

nB123 = 0;
FORLIM = bMeDisp.nVar;
for (j = 1; j<=FORLIM; ++j) {
  if (bMeDisp.varList[_AC(j-1,cMaxMeDisp-1)].B1==vMeDispL.B1 &&
      bMeDisp.varList[j-1].B2==vMeDispL.B2 &&
      bMeDisp.varList[j-1].B3==vMeDispL.B3)
    ++nB123;
  }
}  //pnB123


static
void          pMerge(
  struct LCL_meDispGen* LNK_meDispGen)
{
//put in bMeDisp into Medisp


int           i;
int           M;
tB123         B123Key;
int           FORLIM;

vMeDispL.B1 = 0;
vMeDispL.B2 = 0;
vMeDispL.B3 = 0;
cp(1,1,"pMerge  ",rs("nVar"),bMeDisp.nVar,0.0,"SIB");

FORLIM = bMeDisp.nVar;
for (i = 1; i<=FORLIM; ++i) {
  pbMeDispMax(LNK_meDispGen);     //search greatest entry in bMedisp
  if (bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B1!=vMeDispL.B1 ||
      bMeDisp.varList[iMax-1].B2!=vMeDispL.B2 ||
      bMeDisp.varList[iMax-1].B3!=vMeDispL.B3) {
    pWMeDisp(LNK_meDispGen);      //finish and store old meDisp-record
    vMeDispL.B1 = bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B1;
    vMeDispL.B2 = bMeDisp.varList[iMax-1].B2;
    vMeDispL.B3 = bMeDisp.varList[iMax-1].B3;

    B123Key.U1.B1 = vMeDispL.B1;
    B123Key.U1.B2 = vMeDispL.B2;
    B123Key.U1.B3 = vMeDispL.B3;

    if (findB123MeDisp(&B123Key,&M)) {
      fmeDisp.Flag = 0;
      fmeDisp.RcS = getMeDispSize(B123Medisp[_AC(M-1,cNB123Medisp)].rp->nVar);

      ByIdBy(&vMeDispL,B123Medisp[M-1].rp,fmeDisp.RcS);
    } else {
      fmeDisp.Flag = NoK;
      fmeDisp.Rp->nVar = 0;
      }

    pnB123(LNK_meDispGen);
        //number of entries with B1,B2,B3 in bMeDisp
    iWMeDisp = fmeDisp.Rp->nVar+nB123;
    iRMeDisp = fmeDisp.Rp->nVar;
    nVarOld = fmeDisp.Rp->nVar;

    if (iWMeDisp>cMaxMeDisp) {
      err(0,3367,"MeDispGe",iWMeDisp,vMeDispL.B1,
          vMeDispL.B2*1000.0+vMeDispL.B3,"III");
      goto _L0;
      }
    fmeDisp.Rp->nVar = iWMeDisp;
    }

  while (iRMeDisp>0 && fmeDisp.Rp->varList[_AC(iRMeDisp-1,cMaxMeDisp-1)].Elem>
                       bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].V.Elem) {
    fmeDisp.Rp->varList[_AC(iWMeDisp-1,cMaxMeDisp-1)] =
      fmeDisp.Rp->varList[_AC(iRMeDisp-1,cMaxMeDisp-1)];
    --iWMeDisp;
    --iRMeDisp;
    }
  bMeDisp.varList[iMax-1].V.dId = vdispDes.dId;
  fmeDisp.Rp->varList[_AC(iWMeDisp-1,cMaxMeDisp-1)] =
    bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].V;
  bMeDisp.varList[iMax-1] = bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)];
  --bMeDisp.nVar;
  --iWMeDisp;

  }  //for i

pWMeDisp(LNK_meDispGen);          //finish and store old meDisp-record
_L0: ;
}  //pMerge


static
void          pBTA(
  struct LCL_meDispGen* LNK_meDispGen)
{
//Buffering TA's intermediately

int           j;
int           k;
bool          found;
int           FORLIM;
CondLine*     pWTHj;

FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
for (j = 1; j<=FORLIM; ++j) {
  pWTHj = &DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[j-1];
  //with CL
  if (nbTA==0) {
    bTAcrmd[0].TA.B1 = pWTHj->B1;
    bTAcrmd[0].TA.B2 = pWTHj->B2;
    bTAcrmd[0].TA.B3 = pWTHj->B3;
    bTAcrmd[0].TA.Elem = pWTHj->Elem;
    bTAcrmd[0].TA.Info = pWTHj->Info;
    infoS = pWTHj->Info;
    CleSet(bTAcrmd[0].bInfoSet,tiinfoSet);
    AddSet(bTAcrmd[0].bInfoSet,infoS,tiinfoSet);
    nbTA = 1;
  } else {
    found = false;
    k = 1;
    while (k<=nbTA && !found) {
      if (pWTHj->Elem==bTAcrmd[_AC(k-1,Condmax-1)].TA.Elem &&
          pWTHj->B3==bTAcrmd[k-1].TA.B3 && pWTHj->B2==bTAcrmd[k-1].TA.B2 &&
          pWTHj->B1==bTAcrmd[k-1].TA.B1) {
        found = true;
        infoS = pWTHj->Info;
        AddSet(bTAcrmd[_AC(k-1,Condmax-1)].bInfoSet,infoS,tiinfoSet);
        }
      ++k;
      }
    if (!found) {
      ++nbTA;
      bTAcrmd[_AC(nbTA-1,Condmax-1)].TA.B1 = pWTHj->B1;
      bTAcrmd[nbTA-1].TA.B2 = pWTHj->B2;
      bTAcrmd[nbTA-1].TA.B3 = pWTHj->B3;
      bTAcrmd[nbTA-1].TA.Elem = pWTHj->Elem;
      bTAcrmd[nbTA-1].TA.Info = pWTHj->Info;
      infoS = pWTHj->Info;
      CleSet(bTAcrmd[_AC(nbTA-1,Condmax-1)].bInfoSet,tiinfoSet);
      AddSet(bTAcrmd[_AC(nbTA-1,Condmax-1)].bInfoSet,infoS,tiinfoSet);
      }
    }
  }  //for j
}  //pBTA


static
void          pWbMeDisp(
  struct LCL_meDispGen* LNK_meDispGen)
{
//Entries in bMedisp for typified and 1:1-variable

int           j;
int           k;
int           FORLIM;

pmeVar = &pMeOut->varDes[_AC(meVarInd-1,vipShL-1)];

nbTA = 0;
for (k = 1; k<=Condmax; ++k) {
  CleSet(bTAcrmd[_AC(k-1,Condmax-1)].bInfoSet,tiinfoSet);
  bTAcrmd[k-1].bDSG = false;
  bTAcrmd[k-1].bAckP = false;
  }

DtNr = pmeVar->deTaNA;
if (DtNr!=0) {
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    goto _L0;
    }
  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notTy()) {
    DatErr(0,2315,"DT inval",meVarInd,pGDispDesRpP->dId,DtNr,"III","CoDeTa  ",
           TA,0);
    errfmt(0,2315,"DT inval","pWbMeDisp: invalid CoDeta (deTaNA): DtNr/dId/meVarInd: %d/%d/%d"
                 ,DtNr,pGDispDesRpP->dId,meVarInd);
    goto _L0;
    }
  FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
  for (j = 1; j<=FORLIM; ++j) {
    if (InSet(pmeVar->proMode,(int)deTaTyp,sProMode)) {  //typisiert
      DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[_AC(j-1,Condmax-1)].B1 =
        pmeVar->taMeVar.uCmb.sTa4.B1;
      DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[_AC(j-1,Condmax-1)].B2 =
        pmeVar->taMeVar.uCmb.sTa4.B2;
      DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[_AC(j-1,Condmax-1)].B3 =
        pmeVar->taMeVar.uCmb.sTa4.B3;
      }
    }

  pBTA(LNK_meDispGen);            //TA's zwischenpuffern
  }  //DtNr <> 0

DtNr = pmeVar->deTaNF;
if (DtNr!=0) {
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    goto _L0;
    }
  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notTy()) {
    DatErr(0,2315,"DT inval",meVarInd,pGDispDesRpP->dId,DtNr,"III","CoDeTa  ",
           TA,0);
    errfmt(0,2315,"DT inval","pWbMeDisp: invalid CoDeta (deTaNF): DtNr/dId/meVarInd: %d/%d/%d"
                     ,DtNr,pGDispDesRpP->dId,meVarInd);
    goto _L0;
    }

  FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
  for (j = 1; j<=FORLIM; ++j) {
    if (InSet(pmeVar->proMode,(int)deTaTyp,sProMode)) {  //typisiert
      DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[_AC(j-1,Condmax-1)].B1 =
        pmeVar->taMeVar.uCmb.sTa4.B1;
      DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[_AC(j-1,Condmax-1)].B2 =
        pmeVar->taMeVar.uCmb.sTa4.B2;
      DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[_AC(j-1,Condmax-1)].B3 =
        pmeVar->taMeVar.uCmb.sTa4.B3;
      }
    }
  pBTA(LNK_meDispGen);            //TA's zwischenpuffern
  }  //DtNr <> 0
FORLIM = nbTA;
for (j = 1; j<=FORLIM; ++j) {
  bTAcrmd[_AC(j-1,Condmax-1)].TA.Info = 1;
  if (bTAcrmd[j-1].TA.Elem==pmeVar->taMeVar.uCmb.sTa4.Elem &&
      bTAcrmd[j-1].TA.B3==pmeVar->taMeVar.uCmb.sTa4.B3 &&
      bTAcrmd[j-1].TA.B2==pmeVar->taMeVar.uCmb.sTa4.B2 &&
      bTAcrmd[j-1].TA.B1==pmeVar->taMeVar.uCmb.sTa4.B1 &&
      pmeVar->taMeVar.Info>=1 && pmeVar->taMeVar.Info<=Infomax) {
    bTAcrmd[_AC(j-1,Condmax-1)].TA.Info = pmeVar->taMeVar.Info;
    if (InSet(pmeVar->proMode,(int)dispSelG,sProMode))
      bTAcrmd[_AC(j-1,Condmax-1)].bDSG = true;
    if (InSet(pmeVar->proMode,(int)ackPerm,sProMode))
      bTAcrmd[_AC(j-1,Condmax-1)].bAckP = true;
    }
  }

FORLIM = nbTA;
for (j = 1; j<=FORLIM; ++j) {
  ++bMeDisp.nVar;
  bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].B1 =
    bTAcrmd[_AC(j-1,Condmax-1)].TA.B1;
  bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].B2 =
    bTAcrmd[_AC(j-1,Condmax-1)].TA.B2;
  bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].B3 =
    bTAcrmd[_AC(j-1,Condmax-1)].TA.B3;
  bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.Elem =
    bTAcrmd[_AC(j-1,Condmax-1)].TA.Elem;
  bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.dId = 0;
  CleSet(bMeDisp.varList[bMeDisp.nVar-1].V.prM,tPrMSet);
  if (bTAcrmd[_AC(j-1,Condmax-1)].bDSG)
    AddSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.prM,(int)dSG,
           tPrMSet);
  if (bTAcrmd[_AC(j-1,Condmax-1)].bAckP)
    AddSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.prM,(int)AckP,
           tPrMSet);
  bMeDisp.varList[bMeDisp.nVar-1].V.Info = bTAcrmd[_AC(j-1,Condmax-1)].TA.Info;
  bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.Ind = 0;
  bMeDisp.varList[bMeDisp.nVar-1].V.meDispI = meVarInd;

  memcpy(bMeDisp.varList[bMeDisp.nVar-1].V.infoSet,
         bTAcrmd[_AC(j-1,Condmax-1)].bInfoSet,sizeof(tinfoSet));
  }
_L0: ;
}  //pWbMeDisp

void GetInfoSet(const tMeVar* meVar, int *nEntries, tTA* taArray, tinfoSet* infoSetArray, int arraysize)
{
int           j;
int           FORLIM;
struct LCL_meDispGen  l_meDispGen;

l_meDispGen.insert = false;
l_meDispGen.funcErr = false;
*nEntries = 0;
nbTA = 0;

for (j = 0; j<Condmax; ++j) {
  CleSet(bTAcrmd[j].bInfoSet,tiinfoSet);
  }

DtNr = meVar->deTaNA;
if (DtNr!=0) {
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    goto _L0;
    }
  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notTy()) {
    errfmt(0,2315,"DT inval","GetInfoSet: invalid CoDeTa (deTaNA): DtNr: %d",DtNr);
    goto _L0;
    }
  FORLIM = DeTaPointer[DtNr-1].UU.TY->NrCond;
  for (j = 1; j<=FORLIM; ++j) {
    if (InSet(meVar->proMode,(int)deTaTyp,sProMode)) {  //typisiert
      DeTaPointer[DtNr-1].UU.TY->uD.cl[_AC(j-1,Condmax-1)].B1 =
        meVar->taMeVar.uCmb.sTa4.B1;
      DeTaPointer[DtNr-1].UU.TY->uD.cl[j-1].B2 =
        meVar->taMeVar.uCmb.sTa4.B2;
      DeTaPointer[DtNr-1].UU.TY->uD.cl[j-1].B3 =
        meVar->taMeVar.uCmb.sTa4.B3;
      }
    }
   pBTA(&l_meDispGen);            //TA's zwischenpuffern
  }  //DtNr <> 0

DtNr = meVar->deTaNF;
if (DtNr!=0) {
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    goto _L0;
    }
  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notTy()) {
    errfmt(0,2315,"DT inval","GetInfoSet: invalid CoDeTa (deTaNF): DtNr: %d",DtNr);
    goto _L0;
    }
  FORLIM = DeTaPointer[DtNr-1].UU.TY->NrCond;
  for (j = 1; j<=FORLIM; ++j) {
    if (InSet(meVar->proMode,(int)deTaTyp,sProMode)) {  //typisiert
      DeTaPointer[DtNr-1].UU.TY->uD.cl[_AC(j-1,Condmax-1)].B1 =
        meVar->taMeVar.uCmb.sTa4.B1;
      DeTaPointer[DtNr-1].UU.TY->uD.cl[j-1].B2 =
        meVar->taMeVar.uCmb.sTa4.B2;
      DeTaPointer[DtNr-1].UU.TY->uD.cl[j-1].B3 =
        meVar->taMeVar.uCmb.sTa4.B3;
      }
    }
  pBTA(&l_meDispGen);            //TA's zwischenpuffern
  }  //DtNr <> 0

if (nbTA > arraysize) {
  // error
  nbTA = arraysize;
  }

for (j = 0; j<nbTA; ++j) {
  taArray[*nEntries].B1 = bTAcrmd[j].TA.B1;
  taArray[*nEntries].B2 = bTAcrmd[j].TA.B2;
  taArray[*nEntries].B3 = bTAcrmd[j].TA.B3;
  taArray[*nEntries].Elem = bTAcrmd[j].TA.Elem;
  SetSet(infoSetArray[*nEntries], bTAcrmd[j].bInfoSet, tinfoSet);
  (*nEntries)++;
  }
_L0: ;
}

static
void          pWbMeDispEl(
  struct LCL_meDispGen* LNK_meDispGen)
{
//Entry in bMedisp for elementary variable

int           j;
int           FORLIM;

pmeVar = &pMeOut->varDes[_AC(meVarInd-1,vipShL-1)];

//read invar -> determine TA

if (!INREAD(&fInputVar,pGDispDesRpP->dId,
            pVipShort->Data[_AC(meVarInd-1,vipShL-1)].gid,
            &rinVar)) {
  err(0,3364,"pWbMeDis",pGDispDesRpP->dId,
      pVipShort->Data[_AC(meVarInd-1,vipShL-1)].gid,
      0,"iib");
  LNK_meDispGen->funcErr = true;
  goto _L0;
  }
if (rinVar.Info.vipVar!=meVar) {
  err(0,3365,"MeDispGe",pGDispDesRpP->dId,
      pVipShort->Data[_AC(meVarInd-1,vipShL-1)].gid,
      0,"iib");
  LNK_meDispGen->funcErr = true;
  goto _L0;
  }

if (rinVar.Info.uVipV.smeVa.meTa.Info<1 ||
    rinVar.Info.uVipV.smeVa.meTa.Info>Infomax) {
  DatErr(0,2319,"MeDispGe",pGDispDesRpP->dId,
         pVipShort->Data[_AC(meVarInd-1,vipShL-1)].gid,
         0,"IIB","        ",
         rinVar.Info.uVipV.smeVa.meTa,0);
  return;
  }

++bMeDisp.nVar;
bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].B1 =
  rinVar.Info.uVipV.smeVa.meTa.B1;
bMeDisp.varList[bMeDisp.nVar-1].B2 = rinVar.Info.uVipV.smeVa.meTa.B2;
bMeDisp.varList[bMeDisp.nVar-1].B3 = rinVar.Info.uVipV.smeVa.meTa.B3;
bMeDisp.varList[bMeDisp.nVar-1].V.Elem = rinVar.Info.uVipV.smeVa.meTa.Elem;
bMeDisp.varList[bMeDisp.nVar-1].V.dId = 0;
CleSet(bMeDisp.varList[bMeDisp.nVar-1].V.prM,tPrMSet);
if (InSet(pmeVar->proMode,(int)dispSelG,sProMode))
  AddSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.prM,(int)dSG,
         tPrMSet);
if (InSet(pmeVar->proMode,(int)ackPerm,sProMode))
  AddSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.prM,(int)AckP,
         tPrMSet);
bMeDisp.varList[bMeDisp.nVar-1].V.Info = rinVar.Info.uVipV.smeVa.meTa.Info;
bMeDisp.varList[bMeDisp.nVar-1].V.Ind = 0;
bMeDisp.varList[bMeDisp.nVar-1].V.meDispI = meVarInd;

//read DT`s, determine info set
if (pmeVar->deTaNA==0) {
  infoS = rinVar.Info.uVipV.smeVa.meTa.Info;
  CleSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.infoSet,tinfoSet);
  AddSet(bMeDisp.varList[bMeDisp.nVar-1].V.infoSet,infoS,tinfoSet);
} else {
  DtNr = pmeVar->deTaNA;
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    --bMeDisp.nVar;
    goto _L0;
    }

  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notEl()) {
    DatErr(0,2315,"DT inval",meVarInd,pGDispDesRpP->dId,DtNr,"III","CoDeTaEl",
           TA,0);
    errfmt(0,2315,"DT inval","pWbMeDispEl: invalid CoElDeta (deTaNA): DtNr/dId/meVarInd: %d/%d/%d"
                             ,DtNr,pGDispDesRpP->dId,meVarInd);

    --bMeDisp.nVar;
    goto _L0;
    }

  CleSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.infoSet,tinfoSet);
  FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El->NrInfo;
  for (j = 1; j<=FORLIM; ++j) {
    infoS = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El->Info[_AC(j-1,
                                                                 Infomax-1)];
    AddSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.infoSet,infoS,
           tinfoSet);
    }
  }

if (pVipShort->Data[_AC(meVarInd-1,vipShL-1)].vipAct==vipActINteger ||
    pVipShort->Data[meVarInd-1].vipAct==vipActREal || pmeVar->deTaNF==0) {
  infoS = rinVar.Info.uVipV.smeVa.meTa.Info;
  AddSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.infoSet,infoS,
         tinfoSet);
} else {
  DtNr = pmeVar->deTaNF;
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    --bMeDisp.nVar;
    goto _L0;
    }

  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notEl()) {
    DatErr(0,2315,"DT inval",meVarInd,pGDispDesRpP->dId,DtNr,"III","CoDeTaEl",
           TA,0);
    errfmt(0,2315,"DT inval","pWbMeDispEl: invalid CoElDeta (deTaNF): DtNr/dId/meVarInd: %d/%d/%d"
                                 ,DtNr,pGDispDesRpP->dId,meVarInd);
    --bMeDisp.nVar;
    goto _L0;
    }

  FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El->NrInfo;
  for (j = 1; j<=FORLIM; ++j) {
    infoS = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El->Info[_AC(j-1,
                                                                 Infomax-1)];
    AddSet(bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)].V.infoSet,infoS,
           tinfoSet);
    }
  }
_L0: ;
}  //pWbMeDispEl


static
void          storeIntoVMedispL()
{
fmeDisp.RcS = getMeDispSize(fmeDisp.Rp->nVar);
ByIdBy(&vMeDispL,fmeDisp.Rp,fmeDisp.RcS);
fmeDisp.Rp = &vMeDispL;
}


static
void          pDelete(
  struct LCL_meDispGen* LNK_meDispGen)
{
//Delete bMeDisp-entries from Medisp

//j,
int           iW;
int           iR;
int           iB123;
int           FORLIM;

//loop through whole meDisp
vMeDispL.B1 = 0;
vMeDispL.B2 = 0;
vMeDispL.B3 = 0;

cp(75,1,"pDelete ",rs("dId "),vdispDes.dId,dId,"SII");

while (pDIdTbl[vdispDes.dId-1]!=0) {
  if (findB123MeDisp(&pDIdTbl[_AC(vdispDes.dId-1,maxDId-1)]->B123Key,&iB123)) {
    fmeDisp.Rp = B123Medisp[_AC(iB123-1,cNB123Medisp)].rp;
    storeIntoVMedispL();

    //compress rpMeDisp
    iW = 1;
    FORLIM = fmeDisp.Rp->nVar;
    for (iR = 1; iR<=FORLIM; ++iR) {
      if (fmeDisp.Rp->varList[_AC(iR-1,cMaxMeDisp-1)].dId!=vdispDes.dId) {
        if (iW!=iR)
          fmeDisp.Rp->varList[_AC(iW-1,cMaxMeDisp-1)] =
            fmeDisp.Rp->varList[_AC(iR-1,cMaxMeDisp-1)];
        ++iW;
        }
      }
    fmeDisp.Rp->nVar = iW-1;
    pWMeDisp(LNK_meDispGen);      //finish and store old meDisp-record

    }  //if findB123MeDisp
  else
    cp(76,1,"pDelet ?",vdispDes.dId,
       pDIdTbl[_AC(vdispDes.dId-1,maxDId-1)]->B123Key.U0.B1B2,
       pDIdTbl[vdispDes.dId-1]->B123Key.U1.B3,"III");

  //delete B123DId entry in PDIdTbl[]
  nextB123DId = pDIdTbl[_AC(vdispDes.dId-1,maxDId-1)];
  pDIdTbl[vdispDes.dId-1] = pDIdTbl[vdispDes.dId-1]->nextB123DId;
  free(nextB123DId);
  }  //while pDIdTbl[vdispDes.dId]

fmeDisp.Rp = &vMeDispL;
}  //pDelete


//Create relation meDisp out of dispDes und inVar
//insert:  boolean
static
void          meDispGen(
  bool          insert_)
{
struct LCL_meDispGen V;


int           i;
dispDes*      pWTHpGDispDesRpP;
tVipShortP*   pWTHpVipShort;
tMeOut*       pWTHpMeOut;
int           FORLIM;


V.insert = insert_;
V.funcErr = false;

if (current_RelCopKey!=0)
  return;
fmeDisp.Rp = &vMeDispL;

if (V.insert) {
  pWTHpGDispDesRpP = pGDispDesRpP;
  //with pGDispDesRpP^
  bMeDisp.nVar = 0;
  cp(1,1,"meDispGe",rs("ins "),rs("dId "),pWTHpGDispDesRpP->dId,"SSI");

  pWTHpVipShort = pVipShort;
  pWTHpMeOut = pMeOut;
  //with pVipShort
  if (pWTHpMeOut->nMeOut!=pWTHpVipShort->dataL) {
    err(0,3711,"MeDispGe",pWTHpVipShort->dId,pWTHpMeOut->nMeOut,
        pWTHpVipShort->dataL,"iii");
    goto _L98;
    }
  FORLIM = pWTHpVipShort->dataL;
  for (i = 1; i<=FORLIM; ++i) {
    meVarInd = i;
    if (InSet(pWTHpMeOut->varDes[_AC(meVarInd-1,vipShL-1)].proMode,
              (int)deTaEl,sProMode)) {
      pWbMeDispEl(&V);            //entry in bMedisp
      if (V.funcErr)
        goto _L98;
    } else
      pWbMeDisp(&V);              //entries in bMedisp
    }
  pMerge(&V);                     //put in bMeDisp into Medisp
  }
//insert display into meDisp

else {                            //Delete bMeDisp-entries from Medisp
  cp(1,1,"meDispGe",rs("del "),rs("dId "),vdispDes.dId,"SSI");
  pDelete(&V);
  }

_L98: ;
}  //meDispGen


static
void          pWMeDispC(
  struct LCL_meDispGen* LNK_meDispGen)
{
//finish and store old meDisp-record

if (!vMeDispL.B1) return;
if (!vMeDispL.nVar) return;

  fmeDisp.RcS = getMeDispSize(fmeDisp.Rp->nVar);
  if (DbWrite(&fmeDisp,0));
  else if (DbInsert(&fmeDisp,0));
  else err(current_RelCopKey,2357,"meDispGC",
           rii(vMeDispL.B1,vMeDispL.B2),rii(vMeDispL.B3,0),fmeDisp.Flag,"tti");

}

static
void          pMergeC(
  struct LCL_meDispGen* LNK_meDispGen)
{
//put in bMeDisp into Medisp


int           i;
int           M;
tB123         B123Key;
int           FORLIM;

vMeDispL.B1 = 0;
vMeDispL.B2 = 0;
vMeDispL.B3 = 0;
cp(1,1,"pMerge  ",rs("nVar"),bMeDisp.nVar,0.0,"SIB");

FORLIM = bMeDisp.nVar;
for (i = 1; i<=FORLIM; ++i) {
  pbMeDispMax(LNK_meDispGen);     //search greatest entry in bMedisp
  if (bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B1!=vMeDispL.B1 ||
      bMeDisp.varList[iMax-1].B2!=vMeDispL.B2 ||
      bMeDisp.varList[iMax-1].B3!=vMeDispL.B3) {
    pWMeDispC(LNK_meDispGen);      //finish and store old meDisp-record
    vMeDispL.B1 = bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].B1;
    vMeDispL.B2 = bMeDisp.varList[iMax-1].B2;
    vMeDispL.B3 = bMeDisp.varList[iMax-1].B3;

    if (DbRead(&fmeDisp,0));
    else {
      fmeDisp.Flag = NoK;
      fmeDisp.Rp->nVar = 0;
      }

    pnB123(LNK_meDispGen);
        //number of entries with B1,B2,B3 in bMeDisp
    iWMeDisp = fmeDisp.Rp->nVar+nB123;
    iRMeDisp = fmeDisp.Rp->nVar;
    nVarOld = fmeDisp.Rp->nVar;

    if (iWMeDisp>cMaxMeDisp) {
      err(0,3367,"MeDispGe",iWMeDisp,vMeDispL.B1,
          vMeDispL.B2*1000.0+vMeDispL.B3,"III");
      goto _L0;
      }
    fmeDisp.Rp->nVar = iWMeDisp;
    }

  while (iRMeDisp>0 && fmeDisp.Rp->varList[_AC(iRMeDisp-1,cMaxMeDisp-1)].Elem>
                       bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].V.Elem) {
    fmeDisp.Rp->varList[_AC(iWMeDisp-1,cMaxMeDisp-1)] =
      fmeDisp.Rp->varList[_AC(iRMeDisp-1,cMaxMeDisp-1)];
    --iWMeDisp;
    --iRMeDisp;
    }
  bMeDisp.varList[iMax-1].V.dId = vdispDes.dId;
  fmeDisp.Rp->varList[_AC(iWMeDisp-1,cMaxMeDisp-1)] =
    bMeDisp.varList[_AC(iMax-1,cMaxMeDisp-1)].V;
  bMeDisp.varList[iMax-1] = bMeDisp.varList[_AC(bMeDisp.nVar-1,cMaxMeDisp-1)];
  --bMeDisp.nVar;
  --iWMeDisp;

  }  //for i

pWMeDispC(LNK_meDispGen);          //finish and store old meDisp-record
_L0: ;
}  //pMerge



//Create relation meDisp out of dispDes und inVar
static
void          meDispGenC(
  )
{
struct LCL_meDispGen V;


int           i;
dispDes*      pWTHpGDispDesRpP;
tVipShortP*   pWTHpVipShort;
tMeOut*       pWTHpMeOut;
int           FORLIM;


V.funcErr = false;

if (current_RelCopKey==0)
  return;
fmeDisp.Rp = &vMeDispL;

  pWTHpGDispDesRpP = pGDispDesRpP;
  //with pGDispDesRpP^
  bMeDisp.nVar = 0;
  cp(1,1,"meDispGC",rs("ins "),rs("dId "),pWTHpGDispDesRpP->dId,"SSI");

  pWTHpVipShort = pVipShort;
  pWTHpMeOut = pMeOut;
  //with pVipShort
  if (pWTHpMeOut->nMeOut!=pWTHpVipShort->dataL) {
    err(0,3711,"MeDispGC",pWTHpVipShort->dId,pWTHpMeOut->nMeOut,
        pWTHpVipShort->dataL,"iii");
    goto _L98;
    }
  FORLIM = pWTHpVipShort->dataL;
  for (i = 1; i<=FORLIM; ++i) {
    meVarInd = i;
    if (InSet(pWTHpMeOut->varDes[_AC(meVarInd-1,vipShL-1)].proMode,
              (int)deTaEl,sProMode)) {
      pWbMeDispEl(&V);            //entry in bMedisp
      if (V.funcErr)
        goto _L98;
    } else
      pWbMeDisp(&V);              //entries in bMedisp
    }
  pMergeC(&V);                     //put in bMeDisp into Medisp

_L98: ;
}  //meDispGen


static
void          pMedispC()
{
//write medisp records that are relvant for JGCD into relation copy

int           i;
int           FORLIM;
meDisp*       pWTHRp;


if (!DbcOpen(&fmeDisp,receive.rMedispC.RelCopKey)) {
  err(receive.rMedispC.RelCopKey,3304,"DbcOpen ",rs("MEDI"),rs("SP "),
      rii(fmeDisp.Flag,fmeDisp.Fv.FlagS),"ZZT");
  return;
  // reset medisp in Study first

  }

DbDelete(&fmeDisp,DelAllRc);
FORLIM = NB123Medisp;
for (i = 1; i<=FORLIM; ++i) {
  if (B123Medisp[_AC(i-1,cNB123Medisp)].B123.U1.B1==B1_JGC &&
      B123Medisp[i-1].rp!=0) {
    fmeDisp.Rp = B123Medisp[_AC(i-1,cNB123Medisp)].rp;
    fmeDisp.RcS = getMeDispSize(fmeDisp.Rp->nVar);
    if (!DbWrite(&fmeDisp,ActPage)) {
      if (!DbInsert(&fmeDisp,ActPage)) {
        pWTHRp = fmeDisp.Rp;
        err(receive.rMedispC.RelCopKey,3360,"MeDispGe",fmeDisp.Flag,
            pWTHRp->B1,pWTHRp->B2*1000.0+pWTHRp->B3,"iii");
        }
      }
    }
  }

fmeDisp.Rp = &vMeDispL;

if (!DbcOpen(&fmeDisp,0))
  err(0,3304,"DbcOpen ",rs("MEDI"),rs("SP  "),
      rii(fmeDisp.Flag,fmeDisp.Fv.FlagS),"ZZT");

}  //pMedispC


static
void          pDMSDId()
{
//dId changed by DMS

int           j;
trMmiDir*     pWTHj;
int           sbflag;

/**t*bf*Achtung!!! Bei Bildaendrung muessen alle Eintraege des Bildes in
bFlash geloescht werden (automatisches Quittieren??). Aber Vorsicht: Die
Variablenindizes und x,y sind im geaenderten Bild vielleicht auch
geaendert!! Wenn das Problem geloest ist: Grabner kann die
SumSignD-Abfrage aus actm eliminieren)*/

cp(1,1,"DMSDId  ",receive.DMSDId.dId,0.0,0.0,"III");
if (receive.DMSDId.dId==0)
  {  //all updating files are set invalid (decision table changed)
  CleSet(okActFileSet,tActFileSet);
  CleSet(HISActFileSet,tActFileSet);
  pClearCoDeTa();
  pFillCoDeTa();
  createNew = cQuickDIdGen;
  sendBVipSH(0,0);                //send all stored vipShort
  while (bCAF!=0)                 //delete bCAF entries
    delCAF(bCAF);
  for (j = 1; j<=cMaxComput; ++j) {
    if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
      pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];
      CleSet(pWTHj->mmiActFileSet,tActFileSet);
      if (pWTHj->mmiState==updateOk)
        pWTHj->mmiState = doupdate;
      }
    }  //for j
  RebuildDynMeDisp();
  }
else {
  if (receive.DMSDId.dId>maxDId) {
    err(0,2603,"maxDId ",receive.DMSDId.dId,maxDId,0.0,"III");
    goto _L0;
    }

#if SUBS_SHHIS
  bDIdHIS.did = receive.DMSDId.dId;
  GetTim(&bDIdHIS.Sec,&bDIdHIS.mSec);
  vdispDes.dId = receive.DMSDId.dId;
  if (!DbRead(&fDispDes.Flag,RpOnPage))
    bDIdHIS.SubRequest = sr_BUF_KIND_DID_DEL;
  else
    bDIdHIS.SubRequest = sr_BUF_KIND_DID_MOD;
  if (!(MyOpMode == opmTraining) &&
      !McTransmit(&sbflag,McOwnLevel(),s_HIS_TOPR,true,
                  sizeof(tDIdHIS),&bDIdHIS,0,0,0))
    err(0,3222,"SENDTOPR",ri(sbflag),ri(bDIdHIS.did),0,"IIB");
#endif

  SubSet(okActFileSet,receive.DMSDId.dId,tActFileSet);
  SubSet(HISActFileSet,receive.DMSDId.dId,tActFileSet);
  AddSet(ActFileSet,receive.DMSDId.dId,tActFileSet);  //check in pRDispDesC
  sendBVipSH(0,0);                //send all stored vipShort
  if (findBDId(receive.DMSDId.dId,0,bCAF))  //delete bCAF entries
    delCAF(nextBDId);
  for (j = 1; j<=cMaxComput; ++j) {
    if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
      pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];
      SubSet(pWTHj->mmiActFileSet,receive.DMSDId.dId,tActFileSet);
      if (pWTHj->mmiState==updateOk)
        pWTHj->mmiState = doupdate;
      }
    }  //for j

  //work around: reselect dId, because in jumper coloring the
  //reselection by program woman is sometimes missing
  if (receive.U1.Header1==obMEDI && receive.U1.Header2==jDMSDId)
    AddSet(ReSelDidSet,receive.DMSDId.dId,tActFileSet);

  }
_L0:
SbCancel(&sbF,ClockId);
Rastint.Sys.Min = 0;
Rastint.Sys.Sec = 3;
SbAlarm(&sbF,MyComputId,saMeactF,obMEDI,sizeof(tCheckAf),&rCheckAF,ClockId,
        &Rastint,SbAfter,SbIntern);
}  //pDMSDId
//reselect changed dIds


static
void          pReSelect()
{
const char* const fname = "ReSelect";

int           Flag;
int           j;
int           kSeg;
int           iActivChan;
int           iChan;
int           iArInd;
trMmiDir*     pWTHj;
int           FORLIM2;

SoutDisp.Header1 = obDIMA;
SoutDisp.Header2 = joutDisp;
SoutDisp.AckPara.AckCont = AckNo;
for (j = 1; j<=cMaxComput; ++j) {
  if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
    pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];
    //mmiDir[j]
    if (pWTHj->nActivChan>0) {
      iChan = 1;
      iActivChan = 0;
      while (iChan<=cMaxChan && iActivChan<pWTHj->nActivChan) {
        if (pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]!=0) {
          for (iArInd = 0; iArInd<=3; ++iArInd) {
            if (InSet(AllDidSet,
                      pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->area[_AC(iArInd,
                                                                      3)].dId,
                      tActFileSet)) {
              SoutDisp.AckPara.Comput = j;
              SoutDisp.RelCopKey = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->
                                   RelCopKey;
              SoutDisp.chan = iChan;
              SoutDisp.area = iArInd;
              SoutDisp.dId =
                pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->area[_AC(iArInd,3)].dId;
              SoutDisp.digr = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->digr;
              SoutDisp.newDisp = false;
              SoutDisp.sMod = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->sMod;
              SoutDisp.CondSel = true;
              if (!SbTransm(&Flag,MyComputId,saMeactF,obMEDI,sizeof(toutDisp),
                            &SoutDisp.Header1))
                err(0,3211,fname,Flag,saMeactF,obMEDI,"IAO");
              if (DEBUGlevel(100))
                cp(0,100,fname,SoutDisp.AckPara.Comput,SoutDisp.dId
                               ,SoutDisp.chan,"CII");
              }
            if (pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->area[_AC(iArInd,3)].
                segmented) {
              FORLIM2 = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->segDisp.nSeg;
              for (kSeg = 2; kSeg<=FORLIM2; ++kSeg) {
                if (InSet(AllDidSet,
                          pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->segDisp.
                          segments[_AC(kSeg-2,cMaxSeg-2)].dId,tActFileSet)) {
                  SoutDisp.AckPara.Comput = j;
                  SoutDisp.RelCopKey = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->
                                       RelCopKey;
                  SoutDisp.chan = iChan;
                  SoutDisp.area = iArInd;
                  SoutDisp.dId = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->
                                 segDisp.segments[_AC(kSeg-2,cMaxSeg-2)].dId;
                  SoutDisp.digr = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->digr;
                  SoutDisp.newDisp = false;
                  SoutDisp.sMod = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]->sMod;
                  SoutDisp.CondSel = true;
                  if (!SbTransm(&Flag,MyComputId,saMeactF,obMEDI,
                                sizeof(toutDisp),&SoutDisp.Header1))
                    err(0,3211,fname,Flag,saMeactF,obMEDI,"IAO");
                  if (DEBUGlevel(100))
                    cp(0,100,fname,SoutDisp.AckPara.Comput,SoutDisp.dId
                                   ,SoutDisp.chan,"CII");
                  }
                }
              }
            }  //for iArind
          ++iActivChan;
          }  //if VChan[iChan]
        ++iChan;
        }
      }  //if nActivChan
    }
  }  //for j
}  //pReSelect
//process changes blocks/elements


//determine all dIds for changed B1-B3 using meDisp
static
bool          FindAllDids()
{
bool          FindAllDids_RTN;
tB123         B123Key;
int           M;
int           ind;
tSegId        i_did;
int           FORLIM;

FindAllDids_RTN = false;
CleSet(AllDidSet,tActFileSet);

B123Key.U1.B1 = receive.DmsDataP.Modi.uOId.sAllo.bb1;
B123Key.U1.B2 = receive.DmsDataP.Modi.uOId.sAllo.bb2;
B123Key.U1.B3 = receive.DmsDataP.Modi.uOId.sAllo.bb3;
if (findB123MeDisp(&B123Key,&M)) {
  fmeDisp.Rp = B123Medisp[_AC(M-1,cNB123Medisp)].rp;
  FORLIM = fmeDisp.Rp->nVar;
  for (ind = 1; ind<=FORLIM; ++ind)
    AddSet(AllDidSet,fmeDisp.Rp->varList[_AC(ind-1,cMaxMeDisp-1)].dId,
           tActFileSet);
  }

SetSet(AllDidSetSave,AllDidSet,tActFileSet);
if (!NulSet(AllDidSet,tActFileSet))
  FindAllDids_RTN = true;

if (DEBUGlevel(200)) {
  i_did = 1;
  M = 0;
  while (i_did<maxDId) {
    if (InSet(AllDidSet,i_did,tActFileSet)) {
      cp(0,200,"FindAllD",
         rii(receive.DmsDataP.Modi.uOId.sAllo.bb1,
             receive.DmsDataP.Modi.uOId.sAllo.bb2),
         receive.DmsDataP.Modi.uOId.sAllo.bb3,i_did,"TII");
      ++M;
      }
    ++i_did;
    }
  cp(0,200,"FindAllD",
     rii(receive.DmsDataP.Modi.uOId.sAllo.bb1,
         receive.DmsDataP.Modi.uOId.sAllo.bb2),
     receive.DmsDataP.Modi.uOId.sAllo.bb3,M,"TII");
  }  //if debug
fmeDisp.Rp = &vMeDispL;
return FindAllDids_RTN;
}  //FindAllDids


static
void          Process_DBA_Changes()
{
const char* const name = "Dba_Chan";

typedef CL_SETDEF(torder_set, lastof_torder);
typedef CL_SETDEF(tordmode_set, lastof_tordmode);
tSegId        l_did;              // variable of loop
tTA3          l_ta3;

l_did = 0;
(void)memset(&l_ta3,0,sizeof(l_ta3));
if (DEBUGlevel(100))
  cp(0,100,name,receive.DmsDataP.Header1,(int)receive.DmsDataP.Header2,
     rii((int)receive.DmsDataP.Modi.order,(int)receive.DmsDataP.Modi.ordmode),
     "OIT");

if (receive.DmsDataP.Header2==datmod &&
    (receive.DmsDataP.Modi.order==ELEMENTS ||
     receive.DmsDataP.Modi.order==DBA_BLOCK) &&
    (receive.DmsDataP.Modi.ordmode==DBAord_modify ||
     receive.DmsDataP.Modi.ordmode==DBAord_define ||
     receive.DmsDataP.Modi.ordmode==DBAord_delete)) {
  if (FindAllDids()) {
    do {
      ++l_did;
      if (InSet(AllDidSet,l_did,tActFileSet)) {
        if (DEBUGlevel(100))
          cp(0,100,name,
             rii(receive.DmsDataP.Modi.uOId.sAllo.bb1,
                 receive.DmsDataP.Modi.uOId.sAllo.bb2),
             receive.DmsDataP.Modi.uOId.sAllo.bb3,l_did,"TII");
        receive.DMSDId.dId = l_did;
            //receive-record fuer pDMSDid versorge n
        pDMSDId();                //delete found dId from ActFile
        vdispDes.dId = l_did;     //store dId for meDispGen
        meDispGen(false);         //delete  dId in all medisp records
        SubSet(AllDidSet,l_did,tActFileSet);
        }
      } while (!(NulSet(AllDidSet,tActFileSet) || l_did>=maxDId));
    }

  SetSet(AllDidSet,AllDidSetSave,tActFileSet);
  pReSelect();                    //reselect changed dIds

  // check for TA in dynamic registered variables
  l_ta3.B1 = receive.DmsDataP.Modi.uOId.sAllo.bb1;
  l_ta3.B2 = receive.DmsDataP.Modi.uOId.sAllo.bb2;
  l_ta3.B3 = receive.DmsDataP.Modi.uOId.sAllo.bb3;
  RebuildDynMeDispOnTA(l_ta3);

  }  //if datmod
else {
  //H2 <> datmod (datnlock) : irrelevant for medi
  //order <> Elements,Block : will served by other interfaces
  if (DEBUGlevel(100)) {
    cp(0,100,name,rs("Noth"),rs("ing "),rs("Do  "),"DDD");

    }
  }
}  //Process_DBA_Changes


static
void          pDMSAFGr()
{
//attribut or figure group changed by DMS

int           j;
trMmiDir*     pWTHj;


for (j = 1; j<=cMaxComput; ++j) {
  if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
    pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];
    CleSet(pWTHj->mmiActFileSet,tActFileSet);
    if (pWTHj->mmiState==updateOk)
      pWTHj->mmiState = doupdate;
    }
  }  //for j

// Force update of all dynamic variables
DynVarForceUpdateAll();
}  //pDMSAFGr


//find dID entry in bFlash,bCAF,bStay
static
bool          findBDId(
  tSegId        bDId,
  tRelCopKey    bRelCopKey,
  tbDId*        bSelect)
{
bool          findBDId_RTN;
if (bSelect==0 || bSelect->AFDId>bDId ||
    bSelect->AFDId==bDId && bSelect->RelCopKey>bRelCopKey) {
  nextBDId = 0;
  findBDId_RTN = false;
  goto _L0;
  }
nextBDId = bSelect;
while (nextBDId->nextBDId!=0 &&
       (nextBDId->nextBDId->AFDId<bDId ||
        (nextBDId->nextBDId->AFDId==bDId &&
         nextBDId->nextBDId->RelCopKey<=bRelCopKey)))
  nextBDId = nextBDId->nextBDId;

if (nextBDId->AFDId==bDId && nextBDId->RelCopKey==bRelCopKey)
  findBDId_RTN = true;
else
  findBDId_RTN = false;
_L0:
return findBDId_RTN;
}  //findBDId


//find dispI entry in bFlash,bCAF,bStay
static
bool          findBDispI(
  tSegId        bDispI)           //key: Index of variable in dispDes
{
bool          findBDispI_RTN;
if (nextBDId->nextBDispI==0 || nextBDId->nextBDispI->AFDispI>bDispI) {
  nextBDispI = 0;
  findBDispI_RTN = false;
  goto _L0;
  }
nextBDispI = nextBDId->nextBDispI;
while (nextBDispI->nextBDispI!=0 && nextBDispI->nextBDispI->AFDispI<=bDispI)
  nextBDispI = nextBDispI->nextBDispI;
if (nextBDispI->AFDispI==bDispI)
  findBDispI_RTN = true;
else
  findBDispI_RTN = false;
_L0:
return findBDispI_RTN;
}  //findBDispI


//find TA entry in bFlash
// Local variables for findBTA:
struct LCL_findBTA {
  tbTA*         bFTA;             //key: TA entry in bFlash
};


static
bool          TAGreater(
  tbTA*         nextTA,
  struct LCL_findBTA* LNK_findBTA)
{
bool          TAGreater_RTN;
TAGreater_RTN = false;
if (nextTA->ta.B1>LNK_findBTA->bFTA->ta.B1)
  return true;
if (nextTA->ta.B1==LNK_findBTA->bFTA->ta.B1) {
  if (nextTA->ta.B2>LNK_findBTA->bFTA->ta.B2)
    return true;
  if (nextTA->ta.B2==LNK_findBTA->bFTA->ta.B2) {
    if (nextTA->ta.B3>LNK_findBTA->bFTA->ta.B3)
      return true;
    if (nextTA->ta.B3==LNK_findBTA->bFTA->ta.B3) {
      if (nextTA->ta.Elem>LNK_findBTA->bFTA->ta.Elem)
        return true;
      if (nextTA->ta.Elem==LNK_findBTA->bFTA->ta.Elem) {
        if (nextTA->ta.Info>LNK_findBTA->bFTA->ta.Info)
          return true;
        }
      }
    }
  }
return false;
}


static
bool          findBTA(
  tbTA*         bFTA_)
{
struct LCL_findBTA V;
bool          findBTA_RTN;

V.bFTA = bFTA_;
if (nextBDispI->nextBTA==0 || TAGreater(nextBDispI->nextBTA,&V)) {
  nextBTA = 0;
  findBTA_RTN = false;
  goto _L0;
  }
nextBTA = nextBDispI->nextBTA;
while (nextBTA->nextBTA!=0 && !TAGreater(nextBTA->nextBTA,&V))
  nextBTA = nextBTA->nextBTA;

if (nextBTA->ta.B1==V.bFTA->ta.B1 && nextBTA->ta.B2==V.bFTA->ta.B2 &&
    nextBTA->ta.B3==V.bFTA->ta.B3 && nextBTA->ta.Elem==V.bFTA->ta.Elem &&
    nextBTA->ta.Info==V.bFTA->ta.Info)
  findBTA_RTN = true;
else
  findBTA_RTN = false;
_L0:
return findBTA_RTN;
}  //findBTA


//find variable entry in bFlash
//*t**bf* wird vorlaeufig nicht verwendet
static
bool          findBVar(
  tSegId        bDId,             //key1: Display identification
  tRelCopKey    bRelCopKey,       //key2: Copy
  tSegId        bDispI)           //key3: Index of variable in dispDes
{
bool          findBVar_RTN;

findBVar_RTN = false;
if (findBDId(bDId,bRelCopKey,bFlash)) {  //find dID entry in bFlash
  if (findBDispI(bDispI))         //find dispI entry in bFlash
    return true;
  }
return false;
}  //findBVar


//dispose TA entry in bFlash
static
void          disposeTA(
  tPtB*         PtB)              //buffer pointers
{
if (PtB->precBTA==0) {
  PtB->nextBDispI->nextBTA = PtB->nextBTA->nextBTA;
  free(PtB->nextBTA);
  PtB->nextBTA = PtB->nextBDispI->nextBTA;
} else {
  PtB->precBTA->nextBTA = PtB->nextBTA->nextBTA;
  free(PtB->nextBTA);
  PtB->nextBTA = PtB->precBTA->nextBTA;
  }
}


//dispose DispI entry in bFlash
static
void          disposeDispI(
  tPtB*         PtB)              //buffer pointers
{
if (PtB->precBDispI==0) {
  PtB->nextBDId->nextBDispI = PtB->nextBDispI->nextBDispI;
  free(PtB->nextBDispI);
  PtB->nextBDispI = PtB->nextBDId->nextBDispI;
} else {
  PtB->precBDispI->nextBDispI = PtB->nextBDispI->nextBDispI;
  free(PtB->nextBDispI);
  PtB->nextBDispI = PtB->precBDispI->nextBDispI;
  }
}


//dispose dId entry in bFlash
static
void          disposeDId(
  tPtB*         PtB)              //buffer pointers
{
if (PtB->precBDId==0) {
  PtB->bSelect = PtB->nextBDId->nextBDId;
  free(PtB->nextBDId);
  PtB->nextBDId = PtB->bSelect;
} else {
  PtB->precBDId->nextBDId = PtB->nextBDId->nextBDId;
  free(PtB->nextBDId);
  PtB->nextBDId = PtB->precBDId->nextBDId;
  }
}


//dispose all bFlash entries for dId
static
void          disposebDId(
  tSegId        dDId,             //Display identification
  tRelCopKey    dRelCopKey,       //RelCopKey
  tbDId**       bSelect)          //pointer to buffer (bFlash,bStay)
{
tbDId*        precBDId;           //precursor dId entry in bCAF
tbDispI*      precBDispI;         //precursor dispI entry in bFlash
tbTA*         precBTA;            //precursor TA entry in bFlash
bool          found;


if (!findBDId(dDId,dRelCopKey,*bSelect)) {  //find dID in bFlash,bStay
  nextBDId = 0;
  return;
  }

nextBDispI = nextBDId->nextBDispI;
precBDispI = 0;
while (nextBDispI!=0) {
  nextBTA = nextBDispI->nextBTA;
  precBTA = 0;
  while (nextBTA!=0) {
    precBTA = nextBTA;
    nextBTA = nextBTA->nextBTA;
    free(precBTA);
    }
  precBDispI = nextBDispI;
  nextBDispI = nextBDispI->nextBDispI;
  free(precBDispI);
  }

nextBDId->nextBDispI = 0;

//find precursor of dId
found = false;
nextBDId = *bSelect;
precBDId = 0;
while (nextBDId!=0 && !found) {
  if (nextBDId->AFDId==dDId && nextBDId->RelCopKey==dRelCopKey)
    found = true;
  else {
    precBDId = nextBDId;
    nextBDId = nextBDId->nextBDId;
    }
  }

//pointer from precursor to successor of deleted dId
if (precBDId==0) {
  *bSelect = nextBDId->nextBDId;
  free(nextBDId);
} else {
  precBDId->nextBDId = nextBDId->nextBDId;
  free(nextBDId);
  }

nextBDId = 0;
}  //disposebDId


//prepare dId in selected buffer
static
void          prepbDId(
  tbDId**       bSelect,
  tSegId        bDId,
  tRelCopKey    bRelCopKey)
{
tbDId*        newbDId;            //new dId entry in bFlash or bCAF or bStay
newbDId = (tbDId*)malloc_chk(sizeof(tbDId));
newbDId->AFDId = bDId;
newbDId->RelCopKey = bRelCopKey;
newbDId->nextBDispI = 0;
if (nextBDId==0) {
  newbDId->nextBDId = *bSelect;
  *bSelect = newbDId;
} else {
  newbDId->nextBDId = nextBDId->nextBDId;
  nextBDId->nextBDId = newbDId;
  }
nextBDId = newbDId;
}


//prepare dispI in selected buffer
static
void          prepbDispI(
  tSegId        bDispI)
{
  tbDispI*      lNewbDispI;

  lNewbDispI = (tbDispI*)malloc_chk(sizeof(tbDispI));
  lNewbDispI->AFDispI = bDispI;
// TODO 2: [ppoh} check
  lNewbDispI->gid = 0xffffffff;
  lNewbDispI->fAltNorm = 10001;
  lNewbDispI->aAltNorm = 10001;
  lNewbDispI->aAltNormA = 10001;
  lNewbDispI->aAltNormB = 10001;
  lNewbDispI->aAltNormC = 10001;
  lNewbDispI->fAltFl = 10001;
  lNewbDispI->aAltFl = 10001;
  lNewbDispI->aAltFlA = 10001;
  lNewbDispI->aAltFlB = 10001;
  lNewbDispI->aAltFlC = 10001;
  lNewbDispI->aAltPosFl = -1;
  lNewbDispI->saveAGroup = 0;
  lNewbDispI->nextBTA = 0;
if (nextBDispI==0) {
    lNewbDispI->nextBDispI = nextBDId->nextBDispI;
    nextBDId->nextBDispI = lNewbDispI;
} else {
    lNewbDispI->nextBDispI = nextBDispI->nextBDispI;
    nextBDispI->nextBDispI = lNewbDispI;
    }
  nextBDispI = lNewbDispI;
}


//prepare TA in selected buffer
static
void          prepbTA(
  tbTA*         bFTA)
{
tbTA*         newBTA;             //next TA entry in bFlash,bCAF,bStay
newBTA = (tbTA*)malloc_chk(sizeof(tbTA));
newBTA->ta = bFTA->ta;
newBTA->acknFlash = false;
newBTA->desFlash = false;
newBTA->posFlash = false;
if (nextBTA==0) {
  newBTA->nextBTA = nextBDispI->nextBTA;
  nextBDispI->nextBTA = newBTA;
} else {
  newBTA->nextBTA = nextBTA->nextBTA;
  nextBTA->nextBTA = newBTA;
  }
nextBTA = newBTA;
}


//insert variable/TA entry in bFlash
static
bool          insBTA(
  tSegId        bDId,             //key1: Display identification
  tRelCopKey    bRelCopKey,       //key2: Copy
  tSegId        bDispI,           //key3: Index of variable in dispDes
  tbTA*         bFTA)             //key4: TA entry in bFlash
{
bool          insBTA_RTN;

insBTA_RTN = false;

if (!findBDId(bDId,bRelCopKey,bFlash))  //find dID entry in bFlash
  prepbDId(&bFlash,bDId,bRelCopKey);

if (!findBDispI(bDispI))          //find dispI entry in bFlash
  prepbDispI(bDispI);

if (!findBTA(bFTA))               //find TA entry in bFlash
  prepbTA(bFTA);

if (bFTA->desFlash) {
  insBTA_RTN = true;
  nextBTA->desFlash = true;
  nextBTA->InfVal = bFTA->InfVal;
  nextBTA->comp = bFTA->comp;
  nextBTA->conChan = bFTA->conChan;
  nextBTA->siChan = bFTA->siChan;
  }

if (bFTA->posFlash && !nextBTA->desFlash) {
  insBTA_RTN = true;
  nextBTA->posFlash = true;
  nextBTA->comp = bFTA->comp;
  nextBTA->conChan = bFTA->conChan;
  nextBTA->siChan = bFTA->siChan;
  }

if (bFTA->acknFlash) {
  if (nextBTA->acknFlash)
    return false;
  insBTA_RTN = true;
  nextBTA->acknFlash = true;
  }

return insBTA_RTN;
}  //insBTA


//insert variable/TA entry in bStay
static
bool          insBStay(
  tSegId        bDId,             //key1: Display identification
  tRelCopKey    bRelCopKey,       //key2: Copy
  tSegId        bDispI,           //key3: Index of variable in dispDes
  tbTA*         bStTA)            //key4: TA entry in bStay
{
bool          insBStay_RTN;

insBStay_RTN = false;

if (!findBDId(bDId,bRelCopKey,bStay))  //find dID entry in bStay
  prepbDId(&bStay,bDId,bRelCopKey);

if (!findBDispI(bDispI))          //find dispI entry in bStay
  prepbDispI(bDispI);

if (!findBTA(bStTA)) {            //find TA entry in bStay
  prepbTA(bStTA);
  return true;
  }
return false;
}  //insBStay


//acknowledge TA in bFlash
// Local variables for acknTA:
struct LCL_acknTA {
  tTA*          ackTA;            //key: technological address
  tPtB          PtB;              //buffer pointers
};


static
bool          TAEqual(
  struct LCL_acknTA* LNK_acknTA)
{
if (LNK_acknTA->PtB.nextBTA->ta.B1==LNK_acknTA->ackTA->B1 &&
    LNK_acknTA->PtB.nextBTA->ta.B2==LNK_acknTA->ackTA->B2 &&
    LNK_acknTA->PtB.nextBTA->ta.B3==LNK_acknTA->ackTA->B3 &&
    LNK_acknTA->PtB.nextBTA->ta.Elem==LNK_acknTA->ackTA->Elem &&
    NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)]->uStDc.InfoNorm[_AC(
            LNK_acknTA->PtB.nextBTA->ta.Info-1,Infomax-1)].sUni.AcknInf==
      NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)]->uStDc.InfoNorm[_AC(
            LNK_acknTA->ackTA->Info-1,Infomax-1)].sUni.AcknInf)
  return true;
else
  return false;
}


static
bool          acknTA(
  tRelCopKey    ackRelCopKey,
  tTA*          ackTA_)
{
struct LCL_acknTA V;
bool          acknTA_RTN;

bool          delTA;

V.ackTA = ackTA_;
acknTA_RTN = false;

V.PtB.bSelect = bFlash;
//bFlash loop: 1
V.PtB.nextBDId = V.PtB.bSelect;
V.PtB.precBDId = 0;

while (V.PtB.nextBDId!=0) {
  V.PtB.nextBDispI = V.PtB.nextBDId->nextBDispI;
  V.PtB.precBDispI = 0;
  while (V.PtB.nextBDispI!=0) {
    V.PtB.nextBTA = V.PtB.nextBDispI->nextBTA;
    V.PtB.precBTA = 0;
    while (V.PtB.nextBTA!=0) {
      delTA = false;
      //bFlash loop: 2

      if (TAEqual(&V) && V.PtB.nextBDId->RelCopKey==ackRelCopKey) {
        if (V.PtB.nextBTA->acknFlash==true) {
          acknTA_RTN = true;
          V.PtB.nextBTA->acknFlash = false;
          }
        if (V.PtB.nextBTA->desFlash==false &&
            V.PtB.nextBTA->posFlash==false &&
            V.PtB.nextBTA->acknFlash==false) {
          delTA = true;
          disposeTA(&V.PtB);
          }
        }

      //bFlash loop: 3
      if (!delTA) {
        V.PtB.precBTA = V.PtB.nextBTA;
        V.PtB.nextBTA = V.PtB.nextBTA->nextBTA;
        }
      }
    if (V.PtB.nextBDispI->nextBTA==0)
      disposeDispI(&V.PtB);
    else {
      V.PtB.precBDispI = V.PtB.nextBDispI;
      V.PtB.nextBDispI = V.PtB.nextBDispI->nextBDispI;
      }
    }
  if (V.PtB.nextBDId->nextBDispI==0)
    disposeDId(&V.PtB);
  else {
    V.PtB.precBDId = V.PtB.nextBDId;
    V.PtB.nextBDId = V.PtB.nextBDId->nextBDId;
    }
  }
//bFlash loop: 4

bFlash = V.PtB.bSelect;
return acknTA_RTN;
}  //acknTA


//delete disappearing  TA in bStay
// Local variables for disAppTA:
struct LCL_disAppTA {
  tTA*          dTA;              //key: technological address
  tPtB          PtB;              //buffer pointers
};


static
bool          TAEqual_1(
  struct LCL_disAppTA* LNK_disAppTA)
{
if (LNK_disAppTA->PtB.nextBTA->ta.B1==LNK_disAppTA->dTA->B1 &&
    LNK_disAppTA->PtB.nextBTA->ta.B2==LNK_disAppTA->dTA->B2 &&
    LNK_disAppTA->PtB.nextBTA->ta.B3==LNK_disAppTA->dTA->B3 &&
    LNK_disAppTA->PtB.nextBTA->ta.Elem==LNK_disAppTA->dTA->Elem &&
    LNK_disAppTA->PtB.nextBTA->ta.Info==LNK_disAppTA->dTA->Info)
  return true;
else
  return false;
}


static
bool          disAppTA(
  tRelCopKey    dRelCopKey,
  tTA*          dTA_)
{
struct LCL_disAppTA V;
bool          disAppTA_RTN;

bool          delTA;

V.dTA = dTA_;
disAppTA_RTN = false;

V.PtB.bSelect = bStay;
//bFlash loop: 1
V.PtB.nextBDId = V.PtB.bSelect;
V.PtB.precBDId = 0;

while (V.PtB.nextBDId!=0) {
  V.PtB.nextBDispI = V.PtB.nextBDId->nextBDispI;
  V.PtB.precBDispI = 0;
  while (V.PtB.nextBDispI!=0) {
    V.PtB.nextBTA = V.PtB.nextBDispI->nextBTA;
    V.PtB.precBTA = 0;
    while (V.PtB.nextBTA!=0) {
      delTA = false;
      //bFlash loop: 2

      if (TAEqual_1(&V) && V.PtB.nextBDId->RelCopKey==dRelCopKey) {
        disAppTA_RTN = true;
        delTA = true;
        disposeTA(&V.PtB);
        }

      //bFlash loop: 3
      if (!delTA) {
        V.PtB.precBTA = V.PtB.nextBTA;
        V.PtB.nextBTA = V.PtB.nextBTA->nextBTA;
        }
      }
    if (V.PtB.nextBDispI->nextBTA==0)
      disposeDispI(&V.PtB);
    else {
      V.PtB.precBDispI = V.PtB.nextBDispI;
      V.PtB.nextBDispI = V.PtB.nextBDispI->nextBDispI;
      }
    }
  if (V.PtB.nextBDId->nextBDispI==0)
    disposeDId(&V.PtB);
  else {
    V.PtB.precBDId = V.PtB.nextBDId;
    V.PtB.nextBDId = V.PtB.nextBDId->nextBDId;
    }
  }
//bFlash loop: 4

bStay = V.PtB.bSelect;
return disAppTA_RTN;
}  //disappTA


//insert variable/TA into bFlash
static
bool          insBFlash()
{
bool          insBFlash_RTN;

bool          Ackn;
tTA5*         pWTHTA;
insBFlash_RTN = true;

bFTA.ta.B1 = receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].TA.B1;
bFTA.ta.B2 = receive.rawInMe.Data[iRawInMe-1].TA.B2;
bFTA.ta.B3 = receive.rawInMe.Data[iRawInMe-1].TA.B3;
bFTA.ta.Elem = receive.rawInMe.Data[iRawInMe-1].TA.Elem;
bFTA.ta.Info = receive.rawInMe.Data[iRawInMe-1].TA.Info;


if (receive.U1.Header1!=obMEDI ||
    receive.U1.Header2!=jrawInMe && receive.U1.Header2!=jFlash) {
  if (NimVarMp.NedePoi[_AC(
          receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].NoElTy,neDim)]->
      uStDc.InfoNorm[_AC(bFTA.ta.Info-1,Infomax-1)].sUni.AcknInf==0)
    insBFlash_RTN = false;

  if (!InSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].infoSet,
             NimVarMp.NedePoi[_AC(receive.rawInMe.Data[_AC(iRawInMe-1,
                                      cMaxMess-1)].NoElTy,neDim)]->
             uStDc.InfoNorm[_AC(bFTA.ta.Info-1,Infomax-1)].sUni.AcknInf,
             tinfoSet))
    insBFlash_RTN = false;


  goto _L0;
  }
if (receive.U1.Header2==jrawInMe) {
  bFTA.acknFlash = false;
  bFTA.desFlash = false;
  bFTA.posFlash = false;

  if (receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].statinfo==siChg ||
      receive.rawInMe.Data[iRawInMe-1].statinfo==siAppA) {
    Ackn = true;
    bFTA.acknFlash = true;

    if (NimVarMp.NedePoi[_AC(
            receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].NoElTy,neDim)]->
        uStDc.InfoNorm[_AC(bFTA.ta.Info-1,Infomax-1)].sUni.AcknInf==0)
      Ackn = false;
    else {
      if (!InSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].infoSet,
                 NimVarMp.NedePoi[_AC(receive.rawInMe.Data[_AC(iRawInMe-1,
                                          cMaxMess-1)].NoElTy,neDim)]->
                 uStDc.InfoNorm[_AC(bFTA.ta.Info-1,Infomax-1)].sUni.AcknInf,
                 tinfoSet))
        Ackn = false;

      else {
        if (!insBTA(dId,current_RelCopKey,
                    pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].meDispI,
                    &bFTA))
          Ackn = false;
        else {
          pWTHTA = &receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].TA;

          TA.B1 = pWTHTA->B1;
          TA.B2 = pWTHTA->B2;
          TA.B3 = pWTHTA->B3;
          TA.Elem = pWTHTA->Elem;
          TA.Info = pWTHTA->Info;
          pInfoType();            //Determine info type, read Intyde
          TA.Info = 0;

          if (!NIMRMP(&NimVarMp.FlagS,nTeAd+nLock,current_RelCopKey,0)) {
            err(0,2331,"NIM ",NimVarMp.FlagS,receive.singAck.TA.B3,
                receive.singAck.TA.Elem,"III");
            endOfJob = true;
            longjmp(_JL99,1);     //end of job
            }

          //Set acknowledge bit
          InfVal = 1;
          SETINFO(receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].TA.Info,
                  SubAckn,NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],
                  NimVarMp.pInfStr,&InfVal,&Leng);

          if (!NIMWMP(&NimVarMp.FlagS,0,current_RelCopKey,0)) {
            err(0,2332,"NIM ",NimVarMp.FlagS,receive.singAck.TA.B3,
                receive.singAck.TA.Elem,"III");
            endOfJob = true;
            longjmp(_JL99,1);     //end of job
            }
          }
        }
      }

    if (Ackn==false) {
      insBFlash_RTN = false;
      if (receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].statinfo==siAppA) {
        receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].statinfo = siApp;
        insBFlash_RTN = true;
        }
      }
    }

  if (receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].statinfo==siApp ||
      receive.rawInMe.Data[iRawInMe-1].statinfo==siAppA) {
    if (!insBStay(dId,current_RelCopKey,
                  pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].meDispI,&bFTA))
      insBFlash_RTN = false;
    }

  else {
    if (receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].statinfo==siDisApp)
      disAppTA(current_RelCopKey,
               &receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].TA);
    }
} else {

  if (receive.U1.Header2==jFlash && saveFlash.flash) {
    if (saveFlash.flashMod==desFlash) {
      bFTA.desFlash = true;
      bFTA.posFlash = false;
    } else {
      bFTA.desFlash = false;
      bFTA.posFlash = true;
      }
    bFTA.acknFlash = false;
    bFTA.InfVal = saveFlash.InfVal;
    bFTA.comp = saveFlash.CompId;
    bFTA.conChan = saveFlash.ConChan;
    bFTA.siChan = saveFlash.chan;
    insBTA(dId,current_RelCopKey,
           pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].meDispI,&bFTA);
    }
  }
_L0:
return insBFlash_RTN;

//delete disappearing  TA in bStay
}  //insBFlash


//correct special states in vipShort due to responsibility
static
void          corrSpecialState(
  ComputId      Comput,           //computer for updating
  tVipShortP*   vipShort,         //Buffer for VipShort
  bool          resetNorm)        //reset vipshort to normal state
{
int           i;
bool          chanOk;
bool          acknVarFl;
int           FORLIM;
ttVipShortP*  pWTHi;

// correct old header2 (from DISPDES)
if (vipShort->Header2<vipJobMN)
  vipShort->Header2 += vipJobMN;

/*t*bf*Hier wird eine eventuelle mmi-spezifische Ausgabe
eingebaut.Achtung: in insbflsh ist fuer die Variablen DeTeAr noch nicht
versorgt*/

if (nextBDId!=0) {
  if (corrSSStart || resetNorm) {
    //correct entries in vipShort
    corrSSStart = false;
    nextBDispI = nextBDId->nextBDispI;
    while (nextBDispI!=0) {
      FORLIM = vipShort->dataL;
      for (i = 1; i<=FORLIM; ++i) {
        pWTHi = &vipShort->Data[i-1];
        // [ppoh] Adoption tGID
        // if (pWTHi->X==nextBDispI->x && pWTHi->y==nextBDispI->y) {
        if (pWTHi->gid==nextBDispI->gid) {
          if (nextBDispI->fAltNorm==10001)
            goto _L1;
          if (!resetNorm) {       //*t* for mmi specific flashing: function
            if (nextBDispI->saveAGroup!=0) {
              pWTHi->aGroup = cFlashAGroup;
              if (nextBDispI->aAltPosFl>0 && !nextBDispI->nextBTA->desFlash) {
                corrSSStart = true;
                chanOk = false;
                acknVarFl = false;
                nextBTA = nextBDispI->nextBTA;
                while (nextBTA!=0) {
                  if (nextBTA->posFlash && nextBTA->comp==Comput &&
                      nextBTA->siChan==vipShort->chan)
                    chanOk = true;
                  if (nextBTA->acknFlash)
                    acknVarFl = true;
                  nextBTA = nextBTA->nextBTA;
                  }

                if (chanOk) {
                  pWTHi->aAlt = nextBDispI->aAltPosFl;
                  pWTHi->uvipA.svALG.aAltPhaseA = pWTHi->aAlt;
                  pWTHi->uvipA.svALG.aAltPhaseB = pWTHi->aAlt;
                  pWTHi->uvipA.svALG.aAltPhaseC = pWTHi->aAlt;
                } else {
                  pWTHi->aGroup = nextBDispI->saveAGroup;
                  if (acknVarFl)
                    //pWTHi->aAlt = nextBDispI->aAltFl;
                    set_aAltFl_mph(pWTHi,nextBDispI,C_SET_ALT_VIPSH);
                  else
                    // pWTHi->aAlt = nextBDispI->aAltNorm;
                    set_aAltNorm_mph(pWTHi,nextBDispI,C_SET_ALT_VIPSH);
                  }
              } else {
                  //pWTHi->aAlt = nextBDispI->aAltFl;
                  set_aAltFl_mph(pWTHi,nextBDispI,C_SET_ALT_VIPSH);
                }
            } else
              //pWTHi->aAlt = nextBDispI->aAltFl;
              set_aAltFl_mph(pWTHi,nextBDispI,C_SET_ALT_VIPSH);

            if (pWTHi->vipAct!=vipActCHan) {
              if (pWTHi->vipAct==vipActLFig || pWTHi->vipAct==vipActGFig)
                pWTHi->uvipA.svALG.fAlt = nextBDispI->fAltFl;
              else {
                if (pWTHi->vipAct==vipActINteger)
                  pWTHi->uvipA.svAI.iValue = nextBDispI->iValDes;
                else {
                  if (pWTHi->vipAct==vipActREal)
                    pWTHi->uvipA.svAR.rValue = nextBDispI->rValDes;
                  }
                }
              }
            }
          else {
            //pWTHi->aAlt = nextBDispI->aAltNorm;
            set_aAltNorm_mph(pWTHi,nextBDispI,C_SET_ALT_VIPSH);
            if (nextBDispI->saveAGroup!=0)
              pWTHi->aGroup = nextBDispI->saveAGroup;

            if (pWTHi->vipAct!=vipActCHan) {
              if (pWTHi->vipAct==vipActLFig || pWTHi->vipAct==vipActGFig)
                pWTHi->uvipA.svALG.fAlt = nextBDispI->fAltNorm;
              else {
                if (pWTHi->vipAct==vipActINteger)
                  pWTHi->uvipA.svAI.iValue = nextBDispI->iValNorm;
                else {
                  if (pWTHi->vipAct==vipActREal)
                    pWTHi->uvipA.svAR.rValue = nextBDispI->rValNorm;
                  }
                }
              }
            }
          }
        }
_L1:
      nextBDispI = nextBDispI->nextBDispI;
      }
    }
  /**t*fb*
  or (not corrSSStart)and(responsibility geaendert) *t*bf**/
  }
}  //corrSpecialState


static
void          pAckn()
{
//Acknowledgement to DIMA

if (receive.outDisp.AckPara.AckCont!=AckNo) {
  if (!SbTransm(&sbF,receive.U1.AckPara.Comput,receive.U1.AckPara.Adres,
                receive.U1.AckPara.Object,sizeof(tAcknowl),
                &receive.U1.AckPara.Acknowl))
    err(0,3211,"ACKNOWL ",rs("SBTR"),sbF,receive.U1.AckPara.Adres,"SII");
  }
}  //pAckn

// ---------------------------------------------------------------------
// get net work group attribute alternative
//
static inline short getNetGrAlt
( short       iNetGrN
, short       iDefault            // I : default value if erroneous
, int         iErrNo              // I : error number if > 0
) {

  if (iNetGrN == 0) {
    if (iErrNo > 0)
      errfmt(0,2604,"netgroup"
            ,"getNetGrAlt: dId/NetGrN/VipShIdx: %d/%d/%d"
            ,pVipShort->dId,iNetGrN,iWVipShort);
    return iDefault;
  }

  return (cNetGrAlt+iNetGrN%100);
}
// ---------------------------------------------------------------------
// colouring, elementary variable
//
static void pElNetGrCol
( tInfo       iNeGrInfo           // I : network group info (nede)
, tInfo       iNeGrInfoA          // I : network group info Phase A
, tInfo       iNeGrInfoB          // I : network group info Phase B
, tInfo       iNeGrInfoC          // I : network group info Phase C
) {

NEDE*         pNede;
int           netGrN;             //Number of netgroup
tLength       Leng;

ttVipShortP* pttVipSh = &pVipShort->Data[iWVipShort-1];
  pNede = NimVarMp.NedePoi[_AC(pmeVar->taMeVar.uCmb.sTAns.NoElType,neDim)];

if (pttVipSh->aAlt==99) {
  GETINFO(pNede->NeGrInf,pNede,(tInterStr*)pInfoStr,&netGrN,&Leng);
  pttVipSh->aAlt = getNetGrAlt(netGrN,10000,2604);

#if NETGRP_DASHED
  if (pttVipSh->vipAct==vipActLFig || pttVipSh->vipAct==vipActGFig) {
    GETINFO(pNede->NeGrInf+2,pNede,(tInterStr*)pInfoStr,&netGrN,&Leng);
    pttVipSh->uvipA.svALG.aAltOld = getNetGrAlt(netGrN,pttVipSh->aAlt,0);
    }

#endif

  }

if (!IsMultiPhaseYes) return;

// additional check - should never happen
if (iNeGrInfo != pNede->NeGrInf) {
  errfmt(0,2604,"netgroup","pElNetGrCol: NetwGrNo Error: %d/%d%/%d"
        ,pmeVar->taMeVar.uCmb.sTAns.NoElType
        ,pNede->NeGrInf,iNeGrInfo);

  pttVipSh->uvipA.svALG.aAltPhaseA = -1;
  pttVipSh->uvipA.svALG.aAltPhaseB = -1;
  pttVipSh->uvipA.svALG.aAltPhaseC = -1;
}

if (pttVipSh->uvipA.svALG.aAltPhaseA == 99) {
  GETINFO(iNeGrInfoA,pNede,(tInterStr*)pInfoStr,&netGrN,&Leng);
  pttVipSh->uvipA.svALG.aAltPhaseA = getNetGrAlt(netGrN,-1,0);
}
if (pttVipSh->uvipA.svALG.aAltPhaseB == 99) {
  GETINFO(iNeGrInfoB,pNede,(tInterStr*)pInfoStr,&netGrN,&Leng);
  pttVipSh->uvipA.svALG.aAltPhaseB = getNetGrAlt(netGrN,-1,0);
}
if (pttVipSh->uvipA.svALG.aAltPhaseC == 99) {
  GETINFO(iNeGrInfoC,pNede,(tInterStr*)pInfoStr,&netGrN,&Leng);
  pttVipSh->uvipA.svALG.aAltPhaseC = getNetGrAlt(netGrN,-1,0);
}


}  //pElNetGrCol
// ---------------------------------------------------------------------
// get open line vipshort attribute
//
static inline ByteAbs pGetSemiOpenAttr
( NormElem    iNoElTy             // I : norm element type
, tInfo       iOLStInfo           // I : open line info (nede)
, tInfo       iNetwStInfo         // I : network status info (nede)
) {

  //                      Energized    |  Deenergized
  // -------------------------------------------------
  // T1OpenT2Open     | SO_NotSemiOpen | SO_NotSemiOpen
  // T1ClosedT2Closed | SO_NotSemiOpen | SO_NotSemiOpen
  // T1OpenT2Closed   | SO_OpenT1      | SO_NotSemiOpen
  // T1ClosedT2Open   | SO_OpenT2      | SO_NotSemiOpen

  if (iOLStInfo <= 0) return SO_NotSemiOpen;

  tLength vInfLen;
  int     vInfVal;
  ByteAbs vVipShAttr;

  GETINFO(iOLStInfo,NimVarMp.NedePoi[iNoElTy]
         ,(tInterStr*)pInfoStr,&vInfVal,&vInfLen);

  switch (vInfVal) {
    case T1OpenT2Closed: vVipShAttr = SO_OpenT1; break;
    case T1ClosedT2Open: vVipShAttr = SO_OpenT2; break;
    default: // T1OpenT2Open, T1ClosedT2Closed
      vVipShAttr = SO_NotSemiOpen;
      break;
  }

  if ((vVipShAttr  == SO_NotSemiOpen) ||
      (iNetwStInfo <= 0)) return vVipShAttr;

  // read network status
  GETINFO(iNetwStInfo,NimVarMp.NedePoi[iNoElTy]
         ,(tInterStr*)pInfoStr,&vInfVal,&vInfLen);

  if (vInfVal == spglos) return SO_NotSemiOpen;

  return vVipShAttr;

} // pGetSemiOpenAttr
// ---------------------------------------------------------------------

static
void          pTypNetGrCol()
{
//Colouring, typified an common combined variables

NEDE*         pNede;
int           netGrN;             //Number of netgroup
tLength       Leng;
int           i;
bool          notFound;
ttVipShortP*  pWTHiWVipShort;

pWTHiWVipShort = &pVipShort->Data[iWVipShort-1];
if (pWTHiWVipShort->aAlt==99) {
  i = 0;
  notFound = true;
  while (i<nbTA && notFound) {
    ++i;
    pNede = NimVarMp.NedePoi[_AC(bTA[_AC(i-1,Condmax-1)].NoElTy,neDim)];
    if (pNede->NeGrInf!=0)
      notFound = false;
    }
  if (notFound)
    pWTHiWVipShort->aAlt = 10000;
  else {
    GETINFO(pNede->NeGrInf,pNede,&bTA[_AC(i-1,Condmax-1)].InfStr,&netGrN,
            &Leng);
    pWTHiWVipShort->aAlt = getNetGrAlt(netGrN,10000,2604);

#if NETGRP_DASHED
    if (pWTHiWVipShort->vipAct==vipActLFig ||
        pWTHiWVipShort->vipAct==vipActGFig) {
      GETINFO(pNede->NeGrInf+2,pNede,&bTA[_AC(i-1,Condmax-1)].InfStr,
              &netGrN,&Leng);
      pWTHiWVipShort->uvipA.svALG.aAltOld =
        getNetGrAlt(netGrN,pWTHiWVipShort->aAlt,0);
      }
#endif

    }
  }
}  //pTypNetGrCol
//send status sign to seds
//send status sign to seds
// Local variables for pVarDisplay:
struct LCL_pVarDisplay {
  //Display of one variable

  //processing of variable is interrupted by error
  //return

  int           locInfVal;
  bool          funcErr;
};

// Local variables for initBFlashEl:
struct LCL_initBFlashEl {
  struct LCL_pVarDisplay* LNK_pVarDisplay;
  //insert entries of elem. var.into bFlash

  tinfoSet      infoSet;
  int           jInfo;
  int           infoVMax;
  bool          funcErr1;
};


static
void          getTA(
  struct LCL_initBFlashEl* LNK_initBFlashEl)
{
//dorr: 2Spalten eingerueckt Anfang
//read invar -> determine TA
dispDes*      pWTHpGDispDesRpP;
ttVipShortP*  pWTHmeVarInd;

if (!INREAD(&fInputVar,pGDispDesRpP->dId,
            pVipShort->Data[_AC(iWVipShort-1,vipShL-1)].gid,
            &rinVar)) {
  pWTHpGDispDesRpP = pGDispDesRpP;
  pWTHmeVarInd = &pVipShort->Data[iWVipShort-1];                    // VK01
  err(0,3364,"initBFEl",pWTHpGDispDesRpP->dId,pWTHmeVarInd->gid,
      0,"iib");
  LNK_initBFlashEl->funcErr1 = true;
  goto _L0;
  }
if (rinVar.Info.vipVar!=meVar) {
  pWTHpGDispDesRpP = pGDispDesRpP;
  pWTHmeVarInd = &pVipShort->Data[iWVipShort-1];                    // VK01
  err(0,3365,"initBFEl",pWTHpGDispDesRpP->dId,pWTHmeVarInd->gid,
      0,"iib");
  LNK_initBFlashEl->funcErr1 = true;
  goto _L0;
  }

//provide TA for pInfoType
TA.B1 = rinVar.Info.uVipV.smeVa.meTa.B1;
TA.B2 = rinVar.Info.uVipV.smeVa.meTa.B2;
TA.B3 = rinVar.Info.uVipV.smeVa.meTa.B3;
TA.Elem = rinVar.Info.uVipV.smeVa.meTa.Elem;
TA.Info = rinVar.Info.uVipV.smeVa.meTa.Info;
_L0: ;
}


static
bool          pInfoTypeLoc(
  struct LCL_initBFlashEl* LNK_initBFlashEl)
{
bool          pInfoTypeLoc_RTN;
//Determine info type

pInfoTypeLoc_RTN = true;
CleSet(NimVarTy.RelSet,tsNimRfix);
AddSet(NimVarTy.RelSet,xInfoDef,tsNimRfix);
AddSet(NimVarTy.RelSet,xInTyDe,tsNimRfix);
if (!NIMRTY(&NimVarTy.FlagS,nTeAd+nIn,current_RelCopKey,0))
  return false;
return true;
}


static
void          getInfoSet(
  struct LCL_initBFlashEl* LNK_initBFlashEl)
{
//read DT`s, determine info set

int           j;
int           FORLIM;
LNK_initBFlashEl->infoVMax = 0;

if (pmeVar->deTaNA==0) {
  // check for info = 0
  if (pmeVar->taMeVar.Info==0) {
    err(0,2340,"TA-dtNA ",pmeVar->taMeVar.uCmb.sTAns.NoElType,
        pmeVar->taMeVar.Info,pmeVar->taMeVar.uCmb.sTAns.NimSatz,"iii");
    LNK_initBFlashEl->funcErr1 = true;
    goto _L0;
    }
  infoS = pmeVar->taMeVar.Info;
  CleSet(LNK_initBFlashEl->infoSet,tinfoSet);
  AddSet(LNK_initBFlashEl->infoSet,infoS,tinfoSet);
  LNK_initBFlashEl->infoVMax = infoS;
} else {
  DtNr = pmeVar->deTaNA;
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    LNK_initBFlashEl->funcErr1 = true;
    goto _L0;
    }

  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notEl()) {
    DatErr(0,2315,"DT inval",iWVipShort,pGDispDesRpP->dId,DtNr,"III",
           "CoDeTaEl",TA,0);
    errfmt(0,2315,"DT inval","getInfoSet: invalid CoElDeta (deTaNA): DtNr/dId/iWVipShort: %d/%d/%d"
                             ,DtNr,pGDispDesRpP->dId,iWVipShort);
    LNK_initBFlashEl->funcErr1 = true;
    goto _L0;
    }
  CleSet(LNK_initBFlashEl->infoSet,tinfoSet);
  FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El->NrInfo;
  for (j = 1; j<=FORLIM; ++j) {
    infoS = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El->Info[_AC(j-1,
                                                                 Infomax-1)];
    AddSet(LNK_initBFlashEl->infoSet,infoS,tinfoSet);
    if (infoS>LNK_initBFlashEl->infoVMax)
      LNK_initBFlashEl->infoVMax = infoS;
    }
  }

if (pVipShort->Data[_AC(iWVipShort-1,vipShL-1)].vipAct==vipActINteger ||
    pVipShort->Data[iWVipShort-1].vipAct==vipActREal || pmeVar->deTaNF==0) {
  // check for info = 0
  if (pmeVar->taMeVar.Info==0) {
    err(0,2340,"TA-dtNF ",pmeVar->taMeVar.uCmb.sTAns.NoElType,
        pmeVar->taMeVar.Info,pmeVar->taMeVar.uCmb.sTAns.NimSatz,"iii");
    LNK_initBFlashEl->funcErr1 = true;
    goto _L0;
    }
  infoS = pmeVar->taMeVar.Info;
  AddSet(LNK_initBFlashEl->infoSet,infoS,tinfoSet);
  if (infoS>LNK_initBFlashEl->infoVMax)
    LNK_initBFlashEl->infoVMax = infoS;
} else {
  DtNr = pmeVar->deTaNF;
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    LNK_initBFlashEl->funcErr1 = true;
    goto _L0;
    }
  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notEl()) {
    DatErr(0,2315,"DT inval",iWVipShort,pGDispDesRpP->dId,DtNr,"III",
           "CoDeTaEl",TA,0);
    errfmt(0,2315,"DT inval","getInfoSet: invalid CoElDeta (deTaNF): DtNr/dId/iWVipShort: %d/%d/%d"
                                 ,DtNr,pGDispDesRpP->dId,iWVipShort);
    LNK_initBFlashEl->funcErr1 = true;
    goto _L0;
    }
  FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El->NrInfo;
  for (j = 1; j<=FORLIM; ++j) {
    infoS = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.El->Info[_AC(j-1,
                                                                 Infomax-1)];
    AddSet(LNK_initBFlashEl->infoSet,infoS,tinfoSet);
    if (infoS>LNK_initBFlashEl->infoVMax)
      LNK_initBFlashEl->infoVMax = infoS;
    }
  }
_L0: ;
}


static
bool          insBSt(
  struct LCL_initBFlashEl* LNK_initBFlashEl)
{
bool          insBSt_RTN;
//insert TA into bstay
insBSt_RTN = true;
bFTA.ta.B1 = rinVar.Info.uVipV.smeVa.meTa.B1;
bFTA.ta.B2 = rinVar.Info.uVipV.smeVa.meTa.B2;
bFTA.ta.B3 = rinVar.Info.uVipV.smeVa.meTa.B3;
bFTA.ta.Elem = rinVar.Info.uVipV.smeVa.meTa.Elem;
bFTA.ta.Info = LNK_initBFlashEl->jInfo;
bFTA.acknFlash = false;
bFTA.desFlash = false;
bFTA.posFlash = false;

if (!insBStay(pVipShort->dId,current_RelCopKey,iWVipShort,&bFTA))
  return false;
return true;
}


static
bool          insBFl(
  struct LCL_initBFlashEl* LNK_initBFlashEl)
{
bool          insBFl_RTN;
//insert TA into bFlash
insBFl_RTN = true;
bFTA.ta.B1 = rinVar.Info.uVipV.smeVa.meTa.B1;
bFTA.ta.B2 = rinVar.Info.uVipV.smeVa.meTa.B2;
bFTA.ta.B3 = rinVar.Info.uVipV.smeVa.meTa.B3;
bFTA.ta.Elem = rinVar.Info.uVipV.smeVa.meTa.Elem;
bFTA.ta.Info = LNK_initBFlashEl->jInfo;
bFTA.acknFlash = true;
bFTA.desFlash = false;
bFTA.posFlash = false;

if (!insBTA(pVipShort->dId,current_RelCopKey,iWVipShort,&bFTA))
  return false;
return true;
}


static
void          stSign(
  tstatchange   stInfo,
  struct LCL_initBFlashEl* LNK_initBFlashEl)
{
if (current_RelCopKey!=0)
  return;

bActSign.RelCopKey = 0;
++bActSign.ActSignNu;
bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].B1 =
  rinVar.Info.uVipV.smeVa.meTa.B1;
bActSign.ActSignAr[bActSign.ActSignNu-1].B2 = rinVar.Info.uVipV.smeVa.meTa.B2;
bActSign.ActSignAr[bActSign.ActSignNu-1].B3 = rinVar.Info.uVipV.smeVa.meTa.B3;
if (cMaxTear>1) {
  rNimad.B1   = rinVar.Info.uVipV.smeVa.meTa.B1;
  rNimad.B2   = rinVar.Info.uVipV.smeVa.meTa.B2;
  rNimad.B3   = rinVar.Info.uVipV.smeVa.meTa.B3;
  rNimad.Elem = rinVar.Info.uVipV.smeVa.meTa.Elem;
  if (!DbRead(&fNimad,0)) {
    err(0,2335,"Nimad   ",fNimad.Flag,rNimad.B1,rNimad.B2,"III");
    err(0,2336,"Nimad   ",0,rNimad.B3,rNimad.Elem,"BII");
    bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].TeAr = cMaxTear;
  } else
    bActSign.ActSignAr[bActSign.ActSignNu-1].TeAr = rNimad.TeArea;
} else
  bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].TeAr = 1;
bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].MeCl =
  NimVarTy.pInTyDe->MeldeKlasse;
bActSign.ActSignAr[bActSign.ActSignNu-1].dId = pVipShort->dId;
bActSign.ActSignAr[bActSign.ActSignNu-1].ResSig = false;
bActSign.ActSignAr[bActSign.ActSignNu-1].statinfo = stInfo;

//transmission of record to SEDS
if (bActSign.ActSignNu>=cActSignAr)
  pSendSeds();
}


static
bool          appState(
  struct LCL_initBFlashEl* LNK_initBFlashEl)
{
bool          appState_RTN;
//appearing state
appState_RTN = false;
if (NimVarTy.pInTyDe->KomGeh) {  //appear- / disappear event
  //Is status = 1?
  InfVal = 0;
  if (ASKINFO(LNK_initBFlashEl->jInfo,noSub,
        NimVarMp.NedePoi[_AC(pmeVar->taMeVar.uCmb.sTAns.NoElType,neDim)],
        NimVarMp.pInfStr,&InfVal,&Leng))
    {                             //event is appearing
    if (NimVarTy.pInTyDe->SignStayGr)
      return true;
    }
  }
return false;
}


static
void          initBFlashEl(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
struct LCL_initBFlashEl V;
int           jI;
int           FORLIM;

V.LNK_pVarDisplay = LNK_pVarDisplay;
V.funcErr1 = false;
getTA(&V);                        //read invar -> determine TA
if (V.funcErr1)
  goto _L0;
getInfoSet(&V);                   //read DT`s, determine info set
if (V.funcErr1)
  goto _L0;

FORLIM = V.infoVMax;
for (jI = 1; jI<=FORLIM; ++jI) {
  if (InSet(V.infoSet,jI,tinfoSet)) {
    V.jInfo = jI;
    TA.Info = V.jInfo;
    if (!pInfoTypeLoc(&V))        //Determine info type, read Intyde
      goto _L1;

    if (NimVarMp.NedePoi[_AC(pmeVar->taMeVar.uCmb.sTAns.NoElType,neDim)]->
        uStDc.InfoNorm[_AC(V.jInfo-1,Infomax-1)].sUni.AcknInf!=0)
      {  //acknowledge info
      //Is the bit 'change of state' set?
      NimVarMp.InfVal = 0;
      if (ASKINFO(V.jInfo,SubAckn,
            NimVarMp.NedePoi[_AC(pmeVar->taMeVar.uCmb.sTAns.NoElType,neDim)],
            NimVarMp.pInfStr,&NimVarMp.InfVal,&Leng)) {
        if (InSet(V.infoSet,
                  NimVarMp.NedePoi[_AC(pmeVar->taMeVar.uCmb.sTAns.NoElType,
                                       neDim)]->
                  uStDc.InfoNorm[_AC(V.jInfo-1,Infomax-1)].sUni.AcknInf,
                  tinfoSet) && insBFl(&V) &&
            InSet(pmeVar->proMode,(int)ackPerm,sProMode))
              //insert TA into bFlash
                stSign(siChg,&V);
        //send status sign to seds
        }
      }

    if (appState(&V) && InSet(pmeVar->proMode,(int)dispSelG,sProMode) &&
        insBSt(&V))
          //appearing state
            stSign(siApp,&V);
    //insert TA into bstay
    //send status sign to seds

_L1: ;
    }  //jI in infoSet
  }
_L0: ;
}


static
bool          corrAllInfos(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
bool          corrAllInfos_RTN;
//correct special Info`s of TA4
corrAllInfos_RTN = true;
nextBTA = nextBDispI->nextBTA;
while (nextBTA!=0) {
  //TA4 is not checked; should be equal
  if (nextBTA->acknFlash) {
    LNK_pVarDisplay->locInfVal = InfVal;
    SETINFO(nextBTA->ta.Info,SubAckn,
            NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],pInfoStr,
            &LNK_pVarDisplay->locInfVal,&Leng);
    }

  if (nextBTA->desFlash && InfVal==1) {
    makeDesState = true;
    LNK_pVarDisplay->locInfVal = nextBTA->InfVal;
    SETINFO(nextBTA->ta.Info,noSub,
            NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],pInfoStr,
            &LNK_pVarDisplay->locInfVal,&Leng);
    //Delete disturbance-info
    LNK_pVarDisplay->locInfVal = 0;
    SETINFO(nextBTA->ta.Info,SubFault,
            NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],pInfoStr,
            &LNK_pVarDisplay->locInfVal,&Leng);
    }

  nextBTA = nextBTA->nextBTA;
  }
return true;
}


static
void          detAltEl(
  struct LCL_pVarDisplay* LNK_pVarDisplay
, NormElem    iNoEl               // I : norm element type
, tNimSet     iNimSet             // I : nimset number
)
{
//Determine figure/attribute-alternatve (elementary)

bool          specialState;
ttVipShortP*  pWTHiWVipShort;
tMeVar*       pWTHpmeVar;

if (checkBFl)
  initBFlashEl(LNK_pVarDisplay);
//insert entries of elem. var.into bFlash
pWTHiWVipShort = &pVipShort->Data[iWVipShort-1];
pWTHpmeVar = pmeVar;
//with pVipShort^...
//look for special states
specialState = false;
if (nextBDId!=0) {
  if (findBDispI(iWVipShort))
    specialState = true;
  }
changeFlash = specialState;
if (specialState) {
  nextBDispI->gid = pWTHiWVipShort->gid;
  nextBDispI->saveAGroup = 0;
  InfVal = 0;
  //correct special Info`s of TA4
  corrAllInfos(LNK_pVarDisplay);
  }

//Determine figure-alternative, write it into VipShort and bDispI
if (pWTHpmeVar->deTaNF==0)
  pWTHiWVipShort->uvipA.svALG.fAlt = 1;
else {
  if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[iNoEl],pWTHpmeVar->deTaNF))
    pWTHiWVipShort->uvipA.svALG.fAlt = 10000;  //Alternative not valid
  else
    pWTHiWVipShort->uvipA.svALG.fAlt = AcEx;
  }

/*Determine attribute-alternative, write it into VipShort and
bDispI*/
if (pWTHpmeVar->deTaNA==pWTHpmeVar->deTaNF)
  pWTHiWVipShort->aAlt = pWTHiWVipShort->uvipA.svALG.fAlt;
else {
  if (pWTHpmeVar->deTaNA==0)
    pWTHiWVipShort->aAlt = 1;
  else {
    if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[iNoEl],pWTHpmeVar->deTaNA))
      pWTHiWVipShort->aAlt = 10000;  //Alternative not valid
    else
      pWTHiWVipShort->aAlt = AcEx;
    }
  }

#if NETGRP_DASHED
pWTHiWVipShort->uvipA.svALG.aAltOld = pWTHiWVipShort->aAlt;
#endif

/* preset phase attributes */
pWTHiWVipShort->uvipA.svALG.aAltPhaseA = -1;
pWTHiWVipShort->uvipA.svALG.aAltPhaseB = -1;
pWTHiWVipShort->uvipA.svALG.aAltPhaseC = -1;

pWTHiWVipShort->uvipA.svALG.semiOpenCombi    = SO_NotSemiOpen;
pWTHiWVipShort->uvipA.svALG.semiOpenPhase[0] = SO_NotSemiOpen;
pWTHiWVipShort->uvipA.svALG.semiOpenPhase[1] = SO_NotSemiOpen;
pWTHiWVipShort->uvipA.svALG.semiOpenPhase[2] = SO_NotSemiOpen;

// network colouring
if (InSet(pWTHpmeVar->proMode,(int)netGrCol,sProMode))
  pElNetGrCol(NoElMPhInfo[iNoEl].NetwGrp
             ,NoElMPhInfo[iNoEl].NeGrPhA
             ,NoElMPhInfo[iNoEl].NeGrPhB
             ,NoElMPhInfo[iNoEl].NeGrPhC
             );

if (IsMultiPhaseYes) {
  tDoMPHproc swProc;

  bool goOn = getMPhSwitchPhase(iNoEl,iNimSet,&swProc);
    /*
     * now we know that some multi phase infos need to be processed,
     * - the normelement has a referenced phase subinfo
     * - the element type is not ganged
     */

  /* check if decision table combines the network group / status */
  if (goOn) {
    (void)getNetGrStDeTaUsage(pWTHpmeVar->deTaNA,iNoEl,&swProc);
    goOn = (swProc.deTaNetwGr || swProc.detaNetwSt);
  }

  tLength vLen;

  if (goOn) {
    int vInAssymS = 0;
    int vInAssymG = 0;

    if (iNoEl == neSwitch) {
      // for neSwitch Assym bit is used for switching status
      // and not for topology, let's always assume its assymetric
      vInAssymS = 1; vInAssymG = 1;
    } else {
      if (NoElMPhInfo[iNoEl].NeStAssym > 0)
        GETINFO(NoElMPhInfo[iNoEl].NeStAssym,NimVarMp.NedePoi[iNoEl]
               ,(tInterStr*)pInfoStr,&vInAssymS,&vLen);

      if (NoElMPhInfo[iNoEl].NeGrAssym > 0)
        GETINFO(NoElMPhInfo[iNoEl].NeGrAssym,NimVarMp.NedePoi[iNoEl]
               ,(tInterStr*)pInfoStr,&vInAssymG,&vLen);
    }

    if ((vInAssymS == 0) && (vInAssymG == 0)) {
      /*
       * synchron operation, set relevant phase alternative to
       * phase alternative
       *
       */
      if ((swProc.doNSt_A) || (swProc.doNGr_A))
        pWTHiWVipShort->uvipA.svALG.aAltPhaseA = pWTHiWVipShort->aAlt;
      if ((swProc.doNSt_B) || (swProc.doNGr_B))
        pWTHiWVipShort->uvipA.svALG.aAltPhaseB = pWTHiWVipShort->aAlt;
      if ((swProc.doNSt_C) || (swProc.doNGr_C))
        pWTHiWVipShort->uvipA.svALG.aAltPhaseC = pWTHiWVipShort->aAlt;

      goOn = false;
    }
  }

  if (goOn) {
    tLength   grLen,stLen;
    int       grInf,stInf,vInf;

    /* save netwstat info */
    GETINFO(NoElMPhInfo[iNoEl].NetwStat,NimVarMp.NedePoi[iNoEl]
           ,(tInterStr*)pInfoStr,&stInf,&stLen);

    /* save netwGrp info */
    GETINFO(NoElMPhInfo[iNoEl].NetwGrp, NimVarMp.NedePoi[iNoEl]
           ,(tInterStr*)pInfoStr,&grInf,&grLen);

    bool netStInfRepl = false;    // netst info replaced
    bool netGrInfRepl = false;    // netgr info replaced
    bool setPhaseInfo = false;    // phase info set

    /*
     * phase A processing
     */
    if ((swProc.detaNetwSt) && (swProc.doNSt_A)) {
      GETINFO(NoElMPhInfo[iNoEl].NeStPhA ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      SETINFO(NoElMPhInfo[iNoEl].NetwStat,noSub
             ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      netStInfRepl = true; setPhaseInfo = true;
    }

    if ((swProc.deTaNetwGr) && (swProc.doNGr_A)) {
      GETINFO(NoElMPhInfo[iNoEl].NeGrPhA ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      SETINFO(NoElMPhInfo[iNoEl].NetwGrp,noSub
             ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      netGrInfRepl = true; setPhaseInfo = true;
    }

    if (setPhaseInfo)
      if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[iNoEl],pWTHpmeVar->deTaNA))
        pWTHiWVipShort->uvipA.svALG.aAltPhaseA = 10000; // alt not valid
      else
        pWTHiWVipShort->uvipA.svALG.aAltPhaseA = AcEx;

    /*
     * phase B processing
     */
    setPhaseInfo = false;
    if ((swProc.detaNetwSt) && (swProc.doNSt_B)) {
      GETINFO(NoElMPhInfo[iNoEl].NeStPhB ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      SETINFO(NoElMPhInfo[iNoEl].NetwStat,noSub
             ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      netStInfRepl = true; setPhaseInfo = true;
    }

    if ((swProc.deTaNetwGr) && (swProc.doNGr_B)) {
      GETINFO(NoElMPhInfo[iNoEl].NeGrPhB ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      SETINFO(NoElMPhInfo[iNoEl].NetwGrp,noSub
             ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      netGrInfRepl = true; setPhaseInfo = true;
    }

    if (setPhaseInfo)
      if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[iNoEl],pWTHpmeVar->deTaNA))
        pWTHiWVipShort->uvipA.svALG.aAltPhaseB = 10000; // alt not valid
      else
        pWTHiWVipShort->uvipA.svALG.aAltPhaseB = AcEx;

    /*
     * phase C processing
     */
    setPhaseInfo = false;
    if ((swProc.detaNetwSt) && (swProc.doNSt_C)) {
      GETINFO(NoElMPhInfo[iNoEl].NeStPhC ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      SETINFO(NoElMPhInfo[iNoEl].NetwStat,noSub
             ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      netStInfRepl = true; setPhaseInfo = true;
    }

    if ((swProc.deTaNetwGr) && (swProc.doNGr_C)) {
      GETINFO(NoElMPhInfo[iNoEl].NeGrPhC ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      SETINFO(NoElMPhInfo[iNoEl].NetwGrp,noSub
             ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&vInf,&vLen);

      netGrInfRepl = true; setPhaseInfo = true;
    }

    if (setPhaseInfo)
      if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[iNoEl],pWTHpmeVar->deTaNA))
        pWTHiWVipShort->uvipA.svALG.aAltPhaseC = 10000; // alt not valid
      else
        pWTHiWVipShort->uvipA.svALG.aAltPhaseC = AcEx;

    /* restore original info string */
    if (netStInfRepl)
      SETINFO(NoElMPhInfo[iNoEl].NetwStat,noSub
             ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&stInf,&stLen);

    if (netGrInfRepl)
      SETINFO(NoElMPhInfo[iNoEl].NetwGrp,noSub
             ,NimVarMp.NedePoi[iNoEl]
             ,(tInterStr*)pInfoStr,&grInf,&grLen);

    // network colouring for phase infos
    if (InSet(pWTHpmeVar->proMode,(int)netGrCol,sProMode))
      pElNetGrCol(NoElMPhInfo[iNoEl].NetwGrp
                 ,NoElMPhInfo[iNoEl].NeGrPhA
                 ,NoElMPhInfo[iNoEl].NeGrPhB
                 ,NoElMPhInfo[iNoEl].NeGrPhC
                 );
  }
} // IsMultiPhaseYes

// one side open line status processing
if (iNoEl == neTopElem) {

  // process main info
  pWTHiWVipShort->uvipA.svALG.semiOpenCombi =
    pGetSemiOpenAttr(iNoEl,OpenLineStatInfo.LineStat,NimVarMp.NedePoi[iNoEl]->NezuInf);

  // phase A/B/C
  pWTHiWVipShort->uvipA.svALG.semiOpenPhase[0] =
    pGetSemiOpenAttr(iNoEl,OpenLineStatInfo.LineStatA,NoElMPhInfo[iNoEl].NeStPhA);

  pWTHiWVipShort->uvipA.svALG.semiOpenPhase[1] =
    pGetSemiOpenAttr(iNoEl,OpenLineStatInfo.LineStatB,NoElMPhInfo[iNoEl].NeStPhB);

  pWTHiWVipShort->uvipA.svALG.semiOpenPhase[2] =
    pGetSemiOpenAttr(iNoEl,OpenLineStatInfo.LineStatC,NoElMPhInfo[iNoEl].NeStPhC);

} // one side open line processing

if (specialState) {
  makeDesState = false;
  InfVal = 1;
  //correct special Info`s of TA4

  corrAllInfos(LNK_pVarDisplay);
  //Determine figure-alternative, write it into VipShort and bDispI
  nextBDispI->fAltNorm = pWTHiWVipShort->uvipA.svALG.fAlt;
  if (pWTHpmeVar->deTaNF==0)
    nextBDispI->fAltFl = pWTHiWVipShort->uvipA.svALG.fAlt;
  else {
    //display with special info
    if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[iNoEl],pWTHpmeVar->deTaNF))
      nextBDispI->fAltFl = 10000;  //Alternative not valid
    else
      nextBDispI->fAltFl = AcEx;
    }

  /*Determine attribute-alternative, write it into VipShort and
  bDispI*/
  //nextBDispI->aAltNorm  = pWTHiWVipShort->aAlt;
  set_aAltNorm_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_DISPI);
  if (pWTHpmeVar->deTaNA==pWTHpmeVar->deTaNF) {
    nextBDispI->aAltFl  = nextBDispI->fAltFl;
    nextBDispI->aAltFlA = nextBDispI->fAltFl;
    nextBDispI->aAltFlB = nextBDispI->fAltFl;
    nextBDispI->aAltFlC = nextBDispI->fAltFl;
  } else {
    if (pWTHpmeVar->deTaNA==0) {
      nextBDispI->aAltFl  = pWTHiWVipShort->aAlt;
      nextBDispI->aAltFlA = pWTHiWVipShort->aAlt;
      nextBDispI->aAltFlB = pWTHiWVipShort->aAlt;
      nextBDispI->aAltFlC = pWTHiWVipShort->aAlt;
    } else {
      //display with special info
      if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[iNoEl],pWTHpmeVar->deTaNA)) {
        nextBDispI->aAltFl  = 10000;  //Alternative not valid
        nextBDispI->aAltFlA = 10000;  //Alternative not valid
        nextBDispI->aAltFlB = 10000;  //Alternative not valid
        nextBDispI->aAltFlC = 10000;  //Alternative not valid
      } else {
        nextBDispI->aAltFl  = AcEx;
        nextBDispI->aAltFlA = AcEx;
        nextBDispI->aAltFlB = AcEx;
        nextBDispI->aAltFlC = AcEx;
      }
      }
    }

  if (InSet(pWTHpmeVar->proMode,(int)netGrCol,sProMode)) {
    //pWTHiWVipShort->aAlt = nextBDispI->aAltFl;
    set_aAltFl_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_VIPSH);
#if NETGRP_DASHED
    pWTHiWVipShort->uvipA.svALG.aAltOld = pWTHiWVipShort->aAlt;
#endif
    pElNetGrCol(NoElMPhInfo[iNoEl].NetwGrp
               ,NoElMPhInfo[iNoEl].NeGrPhA
               ,NoElMPhInfo[iNoEl].NeGrPhB
               ,NoElMPhInfo[iNoEl].NeGrPhB
               );
    //nextBDispI->aAltFl  = pWTHiWVipShort->aAlt;
    set_aAltFl_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_DISPI);
    //pWTHiWVipShort->aAlt = nextBDispI->aAltNorm;
    set_aAltNorm_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_VIPSH);
    }

  if (makeDesState) {
    nextBDispI->saveAGroup = pWTHiWVipShort->aGroup;
    nextBDispI->aAltFl  = (int)saveFlash.flashMod+1;
    nextBDispI->aAltFlA = nextBDispI->aAltFl;
    nextBDispI->aAltFlB = nextBDispI->aAltFl;
    nextBDispI->aAltFlC = nextBDispI->aAltFl;
    }
  }  //if specialState
}


static
void          detValAlt(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
//Determine value and attribute-alternatve (elementary)

bool          specialState;
ttVipShortP*  pWTHiWVipShort;
tMeVar*       pWTHpmeVar;

if (checkBFl)
  initBFlashEl(LNK_pVarDisplay);
//insert entries of elem. var.into bFlash
pWTHiWVipShort = &pVipShort->Data[iWVipShort-1];
pWTHpmeVar = pmeVar;
//with pVipShort^...
//look for special states
specialState = false;
if (nextBDId!=0) {
  if (findBDispI(iWVipShort))
    specialState = true;
  }
changeFlash = specialState;
if (specialState) {
  nextBDispI->gid = pWTHiWVipShort->gid;
  nextBDispI->saveAGroup = 0;
  InfVal = 0;
  //correct special Info`s of TA4
  corrAllInfos(LNK_pVarDisplay);
  }

//Read value out of infostring
GETINFO(pWTHpmeVar->taMeVar.Info,NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],
        (tInterStr*)pInfoStr,&NimVarMp.InfVal,&Leng);
if (pWTHiWVipShort->vipAct==vipActINteger)
  pWTHiWVipShort->uvipA.svAI.iValue = NimVarMp.InfVal;
else
  pWTHiWVipShort->uvipA.svAR.rValue = NimVarMp.InfVal;

/*Determine attribute-alternative, write it into VipShort and
bDispI*/
if (pWTHpmeVar->deTaNA==0)
  pWTHiWVipShort->aAlt = 1;
else {
  if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[NimVarMp.NoElTy],pWTHpmeVar->deTaNA))
    pWTHiWVipShort->aAlt = 10000;  //Alternative not valid
  else
    pWTHiWVipShort->aAlt = AcEx;
  }

if (specialState) {
  if (pWTHiWVipShort->vipAct==vipActINteger)
    nextBDispI->iValNorm = pWTHiWVipShort->uvipA.svAI.iValue;
  else
    nextBDispI->rValNorm = pWTHiWVipShort->uvipA.svAR.rValue;

  //display with special info
  makeDesState = false;
  InfVal = 1;
  corrAllInfos(LNK_pVarDisplay);  //correct special Info`s of TA4

  GETINFO(pWTHpmeVar->taMeVar.Info,
          NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],(tInterStr*)pInfoStr,
          &NimVarMp.InfVal,&Leng);
  if (pWTHiWVipShort->vipAct==vipActINteger)
    nextBDispI->iValDes = NimVarMp.InfVal;
  else
    nextBDispI->rValDes = NimVarMp.InfVal;

  /*Determine attribute-alternative, write it into VipShort and
  bDispI*/
  //nextBDispI->aAltNorm  = pWTHiWVipShort->aAlt;
  set_aAltNorm_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_DISPI);
  if (pWTHpmeVar->deTaNA==0) {
    nextBDispI->aAltFl  = pWTHiWVipShort->aAlt;
    nextBDispI->aAltFlA = pWTHiWVipShort->aAlt;
    nextBDispI->aAltFlB = pWTHiWVipShort->aAlt;
    nextBDispI->aAltFlC = pWTHiWVipShort->aAlt;
  } else {
    if (!coDtPrEl(&Flag,&AcEx,pInfoStr,NimVarMp.StrLeng[NimVarMp.NoElTy],pWTHpmeVar->deTaNA)) {
      nextBDispI->aAltFl  = 10000;  //Alternative not valid
      nextBDispI->aAltFlA = 10000;  //Alternative not valid
      nextBDispI->aAltFlB = 10000;  //Alternative not valid
      nextBDispI->aAltFlC = 10000;  //Alternative not valid
    } else {
      nextBDispI->aAltFl  = AcEx;
      nextBDispI->aAltFlA = AcEx;
      nextBDispI->aAltFlB = AcEx;
      nextBDispI->aAltFlC = AcEx;
    }
    }

  if (makeDesState) {
    nextBDispI->saveAGroup = pWTHiWVipShort->aGroup;
    nextBDispI->aAltFl  = (int)saveFlash.flashMod+1;
    nextBDispI->aAltFlA = nextBDispI->aAltFl;
    nextBDispI->aAltFlB = nextBDispI->aAltFl;
    nextBDispI->aAltFlC = nextBDispI->aAltFl;
    }
  }  //if specialstate
}

// Local variables for pCheckAcknInf:
struct LCL_pCheckAcknInf {
  struct LCL_pVarDisplay* LNK_pVarDisplay;
  int           k;
  int           locInfo;
};


static
bool          getInfoType(
  struct LCL_pCheckAcknInf* LNK_pCheckAcknInf)
{
bool          getInfoType_RTN;
//determine info type, read intyde
//provide TA for pInfoType
TA.B1 = bTA[_AC(LNK_pCheckAcknInf->k-1,Condmax-1)].TA.B1;
TA.B2 = bTA[LNK_pCheckAcknInf->k-1].TA.B2;
TA.B3 = bTA[LNK_pCheckAcknInf->k-1].TA.B3;
TA.Elem = bTA[LNK_pCheckAcknInf->k-1].TA.Elem;
TA.Info = LNK_pCheckAcknInf->locInfo;

getInfoType_RTN = true;
CleSet(NimVarTy.RelSet,tsNimRfix);
AddSet(NimVarTy.RelSet,xInfoDef,tsNimRfix);
AddSet(NimVarTy.RelSet,xInTyDe,tsNimRfix);
if (!NIMRTY(&NimVarTy.FlagS,nTeAd+nIn,current_RelCopKey,0))
  return false;
return true;
}


static
bool          insBSt_1(
  struct LCL_pCheckAcknInf* LNK_pCheckAcknInf)
{
bool          insBSt_1_RTN;
//insert TA into bstay
insBSt_1_RTN = true;
bFTA.ta.B1 = bTA[_AC(LNK_pCheckAcknInf->k-1,Condmax-1)].TA.B1;
bFTA.ta.B2 = bTA[LNK_pCheckAcknInf->k-1].TA.B2;
bFTA.ta.B3 = bTA[LNK_pCheckAcknInf->k-1].TA.B3;
bFTA.ta.Elem = bTA[LNK_pCheckAcknInf->k-1].TA.Elem;
bFTA.ta.Info = LNK_pCheckAcknInf->locInfo;
bFTA.acknFlash = false;
bFTA.desFlash = false;
bFTA.posFlash = false;

if (!insBStay(pVipShort->dId,current_RelCopKey,iWVipShort,&bFTA))
  return false;
return true;
}


static
bool          insBFl_1(
  struct LCL_pCheckAcknInf* LNK_pCheckAcknInf)
{
bool          insBFl_1_RTN;
//insert TA into bFlash
insBFl_1_RTN = true;
bFTA.ta.B1 = bTA[_AC(LNK_pCheckAcknInf->k-1,Condmax-1)].TA.B1;
bFTA.ta.B2 = bTA[LNK_pCheckAcknInf->k-1].TA.B2;
bFTA.ta.B3 = bTA[LNK_pCheckAcknInf->k-1].TA.B3;
bFTA.ta.Elem = bTA[LNK_pCheckAcknInf->k-1].TA.Elem;
bFTA.ta.Info = LNK_pCheckAcknInf->locInfo;
bFTA.acknFlash = true;
bFTA.desFlash = false;
bFTA.posFlash = false;

if (!insBTA(pVipShort->dId,current_RelCopKey,iWVipShort,&bFTA))
  return false;
return true;
}


static
void          stSign_1(
  tstatchange   stInfo,
  struct LCL_pCheckAcknInf* LNK_pCheckAcknInf)
{
if (current_RelCopKey!=0)
  return;

bActSign.RelCopKey = 0;
++bActSign.ActSignNu;
bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].B1 =
  bTA[_AC(LNK_pCheckAcknInf->k-1,Condmax-1)].TA.B1;
bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].B2 =
  bTA[_AC(LNK_pCheckAcknInf->k-1,Condmax-1)].TA.B2;
bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].B3 =
  bTA[_AC(LNK_pCheckAcknInf->k-1,Condmax-1)].TA.B3;
if (cMaxTear>1) {
  rNimad.B1 = bTA[_AC(LNK_pCheckAcknInf->k-1,Condmax-1)].TA.B1;
  rNimad.B2 = bTA[LNK_pCheckAcknInf->k-1].TA.B2;
  rNimad.B3 = bTA[LNK_pCheckAcknInf->k-1].TA.B3;
  rNimad.B3 = bTA[LNK_pCheckAcknInf->k-1].TA.Elem;
  if (!DbRead(&fNimad,0)) {
    err(0,2335,"Nimad   ",fNimad.Flag,rNimad.B1,rNimad.B2,"III");
    err(0,2336,"Nimad   ",0,rNimad.B3,rNimad.Elem,"BII");
    bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].TeAr = cMaxTear;
  } else
    bActSign.ActSignAr[bActSign.ActSignNu-1].TeAr = rNimad.TeArea;
} else
  bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].TeAr = 1;
bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].MeCl =
  NimVarTy.pInTyDe->MeldeKlasse;
bActSign.ActSignAr[bActSign.ActSignNu-1].dId = pVipShort->dId;
bActSign.ActSignAr[bActSign.ActSignNu-1].ResSig = false;
bActSign.ActSignAr[bActSign.ActSignNu-1].statinfo = stInfo;

//transmission of record to SEDS
if (bActSign.ActSignNu>=cActSignAr)
  pSendSeds();
}


static
bool          appState_1(
  struct LCL_pCheckAcknInf* LNK_pCheckAcknInf)
{
bool          appState_1_RTN;
//appearing state
appState_1_RTN = false;
if (NimVarTy.pInTyDe->KomGeh) {  //appear- / disappear event
  //Is status = 1?
  InfVal = 0;
  NimVarMp.pInfStr = &bTA[_AC(LNK_pCheckAcknInf->k-1,Condmax-1)].InfStr;
  if (ASKINFO(LNK_pCheckAcknInf->locInfo,noSub,
              NimVarMp.NedePoi[_AC(bTA[LNK_pCheckAcknInf->k-1].NoElTy,neDim)],
              NimVarMp.pInfStr,&InfVal,&Leng))
    {                             //event is appearing
    if (NimVarTy.pInTyDe->SignStayGr)
      return true;
    }
  }
return false;
}


static
void          pCheckAcknInf(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
struct LCL_pCheckAcknInf V;
//Check acknowledge Info`s
int           jC;
bool          found;
int           FORLIM;
CondLine*     pWTHjC;
_REC_bTA*      pWTHk;

V.LNK_pVarDisplay = LNK_pVarDisplay;
FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
for (jC = 1; jC<=FORLIM; ++jC) {
  pWTHjC = &DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[jC-1];
  //with CL
  found = false;
  V.k = 1;
  while (V.k<=nbTA && !found) {
    pWTHk = &bTA[V.k-1];
    if (pWTHjC->Elem==pWTHk->TA.Elem && pWTHjC->B3==pWTHk->TA.B3 &&
        pWTHjC->B2==pWTHk->TA.B2 && pWTHjC->B1==pWTHk->TA.B1) {
      found = true;
      V.locInfo = pWTHjC->Info;
      if (!getInfoType(&V))       //determine info type, read intyde
        goto _L1;

      if (NimVarMp.NedePoi[_AC(bTA[_AC(V.k-1,Condmax-1)].NoElTy,neDim)]->
          uStDc.InfoNorm[_AC(pWTHjC->Info-1,Infomax-1)].sUni.AcknInf!=0) {
        //Is the bit 'change of state' set?
        NimVarMp.InfVal = 0;
        NimVarMp.pInfStr = &pWTHk->InfStr;
        if (ASKINFO(pWTHjC->Info,SubAckn,
              NimVarMp.NedePoi[_AC(bTA[_AC(V.k-1,Condmax-1)].NoElTy,neDim)],
              NimVarMp.pInfStr,&NimVarMp.InfVal,&Leng))
          {                       //acknowledge bit used in variable?
          if (InSet(bTA[_AC(V.k-1,Condmax-1)].infoSet,
                    NimVarMp.NedePoi[_AC(bTA[V.k-1].NoElTy,neDim)]->
                    uStDc.InfoNorm[_AC(pWTHjC->Info-1,Infomax-1)].sUni.AcknInf,
                    tinfoSet) && insBFl_1(&V) &&
              InSet(pmeVar->proMode,(int)ackPerm,sProMode) &&
              pWTHk->TA.Elem==pmeVar->taMeVar.uCmb.sTa4.Elem &&
              pWTHk->TA.B3==pmeVar->taMeVar.uCmb.sTa4.B3 &&
              pWTHk->TA.B2==pmeVar->taMeVar.uCmb.sTa4.B2 &&
              pWTHk->TA.B1==pmeVar->taMeVar.uCmb.sTa4.B1)
            {                     //insert TA into bFlash
            //and then identical with TA of display variable
            stSign_1(siChg,&V);
            }
          //send status sign to seds
          }

        }
      if (appState_1(&V) && InSet(pmeVar->proMode,(int)dispSelG,sProMode) &&
          pWTHk->TA.Elem==pmeVar->taMeVar.uCmb.sTa4.Elem &&
          pWTHk->TA.B3==pmeVar->taMeVar.uCmb.sTa4.B3 &&
          pWTHk->TA.B2==pmeVar->taMeVar.uCmb.sTa4.B2 &&
          pWTHk->TA.B1==pmeVar->taMeVar.uCmb.sTa4.B1 && insBSt_1(&V))
            //appearing state
              stSign_1(siApp,&V);
      //and then identical with TA of display variable
      //insert TA into bstay
      //send status sign to seds
_L1: ;
      }
    ++V.k;
    }
  }  //for j
}


static
void          initBFlash(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
//insert entries of typ 0r comb. var.into bFlash

int           jC;
int           FORLIM;
CondLine*     pWTHjC;

DtNr = pmeVar->deTaNA;
if (DtNr!=0) {
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    goto _L0;
    }
  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notTy()) {
    DatErr(0,2315,"DT inval",iWVipShort,pGDispDesRpP->dId,DtNr,"III",
           "CoDeTa  ",TA,0);
    errfmt(0,2315,"DT inval","initBFlash: invalid CoDeta (deTaNA): DtNr/dId/iWVipShort: %d/%d/%d"
                     ,DtNr,pGDispDesRpP->dId,iWVipShort);
    goto _L0;
    }
  if (InSet(pmeVar->proMode,(int)deTaTyp,sProMode)) {  //tpisiert
    FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
    for (jC = 1; jC<=FORLIM; ++jC) {
      pWTHjC = &DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[jC-1];
      pWTHjC->B1 = pmeVar->taMeVar.uCmb.sTa4.B1;
      pWTHjC->B2 = pmeVar->taMeVar.uCmb.sTa4.B2;
      pWTHjC->B3 = pmeVar->taMeVar.uCmb.sTa4.B3;
      }
    }

  pCheckAcknInf(LNK_pVarDisplay);  //Check acknowledge Info`s
  }  //DtNr <> 0

DtNr = pmeVar->deTaNF;
if (DtNr!=0 && DtNr!=pmeVar->deTaNA) {
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    goto _L0;
    }

  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notTy()) {
    DatErr(0,2315,"DT inval",iWVipShort,pGDispDesRpP->dId,DtNr,"III",
           "CoDeTa  ",TA,0);
    errfmt(0,2315,"DT inval","initBFlash: invalid CoDeta (deTaNF): DtNr/dId/iWVipShort: %d/%d/%d"
                         ,DtNr,pGDispDesRpP->dId,iWVipShort);
    goto _L0;
    }
  if (InSet(pmeVar->proMode,(int)deTaTyp,sProMode)) {  //tpisiert
    FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
    for (jC = 1; jC<=FORLIM; ++jC) {
      pWTHjC = &DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[jC-1];
      pWTHjC->B1 = pmeVar->taMeVar.uCmb.sTa4.B1;
      pWTHjC->B2 = pmeVar->taMeVar.uCmb.sTa4.B2;
      pWTHjC->B3 = pmeVar->taMeVar.uCmb.sTa4.B3;
      }
    }

  pCheckAcknInf(LNK_pVarDisplay);  //Check acknowledge Info`s
  }  //DtNr <> 0

_L0: ;
}


static
void          pBTA_1(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
//Buffering TA's

int           j;
int           k;
bool          found;
int           FORLIM;
CondLine*     pWTHj;
_REC_bTA*      pWTHk;

FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
for (j = 1; j<=FORLIM; ++j) {
  pWTHj = &DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[j-1];
  //with CL
  found = false;
  k = 1;
  while (k<=nbTA && !found) {
    pWTHk = &bTA[k-1];
    if (pWTHj->Elem==pWTHk->TA.Elem && pWTHj->B3==pWTHk->TA.B3 &&
        pWTHj->B2==pWTHk->TA.B2 && pWTHj->B1==pWTHk->TA.B1)
      found = true;
    ++k;
    }

  if (!found) {
    NimVarMp.pTa->B1 = pWTHj->B1;
    NimVarMp.pTa->B2 = pWTHj->B2;
    NimVarMp.pTa->B3 = pWTHj->B3;
    NimVarMp.pTa->Elem = pWTHj->Elem;
    NimVarMp.pTa->Info = 0;
    if (!NIMRMP(&NimVarMp.FlagS,nTeAd,pVipShort->RelCopKey,0))
      DatErr(0,2311,"NIM     ",NimVarMp.FlagS,pGDispDesRpP->dId,iWVipShort,
             "III","        ",TA,0);
    else {
      ++nbTA;
      bTA[_AC(nbTA-1,Condmax-1)].InfStr = *NimVarMp.pInfStr;
      bTA[nbTA-1].TA = *NimVarMp.pTa;
      bTA[nbTA-1].NoElTy = NimVarMp.NoElTy;
      CleSet(bTA[nbTA-1].infoSet,tinfoSet);
      AddSet(bTA[nbTA-1].infoSet,pWTHj->Info,tinfoSet);
      }
  } else
    AddSet(bTA[_AC(k-2,Condmax-1)].infoSet,pWTHj->Info,tinfoSet);
  }  //for j
}  //pBTA


static
void          fillBTA(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
//fill TA buffer with all TA's needed


int           j;
int           FORLIM;
CondLine*     pWTHj;

nbTA = 0;
DtNr = pmeVar->deTaNA;
if (DtNr!=0) {
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    LNK_pVarDisplay->funcErr = true;
    goto _L0;
    }
  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notTy()) {
    DatErr(0,2315,"DT inval",iWVipShort,pGDispDesRpP->dId,DtNr,"III",
           "CoDeTa  ",TA,0);
    errfmt(0,2315,"DT inval","fillBTA: invalid CoDeta (deTaNA): DtNr/dId/iWVipShort: %d/%d/%d"
                     ,DtNr,pGDispDesRpP->dId,iWVipShort);
    LNK_pVarDisplay->funcErr = true;
    goto _L0;
    }
  if (InSet(pmeVar->proMode,(int)deTaTyp,sProMode)) {  //tpisiert
    FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
    for (j = 1; j<=FORLIM; ++j) {
      pWTHj = &DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[j-1];
      pWTHj->B1 = pmeVar->taMeVar.uCmb.sTa4.B1;
      pWTHj->B2 = pmeVar->taMeVar.uCmb.sTa4.B2;
      pWTHj->B3 = pmeVar->taMeVar.uCmb.sTa4.B3;
      }
    }

  pBTA_1(LNK_pVarDisplay);        //TA's puffern
  }  //DtNr <> 0

DtNr = pmeVar->deTaNF;
if (DtNr!=0 && DtNr!=pmeVar->deTaNA) {
  if (DtNr>cDTKeyMax) {
    err(0,2601,"DTNR    ",DtNr,cDTKeyMax,0.0,"III");
    LNK_pVarDisplay->funcErr = true;
    goto _L0;
    }
  if (DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].notTy()) {
    DatErr(0,2315,"DT inval",iWVipShort,pGDispDesRpP->dId,DtNr,"III",
           "CoDeTa  ",TA,0);
    errfmt(0,2315,"DT inval","fillBTA: invalid CoDeta (deTaNF): DtNr/dId/iWVipShort: %d/%d/%d"
                         ,DtNr,pGDispDesRpP->dId,iWVipShort);
    LNK_pVarDisplay->funcErr = true;
    goto _L0;
    }
  if (InSet(pmeVar->proMode,(int)deTaTyp,sProMode)) {  //tpisiert
    FORLIM = DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->NrCond;
    for (j = 1; j<=FORLIM; ++j) {
      pWTHj = &DeTaPointer[_AC(DtNr-1,cDTKeyMax-1)].UU.TY->uD.cl[j-1];
      pWTHj->B1 = pmeVar->taMeVar.uCmb.sTa4.B1;
      pWTHj->B2 = pmeVar->taMeVar.uCmb.sTa4.B2;
      pWTHj->B3 = pmeVar->taMeVar.uCmb.sTa4.B3;
      }
    }

  pBTA_1(LNK_pVarDisplay);        //TA's zwischenpuffern
  }  //DtNr <> 0

_L0: ;
}


static
bool          corrTyAllInfos(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
bool          corrTyAllInfos_RTN;
//correct special Info`s of TA4 for typ. or 1:1 variable
bool          found;
int           k;
int           locInfVal;
tTA5*         pWTHTA;
corrTyAllInfos_RTN = true;
nextBTA = nextBDispI->nextBTA;
while (nextBTA!=0) {
  if (nextBTA->acknFlash) {
    found = false;
    k = 1;
    while (k<=nbTA && !found) {
      pWTHTA = &bTA[_AC(k-1,Condmax-1)].TA;
      if (pWTHTA->Elem==nextBTA->ta.Elem && pWTHTA->B3==nextBTA->ta.B3 &&
          pWTHTA->B2==nextBTA->ta.B2 && pWTHTA->B1==nextBTA->ta.B1) {
        found = true;
        locInfVal = InfVal;
        SETINFO(nextBTA->ta.Info,SubAckn,
                NimVarMp.NedePoi[_AC(bTA[_AC(k-1,Condmax-1)].NoElTy,neDim)],
                &bTA[_AC(k-1,Condmax-1)].InfStr,&locInfVal,&Leng);
        }
      ++k;
      }
    }

  if (nextBTA->desFlash && InfVal==1) {
    makeDesState = true;
    found = false;
    k = 1;
    while (k<=nbTA && !found) {
      pWTHTA = &bTA[_AC(k-1,Condmax-1)].TA;
      if (pWTHTA->Elem==nextBTA->ta.Elem && pWTHTA->B3==nextBTA->ta.B3 &&
          pWTHTA->B2==nextBTA->ta.B2 && pWTHTA->B1==nextBTA->ta.B1) {
        found = true;
        locInfVal = nextBTA->InfVal;
        SETINFO(nextBTA->ta.Info,noSub,
                NimVarMp.NedePoi[_AC(bTA[_AC(k-1,Condmax-1)].NoElTy,neDim)],
                &bTA[_AC(k-1,Condmax-1)].InfStr,&locInfVal,&Leng);
        //Delete disturbance-info
        locInfVal = 0;
        SETINFO(nextBTA->ta.Info,SubFault,
                NimVarMp.NedePoi[_AC(bTA[_AC(k-1,Condmax-1)].NoElTy,neDim)],
                &bTA[_AC(k-1,Condmax-1)].InfStr,&locInfVal,&Leng);
        }
      ++k;
      }
    }

  nextBTA = nextBTA->nextBTA;
  }
return true;
}


static
void          detAlt(
  struct LCL_pVarDisplay* LNK_pVarDisplay)
{
//Determine figure/attribute-alternatve (typ. or comm.)

bool          specialState;
ttVipShortP*  pWTHiWVipShort;
tMeVar*       pWTHpmeVar;
/**t*bf*Achtung: Wenn mepr die Quittierbits nicht mehr setzt, dann kann
corrTyAllInfos mit infVal=0 entfallen!!*/
if (checkBFl)
  initBFlash(LNK_pVarDisplay);
//insert entries of typ or comb. var.into bFlash
pWTHiWVipShort = &pVipShort->Data[iWVipShort-1];
pWTHpmeVar = pmeVar;
//with pVipShort^...
//look for special states
specialState = false;
if (nextBDId!=0) {
  if (findBDispI(iWVipShort))
    specialState = true;
  }
changeFlash = specialState;
if (specialState) {
  nextBDispI->gid = pWTHiWVipShort->gid;
  nextBDispI->saveAGroup = 0;
  }

//Determine figure-alternative, write it into VipShort and bDispI

DtPara.TA.B1 = pWTHpmeVar->taMeVar.uCmb.sTa4.B1;
DtPara.TA.B2 = pWTHpmeVar->taMeVar.uCmb.sTa4.B2;
DtPara.TA.B3 = pWTHpmeVar->taMeVar.uCmb.sTa4.B3;
//display without special info
if (specialState) {
  InfVal = 0;
  //correct special Info`s
  corrTyAllInfos(LNK_pVarDisplay);
  }

if (pWTHpmeVar->deTaNF==0)
  pWTHiWVipShort->uvipA.svALG.fAlt = 1;
else {
  DtPara.DtNr = pWTHpmeVar->deTaNF;
  coDtPr(&Flag,&DtPara);
  if (Flag!=0 || DtPara.NrOkRu!=1)
    pWTHiWVipShort->uvipA.svALG.fAlt = 10000;  //Alternative not valid
  else
    pWTHiWVipShort->uvipA.svALG.fAlt = DtPara.AcEx;
  }

/* preset phase attributes */
pWTHiWVipShort->uvipA.svALG.aAltPhaseA = -1;
pWTHiWVipShort->uvipA.svALG.aAltPhaseB = -1;
pWTHiWVipShort->uvipA.svALG.aAltPhaseC = -1;

/*Determine attribute-alternative, write it into VipShort and
bDispI*/
if (pWTHpmeVar->deTaNA==pWTHpmeVar->deTaNF)
  pWTHiWVipShort->aAlt = pWTHiWVipShort->uvipA.svALG.fAlt;
else {
  if (pWTHpmeVar->deTaNA==0)
    pWTHiWVipShort->aAlt = 1;
  else {
    DtPara.DtNr = pWTHpmeVar->deTaNA;
    coDtPr(&Flag,&DtPara);
    if (Flag!=0 || DtPara.NrOkRu!=1)
      pWTHiWVipShort->aAlt = 10000;  //Alternative not valid
    else
      pWTHiWVipShort->aAlt = DtPara.AcEx;
    }
  }

#if NETGRP_DASHED
pWTHiWVipShort->uvipA.svALG.aAltOld = pWTHiWVipShort->aAlt;
#endif
if (InSet(pWTHpmeVar->proMode,(int)netGrCol,sProMode))
      //Colouring, elem. var.
        pTypNetGrCol();

if (specialState) {
  makeDesState = false;
  InfVal = 1;
  //correct special Info`s

  corrTyAllInfos(LNK_pVarDisplay);
  nextBDispI->fAltNorm = pWTHiWVipShort->uvipA.svALG.fAlt;
  if (pWTHpmeVar->deTaNF==0)
    nextBDispI->fAltFl = pWTHiWVipShort->uvipA.svALG.fAlt;
  else {
    DtPara.DtNr = pWTHpmeVar->deTaNF;
    coDtPr(&Flag,&DtPara);
    if (Flag!=0 || DtPara.NrOkRu!=1)
      nextBDispI->fAltFl = 10000;  //Alternative not valid
    else
      nextBDispI->fAltFl = DtPara.AcEx;
    }

  /*Determine attribute-alternative, write it into VipShort and
  bDispI*/
  //nextBDispI->aAltNorm  = pWTHiWVipShort->aAlt;
  set_aAltNorm_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_DISPI);
  if (pWTHpmeVar->deTaNA==pWTHpmeVar->deTaNF) {
    nextBDispI->aAltFl  = nextBDispI->fAltFl;
    nextBDispI->aAltFlA = nextBDispI->fAltFl;
    nextBDispI->aAltFlB = nextBDispI->fAltFl;
    nextBDispI->aAltFlC = nextBDispI->fAltFl;
  } else {
    if (pWTHpmeVar->deTaNA==0) {
      nextBDispI->aAltFl  = pWTHiWVipShort->aAlt;
      nextBDispI->aAltFlA = pWTHiWVipShort->aAlt;
      nextBDispI->aAltFlB = pWTHiWVipShort->aAlt;
      nextBDispI->aAltFlC = pWTHiWVipShort->aAlt;
    } else {
      DtPara.DtNr = pWTHpmeVar->deTaNA;
      coDtPr(&Flag,&DtPara);
      if (Flag!=0 || DtPara.NrOkRu!=1) {
        nextBDispI->aAltFl  = 10000;  //Alternative not valid
        nextBDispI->aAltFlA = 10000;  //Alternative not valid
        nextBDispI->aAltFlB = 10000;  //Alternative not valid
        nextBDispI->aAltFlC = 10000;  //Alternative not valid
      } else {
        nextBDispI->aAltFl = DtPara.AcEx;
        nextBDispI->aAltFlA = DtPara.AcEx;
        nextBDispI->aAltFlB = DtPara.AcEx;
        nextBDispI->aAltFlC = DtPara.AcEx;
      }
      }
    }
  if (InSet(pWTHpmeVar->proMode,(int)netGrCol,sProMode)) {
    //pWTHiWVipShort->aAlt = nextBDispI->aAltFl;
    set_aAltFl_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_VIPSH);
#if NETGRP_DASHED
    pWTHiWVipShort->uvipA.svALG.aAltOld = pWTHiWVipShort->aAlt;
#endif
    pTypNetGrCol();               //Colouring, elem. var.
    //nextBDispI->aAltFl  = pWTHiWVipShort->aAlt;
    set_aAltFl_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_DISPI);
    //pWTHiWVipShort->aAlt = nextBDispI->aAltNorm;
    set_aAltNorm_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_VIPSH);
    }

  if (makeDesState) {
    nextBDispI->saveAGroup = pWTHiWVipShort->aGroup;
    nextBDispI->aAltFl = (int)saveFlash.flashMod+1;
    nextBDispI->aAltFlA = nextBDispI->aAltFl;
    nextBDispI->aAltFlB = nextBDispI->aAltFl;
    nextBDispI->aAltFlC = nextBDispI->aAltFl;
    }

  }  //with specialState
//Determine figure-alternative, write it into VipShort and bDispI

}



static
void          pVarDisplay()
{
struct LCL_pVarDisplay V;
ttVipShortP*  pWTHiWVipShort;
tMeVar*       pWTHpmeVar;
short         lDetaIdx;
bool          lElDtOk;

V.funcErr = false;
pWTHiWVipShort = &pVipShort->Data[iWVipShort-1];

pWTHpmeVar = pmeVar;
//with pVipShort^.data[ ],pMeVar
if (pWTHiWVipShort->vipAct==vipActLFig || pWTHiWVipShort->vipAct==vipActGFig)
  {  //Figure

  // check decision table types
  lElDtOk = true;
  if (pWTHpmeVar->deTaNA != 0) {
    lDetaIdx = _AC(pWTHpmeVar->deTaNA-1,cDTKeyMax-1);
    lElDtOk = (((InSet(pWTHpmeVar->proMode,(int)deTaEl,sProMode))  &&
                (DeTaPointer[lDetaIdx].ElDeTa))
            || ((!InSet(pWTHpmeVar->proMode,(int)deTaEl,sProMode)) &&
                (!DeTaPointer[lDetaIdx].ElDeTa)));
  }
  if ((lElDtOk) && (pWTHpmeVar->deTaNF != 0)) {
    lDetaIdx = _AC(pWTHpmeVar->deTaNF-1,cDTKeyMax-1);
    lElDtOk = (((InSet(pWTHpmeVar->proMode,(int)deTaEl,sProMode))  &&
                (DeTaPointer[lDetaIdx].ElDeTa))
            || ((!InSet(pWTHpmeVar->proMode,(int)deTaEl,sProMode)) &&
                (!DeTaPointer[lDetaIdx].ElDeTa)));
  }

  if (!lElDtOk) {
    pWTHiWVipShort->uvipA.svALG.fAlt = 10000;
    pWTHiWVipShort->aAlt = 10000;  //Alternative not valid
    errfmt(0,2000,"pVarDisp","pVarDisplay: DeTa-Error, dId: %d, gid: %d, detaNA: %d, detaNF: %d - break!"
          ,pGDispDesRpP->dId,pWTHiWVipShort->gid,pWTHpmeVar->deTaNA,pWTHpmeVar->deTaNF);
    goto _L98;                     //end of processing of this variable
  }


  if (InSet(pWTHpmeVar->proMode,(int)deTaEl,sProMode))
    {  //elementary combination
    TA.Info = 0;
    NimVarMp.NimSatz = pWTHpmeVar->taMeVar.uCmb.sTAns.NimSatz;
    NimVarMp.NoElTy = pWTHpmeVar->taMeVar.uCmb.sTAns.NoElType;
    if (!NIMRMP(&NimVarMp.FlagS,0,pVipShort->RelCopKey,0)) {
      DatErr(0,2311,"NIM     ",NimVarMp.FlagS,pGDispDesRpP->dId,iWVipShort,
             "III","        ",TA,0);
      pWTHiWVipShort->uvipA.svALG.fAlt = 10000;
      pWTHiWVipShort->aAlt = 10000;  //Alternative not valid
      goto _L98;                  //end of processing of this variable
      }

    pInfoStr = (ByteInt*)NimVarMp.pInfStr;

    detAltEl(&V,NimVarMp.NoElTy,NimVarMp.NimSatz);
        //Determine figure/attribute-alternatve (elementary)
    }  //Elementary combination

  else {
    //Typified or common combination
    fillBTA(&V);                  //fill TA buffer with all TA's needed
    if (V.funcErr)
      goto _L98;
    detAlt(&V);
        //Determine figure/attribute-alternatve (typ. or comm.)
    }
  }  //Figure

else {
  //Number
  //Value
  if (pWTHiWVipShort->vipAct!=vipActREal &&
      pWTHiWVipShort->vipAct!=vipActINteger)
    goto _L98;

  TA.Info = 0;
  NimVarMp.NimSatz = pWTHpmeVar->taMeVar.uCmb.sTAns.NimSatz;
  NimVarMp.NoElTy = pWTHpmeVar->taMeVar.uCmb.sTAns.NoElType;
  if (!NIMRMP(&NimVarMp.FlagS,0,pVipShort->RelCopKey,0)) {
    DatErr(0,2311,"NIM     ",NimVarMp.FlagS,pGDispDesRpP->dId,iWVipShort,
           "III","        ",TA,0);
    pWTHiWVipShort->aAlt = 10000;  //Value, alternative not valid
    if (pWTHiWVipShort->vipAct==vipActINteger)
      pWTHiWVipShort->uvipA.svAI.iValue = 0;
    else
      pWTHiWVipShort->uvipA.svAR.rValue = 0.0;
    goto _L98;                    //end of processing of this variable
    }

  pInfoStr = (ByteInt*)NimVarMp.pInfStr;

  detValAlt(&V);
      //Determine value and attribute-alternatve (elementary)
  }
goto _L0;
_L98:
//Starting point after breaking the processing a variable
receive.U1.AckPara.Acknowl.Ok = false;
_L0: ;
}  //pVarDisplay


// Buid actual vipshort for one dynamic registered variable
void          pUpdtDynVarD(ttVipShortP* i_ptr_VipShort, tMeVar* i_ptr_meVar, tRelCopKey i_RcK)
{
  tVipShortP  TmpVipShort;

  //medi needs a full vipshort so we have to create a temporary one
  TmpVipShort.RelCopKey = i_RcK;
  memcpy(&TmpVipShort.Data[0], i_ptr_VipShort, sizeof(ttVipShort));

  //Preset global variables in such a way that they point to the dynamic variable.
  pVipShort = &TmpVipShort;
  pmeVar = i_ptr_meVar;
  iRVarDes = 1;
  iWVipShort = 1;
  checkBFl = false;
  nextBDId = 0;

  pVarDisplay();

  // copy result back to our vipShort variable
  memcpy(i_ptr_VipShort, &TmpVipShort.Data[0], sizeof(ttVipShort));

  pVipShort = NULL;

}


static
void          resetSeds()
{
//reset signalling for dId

if (current_RelCopKey!=0)
  return;

bActSign.RelCopKey = 0;
++bActSign.ActSignNu;
bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].B1 = 0;
bActSign.ActSignAr[bActSign.ActSignNu-1].B2 = 0;
bActSign.ActSignAr[bActSign.ActSignNu-1].B3 = 0;
bActSign.ActSignAr[bActSign.ActSignNu-1].TeAr = 1;
bActSign.ActSignAr[bActSign.ActSignNu-1].MeCl = 1;
bActSign.ActSignAr[bActSign.ActSignNu-1].dId = pVipShort->dId;
bActSign.ActSignAr[bActSign.ActSignNu-1].ResSig = true;
bActSign.ActSignAr[bActSign.ActSignNu-1].statinfo = NoStatChange;

//transmission of record to SEDS
if (bActSign.ActSignNu>=cActSignAr)
  pSendSeds();
}


static
void          pWrActFile()
{
//Generate (write) actifile

int           iR;
int           FORLIM;
int           sbflag,dataL;

checkBFl = true;
resetSeds();                      //reset signalling for dId
disposebDId(pVipShort->dId,current_RelCopKey,&bFlash);
//dispose all bFlash entries for dId
disposebDId(pVipShort->dId,current_RelCopKey,&bStay);
//dispose all bStay  entries for dId

#if SUBS_SHHIS
GetTim(&bDIdHIS.Sec,&bDIdHIS.mSec);
if (pVipShort->dId > 0)
bDIdHIS.did = pVipShort->dId;
else
  bDIdHIS.did = dIdCheck;
bDIdHIS.SubRequest = sr_BUF_KIND_DID_INIT;
#endif

dataL = 0;

FORLIM = pMeOut->nMeOut;
for (iR = 1; iR<=FORLIM; ++iR) {
  iRVarDes = iR;
  iWVipShort = iR;
  pmeVar = &pMeOut->varDes[_AC(iRVarDes-1,vipShL-1)];
  pVarDisplay();
      //Display of one variable, without positioning flashing
  dataL = dataL + 1;
  if (changeFlash==true && InSet(pmeVar->proMode,(int)ackPerm,sProMode))
    pVipShort->Data[_AC(iWVipShort-1,vipShL-1)].Mark = 0xff;
  else
    pVipShort->Data[iWVipShort-1].Mark = '\0';
  }

if (current_RelCopKey==0) {
  writeDispdes = true;            //Write actfile into datbase
  //*t*spr-corr auch naechste Zeile
  meDispGen(false);
      //Delete dId in all concerned medisp records
  meDispGen(true);                //Create meDisp entries for dId
  AddSet(okActFileSet,pVipShort->dId,tActFileSet);
  pGDispDesRpP->spare2 = checkDispD;  //internal dispDes - test

#if SUBS_SHHIS
  if ((dataL > 0) && initialUpdate)
    if (!(MyOpMode == opmTraining) &&
        !McTransmit(&sbflag,McOwnLevel(),s_HIS_TOPR,true,
                    sizeof(tDIdHIS),&bDIdHIS,0,0,0))
      err(0,3222,"SENDTOPR",ri(sbflag),ri(bDIdHIS.did),0,"IIB");
#endif

} else {
  writeActFileC = true;           //Write actfile-copy into database
  if (!DbcOpen(&fmeDisp,current_RelCopKey))
    err(current_RelCopKey,3304,"DbcOpen ",rs("MEDI"),rs("SP "),
      rii(fmeDisp.Flag,fmeDisp.Fv.FlagS),"ZZT");
  else {
    meDispGenC();                //Create meDisp entries for dId
//  AddSet(okActFileSet,pVipShort->dId,tActFileSet);
//  pGDispDesRpP->spare2 = checkDispD;  //internal dispDes - test
    if (!DbcOpen(&fmeDisp,0))
       err(0,3304,"DbcOpen ",rs("MEDI"),rs("SP  "),
              rii(fmeDisp.Flag,fmeDisp.Fv.FlagS),"ZZT");
    }
  }

checkBFl = false;
}  //pWrActFile


static
void          pRRel()
{
dispDes*      pWTHpGDispDesRpP;
//Read actfile and variable description

pGDispDesRpP = 0;
vdispDes.dId = receive.outDisp.dId;

if (vdispDes.dId>maxDId) {
  err(0,2603,"maxDId ",vdispDes.dId,maxDId,0.0,"III");
  endOfJob = true;
  longjmp(_JL99,1);               //end of job
  }

  // Free last DispDes Read

  if (!DbRead(&fDispDes,RpOnPage)) {
    err(0,2310,"DBREAD  ",rs("DISP"),vdispDes.dId,fDispDes.Flag,"SII");
    endOfJob = true;
    longjmp(_JL99,1);             //end of job
  } else
    pGDispDesRpP = fDispDes.RpP;

pWTHpGDispDesRpP = pGDispDesRpP;

if (InSet(pWTHpGDispDesRpP->sDispVar,(int)meVar,pWTHpGDispDesRpP->sDispVar)) {
  if (pWTHpGDispDesRpP->spare2!=checkDispD) {
    SubSet(okActFileSet,vdispDes.dId,tActFileSet);
    SubSet(HISActFileSet,vdispDes.dId,tActFileSet);
  }
  pVipShort = (tVipShortP*)(&pWTHpGDispDesRpP->information[_AC(
                    pWTHpGDispDesRpP->Directory.iVipShort-1,cMaxInf-1)]);
  pMeOut = (tMeOut*)(&pWTHpGDispDesRpP->information[_AC(
                         pWTHpGDispDesRpP->Directory.iMeOut-1,cMaxInf-1)]);
} else {
  //*t*Fehlermeldung? eventuell auch in mmiDir aus set loeschen?
  SubSet(ActFileSet,vdispDes.dId,tActFileSet);
  SubSet(okActFileSet,vdispDes.dId,tActFileSet);
  SubSet(HISActFileSet,vdispDes.dId,tActFileSet);
  endOfJob = true;
  longjmp(_JL99,1);               //end of job
  }
if (current_RelCopKey!=0) {
  rActFileC.dId = receive.outDisp.dId;
  rActFileC.RelCopKey = current_RelCopKey;
  if (!DbRead(&fActFileC,RpOnPage)) {
    // we have to insert the record
    // size = full record minus part of VipShort which is not used
    fActFileC.RcS = (int)(sizeof(ActFileC) -
                          (vipShL - pVipShort->dataL)*sizeof(ttVipShortP));
    // to avoid access to unallocated memory we have to use a record of
    // correct size in DBInsert
    void *p = fActFileC.Rp;
    fActFileC.Rp = (ActFileC*)malloc(fActFileC.RcS);
    memcpy(fActFileC.Rp, p, sizeof(rActFileC));
    if (!DbInsert(&fActFileC,0)) {
      err(0,2312,"DBWRITE ",rs("AFC "),rActFileC.dId,fActFileC.Flag,"SII");
      endOfJob = true;
      free(fActFileC.Rp);
      fActFileC.Rp = (ActFileC*)p;
      longjmp(_JL99,1);           //end of job
      }
    free(fActFileC.Rp);
    fActFileC.Rp = (ActFileC*)p;
    if (!DbRead(&fActFileC,RpOnPage)) {
      err(0,2310,"DBREAD  ",rs("AFC "),rActFileC.dId,fActFileC.Flag,"SII");
      endOfJob = true;
      longjmp(_JL99,1);           //end of job
      }
    }
  ByIdBy(&fActFileC.RpP->vipShrtAFC.Header1,pVipShort,
         sizeof(tVipShort)-(vipShL-pVipShort->dataL)*sizeof(ttVipShortP));
  fActFileC.RpP->vipShrtAFC.RelCopKey = current_RelCopKey;
  pVipShort = &fActFileC.RpP->vipShrtAFC;
  }
}  //pRRel


static
void          pFRel()
{
//Release dispDes and actfile

if (writeDispdes) {
  writeDispdes = false;
  if (!DbWrite(&fDispDes,WrRpOnPage))
    err(0,2312,"DBWRITE ",rs("DISP"),vdispDes.dId,fDispDes.Flag,"SII");
} else {
  if (!DbRcFree(&fDispDes,0))
    err(0,2313,"DBRCFREE",rs("DISP"),vdispDes.dId,fDispDes.Flag,"SII");
  }

if (writeActFileC) {
  writeActFileC = false;
  if (!DbWrite(&fActFileC,RpOnPage))
    err(0,2312,"DBWRITE ",rs("AFC "),rActFileC.dId,fActFileC.Flag,"SII");
} else {
  if (lastRelCopKey!=0) {
    if (!DbRcFree(&fActFileC,0))
      err(0,2313,"DBRCFREE",rs("AFC "),rActFileC.dId,fActFileC.Flag,"SII");
    }
  }
}  //pFRel


static
void          pFMedisp()
{
//Release meDisp
if (!DbRcFree(&fmeDisp,0))
  err(0,2313,"DBRCFREE",rs("MeDi"),rs("sp  "),fmeDisp.Flag,"SSI");
if (!DbfNlock(&fmeDisp,0))
  err(0,2313,"DBFNLOCK",rs("MeDi"),rs("sp  "),fmeDisp.Flag,"SSI");

}


static
void          vipSHHead(
  tVipShortP*   VipShort)
{
VipShort->RelCopKey = current_RelCopKey;
VipShort->AckPara = receive.outDisp.AckPara;
VipShort->AckPara.AckCont = AckNo;
if (current_RelCopKey==0)
  VipShort->chan = 0;
else
  VipShort->chan = receive.outDisp.chan;
VipShort->area = receive.outDisp.area;
VipShort->dId = receive.outDisp.dId;
}
// ---------------------------------------------------------------------
// return vipshort pointer for update
// iDIdx == -1 means no data to update
//
static
tVipShortP * getUpdateVipShort
(        int          &iIdx                // I  : reference ongoing data index
) {
  if (pVipShort->dataL <= maxDataL) {
    iIdx = -1;
    return pVipShort;
  }

  // vipshort need to be splitted into parts
  bVipShort.dataL = 0;
  while (iIdx < pVipShort->dataL) {
    bVipShort.dataL++;
    iIdx++;
    bVipShort.Data[_AC(bVipShort.dataL-1,vipShL-1)] = pVipShort->Data[_AC(iIdx-1,vipShL-1)];
    if (bVipShort.dataL == maxDataL) {
      if (iIdx >= pVipShort->dataL) iIdx = -1;
      return &bVipShort;
    }
  }

  if (bVipShort.dataL > 0) iIdx = -1;

  return &bVipShort;

} // getUpdateVipShort
// ---------------------------------------------------------------------
// send vipshort to driver
//
static
void pSndVipShort
(        tVipShortP*   ipVipSH              // I  : pointer to vipshort
,  const SbAddressS    iAddress             // I  : drvier address
,  const ObjectIdS     iObject              // I  : driver object
,  const ComputId      iComput              // I  : cumputid to send
,  const short         iChan                // I  : channel
,  const bool          iErrJmp              // I  : eofJob in case of error
) {
  tpCharLiteral fname = "SndVipSH";

  if (ipVipSH == NULL) {
    errfmt(0,2000,fname,"pSndVipShort: address to vipShort NULL - break!");
    return;
  }

  //mmi has not been updated with this dId
  //find dID entry in bFlash
  if (findBDId(ipVipSH->dId,ipVipSH->RelCopKey,bFlash))
    corrSSStart = true;
  else
    nextBDId = 0;

  //correct special states in vipShort due to responsibility
  corrSpecialState(iComput,ipVipSH,false);

  size_t szVipS = sizeof(tVipShortP)-(vipShL-ipVipSH->dataL)*sizeof(ttVipShortP);
  int sbFlag = 0;

  if (!SbTransm(&sbFlag,iComput,iAddress,iObject,szVipS,ipVipSH)) {
    errfmt(0,3211,fname,"pSndVipShort: error SbTransm, Flag/dId/chan: %d/%d/%d"
                       ,sbFlag,ipVipSH->dId,iChan);
    if (iErrJmp) {
      endOfJob = true;
      longjmp(_JL99,1);               //end of job
    }
  } else {
    if (rTrace.On)
      wrVipShort("pSndVipShort",ipVipSH,iComput);
    if (DEBUGlevel(100))
      cp(0,100,fname,ipVipSH->dId,iComput,rii(ipVipSH->dataL,ipVipSH->chan),"ict");
    }

  //reset special states in vipShort to normal state
  corrSpecialState(iComput,ipVipSH,true);

} // pSndVipShort
// ---------------------------------------------------------------------
// send vipshort to driver (pOutDisp)
//
static
void          pVipAct()
{
  tpCharLiteral fname = "pVipAct ";

  int idxDir  = _AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1);

  bool lGoOn =  (current_RelCopKey!=0 ||
                 (current_RelCopKey==0 &&
                  !InSet(mmiDir[idxDir]->mmiActFileSet,receive.outDisp.dId,tActFileSet)));

  if (!lGoOn) return;

  vipSHHead(pVipShort);

  bool doUpdHIS = (!InSet(HISActFileSet,pVipShort->dId,tActFileSet));

  int DL = 0;
  tVipShortP* pVS;
  do {
    pVS = getUpdateVipShort(DL);
    vipSHHead(pVS);
#if SUBS_SHHIS
  if (doUpdHIS) pVipHIS(pVS,true,true);
#endif
    pSndVipShort(pVS,saOdvGat,obODVGAT,receive.outDisp.AckPara.Comput
                                         ,receive.outDisp.chan,true);
  } while (DL >= 0);

  if (current_RelCopKey==0) {
    trMmiDir* pMmiD = mmiDir[idxDir];
    if (InSet(pMmiD->mmiUpdtSet,pVipShort->dId,tActFileSet) &&
              pMmiD->mmiState==doupdate)
      AddSet(pMmiD->mmiActFileSet,pVipShort->dId,tActFileSet);
    if (IntDifNulSet(ActFileSet,pMmiD->mmiUpdtSet,pMmiD->mmiActFileSet) &&
        mmiDir[idxDir]->mmiState == doupdate)
      pMmiD->mmiState = updateOk;
  }
#if SUBS_SHHIS
  if (doUpdHIS) AddSet(HISActFileSet,pVipShort->dId,tActFileSet);
#endif

}  //pVipAct
// ---------------------------------------------------------------------
//look for selected dId/RelCopKey
static
void          pSearchDId(
  tSegId        sDId,             //I:dId to be searched
  tRelCopKey    sRelCopKey,       //I:RelCopKey to be searched
  short*        nSelDId,          //O:Number of selections found
  tSelChan      selChan)
{
/*O:Array with Comput,chan where
                               did/RelCopKey is selected*/
int           j;
int           kSeg;
int           iActivChan;
int           iChan;
int           iArInd;
trMmiDir*     pWTHj;
trChanDir*    pWTHiChan;
_REC_tSelChan* pWTHnSelDId;
int           FORLIM2;

*nSelDId = 0;
ById00(selChan,sizeof(tSelChan));
for (j = 1; j<=cMaxComput; ++j) {
  if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
    pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];
    if (pWTHj->nActivChan>0) {
      iChan = 1;
      iActivChan = 0;
      while (iChan<=cMaxChan && iActivChan<pWTHj->nActivChan) {
        if (pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]!=0) {
          pWTHiChan = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)];
          //if VChan[iChan]
          if (pWTHiChan->RelCopKey==sRelCopKey) {
            for (iArInd = 0; iArInd<=3; ++iArInd) {
              if (sDId==pWTHiChan->area[_AC(iArInd,3)].dId) {  //found
                ++*nSelDId;
                if (*nSelDId > cMaxSelChanEntries) {
                  errfmt(sRelCopKey,3001,"SearchDi","max. selections (%d) for DId: %d exceeded - break!"
                                   ,cMaxSelChanEntries,sDId);
                  err(sRelCopKey,3001,"SearchDi",j,iChan,iArInd,"cii");
                  continue;
                }
                pWTHnSelDId = &selChan[_AC(*nSelDId-1,cMaxSelChanEntries-1)];
                pWTHnSelDId->sComput = j;
                pWTHnSelDId->sChan = iChan;
                pWTHnSelDId->sArea = iArInd;
                }
              if (pWTHiChan->area[_AC(iArInd,3)].dId!=0 &&
                  pWTHiChan->area[iArInd].segmented) {
                FORLIM2 = pWTHiChan->segDisp.nSeg;
                for (kSeg = 2; kSeg<=FORLIM2; ++kSeg) {
                  if (pWTHiChan->segDisp.segments[_AC(kSeg-2,cMaxSeg-2)].dId==
                      sDId)
                    {  //found
                    ++*nSelDId;
                    if (*nSelDId > cMaxSelChanEntries) {
                      errfmt(sRelCopKey,3002,"SearchDi","max. selections (%d) for DId: %d exceeded - break!"
                                       ,cMaxSelChanEntries,sDId);
                      err(sRelCopKey,3002,"SearchDi",j,iChan,iArInd,"cii");
                      continue;
                    }
                    pWTHnSelDId = &selChan[_AC(*nSelDId-1,cMaxSelChanEntries-1)];
                    pWTHnSelDId->sComput = j;
                    pWTHnSelDId->sChan = iChan;
                    pWTHnSelDId->sArea = iArInd;
                    }
                  }
                }
              }  //for iArind
            }
          ++iActivChan;
          }
        ++iChan;
        }
      }  //if nActivChan
    }
  }  //for j

if (DEBUGlevel(200)) {
  _REC_tSelChan* pSelCh;
  errfmt(0,200,"SearchD1","DId: %d, RelCopKey: %d, nSelDId/cMaxSelChanEntries: %d/%d"
        ,sDId,sRelCopKey,*nSelDId,cMaxSelChanEntries);
  FORLIM2 = *nSelDId;
  for (j = 1; j<=FORLIM2; ++j) {
    pSelCh = &selChan[j-1];
    errfmt(0,200,"SearchD2","Comput: %d, Chan: %d, Area: %d"
          ,pSelCh->sComput,pSelCh->sChan,pSelCh->sArea);
    }
  }
}  //pSearchDId


static
void          pDelActFileC()
{
//Delete ActFileC record if not needed any more

short         nSelDId;
tSelChan      selChan;
/*Array with Comput,chan where
                                  did/RelCopKey is selected*/

//look for selected dId/RelCopKey
pSearchDId(rActFileC.dId,rActFileC.RelCopKey,&nSelDId,selChan);

if (nSelDId==1) {
  //only selected for one time -> dId/RelCopKey is not needed any more
  if (!DbDelete(&fActFileC,0))
    err(0,2314,"DBDELETE",rs("AFC "),rActFileC.dId,fActFileC.Flag,"SII");
  disposebDId(rActFileC.dId,rActFileC.RelCopKey,&bFlash);
  //dispose all bFlash entries for dId
  disposebDId(rActFileC.dId,rActFileC.RelCopKey,&bStay);
  //dispose all bStay entries for dId
  }
}  //pDelActFileC


static
void          pDelChanDir()
{
//Delete display out of channel directory

int           i;
int           k;
bool          SelDigr;            //Display group selected yes/no
trChanDir*    pWTHchan;
int           FORLIM;
trMmiDir*     pWTHComput;
tAreaMedi*    pWTHarea;


if (mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)]==0 ||
    mmiDir[receive.outDisp.AckPara.Comput-1]->VChan[_AC(receive.outDisp.chan-1,
                                                        cMaxChan-1)]==0)
  goto _L0;

pWTHComput = mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)];
pWTHchan = pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)];
pWTHarea = &pWTHchan->area[receive.outDisp.area];

if (pWTHchan->RelCopKey!=0)
  {  //delete all copys of ActFileC not needed any more
  rActFileC.dId = pWTHchan->area[_AC(receive.outDisp.area,3)].dId;
  rActFileC.RelCopKey = pWTHchan->RelCopKey;
  pDelActFileC();
      //Delete ActFileC record if not needed any more
  if (pWTHchan->area[_AC(receive.outDisp.area,3)].segmented) {
    FORLIM = pWTHchan->segDisp.nSeg;
    for (k = 2; k<=FORLIM; ++k) {
      rActFileC.dId = pWTHchan->segDisp.segments[_AC(k-2,cMaxSeg-2)].dId;
      pDelActFileC();
          //Delete ActFileC record if not needed any more
      }
    }
  }  //if RelCopKey <> 0

// deselect dynamic variables
DynVarDeselectDid(receive.outDisp.AckPara.Comput, pWTHchan->RelCopKey,
  receive.outDisp.chan, pWTHarea->dId);
if (pWTHchan->area[_AC(receive.outDisp.area,3)].segmented) {
  FORLIM = pWTHchan->segDisp.nSeg;
  for (k = 2; k<=FORLIM; ++k) {
    DynVarDeselectDid(receive.outDisp.AckPara.Comput, pWTHchan->RelCopKey,
      receive.outDisp.chan, pWTHchan->segDisp.segments[_AC(k-2,cMaxSeg-2)].dId);
    }
  }

//reset channel directory
pWTHarea->dId = 0;
if (pWTHarea->segmented)
  pWTHchan->segDisp.nSeg = 0;
SelDigr = false;
for (i = 0; i<=3; ++i) {
  if (pWTHchan->area[_AC(i,3)].dId!=0)
    SelDigr = true;
  }
if (!SelDigr && receive.outDisp.newDisp && receive.outDisp.dId==0)
  {  //Channel completely deselected and not needed
  free(pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]);
  pWTHComput->VChan[receive.outDisp.chan-1] = 0;
  --pWTHComput->nActivChan;
  }
if (DEBUGlevel(100)) {
  pWTHComput = mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)];
  pWTHarea =
    &pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]->area[receive.
          outDisp.area];
  cp(0,100,"DelChan1",receive.outDisp.AckPara.Comput,receive.outDisp.chan,
     receive.outDisp.area,"III");
  cp(0,100,"DelChan2",
     pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]==0,
     pWTHComput->nActivChan,0.0,"III");
  }
_L0: ;
}  //pDelChanDir


static
void          pDeSelSeg()
{
//Deselection of display segments
//Description see main program.

int           i;
int           j;
tRecsegm*     pWTHsegDisp;
int           FORLIM;
int           FORLIM1;
trChanDir*    pWTHchan;

pWTHsegDisp = &mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)]->
               VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]->segDisp;
if (DEBUGlevel(100)) {
  FORLIM = pWTHsegDisp->nSeg;
  for (i = 2; i<=FORLIM; ++i)
    cp(0,100,"DeSelSe1",pWTHsegDisp->nSeg,i,
       pWTHsegDisp->segments[_AC(i-2,cMaxSeg-2)].dId,"III");
  FORLIM = receive.outDisp.dSeg[0]+1;
  for (i = 2; i<=FORLIM; ++i)
    cp(0,100,"DeSelSe2",receive.outDisp.dSeg[0],i,
       receive.outDisp.dSeg[_AC(i-1,cMaxSeg+1)],"III");
  }

FORLIM = receive.outDisp.dSeg[0]+1;
for (i = 2; i<=FORLIM; ++i) {
  FORLIM1 = pWTHsegDisp->nSeg;
  for (j = 2; j<=FORLIM1; ++j) {
    if (receive.outDisp.dSeg[_AC(i-1,cMaxSeg+1)]==
        pWTHsegDisp->segments[_AC(j-2,cMaxSeg-2)].dId) {
      if (current_RelCopKey!=0) {
        rActFileC.dId = receive.outDisp.dSeg[_AC(i-1,cMaxSeg+1)];
        rActFileC.RelCopKey = current_RelCopKey;
        pDelActFileC();
            //Delete ActFileC record if not needed any more
        }

      DynVarDeselectDid(receive.outDisp.AckPara.Comput, current_RelCopKey,
        receive.outDisp.chan, receive.outDisp.dSeg[_AC(i-1,cMaxSeg+1)]);

      pWTHsegDisp->segments[_AC(j-2,cMaxSeg-2)].dId = 0;
      }
    }
  }

i = 1;
FORLIM = pWTHsegDisp->nSeg;
for (j = 2; j<=FORLIM; ++j) {
  if (pWTHsegDisp->segments[_AC(j-2,cMaxSeg-2)].dId!=0) {
    ++i;
    pWTHsegDisp->segments[_AC(i-2,cMaxSeg-2)].dId =
      pWTHsegDisp->segments[_AC(j-2,cMaxSeg-2)].dId;
    }
  }
FORLIM = pWTHsegDisp->nSeg;
for (j = i+1; j<=FORLIM; ++j)
  pWTHsegDisp->segments[_AC(j-2,cMaxSeg-2)].dId = 0;
pWTHsegDisp->nSeg = i;
pWTHchan =
  mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)]->VChan[_AC(
      receive.outDisp.chan-1,cMaxChan-1)];

if (pWTHsegDisp->nSeg==0)
  pWTHchan->area[_AC(receive.outDisp.area,3)].segmented = false;
if (DEBUGlevel(100)) {
  FORLIM = pWTHsegDisp->nSeg;
  for (i = 2; i<=FORLIM; ++i)
    cp(0,100,"DeSelSe3",pWTHsegDisp->nSeg,i,
       pWTHsegDisp->segments[_AC(i-2,cMaxSeg-2)].dId,"III");
  }
}  //pDeSelSeg


static
void          pWDiChanDir()
{
//Write display into channel directory

short         iArea;
int           chan;
int           j;
trMmiDir*     pWTHComput;
trChanDir*    pWTHchan;
tAreaMedi*    pWTHiArea;
tAreaMedi*    pWTHarea;
tRecsegm*     pWTHsegDisp;

if (receive.outDisp.CondSel)
  return;
if (mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)]==0) {
  //create a record for mmi directory
  mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)] =
    (trMmiDir*)malloc_chk(sizeof(trMmiDir));
  pWTHComput = mmiDir[receive.outDisp.AckPara.Comput-1];
  pWTHComput->Comput = receive.outDisp.AckPara.Comput;
  pWTHComput->mmiState = noUpdate;
  pWTHComput->nActivChan = 0;
  CleSet(pWTHComput->mmiActFileSet,tActFileSet);  //meVar dId`s, updated
  PreRanSet(pWTHComput->mmiUpdtSet,1,maxDId,tActFileSet);
  for (j = 1; j<=cMaxChannelPerMMI; ++j) {
    SetSet(pWTHComput->mmiAMeClS[_AC(j-1,cMaxChannelPerMMI-1)],fullMmiAMeClS,
           tMeClS);
    SetSet(pWTHComput->mmiATeArS[j-1],fullMmiATeArS,tTeArS);
    }
  for (chan = 1; chan<=cMaxChan; ++chan)
    pWTHComput->VChan[_AC(chan-1,cMaxChan-1)] = 0;
  }

if (DEBUGlevel(100)) {
  pWTHComput = mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)];
  cp(0,100,"WDiChan1",receive.outDisp.AckPara.Comput,
     pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]==0,0.0,"cib");
  }

pWTHComput = mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)];
//with mmiDir

if (pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]==0) {
  ++pWTHComput->nActivChan;
  pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)] =
    (trChanDir*)malloc_chk(sizeof(trChanDir));
  pWTHchan = pWTHComput->VChan[receive.outDisp.chan-1];
  pWTHchan->channel = receive.outDisp.chan;
  pWTHchan->RelCopKey = 0;
  pWTHchan->ackPosit = false;
  pWTHchan->segDisp.nSeg = 0;
  for (iArea = 0; iArea<=3; ++iArea) {
    pWTHiArea = &pWTHchan->area[iArea];
    pWTHiArea->dId = 0;
    pWTHiArea->segId = 0;
    pWTHiArea->segmented = false;
    }
  }
pWTHchan = pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)];
pWTHarea = &pWTHchan->area[receive.outDisp.area];
//with area
if (pWTHarea->dId==0) {
  pWTHarea->dId = receive.outDisp.dId;
  pWTHarea->segmented = false;
  pWTHchan->digr = receive.outDisp.digr;
  pWTHchan->sMod = receive.outDisp.sMod;
  pWTHchan->RelCopKey = current_RelCopKey;
} else {
  pWTHsegDisp = &pWTHchan->segDisp;
  if (pWTHarea->segmented==false) {
    pWTHarea->segmented = true;
    pWTHsegDisp->nSeg = 1;
    }
  ++pWTHsegDisp->nSeg;
  if (pWTHsegDisp->nSeg>cMaxSeg) {  //spr-7196
    err(0,2608,"nSeg   ",pWTHsegDisp->nSeg,vdispDes.dId,
        receive.outDisp.AckPara.Comput,"III");
    pWTHsegDisp->nSeg = cMaxSeg;
    endOfJob = true;
    longjmp(_JL99,1);             //end of job
    }
  pWTHsegDisp->segments[_AC(pWTHsegDisp->nSeg-2,cMaxSeg-2)].dId =
    receive.outDisp.dId;
  }
if (DEBUGlevel(100)) {
  pWTHComput = mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)];
  pWTHchan = pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)];
  pWTHarea = &pWTHchan->area[receive.outDisp.area];
  if (pWTHarea->segmented)
    cp(0,100,"WDiChan2",pWTHarea->dId,pWTHchan->segDisp.nSeg,
       pWTHchan->segDisp.segments[_AC(pWTHchan->segDisp.nSeg-2,cMaxSeg-2)].dId,
       "III");
  else
    cp(0,100,"WDiChan3",pWTHarea->dId,pWTHchan->segDisp.nSeg,
       pWTHComput->nActivChan,"III");
  }
}  //pWDiChanDir


//delete all bCAF entries for dId
static
void          delCAF(
  tbDId*        delBDId)
{
tbDId*        nextBDIdLoc;        //dId entry in bCAF
tbDId*        precBDId;           //precursor dId entry in bCAF
bool          found;

if (delBDId==0)
  return;

free(delBDId->VarSet);

//find precursor of dId
found = false;
nextBDIdLoc = bCAF;
precBDId = 0;
while (nextBDIdLoc!=0 && !found) {
  if (nextBDIdLoc->AFDId==delBDId->AFDId)
    found = true;
  else {
    precBDId = nextBDIdLoc;
    nextBDIdLoc = nextBDIdLoc->nextBDId;
    }
  }

if (found) {
  //pointer from precursor to successor of deleted dId
  if (precBDId==0) {
    bCAF = nextBDIdLoc->nextBDId;
    free(nextBDIdLoc);
  } else {
    precBDId->nextBDId = nextBDIdLoc->nextBDId;
    free(nextBDIdLoc);
    }
  }
}  //delCAF


//update from bCAF for unselected did`s
static
void          pUpdateCAF(
  tSegId        dId,
  tRelCopKey    RelCopKey)
{
int           i;
tbDId*        saveNextBDId;

nbChangeAF = 0;

if (findBDId(dId,RelCopKey,bCAF)) {
  saveNextBDId = nextBDId;
  for (i = 1; i<=vipShL; ++i) {
    if (InSet(saveNextBDId->VarSet,i,tVarSet)) {
      pWBChangeAf(saveNextBDId->AFDId,i,RelCopKey);

      //Update displays and actfiles
      if (nbChangeAF==bCAFMax) {
        markbcaf = false;
        pUpdateD();
        }

      }
    }

  delCAF(saveNextBDId);

  //Update displays and actfiles
  if (nbChangeAF>0) {
    markbcaf = false;
    pUpdateD();
    }  //if (nbChangeAF>0)..
  }  //if findBDId..
}  //pUpdateCAF


//insert bVipShort in link list
static
void          insBVipSH(
  tmmiSet       mmiS,
  size_t        lVipS)
{
int           lVipSH;             //Length of vipShort + mmiSet + pointer

if (nBVipSH>=maxDataL)            //or another parameterizing constant
  sendBVipSH(0,0);

nBVipSH+=bVipShort.dataL;

lVipSH = (int)(sizeof(tbVipSH)-
               (vipShL-bVipShort.dataL)*sizeof(ttVipShortP));
newbVipSH = (tbVipSH*)malloc_chk(lVipSH);
newbVipSH->nextVipSH = 0;
ByIdBy(&newbVipSH->VipSH.Header1,&bVipShort,lVipS);
memcpy(newbVipSH->mmiSet,mmiS,sizeof(tmmiSet));

if (bVipSHLast==0)
  bVipSH = newbVipSH;
else
  bVipSHLast->nextVipSH = newbVipSH;

bVipSHLast = newbVipSH;
}  //insBVipSH


static
void          delBVipSH()
{
//delete all entries of bVipSH
tbVipSH*      nextbVipSHLoc;
tbVipSH*      tempbVipSHLoc;

if (bVipSH==0)
  return;
nextbVipSHLoc = bVipSH;
while (nextbVipSHLoc!=0) {
  tempbVipSHLoc = nextbVipSHLoc;
  nextbVipSHLoc = nextbVipSHLoc->nextVipSH;
  free(tempbVipSHLoc);
  }  //(nextbVipSHLoc<>nil)..
bVipSH = 0;
bVipSHLast = 0;
}  //delBVipSH
//transmit to woman and corr. mmiSet
// Local variables for sendBVipSH:
struct LCL_sendBVipSH {
  tSegId        idId;
  tmmi          mmi;
  int           j;
  //, tempbVipSHLoc
  tbVipSH*      nextbVipSHLoc;
};


static
void          checkBVipSH(
  struct LCL_sendBVipSH* LNK_sendBVipSH)
{
trMmiDir*     pWTHj;
//check if too many variables in BVispSH
if (LNK_sendBVipSH->idId!=0 || LNK_sendBVipSH->mmi!=0)
  return;
if (nBVipSH>=maxDataL) {          //or another parameterizing constant
  //set all BVipSH-dIds invalid on concerned mmis
  nBVipSH = 0;

  LNK_sendBVipSH->nextbVipSHLoc = bVipSH;
  while (LNK_sendBVipSH->nextbVipSHLoc!=0) {
    for (LNK_sendBVipSH->j = 1;
         LNK_sendBVipSH->j<=cMaxComput;
         ++LNK_sendBVipSH->j) {
      if (InSet(LNK_sendBVipSH->nextbVipSHLoc->mmiSet,LNK_sendBVipSH->j,
                tmmiSet)) {
        if (mmiDir[_AC(LNK_sendBVipSH->j-1,cMaxComput-1)]!=0) {
          pWTHj = mmiDir[_AC(LNK_sendBVipSH->j-1,cMaxComput-1)];
          SubSet(pWTHj->mmiActFileSet,
                 LNK_sendBVipSH->nextbVipSHLoc->VipSH.dId,tActFileSet);
          if (pWTHj->mmiState==updateOk)
            pWTHj->mmiState = doupdate;
          }
        }  //if j in mmiSet..
      }  //for j..

    LNK_sendBVipSH->nextbVipSHLoc = LNK_sendBVipSH->nextbVipSHLoc->nextVipSH;
    }


  delBVipSH();                    //delete all entries of bVipSH
  }
}


static
void          supplyVipShort(
  struct LCL_sendBVipSH* LNK_sendBVipSH)
{
//supply VipShort

tempbVipSHLoc->VipSH.chan = 0;
tempbVipSHLoc->VipSH.area = 0;
lVipS = sizeof(tVipShortP)-
        (vipShL-tempbVipSHLoc->VipSH.dataL)*sizeof(ttVipShortP);

if (findBDId(tempbVipSHLoc->VipSH.dId,tempbVipSHLoc->VipSH.RelCopKey,bFlash))
  corrSSStart = true;
else
  nextBDId = 0;
}  //supplyVipShort


static
void          sendtoWoman(
  int           mmi,
  struct LCL_sendBVipSH* LNK_sendBVipSH)
{
if (mmiDir[_AC(mmi-1,cMaxComput-1)]!=0 && mmiDir[mmi-1]->mmiState==noUpdate)
  goto _L0;

//correct special states in vipShort due to responsibility
corrSpecialState(mmi,&tempbVipSHLoc->VipSH,false);

if (!SbTransm(&sbF,mmi,saOdvGat,obODVGAT,lVipS,
              &tempbVipSHLoc->VipSH.Header1))
  err(0,3221,"VIPSHORT",sbF,LNK_sendBVipSH->j,saOdvGat,"IIA");
else {
  if (rTrace.On)
  wrVipShort("sendtoWoman",&tempbVipSHLoc->VipSH,mmi);
  if (DEBUGlevel(100))
    cp(0,100,"send2Odv",tempbVipSHLoc->VipSH.dId,mmi
            ,rii(tempbVipSHLoc->VipSH.dataL,tempbVipSHLoc->VipSH.chan),"ict");
}

_L0:
SubSet(tempbVipSHLoc->mmiSet,mmi,tmmiSet);
}  //sendtoWoman


//send bVipShort out of link list
static
void          sendBVipSH(
  tSegId        idId_,
  tmmi          mmi_)
{
struct LCL_sendBVipSH V;

V.idId = idId_;
V.mmi = mmi_;
if (bVipSH==0)
  return;
checkBVipSH(&V);                  //check if too many variables in BVispSH
V.nextbVipSHLoc = bVipSH;
if (V.idId==0 && V.mmi==0) {
  while (V.nextbVipSHLoc!=0) {
    tempbVipSHLoc = V.nextbVipSHLoc;
    V.nextbVipSHLoc = V.nextbVipSHLoc->nextVipSH;
    if (!NulSet(tempbVipSHLoc->mmiSet,tmmiSet)) {
      supplyVipShort(&V);         //supply VipShort
      for (V.j = 1; V.j<=cMaxComput; ++V.j) {
        if (InSet(tempbVipSHLoc->mmiSet,V.j,tmmiSet)) {
          sendtoWoman(V.j,&V);    //SbTransm to woman and corr. mmiSet
          }  //if j in mmiSet..
        }  //for j..
      }  //(if tempbVipSHLoc^.mmiSet <> [])...
    free(tempbVipSHLoc);
    }  //(nextbVipSHLoc<>nil)..
  bVipSH = 0;
  bVipSHLast = 0;
  }  //if(idId=0)..
else {
  V.j = V.mmi;
  while (V.nextbVipSHLoc!=0) {
    if (V.nextbVipSHLoc->VipSH.dId==V.idId) {
      if (InSet(V.nextbVipSHLoc->mmiSet,V.j,tmmiSet)) {
        tempbVipSHLoc = V.nextbVipSHLoc;
        supplyVipShort(&V);       //supply VipShort
        sendtoWoman(V.j,&V);      //SbTransm to woman and corr. mmiSet
        }
      }
    V.nextbVipSHLoc = V.nextbVipSHLoc->nextVipSH;
    }  //while (curentbVipSHLoc^.nextVipSH<>nil)..
  }
}  //sendBVipSH


//Selection of display
//Description see main program.
static
bool          dIdSelected()
{
bool          dIdSelected_RTN;
bool          found;
int           kSeg;
trMmiDir*     pWTHComput;

if (DEBUGlevel(100))
  cp(0,100,"IntRequ ",receive.outDisp.AckPara.Comput,receive.outDisp.dId,
     receive.outDisp.chan,"CII");

dIdSelected_RTN = false;
found = false;


if (mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)]==0)
  return false;
pWTHComput = mmiDir[_AC(receive.outDisp.AckPara.Comput-1,cMaxComput-1)];
if (pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]==0)
  return false;
if (pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]->area[_AC(
        receive.outDisp.area,3)].dId==receive.outDisp.dId)
  found = true;
else {
  if (pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]->area[_AC(
          receive.outDisp.area,3)].segmented) {
    kSeg = 1;
    while (kSeg<pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]->
                segDisp.nSeg && !found) {
      ++kSeg;
      if (pWTHComput->VChan[_AC(receive.outDisp.chan-1,cMaxChan-1)]->segDisp.
          segments[_AC(kSeg-2,cMaxSeg-2)].dId==receive.outDisp.dId)
        found = true;
      }
    }
  }
dIdSelected_RTN = found;

if (DEBUGlevel(100) && found)
  cp(0,100,"CondSel ",receive.outDisp.AckPara.Comput,receive.outDisp.dId,
     receive.outDisp.chan,"CII");
return dIdSelected_RTN;
}  //dIdSelected


static
void          pOutDisp()
{
if (rTrace.On)
  wrOutDisp();

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

if (!(PCNormstatus)) {
  receive.U1.AckPara.Acknowl.Ok = false;
  pAckn();                        //Acknowledgement to DIMA
  return;
  }

receive.U1.AckPara.Acknowl.Ok = true;
if (receive.outDisp.dId==0) {
  if (receive.outDisp.newDisp)    //Complete deselection
    pDelChanDir();
  else {
    //Deselection of non-base segments
    pDeSelSeg();
    }
} else {
  //Selection of display
  //Selection of display
  if (receive.outDisp.CondSel && !dIdSelected())  //check if selected
    return;

  if (receive.outDisp.newDisp)    //Delete display out of channel directory
    pDelChanDir();
  pRRel();                        //Read actfile and variable description
  pWDiChanDir();                  //Write display into channel directory

  if (current_RelCopKey!=0 ||
      (InSet(ActFileSet,receive.outDisp.dId,tActFileSet) &&
       !InSet(okActFileSet,receive.outDisp.dId,tActFileSet)))
        //dId contains meVar but is not updated yet
          pWrActFile();           //Generate (write) actfile
  pVipAct();                      //Updating file to driver if neccesary
  pFRel();                        //Release dispDes and actfile
  sendBVipSH(receive.outDisp.dId,receive.outDisp.AckPara.Comput);
  pUpdateCAF(receive.outDisp.dId,current_RelCopKey);
  }
pAckn();                          //Acknowledgement to DIMA
}  //pOutDisp


//Transmitt softbus record to program SEDS
static
bool          pSendSeds()
{
bool          pSendSeds_RTN;

size_t        sbsize;             // size of SB - type

sbsize = sizeof(ActSign)-(cActSignAr-bActSign.ActSignNu)*sizeof(tActSign);
if (!SbTransm(&sbF,OwnComput,saSeds,obSEDS,sbsize,&bActSign)) {
  err(0,3221,"SENDSEDS",sbF,OwnComput,saSeds,"IIA");

  pSendSeds_RTN = false;
} else
  pSendSeds_RTN = true;
bActSign.ActSignNu = 0;
return pSendSeds_RTN;
}  //pSendSeds


static
void          pActSign()
{
//Activate counters

bool          creSign;
tstatchange   corrStatinfo;
tRawInMeData* pWTHiRawInMe;

if (current_RelCopKey!=0)
  return;

pWTHiRawInMe = &receive.rawInMe.Data[iRawInMe-1];
//with receive...

corrStatinfo = pWTHiRawInMe->statinfo;
switch (pWTHiRawInMe->statinfo) {
  case AckChg:
    if (InSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].prM,(int)AckP,
              tPrMSet))
      creSign = true;
    else
      creSign = false;
    break;
  case siChg:
    creSign = false;
    if (InSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].prM,(int)AckP,
              tPrMSet))
      creSign = true;
    break;
  case siAppA:
    creSign = false;
    if (InSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].prM,(int)AckP,
              tPrMSet)) {
      creSign = true;
      if (!InSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].prM,(int)dSG,
                 tPrMSet))
        corrStatinfo = siChg;
      }
    if (!creSign && InSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].prM,
                          (int)dSG,tPrMSet)) {
      creSign = true;
      corrStatinfo = siApp;
      }
    break;

  case siApp:
  case siDisApp:
    if (InSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].prM,(int)dSG,
              tPrMSet))
      creSign = true;
    else
      creSign = false;
    break;
  default:
    creSign = false;
    break;
  }
if (creSign) {
  pWTHiRawInMe = &receive.rawInMe.Data[iRawInMe-1];
  //with receive.rawInMe.data[iRawInMe]
  bActSign.RelCopKey = current_RelCopKey;
  ++bActSign.ActSignNu;
  bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].B1 =
    pWTHiRawInMe->TA.B1;
  bActSign.ActSignAr[bActSign.ActSignNu-1].B2 = pWTHiRawInMe->TA.B2;
  bActSign.ActSignAr[bActSign.ActSignNu-1].B3 = pWTHiRawInMe->TA.B3;
  if (cMaxTear>1) {
    rNimad.B1   = pWTHiRawInMe->TA.B1;
    rNimad.B2   = pWTHiRawInMe->TA.B2;
    rNimad.B3   = pWTHiRawInMe->TA.B3;
    rNimad.Elem = pWTHiRawInMe->TA.Elem;
    if (!DbRead(&fNimad,0)) {
      err(0,2335,"Nimad   ",fNimad.Flag,pWTHiRawInMe->TA.B1,
          pWTHiRawInMe->TA.B2,"III");
      err(0,2336,"Nimad   ",0,pWTHiRawInMe->TA.B3,pWTHiRawInMe->TA.Elem,"BII");
      bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].TeAr =
        cMaxTear;
    } else
      bActSign.ActSignAr[bActSign.ActSignNu-1].TeAr = rNimad.TeArea;
  } else
    bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].TeAr = 1;
  bActSign.ActSignAr[_AC(bActSign.ActSignNu-1,cActSignAr-1)].MeCl =
    pWTHiRawInMe->meClass;
  bActSign.ActSignAr[bActSign.ActSignNu-1].dId = dId;
  bActSign.ActSignAr[bActSign.ActSignNu-1].ResSig = false;
  bActSign.ActSignAr[bActSign.ActSignNu-1].statinfo = corrStatinfo;

  //transmission of record to SEDS
  if (bActSign.ActSignNu>=cActSignAr)
    pSendSeds();
  }  //Info is identical
}  //pActSign

#if SUBS_SHHIS
// ---------------------------------------------------------------------
// check correct bVipHIS according bVipShort,
// in corrspecialstate flashing status of switches will be set, this
// need to be set for bVipHIS as well
//
static void checkCorr_aAlt_bVipHIS
( tVipShortP*   ip_vipShort          //Buffer for VipShort
) {
  ttVipShortP  *pVSh;
  ttVipShort   *pVH;

  for (int v=0; v < ip_vipShort->dataL; v++) {
    pVSh = &ip_vipShort->Data[v];
    for (int h=0; h < bVipHIS.dataL; h++) {
      pVH = &bVipHIS.data[h].VipShortH;
      if ((bVipHIS.did == ip_vipShort->dId) && (pVH->gid == pVSh->gid))
        pVH->aAlt = pVSh->aAlt;
    }
  }
} // checkCorr_aAlt_bVipHIS
// ---------------------------------------------------------------------
// procdess / send bVipHIS to SHHIS
// NOTE: pVipHIS is basing on an updated bVipShort record
//       corrspecialstate will be called on bVipShort with cMaxComput
//
static
void pVipHIS
(        tVipShortP*  ip_vipShort           // I  : pointer to vipshort
,  const bool         iCreate_bVipHIS       // I  : create bVipHIS out of bVipShort data, use current time
,  const bool         iSet_bVIPHIS_Time     // I  : set time parameters
) {

  if (MyOpMode == opmTraining) return;           // not on opmTraining

  tpCharLiteral fname = "pVipHIS ";
  int sbflag;

  // find dID entry in bFlash
  if (findBDId(ip_vipShort->dId,ip_vipShort->RelCopKey,bFlash))
    corrSSStart = true;
  else
    nextBDId = 0;

  //correct special states in vipShort due to responsibility
  corrSpecialState(cMaxComput,ip_vipShort,false);

  // create bVipHIS data?
  if (iCreate_bVipHIS) {
    IpcGetTime(&gActSec,&gActMSec);
    bVipHIS.did = ip_vipShort->dId;
    bVipHIS.dataL = 0;
    if (ip_vipShort->dataL > MaxVipHIS)  // should never happen
      errfmt(0,3200,fname,"bVipShort.dataL (%d) > MaxVipHIS (%d); entries > MaxVipHIS skipped!"
                         ,ip_vipShort->dataL,MaxVipHIS);
    for (int i=0; i < ip_vipShort->dataL; i++) {
      if (bVipHIS.dataL >= MaxVipHIS) break;
      // add entry to HIS buffer
      memcpy(&bVipHIS.data[bVipHIS.dataL].VipShortH,&ip_vipShort->Data[i],sizeof(ttVipShort));
      bVipHIS.data[bVipHIS.dataL].timeSec  = gActSec;
      bVipHIS.data[bVipHIS.dataL].timemSec = gActMSec;
      bVipHIS.dataL++;
    }
  } else
    checkCorr_aAlt_bVipHIS(ip_vipShort);

  if (iSet_bVIPHIS_Time) {
    IpcGetTime(&gActSec,&gActMSec);
    bVipHIS.Sec  = gActSec;
    bVipHIS.mSec = gActMSec;
  }

  //reset special states in vipShort to normal state
  corrSpecialState(cMaxComput,ip_vipShort,true);

  if (bVipHIS.dataL == 0) return;

  // send record
  size_t sbsize = (sizeof(tVipHIS) - ((MaxVipHIS-bVipHIS.dataL)*sizeof(ttVipShortHIS)));
  if (!McTransmit(&sbflag,McOwnLevel(),s_HIS_TOPR,true,sbsize,&bVipHIS,0,0,0)) {
    err(0,3222,fname,ri(sbflag),bVipHIS.dataL,0,"IIB");
    bVipHIS.dataL = 0;
    return;
  }

  if (DEBUGlevel(200))
    errfmt(0,200,fname,"Update HIS: dId=%d",bVipHIS.did);

  if (SosyData.sdDEBUGlevel == 48) {
    ttVipShort *pVH;
    for (int i=0; i < bVipHIS.dataL; i++) {
      pVH = &bVipHIS.data[i].VipShortH;
      errfmt(0,48,fname,"Send s_HIS_TOPR/r_TOPO_data_change - %3d: dId=%d - GID:%8d - AttGr/aAlt:%3d/%2d"
                 ,i,bVipHIS.did,pVH->gid,pVH->aGroup,pVH->aAlt);
    }
  }
  bVipHIS.dataL = 0;
}
#endif
// ---------------------------------------------------------------------
//Vipshort to all channels with dId
static
void          pVipChan(
  bool          allMmis)
{
/*true: all mmi`s concerned
                                 false:Send vipShort to all chan`s
                                 with selected dId*/
short         nSelDId;
short         j;
short         jPosit;
tSelChan      selChan;
/*Array with Comput,chan where
                                  did/RelCopKey is selected*/
int           iW;
int           iR;
int           FORLIM;
trMmiDir*     pWTHj;
_REC_tSelChan* pWTHj1;
ttVipShortP*  pWTHiW;
tmmiSet       sent2mmi;           //channel 0 sent to woman

//preset selChan with zeros
ById00(selChan,sizeof(tSelChan));

//look for selected dId/RelCopKey
pSearchDId(bVipShort.dId,bVipShort.RelCopKey,&nSelDId,selChan);

CleSet(mmiSet,tmmiSet);
if (allMmis) {  //dId for background updating of mmi`s
  if (InSet(okActFileSet,bVipShort.dId,tActFileSet)) {
    for (j = 1; j<=cMaxComput; ++j) {
      if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
        pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];
        if ((pWTHj->mmiState==doupdate || pWTHj->mmiState==updateOk) &&
            InSet(pWTHj->mmiUpdtSet,bVipShort.dId,tActFileSet))
          AddSet(mmiSet,j,tmmiSet);
        }
      }
    }
  }

addrVipShort = &bVipShort;
lVipS = sizeof(tVipShortP)-(vipShL-bVipShort.dataL)*sizeof(ttVipShortP);

// find dID entry in bFlash
if (findBDId(bVipShort.dId,bVipShort.RelCopKey,bFlash))
  corrSSStart = true;
else
  nextBDId = 0;

bool sndVipSh2DriverObj = true;   // send vipShort to woman;
CleSet(sent2mmi,tmmiSet);

//Send to all Comput/chan/area with selected dId/RelCopKey
for (j = 1; j<=nSelDId; ++j) {
  pWTHj1 = &selChan[j-1];

  sndVipSh2DriverObj = true;

  if (allMmis) {
    bVipShort.chan = 0;

    if (!InSet(sent2mmi,pWTHj1->sComput,tmmiSet))
      AddSet(sent2mmi,pWTHj1->sComput,tmmiSet);
    else
      sndVipSh2DriverObj = false;

  } else
    bVipShort.chan = pWTHj1->sChan;
  bVipShort.area = pWTHj1->sArea;

  //correct special states in vipShort due to responsibility
  corrSpecialState(pWTHj1->sComput,&bVipShort,false);

  if (sndVipSh2DriverObj)
    if (!SbTransm(&sbF,pWTHj1->sComput,saOdvGat,obODVGAT,lVipS,addrVipShort))
      err(0,3221,"VIPSHORT",sbF,pWTHj1->sComput,saOdvGat,"IIA");
    else {
      if (rTrace.On)
        wrVipShort("pVipChan-1",&bVipShort,pWTHj1->sComput);
      if (DEBUGlevel(100))
        cp(0,100,"pVipCha",bVipShort.dId,pWTHj1->sComput,0.0,"icb");

      if (allMmis)
        SubSet(mmiSet,pWTHj1->sComput,tmmiSet);
      }
  }
if (!(NulSet(mmiSet,tmmiSet) && selChan[0].sComput!=MinComput)) {
  corrSpecialState(selChan[0].sComput,&bVipShort,true);
  insBVipSH(mmiSet,lVipS);
  }

//restore positionability
jPosit = 0;
for (j = nSelDId; j>=1; --j) {
  pWTHj1 = &selChan[j-1];
  if (mmiDir[_AC(pWTHj1->sComput-1,cMaxComput-1)]->VChan[_AC(pWTHj1->sChan-1,
          cMaxChan-1)]->ackPosit)
    jPosit = j;
  }
if (jPosit!=0) {  //There is at least one channel positionable
  //Compress bVipShort: Only acknowledgable  variables
  iW = 1;
  FORLIM = bVipShort.dataL;
  for (iR = 1; iR<=FORLIM; ++iR) {
    if (bVipShort.Data[_AC(iR-1,vipShL-1)].Mark!=0) {
      if (iW!=iR)
        bVipShort.Data[_AC(iW-1,vipShL-1)] =
          bVipShort.Data[_AC(iR-1,vipShL-1)];
      pWTHiW = &bVipShort.Data[iW-1];
      pWTHiW->vipAct = vipActCHan;
      pWTHiW->uvipA.svACh.COption = vipOPos+vipmedi;
      pWTHiW->uvipA.svACh.cLen = 1;
      ++iW;
      }
    }
  bVipShort.dataL = iW-1;
  lVipS = sizeof(tVipShortP)-(vipShL-bVipShort.dataL)*sizeof(ttVipShortP);
  if (bVipShort.dataL>0) {
    /*Send to all Comput/chan/area with selected and positionable
    dId/RelCopKey*/
    if (nextBDId!=0)
      corrSSStart = true;
    for (j = jPosit; j<=nSelDId; ++j) {
      pWTHj1 = &selChan[j-1];
      bVipShort.chan = pWTHj1->sChan;
      bVipShort.area = pWTHj1->sArea;
      //correct special states in vipShort due to responsibility
      corrSpecialState(pWTHj1->sComput,&bVipShort,false);
      if (!SbTransm(&sbF,pWTHj1->sComput,saOdvGat,obODVGAT,lVipS,
                    addrVipShort))
        err(0,3221,"VIPSHORT",sbF,pWTHj1->sComput,saOdvGat,"IIA");
      else {
        if (rTrace.On)
          wrVipShort("pVipChan-2",&bVipShort,pWTHj1->sComput);
        if (DEBUGlevel(100))
          cp(0,100,"pVipCha",bVipShort.dId,pWTHj1->sComput,0.0,"icb");
      }

      }
    }
  }
}  //pVipChan


static
void          pVipPosFl()
{
//VipShort to selected channel

short         nSelDId;
short         j;
tSelChan      selChan;
_REC_tSelChan* pWTHj;
/*Array with Comput,chan where
                                  did/RelCopKey is selected*/
//look for selected dId/RelCopKey
pSearchDId(bVipShort.dId,bVipShort.RelCopKey,&nSelDId,selChan);

addrVipShort = &bVipShort;
lVipS = sizeof(tVipShortP)-(vipShL-bVipShort.dataL)*sizeof(ttVipShortP);

//Send to all Comput/chan/area with selected dId/RelCopKey
for (j = 1; j<=nSelDId; ++j) {
  pWTHj = &selChan[j-1];
  if (pWTHj->sComput==saveFlash.AckPara.Comput && pWTHj->sChan==saveFlash.chan ||
      saveFlash.chan==0) {
    if (saveFlash.chan==0 && bVipShort.RelCopKey==0)
      bVipShort.chan = 0;
    else
      bVipShort.chan = pWTHj->sChan;

    bVipShort.area = pWTHj->sArea;

    //correct special states in vipShort due to responsibility
    corrSSStart = true;
    corrSpecialState(pWTHj->sComput,&bVipShort,false);

    if (!SbTransm(&sbF,pWTHj->sComput,saOdvGat,obODVGAT,lVipS,addrVipShort))
      err(0,3221,"VIPSHORT",sbF,pWTHj->sComput,saOdvGat,"IIA");
    else {
      if (rTrace.On)
      wrVipShort("pVipPosFl",&bVipShort,pWTHj->sComput);
      if (DEBUGlevel(100))
        cp(0,100,"pVipPos",bVipShort.dId,pWTHj->sComput,0.0,"icb");
      }
    }
  }
}  //pVipPosFl


//Read dispDes
static
bool          pRDispDes()
{
bool          pRDispDes_RTN;
dispDes*      pWTHpGDispDesRpP;

pRDispDes_RTN = true;

pGDispDesRpP = 0;
vdispDes.dId = dId;

// Free last DispDes Read

if (!DbRead(&fDispDes,RpOnPage)) {
  err(0,2320,"DBREAD  ",rs("DISP"),vdispDes.dId,fDispDes.Flag,"SII");

  if (fDispDes.Flag==NoK) {
    SubSet(ActFileSet,vdispDes.dId,tActFileSet);
    SubSet(okActFileSet,vdispDes.dId,tActFileSet);
    SubSet(HISActFileSet,vdispDes.dId,tActFileSet);
    meDispGen(false);
        //Delete dId in all concerned medisp records
    if (findBDId(dId,0,bCAF))   //delete bCAF entries
      delCAF(nextBDId);
    }

  pRDispDes_RTN = false;
  goto _L0;
  }
pGDispDesRpP = fDispDes.RpP;

pWTHpGDispDesRpP = pGDispDesRpP;
if (!InSet(pWTHpGDispDesRpP->sDispVar,(int)meVar,pWTHpGDispDesRpP->sDispVar)) {
  pVipShort = 0;
  pMeOut = 0;
  pRDispDes_RTN = false;
  goto _L0;
  }
pVipShort = (tVipShortP*)(&pWTHpGDispDesRpP->information[_AC(
                  pWTHpGDispDesRpP->Directory.iVipShort-1,cMaxInf-1)]);
pMeOut = (tMeOut*)(&pWTHpGDispDesRpP->information[_AC(
                       pWTHpGDispDesRpP->Directory.iMeOut-1,cMaxInf-1)]);
if (current_RelCopKey!=0) {
  rActFileC.dId = pWTHpGDispDesRpP->dId;
  rActFileC.RelCopKey = current_RelCopKey;
  if (!DbRead(&fActFileC,RpOnPage)) {
    err(0,2320,"DBREAD  ",rs("AFC "),rActFileC.dId,fActFileC.Flag,"SII");
    pRDispDes_RTN = false;
    goto _L0;
    }
  pVipShort = &fActFileC.RpP->vipShrtAFC;
  }
if (current_RelCopKey==0 && pGDispDesRpP->spare2!=checkDispD &&
    InSet(okActFileSet,vdispDes.dId,tActFileSet)) {
  err(0,2338,"DISPDES",vdispDes.dId,rs("SP-2"),0.0,"ISI");
  pWrActFile();
  }
_L0:
return pRDispDes_RTN;
}  //pRDispDes


//buffer var into bChangeAF
//(idId:       short;
//Display identification
// DispI:      short;
//Index of variable in dispDes
// aRelCopKey:  tRelCopKey;
static
void          pWBChangeAf(
  tSegId        idId,
  tSegId        DispI,
  tRelCopKey    aRelCopKey,
  tSecs         iSec,                  // seconds, default: gActSec
  tMilS         iMSec)                 // millis,  default: gActMSec
{
tchangeAF*    pWTHnbChangeAF;

if (aRelCopKey!=0) {
  rActFileC.dId = idId;
  rActFileC.RelCopKey = aRelCopKey;
  if (!DbPos(&fActFileC,0))
    return;
  }

++nbChangeAF;
pWTHnbChangeAF = &bChangeAF[nbChangeAF-1];
pWTHnbChangeAF->AFDId = idId;
pWTHnbChangeAF->AFDispI = DispI;
pWTHnbChangeAF->RelCopKey = aRelCopKey;
pWTHnbChangeAF->timeSec = iSec;
pWTHnbChangeAF->timemSec = iMSec;
}  //pWBChangeAf


static
void          pFirstVar()
{
//Search first TA in meDisp

int           u;
int           o;
int           M;
tB123         B123Key;
meDisp*       pWTHpmeDisp;

varFound = false;

B123Key.U1.B1 = receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].TA.B1;
B123Key.U1.B2 = receive.rawInMe.Data[iRawInMe-1].TA.B2;
B123Key.U1.B3 = receive.rawInMe.Data[iRawInMe-1].TA.B3;

if (findB123MeDisp(&B123Key,&M)) {
  pmeDisp = B123Medisp[_AC(M-1,cNB123Medisp)].rp;
  infoS = receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].TA.Info;
  pWTHpmeDisp = pmeDisp;

  //with pmeDisp^
  compElem = receive.rawInMe.Data[_AC(iRawInMe-1,cMaxMess-1)].TA.Elem;
  u = 1;
  o = pWTHpmeDisp->nVar;
  do {
    M = (u+o)/2;
    if (compElem>pWTHpmeDisp->varList[_AC(M-1,cMaxMeDisp-1)].Elem)
      u = M+1;
    else
      o = M-1;
    } while (pWTHpmeDisp->varList[_AC(M-1,cMaxMeDisp-1)].Elem!=compElem &&
             u<=o);

  if (pWTHpmeDisp->varList[_AC(M-1,cMaxMeDisp-1)].Elem==compElem) {
    varInd = M-pWTHpmeDisp->varList[_AC(M-1,cMaxMeDisp-1)].Ind;
    if (InSet(pWTHpmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].infoSet,infoS,
              tinfoSet)) {
      varFound = true;
      if (pWTHpmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].dId!=dId) {
        newDId = true;
        dId = pWTHpmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].dId;
      } else
        newDId = false;
    } else
      pNextVar();
    }  //varList[M].Elem = compElem

  }  //findB123MeDisp
_L0: ;
}  //pFirstVar


static
void          pNextVar()
{
meDisp*       pWTHpmeDisp;
// Search next variable of same TA

varFound = false;
pWTHpmeDisp = pmeDisp;
//with pmeDisp^
++varInd;
while (varInd<=pWTHpmeDisp->nVar && !varFound) {
  if (compElem==pWTHpmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].Elem) {
    if (InSet(pWTHpmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].infoSet,infoS,
              tinfoSet)) {
      varFound = true;
      if (pWTHpmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].dId!=dId)
        newDId = true;
      else
        newDId = false;
      dId = pWTHpmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].dId;
    } else
      ++varInd;
  } else
    varInd = pWTHpmeDisp->nVar+1;
  }
}  //pNextVar


//insert variable/   entry in bCAF
static
bool          insBCAF(
  tSegId        bCDId,            //key1: Display identification
  tRelCopKey    bCRelCopKey,      //key2: Copy
  tSegId        bCDispI)          //key3: Index of variable in dispDes
{
if (!findBDId(bCDId,bCRelCopKey,bCAF)) {  //find dID entry in bCAF
  prepbDId(&bCAF,bCDId,bCRelCopKey);
  ++nCAF;
  nextBDId->VarSet = (uint16_t*)malloc_chk(sizeof(tVarSet));
  CleSet(nextBDId->VarSet,tVarSet);
  }

AddSet(nextBDId->VarSet,bCDispI,tVarSet);

return true;
}  //insBCAF
//Update selected displays and actfiles


// Local variables for pUpdateD:
struct LCL_pUpdateD {
  bool          posFlash;
};


static
void          pUpdtVarD(
  struct LCL_pUpdateD* LNK_pUpdateD)
{
ttVipShortP*  pWTHiWVipShort;
//Update one Variable into pVipShort^
pmeVar = &pMeOut->varDes[_AC(pChangeAF->AFDispI-1,vipShL-1)];
iRVarDes = pChangeAF->AFDispI;
iWVipShort = pChangeAF->AFDispI;

if (pMeOut->nMeOut<iWVipShort) {
  if (InSet(okActFileSet,vdispDes.dId,tActFileSet))
    err(0,2001,"wrong IW",vdispDes.dId,iWVipShort,pMeOut->nMeOut,"III");
  return;
  }

if (!LNK_pUpdateD->posFlash) {
  pVarDisplay();                  //Display of one variable
  if (InSet(pmeVar->proMode,(int)ackPerm,sProMode) && changeFlash)
    pVipShort->Data[_AC(iWVipShort-1,vipShL-1)].Mark = 0xff;
  else
    pVipShort->Data[iWVipShort-1].Mark = '\0';
  }
++bVipShort.dataL;
bVipShort.Data[_AC(bVipShort.dataL-1,vipShL-1)] =
  pVipShort->Data[_AC(iWVipShort-1,vipShL-1)];

#if SUBS_SHHIS
// add entry to HIS buffer
memcpy(&bVipHIS.data[bVipHIS.dataL].VipShortH,
       &pVipShort->Data[iWVipShort-1],sizeof(ttVipShort));
//bVipHIS.data[bVipHIS.dataL].VipShortH = pVipShort->Data[iWVipShort];
bVipHIS.data[bVipHIS.dataL].timeSec = pChangeAF->timeSec;
bVipHIS.data[bVipHIS.dataL].timemSec = pChangeAF->timemSec;
bVipHIS.dataL = bVipHIS.dataL + 1;
#endif

if (LNK_pUpdateD->posFlash) {
  bVipShort.Data[_AC(bVipShort.dataL-1,vipShL-1)].aGroup = cFlashAGroup;
  bVipShort.Data[bVipShort.dataL-1].aAlt = (int)saveFlash.flashMod+1;
  if (nextBDId!=0) {
    if (findBDispI(iWVipShort)) {
      pWTHiWVipShort = &pVipShort->Data[iWVipShort-1];
      nextBDispI->gid = pWTHiWVipShort->gid;
      nextBDispI->saveAGroup = pWTHiWVipShort->aGroup;
      nextBDispI->aAltPosFl = (int)saveFlash.flashMod+1;
      if (nextBDispI->fAltNorm==10001) {
        nextBDispI->fAltFl = pWTHiWVipShort->uvipA.svALG.fAlt;
        nextBDispI->fAltNorm = pWTHiWVipShort->uvipA.svALG.fAlt;
        //nextBDispI->aAltFl  = pWTHiWVipShort->aAlt;
        set_aAltFl_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_DISPI);
        //nextBDispI->aAltNorm  = pWTHiWVipShort->aAlt;
        set_aAltNorm_mph(pWTHiWVipShort,nextBDispI,C_SET_ALT_DISPI);
        if (pWTHiWVipShort->vipAct==vipActINteger)
          nextBDispI->iValDes = pWTHiWVipShort->uvipA.svAI.iValue;
        if (pWTHiWVipShort->vipAct==vipActREal)
          nextBDispI->rValDes = pWTHiWVipShort->uvipA.svAR.rValue;
        }
      }
    }
  }
}


static
void          pUpdateD()
{
struct LCL_pUpdateD V;
int           i;
int           j;
bool          updtPosFlash;
short         nSelDId;
tSelChan      selChan;
    //Array with Comp.,chan where did/RelCopK. is select
int           FORLIM;
tchangeAF*    pWTHi;
int           FORLIM1;

V.posFlash = false;
updtPosFlash = false;
if (receive.U1.Header2==jFlash && saveFlash.flashMod!=desFlash) {
  if (saveFlash.chan!=0)
    updtPosFlash = true;
  if (saveFlash.flash)
    V.posFlash = true;
  }

#if SUBS_SHHIS
if (receive.U1.Header2==jrawInMe) {
  bVipHIS.Sec = receive.rawInMe.TimeSec;
  bVipHIS.mSec = receive.rawInMe.TimemSec;
  }
#endif

#if SUBS_SHHIS
if (markbcaf && receive.U1.Header2!=jrawInMe) {
#else
if (markbcaf) {
#endif
  FORLIM = nbChangeAF;
  for (i = 1; i<=FORLIM; ++i) {
    pWTHi = &bChangeAF[i-1];
    //with..
    if (pWTHi->AFDId!=0) {
      pSearchDId(pWTHi->AFDId,pWTHi->RelCopKey,&nSelDId,selChan);
      if (nSelDId==0 && pWTHi->RelCopKey==0) {
        pChangeAF = &bChangeAF[_AC(i-1,bCAFMax-1)];
        if (insBCAF(pWTHi->AFDId,pWTHi->RelCopKey,pWTHi->AFDispI)) {
          pChangeAF->AFDId = 0;
          }  //if insBCAF..
        }  //if nSelDId=0
      }  //if AFDId<>0 then..
    }
  }  //if..
FORLIM = nbChangeAF;
for (i = 1; i<=FORLIM; ++i) {
  pWTHi = &bChangeAF[i-1];
  //bChangeAF[i]
  if (pWTHi->AFDId!=0) {
    dId = pWTHi->AFDId;
    if (pRDispDes()) {            //Read dispDes
      bVipShort.RelCopKey = pWTHi->RelCopKey;
      bVipShort.dId = pVipShort->dId;
      bVipShort.dataL = 0;

#if SUBS_SHHIS
      bVipHIS.did = pVipShort->dId;
      bVipHIS.dataL = 0;
#endif

      pChangeAF = &bChangeAF[_AC(i-1,bCAFMax-1)];
      if (!findBDId(bVipShort.dId,pWTHi->RelCopKey,bFlash)) {
        //find dID entry in bFlash
        nextBDId = 0;
        }
      pUpdtVarD(&V);              //Update one Variable into pVipShort^
      FORLIM1 = nbChangeAF;
      for (j = i+1; j<=FORLIM1; ++j) {
        if (bChangeAF[_AC(j-1,bCAFMax-1)].AFDId==dId &&
            bChangeAF[j-1].RelCopKey==pWTHi->RelCopKey) {
          pChangeAF = &bChangeAF[_AC(j-1,bCAFMax-1)];
          pUpdtVarD(&V);          //Update one Variable into pVipShort^
          pChangeAF->AFDId = 0;
          }
        }
      if (current_RelCopKey==0) {
        if (updtPosFlash)         //VipShort to selected channel
          pVipPosFl();
        else {
          if (!DbWrite(&fDispDes,WrRpOnPage))
            err(0,2321,"DBWRITE ",rs("DISP"),vdispDes.dId,fDispDes.Flag,"SII");
#if SUBS_SHHIS
          pVipHIS(&bVipShort,false,false); //data to HIS
#endif
          pVipChan(true);         //VipShort to all channels
          }
      } else {
        if (updtPosFlash)         //VipShort to selected channel
          pVipPosFl();
        else {
          if (!DbWrite(&fActFileC,RpOnPage))
            err(0,2321,"DBWRITE ",rs("AFC "),rActFileC.dId,fActFileC.Flag,
                "SII");
          pVipChan(false);
              //Send vipShort to all chan`s with sel. dId
          }
        }
      }  //if pRDispDes
    }  //AFDid <> 0
  }
markbcaf = true;
nbChangeAF = 0;
}  //pUpdateD


//delete siFlash TA in bFlash
// Local variables for delSiTA:
struct LCL_delSiTA {
  tTA*          delSTA;           //key: technological address
  ComputId      comp;             //computer ID
  short         conChan;          //System service channel
  short         siChan;           //channel with siFlash
  tPtB          PtB;              //buffer pointers
};


static
bool          TAEqual_2(
  struct LCL_delSiTA* LNK_delSiTA)
{
if (LNK_delSiTA->delSTA->B1!=0) {
  if (LNK_delSiTA->PtB.nextBTA->ta.B1==LNK_delSiTA->delSTA->B1 &&
      LNK_delSiTA->PtB.nextBTA->ta.B2==LNK_delSiTA->delSTA->B2 &&
      LNK_delSiTA->PtB.nextBTA->ta.B3==LNK_delSiTA->delSTA->B3 &&
      LNK_delSiTA->PtB.nextBTA->ta.Elem==LNK_delSiTA->delSTA->Elem &&
      LNK_delSiTA->PtB.nextBTA->ta.Info==LNK_delSiTA->delSTA->Info &&
      (LNK_delSiTA->PtB.nextBTA->comp==LNK_delSiTA->comp || LNK_delSiTA->comp==0) &&
      (LNK_delSiTA->PtB.nextBTA->conChan==LNK_delSiTA->conChan ||
       LNK_delSiTA->conChan==0))
    return true;
} else {
  if (LNK_delSiTA->comp!=0) {
    if (LNK_delSiTA->PtB.nextBTA->comp==LNK_delSiTA->comp &&
        LNK_delSiTA->PtB.nextBTA->conChan==LNK_delSiTA->conChan)
      return true;
  } else {
    if (LNK_delSiTA->PtB.nextBTA->siChan==LNK_delSiTA->siChan)
      return true;
    }
  }
return false;
}


static
bool          delSiTA(
  tRelCopKey    delRelCopKey,
  tTA*          delSTA_,
  ComputId      comp_,
  short         conChan_,
  short         siChan_,
  bool          i_update)         //update variablens y/n
{
struct LCL_delSiTA V;
bool          delSiTA_RTN;
bool          delTA;

V.delSTA = delSTA_;
V.comp = comp_;
V.conChan = conChan_;
V.siChan = siChan_;
nbChangeAF = 0;
delSiTA_RTN = false;

V.PtB.bSelect = bFlash;
//bFlash loop: 1
V.PtB.nextBDId = V.PtB.bSelect;
V.PtB.precBDId = 0;

while (V.PtB.nextBDId!=0) {
  V.PtB.nextBDispI = V.PtB.nextBDId->nextBDispI;
  V.PtB.precBDispI = 0;
  while (V.PtB.nextBDispI!=0) {
    V.PtB.nextBTA = V.PtB.nextBDispI->nextBTA;
    V.PtB.precBTA = 0;
    while (V.PtB.nextBTA!=0) {
      delTA = false;
      //bFlash loop: 2

      if ((V.PtB.nextBTA->desFlash==true || V.PtB.nextBTA->posFlash==true) &&
          TAEqual_2(&V) && V.PtB.nextBDId->RelCopKey==delRelCopKey) {
        if ((receive.U1.Header2==jFlash && saveFlash.flashMod==desFlash ||
             V.siChan==0) && V.PtB.nextBTA->desFlash==true) {
          delSiTA_RTN = true;
          V.PtB.nextBTA->desFlash = false;
          if (V.PtB.nextBTA->posFlash==false)
            V.PtB.nextBDispI->saveAGroup = 0;
          //buffer var into bChangeAF
          pWBChangeAf(V.PtB.nextBDId->AFDId,V.PtB.nextBDispI->AFDispI,
                      current_RelCopKey,
                      receive.rawInMe.Data[iRawInMe-1].timeSec,
                      receive.rawInMe.Data[iRawInMe-1].timemSec);
          if (nbChangeAF==bCAFMax && i_update) {
            bFlash = V.PtB.bSelect;
            markbcaf = false;
            pUpdateD();
            }
          }

        if ((receive.U1.Header2==jFlash && saveFlash.flashMod!=desFlash ||
             V.siChan==0) && V.PtB.nextBTA->posFlash==true) {
          delSiTA_RTN = true;
          V.PtB.nextBTA->posFlash = false;
          if (V.PtB.nextBTA->desFlash==false)
            V.PtB.nextBDispI->saveAGroup = 0;
          V.PtB.nextBDispI->aAltPosFl = -1;
          //buffer var into bChangeAF
          pWBChangeAf(V.PtB.nextBDId->AFDId,V.PtB.nextBDispI->AFDispI,
                      current_RelCopKey,
                      receive.rawInMe.Data[iRawInMe-1].timeSec,
                      receive.rawInMe.Data[iRawInMe-1].timemSec);
          if (nbChangeAF==bCAFMax && i_update) {
            bFlash = V.PtB.bSelect;
            markbcaf = false;
            pUpdateD();
            }
          }

        if (V.PtB.nextBTA->desFlash==false &&
            V.PtB.nextBTA->posFlash==false &&
            V.PtB.nextBTA->acknFlash==false) {
          delTA = true;
          disposeTA(&V.PtB);
          }
        }

      //bFlash loop: 3
      if (!delTA) {
        V.PtB.precBTA = V.PtB.nextBTA;
        V.PtB.nextBTA = V.PtB.nextBTA->nextBTA;
        }
      }
    if (V.PtB.nextBDispI->nextBTA==0)
      disposeDispI(&V.PtB);
    else {
      V.PtB.precBDispI = V.PtB.nextBDispI;
      V.PtB.nextBDispI = V.PtB.nextBDispI->nextBDispI;
      }
    }
  if (V.PtB.nextBDId->nextBDispI==0)
    disposeDId(&V.PtB);
  else {
    V.PtB.precBDId = V.PtB.nextBDId;
    V.PtB.nextBDId = V.PtB.nextBDId->nextBDId;
    }
  }
//bFlash loop: 4

bFlash = V.PtB.bSelect;
if (nbChangeAF>0 && i_update) {
  markbcaf = false;
  pUpdateD();
  }  //Upd. sel. displ. and actfiles
return delSiTA_RTN;
}  //delSiTA


static
void          pRawInMe()
{
//Status change
//Description see main program.

int           i;
tstatchange   statinfoSave;
int           FORLIM;
tRawInMeData* pWTHi;
if (rTrace.On)
  wrRawInMe();

FORLIM = receive.rawInMe.nRawInMe;
for (i = 1; i<=FORLIM; ++i) {
  pWTHi = &receive.rawInMe.Data[i-1];
  iRawInMe = i;
  if (pWTHi->delSiFlash)
    delSiTA(current_RelCopKey,&pWTHi->TA,0,0,0,false);
        //delete siFlash TA in bFlash
  }

nbChangeAF = 0;
dId = 0;

FORLIM = receive.rawInMe.nRawInMe;
for (i = 1; i<=FORLIM; ++i) {
  iRawInMe = i;
  pFirstVar();                    //Search first TA in meDisp
  while (varFound) {
    statinfoSave = receive.rawInMe.Data[_AC(i-1,cMaxMess-1)].statinfo;
    if (insBFlash() &&
        !NulSet(pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].prM,tPrMSet))
          //insert variable/TA into bFlash
            pActSign();
    //Activate counters
    receive.rawInMe.Data[_AC(i-1,cMaxMess-1)].statinfo = statinfoSave;
    //buffer var into bChangeAF
    pWBChangeAf(dId,pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].meDispI,
                current_RelCopKey,
                receive.rawInMe.Data[i-1].timeSec,
                receive.rawInMe.Data[i-1].timemSec);
    if (nbChangeAF==bCAFMax)
      pUpdateD();
    pNextVar();                   //Search next variable of same TA
    }  //while varFound
  }  //for i
if (nbChangeAF>0)                 //Update selected displays and actfiles
  pUpdateD();
pFRel();                          //Release dispDes
                                  //*t* pFDispDes ??
// Now process all dynamic allocated variables.
DynVarProcessRawInf(&receive.rawInMe);

receive.rawInMe.nRawInMe = 0;
}  //pRawInMe


static
void          pRawInMeTopo()
{  //pRawInMe
//Status change
//Description see main program.

nbChangeAF = 0;
dId = 0;

if (rTrace.On) wrRawInMeTopo();

tRawInMeDataTopo* pReceTopoData = NULL;
int nItems = receive.rawInMeTopo.nRawInMe;

bool lOk = true;
if (nItems > 0) {
  pReceTopoData = new (nothrow)tRawInMeDataTopo[nItems];
  if (pReceTopoData == NULL) {
    errfmt(0,3001,"pRawInMT","pRawInMeTopo: RawInMeTopoData memory alloc error - break");
    lOk = false;
  } else {
    for (int i=0; i < nItems; i++)
      pReceTopoData[_AC(i,cMaxMessTopo-1)] = receive.rawInMeTopo.Data[i];
  }

}

iRawInMe = 1;
if (lOk) for (i = 0; i < nItems; i++) {
  receive.rawInMe.Data[0].TA.B1 = pReceTopoData[i].TA.B1;
  receive.rawInMe.Data[0].TA.B2 = pReceTopoData[i].TA.B2;
  receive.rawInMe.Data[0].TA.B3 = pReceTopoData[i].TA.B3;
  receive.rawInMe.Data[0].TA.Elem = pReceTopoData[i].TA.Elem;
  receive.rawInMe.Data[0].TA.Info = pReceTopoData[i].TA.Info;
  receive.rawInMe.Data[0].timeSec = pReceTopoData[i].timeSec;
  receive.rawInMe.Data[0].timemSec = pReceTopoData[i].timemSec;
  pFirstVar();                    //Search first TA in meDisp
  while (varFound) {
    //buffer var into bChangeAF
    pWBChangeAf(dId,pmeDisp->varList[_AC(varInd-1,cMaxMeDisp-1)].meDispI,
                current_RelCopKey,
                receive.rawInMe.Data[iRawInMe-1].timeSec,
                receive.rawInMe.Data[iRawInMe-1].timemSec);
    if (nbChangeAF==bCAFMax)
      pUpdateD();
    pNextVar();                   //Search next variable of same TA
    }  //while varFound

  // Now process dynamic allocated variables.
  DynVarProcessRawInf(&receive.rawInMe);

  }  //for i
if (nbChangeAF>0)                 //Update selected displays and actfiles
  pUpdateD();
pFRel();                          //Release dispDes
                                  //*t* pFDispDes ??
receive.rawInMeTopo.nRawInMe = 0;
if (pReceTopoData != NULL)
  delete [] pReceTopoData;
}  //pRawInMeTopo


static
void          pFlash()
{
tRawInMeData* pWTHData;
//Flashing
//Description see main program.

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

saveFlash = receive.flash;
if (receive.flash.flash) {
  receive.rawInMe.nRawInMe = 1;
  pWTHData = receive.rawInMe.Data;
  pWTHData->TA = saveFlash.TA;
  pWTHData->statinfo = NoStatChange;
  pWTHData->meClass = 0;
  pWTHData->InfoType = 0;
  pWTHData->NoElTy = saveFlash.NoElType;
  pWTHData->delSiFlash = false;
  pWTHData->timeSec = gActSec;
  pWTHData->timemSec = gActMSec;
  pRawInMe();
} else {
  //reset flashing
  receive.rawInMe.nRawInMe = 1;
  iRawInMe = 1;
  receive.rawInMe.Data[0].statinfo = NoStatChange;
  receive.rawInMe.Data[0].timeSec = gActSec;
  receive.rawInMe.Data[0].timemSec = gActMSec;

  delSiTA(saveFlash.RelCopKey,&saveFlash.TA,saveFlash.CompId,saveFlash.ConChan,
          saveFlash.chan,true);
      //delete siFlash TA in bFlash
  }
}  //pFlash


//acknowledgement permitted (due to MeCl and TeArea)
static
bool          AcknPermit(
  ComputId      Comput,           //acknowledgeing mmi
  short         Chan)             //acknowledgeing channel
{
bool          DeTeArEqu;
bool          MeClEqu;
trMmiDir*     pWTHComput;
DeTeArEqu = false;
MeClEqu = false;
if (mmiDir[_AC(Comput-1,cMaxComput-1)]!=0) {
  pWTHComput = mmiDir[_AC(Comput-1,cMaxComput-1)];
  if (EquSet(pWTHComput->mmiATeArS[_AC(Chan-1,cMaxChannelPerMMI-1)],
             fullMmiATeArS,tTeArS) || cMaxTear==1)
    DeTeArEqu = true;
  else {
    rNimad.B1   = TA.B1;
    rNimad.B2   = TA.B2;
    rNimad.B3   = TA.B3;
    rNimad.Elem = TA.Elem;
    if (!DbRead(&fNimad,0)) {
      err(0,2335,"Nimad   ",fNimad.Flag,TA.B1,TA.B2,"III");
      err(0,2336,"Nimad   ",0,TA.B3,TA.Elem,"BII");
      }
    else {
      if (InSet(pWTHComput->mmiATeArS[_AC(Chan-1,cMaxChannelPerMMI-1)],
                rNimad.TeArea,tTeArS))
        DeTeArEqu = true;
      }
    }
  if (EquSet(pWTHComput->mmiAMeClS[_AC(Chan-1,cMaxChannelPerMMI-1)],
             fullMmiAMeClS,tMeClS))
    MeClEqu = true;
  else {
    if (InSet(pWTHComput->mmiAMeClS[_AC(Chan-1,cMaxChannelPerMMI-1)],
              NimVarTy.pInTyDe->MeldeKlasse,tMeClS))
      MeClEqu = true;
    }
  }
return DeTeArEqu && MeClEqu;
}  //AcknPermit


static
void          pSvcAck()
{
//Acknowledgement to PSM

if (saveAckpara.AckCont!=AckNo) {
  //with receive
  SvcA.sSAk0.Header1 = saveAckpara.Acknowl.Header1;
  SvcA.sSAk0.Header2 = saveAckpara.Acknowl.Header2;
  SvcA.sSAk0.RelCopKey = receive.U1.RelCopKey;
  SvcA.sSAk0.Comput = saveAckpara.Comput;
  SvcA.sSAk0.AckIdent = saveAckpara.Acknowl.AckIdent;

  if (!SbTransm(&sbF,saveAckpara.Comput,saveAckpara.Adres,saveAckpara.Object,
                lSvcA,&SvcA))
    err(0,3211,"SVCA    ",rs("SBTR"),sbF,saveAckpara.Adres,"SII");
  }
}  //pSvcAck


static
void          pTransmLidi()
{
//Order to LIDI

size_t        sbsize;             //size of SB transmit buffer

sbsize = sizeof(tLiUAckNow)-(cLiQSL-bLiUAcknow.n)*sizeof(tMessLiAck);
bLiUAcknow.RelCopKey = current_RelCopKey;
if (!SbTransm(&sbF,OwnComput,saLiRaw,obLIDI,sbsize,&bLiUAcknow)) {
  //chr(obLIDI),sizeof(tLiUAckNow),addr(bLiUAcknow))
  err(0,3211,"LIDI    ",rs("SBTR"),sbF,saLiRaw,"SII");
  }
bLiUAcknow.n = 0;
}  //pTransmLidi


//send acknowledgement to the other control centers
static
void          CCAcknowledge(
  tRelCopKey    iRelCopKey,
  tOpId         iOpId,
  NormElem      iNoEl,
  tInfo         ivarInfo,
  tNimSet       iNimSet)
{
tSecs         Sec;                //seconds
tMilS         SecM;               //milliseconds
tTechId       TechId;             //technological identifier
tTechData     TechData;           //technological data

//Build technological identifier.
TechId.NoEl = iNoEl;
TechId.Info = ivarInfo;
TechId.SetNr = iNimSet;

//Build technological data.
IpcGetTime(&Sec,&SecM);
InitTechData(&TechData);
TechData.Sec = Sec;
TechData.mSec = SecM;
TechData.Qb0 = vaOpAckGraf;
TechData.Val.VshortA[3] = 0;      //info type

//Send acknowledgement.
NIMSETOPC4(&iOpId,opc4_Global);
UIcAcknOrd(&UIcIntVar,0,iRelCopKey,&iOpId,&TechId,&TechData,0);
}  //CCAcknowledge


static
void          pSingAck()
{
//Single acknowledgement
//Description see main program.

bool          ackOk;
tInfotyp      saveInfoType;
tNimSet       saveNimSatz;
NormElem      saveNoElTy;
tSingAck      saveSingAck;
tTA           saveTA;
tInfo         varInfo;            //Info number out of variable description
tMediAck      AckToev;
int           Flag;
tOpId         saveOpId;
tRawInMeData* pWTHData;
tMessLiAck*   pWTHmessages;

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

saveOpId = receive.singAck.OpId;
saveAckpara = receive.U1.AckPara;

receive.U1.AckPara.AckCont = AckNo;
if (!McPTran(&sbFRec,saMedi,obMEDI,SosyData.sdSBRsiz,&receive.U1.Header1,
             PCOnly))
  err(0,3232,"SingAck ",rs("MCPT"),sbFRec,diAddr,"SIA");
sbFRec = 0;
receive.U1.AckPara.AckCont = saveAckpara.AckCont;

SvcA.sSAk0.Ok = true;
SvcA.sSAk0.textid = sTxNoText;

TA.B1 = receive.singAck.TA.B1;
TA.B2 = receive.singAck.TA.B2;
TA.B3 = receive.singAck.TA.B3;
TA.Elem = receive.singAck.TA.Elem;
TA.Info = receive.singAck.TA.Info;
varInfo = receive.singAck.TA.Info;
pInfoType();                      //Determine info type, read Intyde
if (!AcknPermit(saveAckpara.Comput,receive.singAck.chan)) {
  //acknowledgement permitted (due to MeCl and TeArea)
  SvcA.sSAk0.Ok = false;
  SvcA.sSAk0.textid = stxMeNoAckn;
  goto _L0;
  }

TA.Info = 0;
if (!NIMRMP(&NimVarMp.FlagS,nTeAd+nLock,current_RelCopKey,0)) {
  err(0,2331,"NIM     ",NimVarMp.FlagS,receive.singAck.TA.B3,
      receive.singAck.TA.Elem,"III");
  endOfJob = true;
  longjmp(_JL99,1);               //end of job
  }

//Delete acknowledge bit
InfVal = 0;
if (SETINFO(receive.singAck.TA.Info,SubAckn,
            NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],NimVarMp.pInfStr,
            &InfVal,&Leng))
  ackOk = true;
else
  ackOk = false;

if (!NIMWMP(&NimVarMp.FlagS,0,current_RelCopKey,0)) {
  err(0,2332,"NIM     ",NimVarMp.FlagS,receive.singAck.TA.B3,
      receive.singAck.TA.Elem,"III");
  endOfJob = true;
  longjmp(_JL99,1);               //end of job
  }

TA.Info = receive.singAck.TA.Info;

if (ackOk)
  acknTA(receive.singAck.RelCopKey,&TA);
else
  ackOk = acknTA(receive.singAck.RelCopKey,&TA);

if (!ackOk) {
  SvcA.sSAk0.Ok = false;
  SvcA.sSAk0.textid = stxMeNoAckn;
} else {
  //ackOk
  NimVarTy.InfoRef = NimVarMp.InfoRef;
  CleSet(NimVarTy.RelSet,tsNimRfix);
  AddSet(NimVarTy.RelSet,xInfoDef,tsNimRfix);
  AddSet(NimVarTy.RelSet,xInTyDe,tsNimRfix);
  NimVarTy.NoElTy = NimVarMp.NoElTy;
  NimVarTy.ElNimSatz = NimVarMp.NimSatz;
  if (!NIMRTY(&NimVarTy.FlagS,nMe+nIn,current_RelCopKey,0)) {
    err(0,2333,"NIM     ",NimVarTy.FlagS,NimVarMp.InfoRef,TA.Info,"III");
    endOfJob = true;
    longjmp(_JL99,1);             //end of job
    }

  saveSingAck = receive.singAck;
  saveTA = *NimVarMp.pTa;
  saveNoElTy = NimVarMp.NoElTy;
  saveNimSatz = NimVarMp.NimSatz;
  saveInfoType = NimVarTy.pTyNu->InfoType;

  //Create raw information
  receive.rawInMe.nRawInMe = 1;
  pWTHData = receive.rawInMe.Data;
  pWTHData->TA = *NimVarMp.pTa;
  pWTHData->statinfo = AckChg;
  pWTHData->meClass = NimVarTy.pInTyDe->MeldeKlasse;
  pWTHData->InfoType = 0;
  pInfoStr = (ByteInt*)NimVarMp.pInfStr;
  memcpy(pWTHData->InfStr,pInfoStr,NimVarMp.StrLeng[NimVarMp.NoElTy]);
  pWTHData->NoElTy = NimVarMp.NoElTy;
  pWTHData->delSiFlash = false;
  pWTHData->timeSec = gActSec;
  pWTHData->timemSec = gActMSec;
  pRawInMe();

  if (NimVarTy.pInTyDe->AckGrafLi) {
    //Common acknowledgement to list
    bLiUAcknow.n = 1;
    bLiUAcknow.OpId = saveOpId;
    pWTHmessages = bLiUAcknow.messages;
    pWTHmessages->TA = saveTA;
    pWTHmessages->TA.Info = varInfo;
    pWTHmessages->infoTyp = saveInfoType;
    pTransmLidi();                //Order to LIDI
    }

  if (NimVarMp.NoElTy==neSwitch) {
    AckToev.Entries = 1;
    AckToev.messages[0].B1 = saveTA.B1;
    AckToev.messages[0].B2 = saveTA.B2;
    AckToev.messages[0].B3 = saveTA.B3;
    AckToev.messages[0].Elem = saveTA.Elem;
    AckToev.messages[0].Info = varInfo;
    TopIfMedi(&Flag,&AckToev);    //Order to TOEV
    AckToev.Entries = 0;
    }

  //send acknowledgement to the other control centers
  CCAcknowledge(saveSingAck.RelCopKey,saveOpId,saveNoElTy,varInfo,saveNimSatz);
  }

_L0:
pSvcAck();                        //Acknowledgement to PSM
}  //pSingAck


static
void          pInfoType()
{
//Determine info type

CleSet(NimVarTy.RelSet,tsNimRfix);
AddSet(NimVarTy.RelSet,xInfoDef,tsNimRfix);
AddSet(NimVarTy.RelSet,xInTyDe,tsNimRfix);
if (!NIMRTY(&NimVarTy.FlagS,nTeAd+nIn,current_RelCopKey,0)) {
  err(0,2333,"NIM     ",NimVarTy.FlagS,NimVarMp.InfoRef,TA.Info,"III");
  endOfJob = true;
  longjmp(_JL99,1);
  }
}  //pInfoType


static
void          pAfUpdate()
{
//Update updating file

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

if (saveVipDispAck.RelCopKey!=0 ||
    InSet(ActFileSet,dId,tActFileSet) && !InSet(okActFileSet,dId,tActFileSet))
      //dId contains meVar but is not updated yet
        pWrActFile();
//Generate (write) actfile
pUpdateCAF(dId,saveVipDispAck.RelCopKey);
}  //pAfUpdate
//Acknowledge display segment


// Local variables for pFulSegAck:
struct LCL_pFulSegAck {
  int           iR;
  int           nimOption;
  int           saveDId;
  tInfo         varInfo;          //Info number out of variable description
  bool          ackOk;
  tMediAck      AckToev;
  int           Flag;
};


static
void          ackVar(
  struct LCL_pFulSegAck* LNK_pFulSegAck)
{
//Acknowledge variable
tRawInMeData* pWTHnRawInMe;
tMessLiAck*   pWTHn;

if (!INREAD(&fInputVar,dId,
            pVipShort->Data[_AC(LNK_pFulSegAck->iR-1,vipShL-1)].gid,
            &rinVar)) {
  err(0,2334,"INPUTVAR",dId,
      pVipShort->Data[_AC(LNK_pFulSegAck->iR-1,vipShL-1)].gid,
      0,"III");
  endOfJob = true;
  longjmp(_JL99,1);
  }

TA = rinVar.Info.uVipV.smeVa.meTa;

pInfoType();                      //Determine info type, read Intyde

if (AcknPermit(saveVipDispAck.AckPara.Comput,saveVipDispAck.chan)) {
  //acknowledgement permitted (due to MeCl and TeArea)
  TA.Info = 0;
  if (InSet(pmeVar->proMode,(int)deTaEl,sProMode)) {
    LNK_pFulSegAck->nimOption = nLock;
    NimVarMp.NimSatz = pmeVar->taMeVar.uCmb.sTAns.NimSatz;
    NimVarMp.NoElTy = pmeVar->taMeVar.uCmb.sTAns.NoElType;
  } else
    LNK_pFulSegAck->nimOption = nTeAd+nLock;
  if (!NIMRMP(&NimVarMp.FlagS,LNK_pFulSegAck->nimOption,current_RelCopKey,0)) {
    err(0,2331,"NIM     ",NimVarMp.FlagS,TA.B3,TA.Elem,"III");
    endOfJob = true;
    longjmp(_JL99,1);
    }

  //Delete acknowledge bit
  InfVal = 0;
  if (SETINFO(rinVar.Info.uVipV.smeVa.meTa.Info,SubAckn,
              NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],NimVarMp.pInfStr,
              &InfVal,&Leng))
    LNK_pFulSegAck->ackOk = true;
  else
    LNK_pFulSegAck->ackOk = false;

  if (!NIMWMP(&NimVarMp.FlagS,0,current_RelCopKey,0)) {
    err(0,2332,"NIM     ",NimVarMp.FlagS,TA.B3,TA.Elem,"III");
    endOfJob = true;
    longjmp(_JL99,1);
    }

  //Restore Info number of acknowledge info
  LNK_pFulSegAck->varInfo = rinVar.Info.uVipV.smeVa.meTa.Info;
  TA.Info = LNK_pFulSegAck->varInfo;

  if (LNK_pFulSegAck->ackOk)
    acknTA(receive.vDispAck.RelCopKey,&TA);
  else
    LNK_pFulSegAck->ackOk = acknTA(receive.vDispAck.RelCopKey,&TA);

  if (LNK_pFulSegAck->ackOk) {
    //Create raw information
    ++receive.rawInMe.nRawInMe;
    pWTHnRawInMe = &receive.rawInMe.Data[receive.rawInMe.nRawInMe-1];

    pWTHnRawInMe->TA = *NimVar.pTa;
    pWTHnRawInMe->statinfo = AckChg;
    pWTHnRawInMe->meClass = NimVarTy.pInTyDe->MeldeKlasse;
    pWTHnRawInMe->InfoType = 0;
    pInfoStr = (ByteInt*)NimVarMp.pInfStr;
    memcpy(pWTHnRawInMe->InfStr,pInfoStr,NimVarMp.StrLeng[NimVarMp.NoElTy]);
    pWTHnRawInMe->NoElTy = NimVarMp.NoElTy;
    pWTHnRawInMe->delSiFlash = false;
    pWTHnRawInMe->timeSec = gActSec;
    pWTHnRawInMe->timemSec = gActMSec;
    if (NimVarTy.pInTyDe->AckGrafLi) {
      //Common acknowledgement to list
      if (bLiUAcknow.n==cLiQSL)   //Order to LIDI
        pTransmLidi();
      ++bLiUAcknow.n;
      bLiUAcknow.OpId = saveVipDispAck.OpId;
      pWTHn = &bLiUAcknow.messages[bLiUAcknow.n-1];
      pWTHn->TA = *NimVar.pTa;
      pWTHn->infoTyp = NimVarTy.pTyNu->InfoType;
      }

    if (NimVarMp.NoElTy==neSwitch) {
      if (LNK_pFulSegAck->AckToev.Entries==maxTA5) {
        TopIfMedi(&LNK_pFulSegAck->Flag,&LNK_pFulSegAck->AckToev);
            //Order to TOEV
        LNK_pFulSegAck->AckToev.Entries = 0;
        }
      ++LNK_pFulSegAck->AckToev.Entries;
      LNK_pFulSegAck->AckToev.messages[_AC(LNK_pFulSegAck->AckToev.Entries-1,
                                           maxTA5-1)].B1 = NimVarMp.pTa->B1;
      LNK_pFulSegAck->AckToev.messages[LNK_pFulSegAck->AckToev.Entries-1].B2 =
        NimVarMp.pTa->B2;
      LNK_pFulSegAck->AckToev.messages[LNK_pFulSegAck->AckToev.Entries-1].B3 =
        NimVarMp.pTa->B3;
      LNK_pFulSegAck->AckToev.messages[LNK_pFulSegAck->AckToev.Entries-1].Elem =
        NimVarMp.pTa->Elem;
      LNK_pFulSegAck->AckToev.messages[LNK_pFulSegAck->AckToev.Entries-1].Info =
        LNK_pFulSegAck->varInfo;
      }

    //send acknowledgement to the other control centers
    CCAcknowledge(saveVipDispAck.RelCopKey,saveVipDispAck.OpId,NimVarMp.NoElTy,
                  LNK_pFulSegAck->varInfo,NimVarMp.NimSatz);

    if (receive.rawInMe.nRawInMe==cMaxMess) {
      pRawInMe();
      receive.rawInMe.nRawInMe = 0;
      dId = LNK_pFulSegAck->saveDId;
      pRDispDes();
      }
    }  //ackOk
  }  //if message class and technol. area
}  //ackVar


static
void          pFulSegAck()
{
struct LCL_pFulSegAck V;
bool          xyNoMatch;

/**t*eventuell Optimierung: Die Indizes der Variablen mit Mark<>0 ein
einziges mal heraussuchen und dann die zweite while-Schleife nur ueber
diese Variablen laufen lassen. Ausserdem: Falls die Variablen in der
dispdes noch nach x,y sortiert sind, dann braucht die Schleife auch nur
solange zu laufen, bis x,y aus dispdes > als x,y aus iVipDispAck ist*/

V.AckToev.Entries = 0;
V.Flag = 0;
receive.rawInMe.nRawInMe = 0;
bLiUAcknow.n = 0;
V.saveDId = dId;
bLiUAcknow.OpId = saveVipDispAck.OpId;

while (iVipDispAck<=saveVipDispAck.dataL &&
       dId==saveVipDispAck.Data[_AC(iVipDispAck-1,vipShL-1)].dId) {
  V.iR = 1;
  xyNoMatch = true;
  while (V.iR<=pMeOut->nMeOut && xyNoMatch) {
    pmeVar = &pMeOut->varDes[_AC(V.iR-1,vipShL-1)];
    pttVipShort = &pVipShort->Data[V.iR-1];
    if (pttVipShort->Mark!=0 &&
        pttVipShort->gid==saveVipDispAck.Data[_AC(iVipDispAck-1,vipShL-1)].gid &&
        InSet(pmeVar->proMode,(int)ackPerm,sProMode)) {
      xyNoMatch = false;
      ackVar(&V);                 //Acknowledge variable
      }
    ++V.iR;
    }
  ++iVipDispAck;
  }
if (receive.rawInMe.nRawInMe>0)
  pRawInMe();
if (bLiUAcknow.n>0)               //Order to LIDI
  pTransmLidi();
if (V.AckToev.Entries>0) {
  TopIfMedi(&V.Flag,&V.AckToev);  //Order to TOEV
  V.AckToev.Entries = 0;
  }
}  //pFulSegAck


static
void          pFulDispAck()
{
//Display acknowledgement
// Description see main program

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

dId = 0;
iVipDispAck = 1;
SvcA.sSAk0.Ok = true;
SvcA.sSAk0.textid = sTxNoText;
saveVipDispAck = receive.vDispAck;
saveAckpara = receive.U1.AckPara;

receive.U1.AckPara.AckCont = AckNo;
if (!McPTran(&sbFRec,saMedi,obMEDI,SosyData.sdSBRsiz,&receive.U1.Header1,
             PCOnly))
  err(0,3232,"DispAck ",rs("MCPT"),sbFRec,diAddr,"SIA");
sbFRec = 0;
receive.U1.AckPara.AckCont = saveAckpara.AckCont;

while (iVipDispAck<=saveVipDispAck.dataL) {
  dId = saveVipDispAck.Data[_AC(iVipDispAck-1,vipShL-1)].dId;
  if (pRDispDes()) {
    if (InSet(fDispDes.RpP->sDispVar,(int)meVar,fDispDes.RpP->sDispVar))
      pAfUpdate();
    pFulSegAck();
  } else
    ++iVipDispAck;
  pFRel();                        //Release dispDes
                                  //*t* pFDispDes?
  }
if (saveVipDispAck.LastAck)
  pSvcAck();
}  //pFulDispAck


static
void          pMeCoAck()
{
//Common acknowledgement

int           iR;
bool          ackOk;
tInfo         varInfo;            //Info number out of variable description
tMediAck      AckToev;
int           Flag;
int           FORLIM;
tRawInMeData* pWTHnRawInMe;

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

saveMeCoAck = receive.meCoAck;
AckToev.Entries = 0;

if (!McPTran(&sbFRec,saMedi,obMEDI,SosyData.sdSBRsiz,&receive.U1.Header1,
             PCOnly))
  err(0,3232,"MeCoAck ",rs("MCPT"),sbFRec,diAddr,"SIA");
sbFRec = 0;

receive.rawInMe.nRawInMe = 0;
FORLIM = saveMeCoAck.n;
for (iR = 1; iR<=FORLIM; ++iR) {
  TA = saveMeCoAck.messages[_AC(iR-1,cMaxMess-1)].TA;
  varInfo = TA.Info;
  CleSet(NimVarTy.RelSet,tsNimRfix);
  AddSet(NimVarTy.RelSet,xInfoDef,tsNimRfix);
  AddSet(NimVarTy.RelSet,xInTyDe,tsNimRfix);
  if (!NIMRTY(&NimVarTy.FlagS,nTeAd+nIn,saveMeCoAck.RelCopKey,0)) {
    err(0,2333,"NIM     ",NimVarTy.FlagS,NimVarTy.pTyNu->InfoType,TA.Info,
        "III");
    endOfJob = true;
    longjmp(_JL99,1);
    }
  TA.Info = 0;
  if (!NIMRMP(&NimVarMp.FlagS,nTeAd+nLock,saveMeCoAck.RelCopKey,0)) {
    err(0,2331,"NIM     ",NimVarMp.FlagS,TA.B3,TA.Elem,"III");
    endOfJob = true;
    longjmp(_JL99,1);
    }

  //Delete acknowledge bit
  InfVal = 0;
  if (SETINFO(varInfo,SubAckn,NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],
              NimVarMp.pInfStr,&InfVal,&Leng))
    ackOk = true;
  else
    ackOk = false;

  if (!NIMWMP(&NimVarMp.FlagS,0,saveMeCoAck.RelCopKey,0)) {
    err(0,2332,"NIM     ",NimVarMp.FlagS,TA.B3,TA.Elem,"III");
    endOfJob = true;
    longjmp(_JL99,1);
    }

  //Restore Info number of acknowledge info
  TA.Info = varInfo;

  if (ackOk)
    acknTA(saveMeCoAck.RelCopKey,&TA);
  else
    ackOk = acknTA(saveMeCoAck.RelCopKey,&TA);

  if (ackOk) {
    //Create raw information
    ++receive.rawInMe.nRawInMe;
    pWTHnRawInMe = &receive.rawInMe.Data[receive.rawInMe.nRawInMe-1];

    pWTHnRawInMe->TA = *NimVarMp.pTa;
    pWTHnRawInMe->statinfo = AckChg;
    pWTHnRawInMe->meClass = NimVarTy.pInTyDe->MeldeKlasse;
    pWTHnRawInMe->InfoType = 0;
    pWTHnRawInMe->timeSec = gActSec;
    pWTHnRawInMe->timemSec = gActMSec;
    pInfoStr = (ByteInt*)NimVarMp.pInfStr;
    memcpy(pWTHnRawInMe->InfStr,pInfoStr,NimVarMp.StrLeng[NimVarMp.NoElTy]);
    pWTHnRawInMe->NoElTy = NimVarMp.NoElTy;
    pWTHnRawInMe->delSiFlash = false;
    if (NimVarMp.NoElTy==neSwitch) {
      if (AckToev.Entries==maxTA5) {
        TopIfMedi(&Flag,&AckToev);  //Order to TOEV
        AckToev.Entries = 0;
        }
      ++AckToev.Entries;
      AckToev.messages[_AC(AckToev.Entries-1,maxTA5-1)].B1 = NimVarMp.pTa->B1;
      AckToev.messages[AckToev.Entries-1].B2 = NimVarMp.pTa->B2;
      AckToev.messages[AckToev.Entries-1].B3 = NimVarMp.pTa->B3;
      AckToev.messages[AckToev.Entries-1].Elem = NimVarMp.pTa->Elem;
      AckToev.messages[AckToev.Entries-1].Info = varInfo;
      }
    if (receive.rawInMe.nRawInMe==cMaxMess) {
      pRawInMe();
      receive.rawInMe.nRawInMe = 0;
      }
    }  //ackOk
  }

if (AckToev.Entries>0) {
  TopIfMedi(&Flag,&AckToev);      //Order to TOEV
  AckToev.Entries = 0;
  }
if (receive.rawInMe.nRawInMe>0)
  pRawInMe();
}  //pMeCoAck


//UIPAPI acknowledgement
static
void          pApiAck(
  bool          iOrder)           //I  true/false = order/message
{
bool          ackOk;
int           Ix;                 //index in received order
int           f;                  //flags
int           fAck;               //flag for response (SbSendAck)
static tCTII  saveApiAck;         //saved softbus order
int           Flag;
int           FORLIM;
tTechInf*     pWTHIx;
tRawInMeData* pWTHnRawInMe;
tMessLiAck*   pWTHn;
tMediAck      AckToev;

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

saveApiAck.SboHead.AckClass = receive.ApiAck.SboHead.AckClass;
receive.ApiAck.SboHead.AckClass = AckNo;
if (!McPTran(&sbFRec,saMedi,obMEDI,SosyData.sdSBRsiz,&receive.SboH.Service,
             PCOnly))
  err(0,3232,"ApiAck  ",rs("MCPT"),sbFRec,diAddr,"SIA");
sbFRec = 0;
receive.ApiAck.SboHead.AckClass = saveApiAck.SboHead.AckClass;

ByIdBy(&saveApiAck,&receive.ApiAck,CTII_Size(&receive.ApiAck));

fAck = 0;
receive.rawInMe.RelCopKey = saveApiAck.SboHead.RelCopKey;
receive.rawInMe.nRawInMe = 0;
bLiUAcknow.OpId = saveApiAck.OpId;
bLiUAcknow.n = 0;

FORLIM = saveApiAck.BlockCount*saveApiAck.InfCount;
for (Ix = 1; Ix<=FORLIM; ++Ix) {
  pWTHIx = &saveApiAck.uCtii.TechInfs[Ix-1];

  //Get info string.
  NimVarMp.NoElTy = pWTHIx->TechId.NoEl;
  NimVarMp.NimSatz = pWTHIx->TechId.SetNr;
  TA.Info = 0;
  if (!NIMRMP(&NimVarMp.FlagS,nLock,saveApiAck.SboHead.RelCopKey,0)) {
    err(0,2331,"NIM     ",NimVarMp.FlagS,TA.B3,TA.Elem,"III");
    fAck = -1;
    goto _L77;
    }

  //Delete acknowledge bit.
  InfVal = 0;
  if (SETINFO(pWTHIx->TechId.Info,SubAckn,
              NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],NimVarMp.pInfStr,
              &InfVal,&Leng))
    ackOk = true;
  else
    ackOk = false;

  //Store info string.
  if (!NIMWMP(&NimVarMp.FlagS,0,saveApiAck.SboHead.RelCopKey,0)) {
    err(0,2332,"NIM     ",NimVarMp.FlagS,TA.B3,TA.Elem,"III");
    fAck = -1;
    goto _L77;
    }

  //Determine technological address.
  NimVar.NoElType = pWTHIx->TechId.NoEl;
  NimVar.NimSatz = pWTHIx->TechId.SetNr;
  if (!NIMGETTA(&NimVar.FlagS,0)) {
    err(0,2330,"NIM     ",NimVar.FlagS,NimVar.NoElType,NimVar.NimSatz,"III");
    fAck = -1;
    goto _L77;
    }
  TA.Info = pWTHIx->TechId.Info;

  if (ackOk)
    acknTA(saveApiAck.SboHead.RelCopKey,&TA);
  else
    ackOk = acknTA(saveApiAck.SboHead.RelCopKey,&TA);

  if (DEBUGlevel(200))
    errfmt(0,200,"pApiAck","TA: %d/%d/%d/%d/%d",TA.B1,TA.B2,TA.B3,TA.Elem,TA.Info);

  if (ackOk) {
    //Get information type description.
    NimVarTy.ElNimSatz = pWTHIx->TechId.SetNr;
    NimVarTy.InfoRef = NimVar.pfVari.pfAdNim->Rp->uAdni.sNeEl.InfoRef;
    NimVarTy.NoElTy = pWTHIx->TechId.NoEl;
    CleSet(NimVarTy.RelSet,tsNimRfix);
    AddSet(NimVarTy.RelSet,xInfoDef,tsNimRfix);
    AddSet(NimVarTy.RelSet,xInTyDe,tsNimRfix);
    if (!NIMRTY(&NimVarTy.FlagS,nIn,saveApiAck.SboHead.RelCopKey,0)) {
      err(0,2333,"NIM     ",NimVarTy.FlagS,NimVarTy.InfoRef,
          NimVarTy.pTa->Info,"III");
      fAck = -1;
      goto _L77;
      }

    //Create raw information.
    if (receive.rawInMe.nRawInMe==cMaxMess)
      pRawInMe();
    ++receive.rawInMe.nRawInMe;
    pWTHnRawInMe = &receive.rawInMe.Data[receive.rawInMe.nRawInMe-1];

    pWTHnRawInMe->TA = *NimVar.pTa;
    pWTHnRawInMe->statinfo = AckChg;
    pWTHnRawInMe->meClass = NimVarTy.pInTyDe->MeldeKlasse;
    pWTHnRawInMe->InfoType = NimVarTy.pTyNu->InfoType;
    pWTHnRawInMe->timeSec = gActSec;
    pWTHnRawInMe->timemSec = gActMSec;
    pInfoStr = (ByteInt*)NimVarMp.pInfStr;
    memcpy(pWTHnRawInMe->InfStr,pInfoStr,NimVarMp.StrLeng[pWTHIx->TechId.NoEl]);
    pWTHnRawInMe->NoElTy = pWTHIx->TechId.NoEl;
    //Common acknowledgement to list.
    if (NimVarTy.pInTyDe->AckGrafLi) {
      if (bLiUAcknow.n==cLiQSL)
        pTransmLidi();
      ++bLiUAcknow.n;
      pWTHn = &bLiUAcknow.messages[bLiUAcknow.n-1];
      pWTHn->TA = *NimVar.pTa;
      pWTHn->infoTyp = NimVarTy.pTyNu->InfoType;
      }

    //Send acknowledgement to the other control centers.
    if (iOrder)
      CCAcknowledge(saveApiAck.SboHead.RelCopKey,saveApiAck.OpId,
                    pWTHIx->TechId.NoEl,pWTHIx->TechId.Info,
                    pWTHIx->TechId.SetNr);

    if (NimVarMp.NoElTy==neSwitch) {
      ById00(&AckToev,sizeof(AckToev));
      AckToev.Entries = 1;
      AckToev.messages[0].B1 = pWTHnRawInMe->TA.B1;
      AckToev.messages[0].B2 = pWTHnRawInMe->TA.B2;
      AckToev.messages[0].B3 = pWTHnRawInMe->TA.B3;
      AckToev.messages[0].Elem = pWTHnRawInMe->TA.Elem;
      AckToev.messages[0].Info = pWTHIx->TechId.Info;
      TopIfMedi(&Flag,&AckToev);    //Order to TOEV
      AckToev.Entries = 0;
      }

    }  //ackOk
_L77: ;
  }

//Send acknowledgement.
_L88:
SbSendAck(&f,&saveApiAck.SboHead,fAck,0,0,CTII_Size(&saveApiAck));
if (receive.rawInMe.nRawInMe>0)
  pRawInMe();
if (bLiUAcknow.n>0)
  pTransmLidi();
}  //pApiAck


static
void          totalAckTA()
{
//Update acknowledged TA's
int           i;
bool          found;
tInfo         varInfo;            //Info number out of variable description
tRawInMeData* pWTHnRawInMe;


found = false;
i = 1;
while (i<=receive.rawInMe.nRawInMe && !found) {
  if (receive.rawInMe.Data[_AC(i-1,cMaxMess-1)].TA.B1==TA.B1 &&
      receive.rawInMe.Data[i-1].TA.B2==TA.B2 &&
      receive.rawInMe.Data[i-1].TA.B3==TA.B3 &&
      receive.rawInMe.Data[i-1].TA.Elem==TA.Elem &&
      receive.rawInMe.Data[i-1].TA.Info==TA.Info)
    found = true;
  else
    ++i;
  }
if (found)
  return;

pInfoType();                      //Determine info type, read Intyde

TA = *NimVarMp.pTa;
varInfo = TA.Info;
TA.Info = 0;
if (!NIMRMP(&NimVarMp.FlagS,nTeAd+nLock,current_RelCopKey,0)) {
  err(0,2331,"NIM     ",NimVarMp.FlagS,TA.B3,TA.Elem,"III");
  endOfJob = true;
  longjmp(_JL99,1);
  }

//Delete acknowledge bit
InfVal = 0;
SETINFO(varInfo,SubAckn,NimVarMp.NedePoi[_AC(NimVarMp.NoElTy,neDim)],
        NimVarMp.pInfStr,&InfVal,&Leng);

if (!NIMWMP(&NimVarMp.FlagS,0,current_RelCopKey,0)) {
  err(0,2332,"NIM     ",NimVarMp.FlagS,TA.B3,TA.Elem,"III");
  endOfJob = true;
  longjmp(_JL99,1);
  }

//Restore Info number of acknowledge info
TA.Info = varInfo;

//Create raw information
++receive.rawInMe.nRawInMe;
pWTHnRawInMe = &receive.rawInMe.Data[receive.rawInMe.nRawInMe-1];
pWTHnRawInMe->TA = *NimVarMp.pTa;
pWTHnRawInMe->statinfo = AckChg;
pWTHnRawInMe->meClass = NimVarTy.pInTyDe->MeldeKlasse;
pWTHnRawInMe->InfoType = 0;
pWTHnRawInMe->timeSec = gActSec;
pWTHnRawInMe->timemSec = gActMSec;
pInfoStr = (ByteInt*)NimVarMp.pInfStr;
memcpy(pWTHnRawInMe->InfStr,pInfoStr,NimVarMp.StrLeng[NimVarMp.NoElTy]);
pWTHnRawInMe->NoElTy = NimVarMp.NoElTy;
pWTHnRawInMe->delSiFlash = false;
if (receive.rawInMe.nRawInMe==cMaxMess) {
  pRawInMe();
  receive.rawInMe.nRawInMe = 0;
  }
}  //totalAckTA


static
void          ptotalAck()
{
//total acknowledgement of message variables

bool          delTA;
tPtB          PtB;                //buffer pointers

if (!McPTran(&sbFRec,saMedi,obMEDI,SosyData.sdSBRsiz,&receive.U1.Header1,
             PCOnly))
  err(0,3232,"SingAck ",rs("MCPT"),sbFRec,diAddr,"SIA");
sbFRec = 0;

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

receive.rawInMe.nRawInMe = 0;

PtB.bSelect = bFlash;
//bFlash loop: 1
PtB.nextBDId = PtB.bSelect;
PtB.precBDId = 0;

while (PtB.nextBDId!=0) {
  PtB.nextBDispI = PtB.nextBDId->nextBDispI;
  PtB.precBDispI = 0;
  while (PtB.nextBDispI!=0) {
    PtB.nextBTA = PtB.nextBDispI->nextBTA;
    PtB.precBTA = 0;
    while (PtB.nextBTA!=0) {
      delTA = false;
      //bFlash loop: 2

      if (PtB.nextBTA->acknFlash==true &&
          PtB.nextBDId->RelCopKey==current_RelCopKey) {
        PtB.nextBTA->acknFlash = false;
        TA.B1 = PtB.nextBTA->ta.B1;
        TA.B2 = PtB.nextBTA->ta.B2;
        TA.B3 = PtB.nextBTA->ta.B3;
        TA.Elem = PtB.nextBTA->ta.Elem;
        TA.Info = PtB.nextBTA->ta.Info;
        bFlash = PtB.bSelect;
        totalAckTA();             //Update acknowledged TA's
        }

      if ((PtB.nextBTA->desFlash==false && PtB.nextBTA->posFlash==false &&
           PtB.nextBTA->acknFlash==false) || PtB.nextBDId->RelCopKey!=0) {
        delTA = true;
        disposeTA(&PtB);
        }

      //bFlash loop: 3
      if (!delTA) {
        PtB.precBTA = PtB.nextBTA;
        PtB.nextBTA = PtB.nextBTA->nextBTA;
        }
      }
    if (PtB.nextBDispI->nextBTA==0)
      disposeDispI(&PtB);
    else {
      PtB.precBDispI = PtB.nextBDispI;
      PtB.nextBDispI = PtB.nextBDispI->nextBDispI;
      }
    }
  if (PtB.nextBDId->nextBDispI==0)
    disposeDId(&PtB);
  else {
    PtB.precBDId = PtB.nextBDId;
    PtB.nextBDId = PtB.nextBDId->nextBDId;
    }
  }
//bFlash loop: 4

bFlash = PtB.bSelect;
if (receive.rawInMe.nRawInMe>0)
  pRawInMe();
}  //ptotalAck


static
bool          pRDispDesC()
{
bool          pRDispDesC_RTN;
dispDes*      pWTHpGDispDesRpP;
//Read dispDes

pRDispDesC_RTN = true;
vdispDes.dId = dId;
// pRDispDesC only run with RelCopKey 0
if (!DbRead(&fDispDes,RpOnPage)) {
  pGDispDesRpP = fDispDes.RpP;
  if (fDispDes.Flag==NoK || fDispDes.Flag==EoK) {
    SubSet(ActFileSet,vdispDes.dId,tActFileSet);
    SubSet(okActFileSet,vdispDes.dId,tActFileSet);
    SubSet(HISActFileSet,vdispDes.dId,tActFileSet);
    meDispGen(false);
        //Delete dId in all concerned medisp records
    if (findBDId(dId,0,bCAF))     //delete bCAF entries
      delCAF(nextBDId);
  } else {
    err(0,2320,"DBREAD C",rs("DISP"),vdispDes.dId,fDispDes.Flag,"SII");

    if (fDispDes.Flag<0)
      err(0,3336,"pRDspDsC",fDispDes.Flag,fDispDes.Fv.FlagS,vdispDes.dId,
          "III");
    }
  pRDispDesC_RTN = false;
  goto _L0;
  }
pGDispDesRpP = fDispDes.RpP;

pWTHpGDispDesRpP = pGDispDesRpP;
if (InSet(pWTHpGDispDesRpP->sDispVar,(int)meVar,pWTHpGDispDesRpP->sDispVar)) {
  pVipShort = (tVipShortP*)(&pWTHpGDispDesRpP->information[_AC(
                    pWTHpGDispDesRpP->Directory.iVipShort-1,cMaxInf-1)]);
  pMeOut = (tMeOut*)(&pWTHpGDispDesRpP->information[_AC(
                         pWTHpGDispDesRpP->Directory.iMeOut-1,cMaxInf-1)]);
} else {
  SubSet(ActFileSet,vdispDes.dId,tActFileSet);
  SubSet(okActFileSet,vdispDes.dId,tActFileSet);
  SubSet(HISActFileSet,vdispDes.dId,tActFileSet);
  meDispGen(false);
      //Delete dId in all concerned medisp records
  if (findBDId(pWTHpGDispDesRpP->dId,0,bCAF))  //delete bCAF entries
    delCAF(nextBDId);
  pRDispDesC_RTN = false;
  }
_L0:
return pRDispDesC_RTN;
}  //pRDispDesC
// ---------------------------------------------------------------------
// send to UI server (pCheckAf)
//
static
void          pMmiUpdt()
{
  short    aUpdComp[cMaxComput];
  short    lUpdCnt = 0;

  for (int i=0; i < cMaxComput; i++) aUpdComp[i] = 0;

  // get computer need to be updated
  trMmiDir*   pMmiDir;
  for (int j = 1; j <= cMaxComput; ++j)
    if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
      pMmiDir = mmiDir[j-1];
      if (pMmiDir->mmiState==doupdate &&
          InSet(pMmiDir->mmiUpdtSet,pVipShort->dId,tActFileSet) &&
          !InSet(pMmiDir->mmiActFileSet,pVipShort->dId,tActFileSet)) {
        aUpdComp[lUpdCnt] = j;
        lUpdCnt++;
      }
    }

  if (pVipShort->dataL > maxDataL) {
    bVipShort.RelCopKey = pVipShort->RelCopKey;
    bVipShort.AckPara = pVipShort->AckPara;
    bVipShort.AckPara.AckCont = AckNo;
    bVipShort.area = pVipShort->area;
    bVipShort.chan = 0;
    bVipShort.dId = pVipShort->dId;
    bVipShort.dataL = 0;
  }
  pVipShort->chan = 0;

  bool doUpdHIS = (!InSet(HISActFileSet,pVipShort->dId,tActFileSet));

  ComputId lComp;
  int DL = 0;
  tVipShortP* pVS;
  do {
    pVS = getUpdateVipShort(DL);
#if SUBS_SHHIS
    if (doUpdHIS) pVipHIS(pVS,true,true);
#endif
    // update ui server
    for (int i=0; i < lUpdCnt; i++) {
      lComp = mmiDir[(aUpdComp[i]-1)]->Comput;
      pSndVipShort(pVS,saOdvGat,obODVGAT,lComp,0,false);
    }
  } while (DL >= 0);

  // update mmiDir
  for (int i=0; i < lUpdCnt; i++) {
    pMmiDir = mmiDir[(aUpdComp[i]-1)];
    AddSet(pMmiDir->mmiActFileSet,pVipShort->dId,tActFileSet);
    if (IntDifNulSet(ActFileSet,pMmiDir->mmiUpdtSet,pMmiDir->mmiActFileSet))
      pMmiDir->mmiState = updateOk;
  }
#if SUBS_SHHIS
  if (doUpdHIS) AddSet(HISActFileSet,pVipShort->dId,tActFileSet);
#endif

}  //pMmiUpdt
// ---------------------------------------------------------------------
static
void          pCheckAf()
{
//Check updating file
// Description see main program

int           i;
int           j;
int           iop;
bool          oK;
dispDes*      pWTHpGDispDesRpP;
meDisp*       pWTHRp;
tB123*        pWTHB123;
trMmiDir*     pWTHj;

oK = true;

if (corrSoftInit) {
  //*t*spr-corr
  err(0,2605,"checkAf1",0.0,0.0,0.0,"III");

  vdispDes.dId = 1;
  DbPos(&fDispDes,NxKey);
  while (DbRead(&fDispDes,NextRc+RpOnPage)) {
    pGDispDesRpP = fDispDes.RpP;
    pWTHpGDispDesRpP = pGDispDesRpP;
    vdispDes.dId = pWTHpGDispDesRpP->dId;
    if (InSet(pWTHpGDispDesRpP->sDispVar,(int)meVar,
              pWTHpGDispDesRpP->sDispVar)) {
      if (vdispDes.dId>maxDId)
        err(0,2603,"maxDId ",vdispDes.dId,maxDId,0.0,"III");
      else
        AddSet(ActFileSet,vdispDes.dId,tActFileSet);
      }
    }

  if (fDispDes.Flag!=0 && fDispDes.Flag!=NoK)
    err(0,3336,"checkAf2",fDispDes.Flag,vdispDes.dId,0.0,"III");
  corrSoftInit = false;
  }

if (!NulSet(ReSelDidSet,tActFileSet)) {
  SetSet(AllDidSet,ReSelDidSet,tActFileSet);
  pReSelect();                    //reselect changed dIds
  CleSet(ReSelDidSet,tActFileSet);
  }

sendBVipSH(0,0);

if (DifNulSet(ActFileSet,okActFileSet,tActFileSet))
  {  //All meVar displays are updated
  createNew = false;
  if (initialUpdate)
    errfmt(0,10,"pCheckAf"
              ,"Initial display generation finished - internal MeDisp entries: %d"
              ,NB123Medisp);
  initialUpdate = false;
  goto _L97;                      //All meVar displays are updated
  }

//Create updating file for one dId
dId = dIdCheck;
i = 1;
while (i<=maxDId && oK) {
  ++dId;
  if (dId>maxDId)
    dId = 1;
  if (InSet(ActFileSet,dId,tActFileSet) &&
      !InSet(okActFileSet,dId,tActFileSet))
    {  //dId contains meVar but is not updated yet
    if (pRDispDesC()) {           //Read dispDes
      dIdCheck = dId;
      if (DEBUGlevel(200))
        errfmt(0,200,"pCheckAf","Start af: dId/nMeOut: %d/%d"
                    ,dId,pMeOut->nMeOut);
      if (findBDId(dId,0,bCAF))   //delete bCAF entries
        delCAF(nextBDId);
      pWrActFile();               //Generate (write) actfile
      pMmiUpdt();                 //Send Updating file to mmi`s
      if (DEBUGlevel(200))
        errfmt(0,200,"pCheckAf","End af: dId/nMeOut: %d/%d"
                    ,dId,pMeOut->nMeOut);
      }
    oK = false;
    }

  ++i;
  }
pFRel();                          //Release dispDes

_L97:

if (oK) {  // Update Relation MEDISP
  iop = 0;
  i = 0;
  cp(75,1,"UPD meDi",rs("sp  "),0.0,0.0,"SII");

  if (NB123Medisp>0) {
    fmeDisp.Rp = &vMeDispL;
    while (!B123MdspChgOK && i<cNB123MdspUpd && iop<NB123Medisp) {
      ++iop;

      if (B123Medisp[_AC(iop-1,cNB123Medisp)].B123.U1.MdspChg==true &&
          B123Medisp[iop-1].rp==0) {
        B123Medisp[_AC(iop-1,cNB123Medisp)].B123.U1.MdspChg = false;
        fmeDisp.Rp->B1 = B123Medisp[iop-1].B123.U1.B1;
        fmeDisp.Rp->B2 = B123Medisp[iop-1].B123.U1.B2;
        fmeDisp.Rp->B3 = B123Medisp[iop-1].B123.U1.B3;

        if (!DbDelete(&fmeDisp,0)) {
          pWTHRp = fmeDisp.Rp;
          err(0,3368,"MeDispUp",fmeDisp.Flag,pWTHRp->B1,pWTHRp->B2,"iii");
          goto _L96;
          }
        }

      if (B123Medisp[_AC(iop-1,cNB123Medisp)].B123.U1.MdspChg) {
        B123Medisp[_AC(iop-1,cNB123Medisp)].B123.U1.MdspChg = false;
        ++i;
        fmeDisp.Rp = B123Medisp[_AC(iop-1,cNB123Medisp)].rp;
        fmeDisp.RcS = getMeDispSize(fmeDisp.Rp->nVar);
        if (!DbWrite(&fmeDisp,ActPage)) {
          if (!DbInsert(&fmeDisp,ActPage)) {
            pWTHRp = fmeDisp.Rp;
            err(0,3360,"MeDispGe",fmeDisp.Flag,pWTHRp->B1,
                pWTHRp->B2*1000.0+pWTHRp->B3,"iii");
            goto _L96;
            }
          }
        }

_L96:
      fmeDisp.Rp = &vMeDispL;

      if (B123Medisp[_AC(iop-1,cNB123Medisp)].rp!=0 &&
          (B123Medisp[iop-1].B123.U1.B1!=B123Medisp[iop-1].rp->B1 ||
           B123Medisp[iop-1].B123.U1.B2!=B123Medisp[iop-1].rp->B2 ||
           B123Medisp[iop-1].B123.U1.B3!=B123Medisp[iop-1].rp->B3)) {
        pWTHB123 = &B123Medisp[_AC(iop-1,cNB123Medisp)].B123;
        err(0,3370,"pCh B123",pWTHB123->U1.B1,pWTHB123->U1.B2,pWTHB123->U1.B3,
            "iii");
        }
      }
    cp(50,1,"UpMeDisp",NB123Medisp,iop,i,"iii");
    if (iop==NB123Medisp)
      B123MdspChgOK = true;
    }

  //Send one updating file to mmi`s
  j = 1;
  while (j<=cMaxComput && oK) {
    if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
      pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];
      if (pWTHj->mmiState==doupdate) {
        dId = 1;
        while (dId<=maxDId && oK) {
          if (InSet(okActFileSet,dId,tActFileSet) &&
              InSet(pWTHj->mmiUpdtSet,dId,tActFileSet) &&
              !InSet(pWTHj->mmiActFileSet,dId,tActFileSet))
            {  //mmi has not been updated with this dId
            if (pRDispDesC())     //Read dispDes
              pMmiUpdt();         //Send Updating file to mmi`s
            oK = false;
            }
          ++dId;
          }
        }
      }
    ++j;
    }
  }

#if SUBS_SHHIS
IpcGetTime(&gActSec,&gActMSec);
#endif

//update bCAF entries
if (bCAF!=0) {
  if (InSet(okActFileSet,bCAF->AFDId,tActFileSet))  //first dId in bCAF
    pUpdateCAF(bCAF->AFDId,0);
  else
    delCAF(bCAF);
  //delete bCAF entries
  --nCAF;
} else
  nCAF = 0;

_L98:
if (oK) {
  Rastint.Sys.Min = minAlarm;
  Rastint.Sys.Sec = 0;
} else {
  Rastint.Sys.Min = 0;
  Rastint.Sys.Sec = secAlarm;
  }

if ((createNew) && (!isRTSBrunup)) {
  if (!SbTransm(&sbF,MyComputId,saMeactF,obMEDI,sizeof(tCheckAf),&rCheckAF))
    err(0,2217,"SBTRANS2",sbF,0.0,0.0,"III");
} else {
  if (!SbAlarm(&sbF,MyComputId,saMeactF,obMEDI,sizeof(tCheckAf),&rCheckAF,
               ClockId,&Rastint,SbAfter,SbIntern)) {
    if (sbF!=SBeNew)
      err(0,2217,"SBALARM2",sbF,0.0,0.0,"III");
    }
  }

pFRel();                          //Release dispDes

}  //pCheckAf


static
void          pFDispDes()
{
//Release dispDes and actfileC

if (!DbRcFree(&fDispDes,0))
  err(0,2313,"DBRCFREE",rs("DISP"),vdispDes.dId,fDispDes.Flag,"SII");

if (current_RelCopKey!=0) {
  if (!DbRcFree(&fActFileC,0))
    err(0,2313,"DBRCFREE",rs("AFC "),rActFileC.dId,fActFileC.Flag,"SII");
  }
}  //pFDispDes


static
void          pVipShortP()
{
//Send updating to VIP

if (iWVipShort>1) {  //transmit
  bVipShort.RelCopKey = mmiDir[_AC(receive.mePosit.AckPara.Comput-1,
                            cMaxComput-1)]->VChan[_AC(receive.mePosit.chan-1,
                                                      cMaxChan-1)]->RelCopKey;
  bVipShort.AckPara.AckCont = AckNo;
  bVipShort.chan = receive.mePosit.chan;
  bVipShort.area = arInd;
  bVipShort.dId = pVipShort->dId;
  bVipShort.dataL = iWVipShort-1;

  //find dID entry in bFlash
  if (findBDId(bVipShort.dId,bVipShort.RelCopKey,bFlash))
    corrSSStart = true;
  else {
    nextBDId = 0;

    }
  lVipS = sizeof(tVipShortP)-(vipShL-bVipShort.dataL)*sizeof(ttVipShortP);

  //correct special states in vipShort due to responsibility
  corrSpecialState(receive.mePosit.AckPara.Comput,&bVipShort,false);

  if (!SbTransm(&sbF,receive.mePosit.AckPara.Comput,saOdvGat,obODVGAT,
                lVipS,&bVipShort)) {
    err(0,3231,"VIPSHORT",rs("DITR"),sbF,receive.mePosit.chan,"SII");
    endOfJob = true;
    longjmp(_JL99,1);             //end of job
  } else {
    if (rTrace.On)
      wrVipShort("pVipShortP",&bVipShort,receive.mePosit.AckPara.Comput);
    if (DEBUGlevel(100))
      cp(0,100,"pVipShor",bVipShort.dId,receive.mePosit.AckPara.Comput,0.0,"icb");
    }
  }  //then
}  //pVipShortP


// Fill position update data into vipShort for one variable
void pVipPositionVar(ttVipShortP*  pWTHiWVipShort)
{
sProMode      TmpSet;

pWTHiWVipShort->vipAct = vipActCHan;
if (receive.mePosit.free_mePos) {  //-> positionable
  IntSet(TmpSet,GrMode,receive.mePosit.posIdSet,sProMode);
  if (!NulSet(TmpSet,sProMode) && !EquSet(TmpSet,GrMode,sProMode))
    pWTHiWVipShort->uvipA.svACh.COption = vipOPosMulSel+vipmedi;
  else
    pWTHiWVipShort->uvipA.svACh.COption = vipOPos+vipmedi;

  // Add deselect flag for positioning of select types
  IntSet(TmpSet,SelMode,receive.mePosit.posIdSet,sProMode);
  if (!NulSet(TmpSet,sProMode))
    pWTHiWVipShort->uvipA.svACh.COption+=vipODeselect;
  }
else {
  //-> not positionable
  pWTHiWVipShort->uvipA.svACh.COption = vipmedi;
  }
pWTHiWVipShort->uvipA.svACh.cLen = 1;
}


static
void          pVipPosition()
{

//Generate positioning record for variable
bVipShort.Data[_AC(iWVipShort-1,vipShL-1)] = pVipShort->Data[_AC(iRVarDes-1,
                                                                 vipShL-1)];
//fill in data for one variable
pVipPositionVar(&bVipShort.Data[iWVipShort-1]);

++iWVipShort;
}  //pVipPosition


static
void          pActPos(sProMode proModeHelp)
{
//Update positionable variables

int           iR;
int           FORLIM;

iWVipShort = 1;
FORLIM = pMeOut->nMeOut;
for (iR = 1; iR<=FORLIM; ++iR) {
  iRVarDes = iR;
  pmeVar = &pMeOut->varDes[_AC(iRVarDes-1,vipShL-1)];
  if (!IntNulSet(pmeVar->proMode,proModeHelp,sProMode))
    {                             //Generate positioning record for variable
    if (InSet(proModeHelp,(int)ackPerm,sProMode)) {
      if (receive.mePosit.free_mePos) {
        if (pVipShort->Data[_AC(iRVarDes-1,vipShL-1)].Mark!=0)
              //Generate positioning record for variable
                pVipPosition();
      } else
        pVipPosition();
    } else {
      //Generate positioning record for variable
      pVipPosition();
      }
    }

  // check data size
  if (iWVipShort>maxDataL) {
    pVipShortP();                 // Send updating to VIP
    iWVipShort = 1;
    }

  }
}  //pActPos


static
void          pMePosit()
{
//Let variables be positionable
// Description see main program

int           arI;
int           k;
trChanDir*    pWTHchan;
int           FORLIM1;

sProMode      proModeHelp;
tPosidAck     PosidHelp;

CleSet(proModeHelp,sProMode);
for (PosidHelp = pSingleStep;
     PosidHelp<=ackPerm;
     PosidHelp = (tPosidAck)(PosidHelp+1)) {
  if (InSet(receive.mePosit.posIdSet,(int)PosidHelp,tPosidSetAck))
    AddSet(proModeHelp,(int)PosidHelp,sProMode);
  }

if (receive.mePosit.free_mePos &&
    InSet(receive.mePosit.posIdSet,(int)ackPerm,tPosidSetAck))
  mmiDir[_AC(receive.mePosit.AckPara.Comput-1,cMaxComput-1)]->VChan[_AC(
          receive.mePosit.chan-1,cMaxChan-1)]->ackPosit = true;
else
  mmiDir[_AC(receive.mePosit.AckPara.Comput-1,cMaxComput-1)]->VChan[_AC(
          receive.mePosit.chan-1,cMaxChan-1)]->ackPosit = false;

for (arI = 0; arI<=3; ++arI) {
  arInd = arI;
  dId = mmiDir[_AC(receive.mePosit.AckPara.Comput-1,cMaxComput-1)]->VChan[_AC(
            receive.mePosit.chan-1,cMaxChan-1)]->area[_AC(arInd,3)].dId;
  if (dId!=0) {
    if (!pRDispDes()) {           //Read variable description
      endOfJob = true;
      longjmp(_JL99,1);           //end of job
      }
    pActPos(proModeHelp);         //Update positionable variables
    pVipShortP();                 //Send updating to VIP
    pFDispDes();                  //Release dispDes and actfilec
    if (mmiDir[_AC(receive.mePosit.AckPara.Comput-1,cMaxComput-1)]->VChan[_AC(
            receive.mePosit.chan-1,
            cMaxChan-1)]->area[_AC(arInd,3)].segmented) {
      pWTHchan = mmiDir[_AC(receive.mePosit.AckPara.Comput-1,cMaxComput-1)]->
                 VChan[_AC(receive.mePosit.chan-1,cMaxChan-1)];
      FORLIM1 = pWTHchan->segDisp.nSeg;
      for (k = 2; k<=FORLIM1; ++k) {
        dId = pWTHchan->segDisp.segments[_AC(k-2,cMaxSeg-2)].dId;
        if (!pRDispDes()) {       //Read variable description
          endOfJob = true;
          longjmp(_JL99,1);       //end of job
          }
        pActPos(proModeHelp);     //Update positionable variables
        pVipShortP();             //Send updating to VIP
        pFDispDes();              //Release dispDes and actfileC
        }  //for k
      }
    }
  }

// process dynamic registered variables
DynVarProcVipPosition(receive.mePosit.AckPara.Comput,
                      receive.mePosit.RelCopKey,
                      receive.mePosit.chan,
                      proModeHelp);

}  //pMePosit


static
void          pDbClose()
{
//Close all relations
/*
pDbClose releases and closes the database files of all database
relations of MEDI.
*/
//Release possibly locked relations
pFRel();                          //Release dispDes
                                  //*t* pFDispDes?

//close relations
DbfClose(&fDispDes,0);
DbfClose(&fActFileC,0);
DbfClose(&fInputVar,0);
DbfClose(&fmeDisp,0);
DbfClose(&fCoDeTaEl,0);
DbfClose(&fDeTa,0);
DbfClose(&fNimad,0);

UIcClose(&UIcIntVar,0);

CleSet(NimVar.RelSet,tsNimRfix);
NIMCLOSE(&NimVar.FlagS,0);
CleSet(NimVarMp.RelSet,tsNormElem);
NIMCLMP(&NimVarMp.FlagS,0);

// cancel for console access update
(void)CSIxUD::RequestAuthorizationXid(MyComputId,saMedi,obMEDI
                                     ,sr_CancelCoAcData,0,0);

}  //pDbClose

// ---------------------------------------------------------------------
// dump channel directory,
//
static
void          dumpChanDir
(  ostream &   i_s
)
{
int           j;
int           kSeg;
int           iActivChan;
int           iChan;
int           iArInd;
trMmiDir*     pWTHj;
trChanDir*    pWTHiChan;
int           FORLIM2;
char          sBuffer[150];

i_s << "******* ChanDir entries **********" << endl;
for (j = 1; j<=cMaxComput; ++j) {
  if (mmiDir[_AC(j-1,cMaxComput-1)]!=0) {
    pWTHj = mmiDir[_AC(j-1,cMaxComput-1)];

    //mmiDir[j]
    sprintf(sBuffer,"  computer = %-1d",j);
    i_s << sBuffer;
    i_s << " mmiState:";
    if (pWTHj->mmiState==doupdate) i_s << " doupdate";
    if (pWTHj->mmiState==noUpdate) i_s << " noUpdate";
    if (pWTHj->mmiState==updateOk) i_s << " updateOk";
    sprintf(sBuffer," nActivChannels = %-1d",pWTHj->nActivChan);
    i_s << sBuffer << endl;

    if (pWTHj->nActivChan>0) {
      iChan = 1;
      iActivChan = 0;
      while (iChan<=cMaxChan && iActivChan<pWTHj->nActivChan) {
        if (pWTHj->VChan[_AC(iChan-1,cMaxChan-1)]!=0) {
          pWTHiChan = pWTHj->VChan[_AC(iChan-1,cMaxChan-1)];
          //if VChan[iChan]
          sprintf(sBuffer,"    Channel = %-1d digr = %-1d RelCopKey = %-1d\n"
                         ,iChan,pWTHiChan->digr,pWTHiChan->RelCopKey);
          i_s << sBuffer;
          for (iArInd = 0; iArInd<=3; ++iArInd) {
            sprintf(sBuffer,"      Area = %-1d SegmentNo. = %-1d "
                           ,iArInd,pWTHiChan->area[_AC(iArInd,3)].dId);
            i_s << sBuffer;
            if (pWTHiChan->area[iArInd].dId!=0 &&
                pWTHiChan->area[iArInd].segmented) {
              FORLIM2 = pWTHiChan->segDisp.nSeg;
              for (kSeg = 2; kSeg<=FORLIM2; ++kSeg) {
                sprintf(sBuffer,"%-1d "
                       ,pWTHiChan->segDisp.segments[_AC(kSeg-2,cMaxSeg-2)].dId);
                i_s << sBuffer;
                }
              }
            i_s << endl;
            }  //for iArind
          ++iActivChan;
          }
        ++iChan;
        }
      }  //if nActivChan
    }
  }  //for j
}  //dumpChanDir
// ---------------------------------------------------------------------
// write internal buffers into dump file
//
static
void          dumpbFlashbStay
(  ostream &   i_s
)
{
tbVipSH*      bVipSHLoc;
tmmi          iLoc;
int           i;
char          sBuffer[200];


//test output of bFlash entries
i_s << endl << "******* bFlash entries **********" << endl;
nextBDId = bFlash;
while (nextBDId!=0) {
  sprintf(sBuffer,"dId = %-1d RelcopKey = %-1d: \n"
         ,nextBDId->AFDId,nextBDId->RelCopKey);
  i_s << sBuffer;
  nextBDispI = nextBDId->nextBDispI;
  while (nextBDispI!=0) {
    sprintf(sBuffer,
      " dispI = %3d|  gid = %-1d fN =%-1d aN =%-1d fF =%-1d aF =%-1d aP =%-1d\n",
      nextBDispI->AFDispI,nextBDispI->gid,nextBDispI->fAltNorm,
      nextBDispI->aAltNorm,nextBDispI->fAltFl,nextBDispI->aAltFl,
      nextBDispI->aAltPosFl);
    i_s << sBuffer;
    nextBTA = nextBDispI->nextBTA;
    while (nextBTA!=0) {
      sprintf(sBuffer,"   ta/%-1d/%-1d/%-1d/%-1d/%-1d//d%d/p%d/a%d/\n"
             ,nextBTA->ta.B1,nextBTA->ta.B2,nextBTA->ta.B3,nextBTA->ta.Elem
             ,nextBTA->ta.Info,nextBTA->desFlash,nextBTA->posFlash
             ,nextBTA->acknFlash);
      i_s << sBuffer;
      nextBTA = nextBTA->nextBTA;
      }
    nextBDispI = nextBDispI->nextBDispI;
    }
  nextBDId = nextBDId->nextBDId;
  i_s << endl;
  }

//test output of bCAF entries
i_s << endl << "******* bCAF entries ************" << endl;
nextBDId = bCAF;
while (nextBDId!=0) {
  sprintf(sBuffer,"dId = %-1d RelcopKey = %-1d: \n"
         ,nextBDId->AFDId,nextBDId->RelCopKey);
  i_s << sBuffer;
  i_s << " dispI =";
  for (i = 1; i<=vipShL; ++i) {
    if (InSet(nextBDId->VarSet,i,tVarSet)) {
      sprintf(sBuffer," %3d",i);
      i_s << sBuffer;
      }
    }
  nextBDId = nextBDId->nextBDId;
  i_s << endl;
  }

//test output of bVipSH entries
i_s << endl << "******* bVipSH entries **********" << endl;
bVipSHLoc = bVipSH;
while (bVipSHLoc!=0) {
  sprintf(sBuffer
    ,"dId = %-1d RelcopKey = %-1d chan = %-1d area = %-1d dataL = %-1d mmi = \n"
    ,bVipSHLoc->VipSH.dId,bVipSHLoc->VipSH.RelCopKey,bVipSHLoc->VipSH.chan
    ,bVipSHLoc->VipSH.area,bVipSHLoc->VipSH.dataL);
  i_s << sBuffer;
  for (iLoc = 1; iLoc<=cMaxComput; ++iLoc) {
    if (InSet(bVipSHLoc->mmiSet,iLoc,tmmiSet)) {
      sprintf(sBuffer,"%-1d ",iLoc);
      i_s << sBuffer;
      }
    }

  bVipSHLoc = bVipSHLoc->nextVipSH;
  i_s << endl;
  }

//test output of bStay entries
i_s << endl << "******* bStay entries ***********" << endl;
nextBDId = bStay;
while (nextBDId!=0) {
  sprintf(sBuffer,"dId = %-1d RelcopKey = %-1d: \n"
         ,nextBDId->AFDId,nextBDId->RelCopKey);
  i_s << sBuffer;
  nextBDispI = nextBDId->nextBDispI;
  while (nextBDispI!=0) {
    sprintf(sBuffer
      ," dispI = %3d|  gid = %-1d fN =%-1d aN =%-1d fF =%-1d aF =%-1d aP =%-1d\n"
      ,nextBDispI->AFDispI,nextBDispI->gid,nextBDispI->fAltNorm
      ,nextBDispI->aAltNorm,nextBDispI->fAltFl,nextBDispI->aAltFl
      ,nextBDispI->aAltPosFl);
    i_s << sBuffer;
    nextBTA = nextBDispI->nextBTA;
    while (nextBTA!=0) {
      sprintf(sBuffer,"   ta/%-1d/%-1d/%-1d/%-1d/%-1d//d%d/p%d/a%d/\n"
             ,nextBTA->ta.B1,nextBTA->ta.B2,nextBTA->ta.B3,nextBTA->ta.Elem
             ,nextBTA->ta.Info,nextBTA->desFlash,nextBTA->posFlash
             ,nextBTA->acknFlash);
      i_s << sBuffer;
      nextBTA = nextBTA->nextBTA;
      }
    nextBDispI = nextBDispI->nextBDispI;
    }
  nextBDId = nextBDId->nextBDId;
  i_s << endl;
  }

}  //dumpbFlashbStay
// ---------------------------------------------------------------------
// write internal buffers into dump file
//
static
void          dumpDIdTbl
(  ostream &   i_s
)
{
  i_s << endl << "***pDIdTbl: search for dId=<no>" << endl;

  int cnt = 0;
  tB123Rec*  pB123dId = pDIdTbl[361];
  for (int i=0; i<maxDId; ++i) {
    pB123dId = pDIdTbl[_AC(i,maxDId-1)];

    if (pB123dId != NULL) {
      i_s << "dId=" << (i+1) << endl;
      cnt = 0;
      while (pB123dId != NULL) {
        cnt++;
        i_s << pB123dId->B123Key.U1.B1 << '/'
            << pB123dId->B123Key.U1.B2 << '/'
            << pB123dId->B123Key.U1.B3 << ", ";
        pB123dId = pB123dId->nextB123DId;
        if (cnt >= 10) { i_s << endl; cnt = 0; }
      } // while
      i_s << endl;
    }
  }
  i_s << endl;

} // dumpDIdTbl
// ---------------------------------------------------------------------
// write multiphase norm element infos into dump file
//
static
void          dumpNoElMPhData
(  ostream &   i_s
) {
char    sBuffer[200];

if (!IsMultiPhaseYes) {
  i_s << endl << "******* Multiphase deactivated *******"
      << endl;
  return;
}

i_s << endl
    << "******* Multiphase NormElement-Infos *******"
    << endl;


bool lFnd = false;
for (int i=0; i<=neDim; i++) {
  lFnd = false;
  if (NoElMPhInfo[i].NetwStat > 0) {
    sprintf(sBuffer
           ,"NoEl:%4d - NetwStat:%4d - PhaseA,B,C,Assym: %4d,%d,%d,%d\n"
           ,i
           ,NoElMPhInfo[i].NetwStat,NoElMPhInfo[i].NeStPhA
           ,NoElMPhInfo[i].NeStPhB, NoElMPhInfo[i].NeStPhC
           ,NoElMPhInfo[i].NeStAssym);
    i_s << sBuffer;
  }
  if (NoElMPhInfo[i].NetwGrp > 0) {
    sprintf(sBuffer
           ,"NoEl:%4d - NetwGrp :%4d - PhaseA,B,C,Assym: %4d,%d,%d,%dd\n"
           ,i
           ,NoElMPhInfo[i].NetwGrp ,NoElMPhInfo[i].NeGrPhA
           ,NoElMPhInfo[i].NeGrPhB, NoElMPhInfo[i].NeGrPhC
           ,NoElMPhInfo[i].NeGrAssym);
    i_s << sBuffer;
  }
  if (lFnd)
    i_s << "---" << endl;
}

i_s << endl
    << "NetwStat CoDetaEl: ";

int mxElLin = 20;
int cnt = 0;
for (int i=1; i <= cDTKeyMax; i++) {
  if (InSet(NetwStatCoDetaElS,i,tCoDetaS)) {
    i_s << ' ' << i;
    cnt++;
  }
  if (cnt >= mxElLin) { i_s << endl; cnt = 0; }
}

i_s << endl
    << "NetwGrp  CoDetaEl: ";
cnt = 0;
for (int i=1; i <= cDTKeyMax; i++) {
  if (InSet(NetwGrpCoDetaElS,i,tCoDetaS)) {
    i_s << ' ' << i;
    cnt++;
  }
  if (cnt >= mxElLin) { i_s << endl; cnt = 0; }
}

} // dumpNoElMPhData
// ---------------------------------------------------------------------
// dump open line infos into dump file
//
static
void          dumpOpenLineData
(  ostream &   i_s
) {
  char    sBuffer[200];

  i_s << endl << endl
      << "******* One-side Open Line NormElement-Infos (neTopElem) *******"
      << endl;

  sprintf(sBuffer,"LineStat:%4d - PhaseA,B,C: %d,%d,%d\n"
                 ,OpenLineStatInfo.LineStat
                 ,OpenLineStatInfo.LineStatA
                 ,OpenLineStatInfo.LineStatB
                 ,OpenLineStatInfo.LineStatC
                 );

  i_s << sBuffer;
  i_s << endl << endl;

} // dumpOpenLineData
// ---------------------------------------------------------------------
// do some dumpvar output
//
static
void          MediDumpVar()
{
  errfmt(0,10,"Softbus ","MediDumpVar: ssig DumpVar");

  const char *fn = "/home/logs/medi_dump";

  ofstream f(fn, ios::out | ios::trunc);
  if (!f) {
    errfmt(0,1000,"DumpVar ","Error at open file %s - break!",fn);
    return;
  }
  f << "***\n*** Medi DumpVar\n***" << endl;

  if (SosyData.sdDEBUGlevel == 999) {
    // some more detailed dumps
    dumpDIdTbl(f);


  }

  // dump channel directory
  dumpChanDir(f);
  // dump bFlash, bStay
  dumpbFlashbStay(f);
  // dummp multi phase info configuration in nede
  dumpNoElMPhData(f);
  // dummp one side open line info configuration in nede
  dumpOpenLineData(f);

  f.close();

} // MediDumpVar

// ---------------------------------------------------------------------
// unknown softbus receive error messages Service/Request
//
static
void          pSbUnknown(
  const tSboHead*  ipSboHead)
{
  const char* const ESO = "pSbUnkno";

  err(0,1202,ESO,ipSboHead->Service,ipSboHead->Request
                ,ipSboHead->SubRequest,"vii");
  err(1,1202,ESO,SosyData.sdSBRcomp,SosyData.sdSBRobj,0.0,"cob");

} // pSbUnknown

// ---------------------------------------------------------------------
// unknown softbus receive error messages Header1/Header2
//
static
void          pSbUnknown(
  const ObjectIdB iHeader1
, const tMediJob  iHeader2
) {
  const char* const ESO = "pSbUnkno";

  err(0,1202,ESO,rs("H1H2"),iHeader1,iHeader2,"soi");
  err(1,1202,ESO,SosyData.sdSBRcomp,SosyData.sdSBRobj,0.0,"cob");

} // pSbUnknown
// ---------------------------------------------------------------------

static
void          determine_current_RelcopKey()
{
#if L_ENDIAN
if (receive.SboH.Service!=s_SBSIGNAL &&
    receive.SboH.Service!=s_DIM_ME) {
#else
if (receive.SboH.Service>cLastService) {
#endif
  if (receive.U1.Header1==obDIMA &&
      (receive.outDisp.Header2==jstrtUpd ||
       receive.outDisp.Header2==jstrtRemoUpd ||
       receive.outDisp.Header2==jStopUpd)) {
    current_RelCopKey = 0;
    return;
    }

  if (receive.U1.Header1==obMEDI &&
      (receive.U1.Header2==jDMSDId || receive.U1.Header2==jDMSAFGr)) {
    current_RelCopKey = 0;
    return;
    }

  if (receive.U1.Header1==obPSM || receive.U1.Header1==obACTM ||
      receive.U1.Header1==obDBAM) {
    current_RelCopKey = 0;
    return;
    }

  current_RelCopKey = receive.U1.RelCopKey;
} else {
  if (receive.SboH.Service==s_SBSIGNAL)
    current_RelCopKey = 0;
  else {
    current_RelCopKey = receive.SboH.RelCopKey;
    }
  }
}


//Check of sbReceive input
static
bool          okInputMe()
{
bool          okInputMe_RTN;
bool          testChan;
int           inpChan;
ComputId      inpComput;

okInputMe_RTN = true;
testChan = false;
inpChan = 0;
inpComput = 0;
//with receive
switch (receive.U1.Header1) {
  case obDIMA:
    switch (receive.outDisp.Header2) {
      case joutDisp:
        inpComput = receive.outDisp.AckPara.Comput;
        if (receive.outDisp.dId==0 && !receive.outDisp.newDisp) {
          testChan = true;
          inpChan = receive.outDisp.chan;
          }
        if (receive.outDisp.dId!=0) {
          if (inpComput<=0 || inpComput>=cMaxComput) {
            okInputMe_RTN = false;
            errfmt(0,1201,"MAIN    ","okInputMe: incorrect comput: H1/H2: %d/%d, inpComput/inpChan: %d/%d"
                         ,receive.U1.Header1,receive.U1.Header2,inpComput,inpChan);
            }
          }
        break;
      case jstrtUpd:
      case jstrtRemoUpd:
      case jStopUpd:
      case jRnewUpd:
        // blank case
        break;
      default:
        okInputMe_RTN = false;
        err(0,1203,"MAIN    ",rs("JOB "),receive.U1.Header1,
            receive.U1.Header2,"SII");
        break;
      }//case header2
    break;
  case obMEDI:
    switch (receive.U1.Header2) {
      case jrawInMe:
        // blank case
        break;
      case jrawInMeTopo:
        // blank case
        break;
      case jFlash:
        if (receive.flash.chan!=0) {
          testChan = true;
          inpChan = receive.flash.chan;
          inpComput = receive.flash.AckPara.Comput;
          }
        break;
      case jsingAck:
        // blank case
        break;
      case jmeCoAck:
        // blank case
        break;
      case jtotalAck:
        // blank case
        break;
      case jcheckAf:
        // blank case
        break;
      case jmePosit:
        testChan = true;
        inpChan = receive.mePosit.chan;
        inpComput = receive.mePosit.AckPara.Comput;
        break;
      case jDMSDId:
        // blank case
        break;
      case jDMSAFGr:
        // blank case
        break;
      case jMeTrace:
        // blank case
        break;
      case jMedispC:
        // blank case
        break;
      default:
        okInputMe_RTN = false;
        err(0,1203,"MAIN    ",rs("JOB "),receive.U1.Header1,
            receive.U1.Header2,"SII");
        break;
      }//case header2
    break;
  case obVIP:
    switch (receive.vDispAck.Header2) {
      case vipDispAck:
        // blank case
        break;
      default:
        okInputMe_RTN = false;
        err(0,1203,"MAIN    ",rs("JOB "),receive.U1.Header1,
            receive.U1.Header2,"SII");
        break;
      }//case header2
    break;

  case obACTM:
  case obDBAM:
    // blank case
    break;

  default:
    okInputMe_RTN = false;
    err(0,1202,"MAIN    ",rs("OBJ "),receive.U1.Header1,receive.U1.Header2,
        "SII");
    break;
  }//case header1

if (testChan) {
  if (inpComput<=0 || inpComput>=cMaxComput || inpChan<=0 ||
      inpChan>=cMaxChan || mmiDir[_AC(inpComput-1,cMaxComput-1)]==0 ||
      mmiDir[inpComput-1]->VChan[_AC(inpChan-1,cMaxChan-1)]==0) {
    okInputMe_RTN = false;
    errfmt(0,1201,"MAIN    ","okInputMe: incorrect comput/chan: H1/H2 :%d/%d, inpComput/inpChan: %d/%d"
                            ,receive.U1.Header1,receive.U1.Header2,inpComput,inpChan);

    }
  }
return okInputMe_RTN;
}  //okInputMe


//reinitialized of the new OTS's case is installed
static
void      trainingInit()
{
writeDispdes  = false;
writeActFileC = false;
//determine all dId's with meVar for updating
meSoftInit    = true;
corrSoftInit  = false;
createNew     = true;
CleSet(ActFileSet,tActFileSet);
CleSet(okActFileSet,tActFileSet);
CleSet(HISActFileSet,tActFileSet);
lastRelCopKey = 0;
vdispDes.dId  = 1;
DbPos(&fDispDes,NxKey);
while (DbRead(&fDispDes,NextRc+RpOnPage)) {
  vdispDes.dId = fDispDes.RpP->dId;
  if (InSet(fDispDes.RpP->sDispVar,(int)meVar,
            fDispDes.RpP->sDispVar)) {
    if (vdispDes.dId > maxDId)
      err(0,2102,"trainInt",rs("maxD"),ri(maxDId),
          ri(vdispDes.dId),"SII");
    else {
      AddSet(ActFileSet,vdispDes.dId,tActFileSet);
          } //else
        } //if vdispDes
      } //with and while DbRead
if (fDispDes.Flag<0)
  err(0,2103,"trainInt",ri(fDispDes.Flag),ri(fDispDes.Fv.FlagS),
                        ri(vdispDes.dId),"III");
else
  meSoftInit=false;
pFRel(); //Release dispDes and actfile /
} //trainingInit


//this procedure is used by OTS mode to quickly read all of the
//displays and update the signalization when a new case is installed
static
void      trainingcheck()
{

if (MyOpMode == opmTraining) {  //if Training mode - process
  //need to read all dispdes records, to update signalization ASAP
  cp(0,2001,"TRAINING",rs("MEDI"),rs("STAR"),rs("TED "),"SZZ");
  receive.SboH.RelCopKey = 0;
  receive.U1.RelCopKey = 0; //this is needed for resetSeds -
                            //receive.RelCopKey does not have a value
                            //for Softbus signal SBSIGUSR4
  vdispDes.dId = 1;  //start at the beginning of Dispdes
  dId          = 1;

  pResetUpd();    // reset ui update admin
  while (dId <= maxDId) {
    //loop through all possible disp#
    if ((InSet(ActFileSet,dId,tActFileSet)) &&
        !(InSet(okActFileSet,dId,tActFileSet)))
      //dId contains meVar but is not updated yet
      if (pRDispDesC()) {  //Read dispDes
        dIdCheck = dId;
        pWrActFile();  //Generate (write) actfile
        pMmiUpdt();    //Send Updating file to mmi"s
        } //if pRDispDesC
    pFRel(); //Release dispDes
    dId = dId+1;
    } //while i<=maxDId

  } //if McNormStatus
} //procedure trainingcheck


//main

int main(int argc,char *argv[])
{
PASCAL_MAIN(argc,argv);
if (setjmp(_JL99))
  goto _L99;
TestOut = 0;
SosyInitCE(argc,argv);

RunOK = SbNotify(&sbF,obMEDI,version,0);
RunOK = (RunOK || sbF==0 || sbF==SBeNew);
if (!RunOK)
  err(0,3200,"MAIN    ",rs("SBNO"),sbF,version,"SII");

McNotifyEndGCObjAck(0,true);

pMeStart();                       //Start of MEDI: presettings
relOpen = false;
firstSoftInit = true;

SbModReceLock();

_L10:
    //starting point after break of job execution
Enable_Exception();
if (endOfJob) {
  Disable_Exception();
  //with receive

  if (receive.U1.Header1==obDIMA) {
    if (receive.outDisp.Header2==joutDisp) {
      receive.U1.AckPara.Acknowl.Ok = false;
      pAckn();                    //Acknowledgement to DIMA
      }
    }
  if (receive.U1.Header1==obMEDI) {
    if (receive.U1.Header2==jsingAck) {
      SvcA.sSAk0.Ok = false;
      SvcA.sSAk0.textid = stxMeDatError;
      pSvcAck();                  //Acknowledgement to PSM
      }
    /**t*
    if (   (Header2=jrawInMe)
    then begin
      Es wurden Bilder nicht aktualisiert!!!
      -> Alle Bilder ungueltig setzen, vielleicht noch eine
      weitere Fehlermeldung die darauf hinweist!!!
      end;*t**/
    }
  if (receive.U1.Header1==obVIP) {
    if (receive.vDispAck.Header2==vipDispAck) {
      SvcA.sSAk0.Ok = false;
      SvcA.sSAk0.textid = stxMeDatError;
      pSvcAck();                  //Acknowledgement to PSM
      }
    }
  if (receive.U1.Header1==obMEDI) {
    if (receive.U1.Header2==jcheckAf) {
      err(0,2606,"exceptio",dIdCheck,0.0,0.0,"III");
      if (!InSet(okActFileSet,dIdCheck,tActFileSet))
        SubSet(ActFileSet,dIdCheck,tActFileSet);
      }
    }
  if (meSoftInit &&
      (receive.U1.Header1!=obMEDI || receive.U1.Header2!=jcheckAf)) {
    err(0,2607,"exceptio",fDispDes.Flag,vdispDes.dId,0.0,"III");
    McAckEndGC(&sbF,0);
    SubSet(ActFileSet,vdispDes.dId,tActFileSet);
    corrSoftInit = true;
    meSoftInit = false;
    }
  pFRel();                        //Release dispDes
                                  //*t* pFDispDes?

  SbCancel(&sbF,ClockId);
  Rastint.Sys.Min = 0;
  Rastint.Sys.Sec = secAlarm;
  SbAlarm(&sbF,MyComputId,saMeactF,obMEDI,sizeof(tCheckAf),&rCheckAF,ClockId,
          &Rastint,SbAfter,SbIntern);

  endOfJob = false;
  Enable_Exception();
  }
//break of job execution after error

while (RunOK) {
 try{
  //Clean up
  pDbcOpen(0);
  if (!NIMORIG(&nimF,0) && nimF!=eNimNoOpen)
    err(0,1304,"MAIN    ",rs("NIMO"),nimF,lastRelCopKey,"SII");

  bActSign.ActSignNu>0 && pSendSeds();

  if (LTestOut>rTrace.LMaxTestO)
    //close tracefile and switch to alternate file
    chgTraceFile();

  if (!IpcReceive(&sbFRec,&diAddr,DataSize,&receive.U1.Header1,0))
    err(0,3201,"MAIN    ",rs("IPCR"),sbFRec,diAddr,"SII");
  else {

    //else with begin
    determine_current_RelcopKey();

    //Handle change of database copy
    pDbcOpen(current_RelCopKey);

    switch (receive.SboH.Service) {
      case s_SBSIGNAL:
        switch (receive.SboH.Request) {
          case SBsigEndProg:
            RunOK = false;
            break;
          case SBsigSoftInit:
            if (firstSoftInit) {
              firstSoftInit = false;
              //opening of database; initiation of channel directory
              pMeSoftInit();      //Start of MEDI: opening of database files
              relOpen = true;
              }
            break;
          case SBsigLifeCheck:
            McAckLifeCheck();
            break;
          case SBSIGUSR1:
            //close tracefile and switch to alternate file
            chgTraceFile();
            break;
          case SBSIGUSR2:         //create mmiDir for mmi`s
            pSigUsr2(MyOpMode);
            break;
          case SBSIGUSR3:         //Set updating files invalid
            pSigUsr3();
            break;
          case SBSIGUSR4:         //Open sb-addresses
            pORe();
            break;
          case SBsigOwnComState:
            pSigOwnComState();
            break;
          case SBsigPartnerComState:
             pSigPartnerComState();
             break;
          case SBsigOtherComState:
            pSigOtherComState();
            break;
          case SBsigParaChgd:
            break;
          case SBsigDumpVar:
            MediDumpVar();
            break;
          default:
            cp(0,1,"Softbus ",receive.SboH.Request,rs("????"),0.0,"GSB");
            break;
          }//case SboH.Request
        break;
      case s_DIM_ME:
        switch (receive.SboH.Request) {
          case r_MeAcknOrd:  //UIPAPI acknowledgement message
            pApiAck(true);
            break;
          case r_MeAcknMsg:  //UIPAPI acknowledgement message
            pApiAck(false);
            break;
          case r_OTS_Load:
          if (MyOpMode == opmTraining) {
            cp(0,2000,"TRAINING",rs("OTS "),rs("LOAD"),rs("CASE"),"sss");

            trainingInit();  //initialized the new case
            SetSet(AllDidSet,ActFileSet,tActFileSet);
            trainingcheck(); //for Training mode, update signalization
            pReSelect();                    //reselect changed dIds
            cp(0,2002,"TRAINING",rs("MEDI"),rs("COMP"),rs("LETE"),"SZZ");

            if (receive.SboH.SubRequest == 0) {
              ById00(&OTS_AckSb,sizeof(tSboHead));
#if SGW
              ots_computer = McOpModeBlFServer(McOwnLevel(),
                              blfCOMMUNICATOR,true,opmTraining);
#else
              ots_computer = McOpModeBlFServer(McOwnLevel(),blfOTS,
                                                true,opmTraining);
#endif
              if (!strncmp(getenv("SPECSUBS_DOTS"),"1",1)) {
                OTS_AckSb.Request = r_OTS_MEDI;
                OTS_AckSb.Service = s_DOTS_CCM;
                if (!SbTransm(&sbF,ots_computer,saDOtCcm,obDsOtsCcm,
                    sizeof(tSboHead),&OTS_AckSb))
                  err(0,3211,"mainMedi",rs("SBTR"),ri(sbF),
                              ri(saDOtCcm),"sii");
                else
                  cp(0,2003,"TRAINING",rs("DOTS "),rs("LOAD"),rs("DONE"),"sss");
              }
              else {
                OTS_AckSb.Request = r_OTS_MEDI;
                OTS_AckSb.Service = s_OTS_CCM;
                if (!SbTransm(&sbF,ots_computer,saOtsCcm,obOtsCcmi,
                    sizeof(tSboHead),&OTS_AckSb))
                  err(0,3211,"mainMedi",rs("SBTR"),ri(sbF),
                              ri(saOtsCcm),"sii");
                else
                  cp(0,2003,"TRAINING",rs("OTS "),rs("LOAD"),rs("DONE"),"sss");
              }

            } // if SboH.SubRequest
          else
            cp(0,2003,"TRAINING",rs("OTS "),rs("LOAD"),rs("DONE"),"sss");
          break;
          } //if r_OTS_Load

          case r_ADD_ElementsToSegment:
          case r_REM_ElementsFromSegment:
            ProcessDynVar(&receive.rMeVarRegister);
            break;
          default:
            cp(0,1,"Unknown ",receive.SboH.Service,receive.SboH.Request
                             ,receive.SboH.SubRequest,"vii");
            cp(0,1,"Unknown ",SosyData.sdSBRcomp,SosyData.sdSBRobj,0.0,"cob");
            break;
          }//case SvoH.Request
        break;
      case s_SSI_ACOP:
         switch (receive.SboH.Request) {
           case r_SSI_CoAcData:
             pUpdResp(&receive.vUpdCoAcD.XTData[0]);
             break;
           default:
             cp(0,1,"Unknown ",receive.SboH.Service,receive.SboH.Request
                              ,receive.SboH.SubRequest,"vii");
             cp(0,1,"Unknown ",SosyData.sdSBRcomp,SosyData.sdSBRobj,0.0,"cob");
             break;
         }
         break; // s_SSI_ACOP

      default:
        //Check of sbReceive input
        if (!okInputMe()) {
          endOfJob = true;
          goto _L99;              //end of job
          }

        switch (receive.U1.Header1) {  //otherwise
          case obDIMA:
            switch (receive.outDisp.Header2) {
              case joutDisp:      //Selection of display
                pOutDisp();
                break;
              case jstrtUpd:      //Start updating of a mmi computer
                pStrtUpd();
                break;
              case jStopUpd:      //Stop updating of a mmi computer
                pStopUpd();
                break;
              case jRnewUpd:      //renew updating of a mmi computer
                pRnewUpd();
                break;
              case jstrtRemoUpd:  //Start updating of a remote mmi
                pMmiDId();
                break;
              default:
                break;
              }
            break;
          case obMEDI:
            switch (receive.U1.Header2) {
              case jrawInMe:      //Status change
                pRawInMe();
                break;
              case jFlash:        //Flashing
                pFlash();
                break;
              case jsingAck:      //Single acknowledgement
                pSingAck();
                break;
              case jmeCoAck:      //Common acknowledgement
                pMeCoAck();
                break;
              case jtotalAck:     //Total acknowledgement
                ptotalAck();
                break;
              case jcheckAf:      //Check updating file
                pCheckAf();
                break;
              case jmePosit:      //Let variables be positionable
                pMePosit();
                break;
              case jDMSDId:       //dId changed by DMS
                pDMSDId();
                break;
              case jDMSAFGr:      //attr. or fig. group changed by DMS
                pDMSAFGr();
                break;
              case jMeTrace:      //set trace switches
                pTrace();
                break;
              case jrawInMeTopo:  //topological status change
                pRawInMeTopo();
                break;
              case jMedispC:      //meDisp for study case
                pMedispC();
                break;
              default:
                break;
              }
            break;
          case obVIP:
            switch (receive.vDispAck.Header2) {
              case vipDispAck:    //Display acknowledgement
                pFulDispAck();
                break;
              default:
                break;
              }
            break;
          case obACTM:
          case obDBAM:            //process changes blocks/elements
            Process_DBA_Changes();
            break;
          default:
            break;
          }//case header1
        break;
      }//case SboH.Service
    }
   }
 catch (SubrangeOutOfBounds &e) {  // accepted - malformed order-data
   errfmt(0,3001,"Main","%s",e.what());
   }
 catch (RuntimeError &e) {
   errfmt(0,4001,"Main","%s",e.what());
   endOfJob = true;
   RunOK = false;
   }
  }  //while RunOK
_L99:
if (endOfJob)
  goto _L10;
if (relOpen)                      //Close all relations
  pDbClose();
SbCancel(&sbF,ClockId);
if (TestOut!=0)
  fclose(TestOut);
exit(EXIT_SUCCESS);
}
