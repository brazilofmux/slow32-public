/* s32cc_parse.h -- Parser + code emitter for s32-cc (stage10)
 * Assumes s32cc_lex.h symbols in scope via concatenation.
 * cc-min compatible source. */

#define P_MAX_OUT 2097152
static char p_out[P_MAX_OUT];
static int p_olen;
static int p_lbl;
static int p_lval;
static int p_ety;
#define TY_INT 0
#define TY_CHAR 1
#define TY_VOID 2
#define TY_STRUCT 3

#define P_FORBUF 4096
static char p_fbuf[P_FORBUF];
static int p_flen;
static int p_femit;

#define P_MXLP 16
static int p_brk[P_MXLP];
static int p_ctn[P_MXLP];
static int p_ldep;

#define P_MXCASE 512
static int p_swv[P_MXCASE];
static int p_swl[P_MXCASE];
static int p_swn;
static int p_swdef;
static int p_swend;
static int p_swdisp;
static int p_swoff;
static int p_insw;

#define P_NSZ 48

static int p_retl;
static int p_ppos;

#define P_MXLOC 384
#define P_MXFN 512
#define P_MXGLOB 512
#define P_LNBUF 18432
#define P_FNBUF 24576
#define P_GNBUF 24576
static char p_ln[P_LNBUF];
static int p_lo[P_MXLOC];
static int p_lt[P_MXLOC];
static int p_ls[P_MXLOC];
static int p_nl;
static int p_loff;
static char p_fn[P_FNBUF];
static int p_fp[P_MXFN];
static int p_fr[P_MXFN];
static int p_nf;
static char p_gn[P_GNBUF];
static int p_gty[P_MXGLOB];
static int p_gsz[P_MXGLOB];
static int p_gby[P_MXGLOB];
static int p_giv[P_MXGLOB];
static int p_ng;
#define P_MXST 128
#define P_MXFD 768
static char p_sn[6144];
static int p_ss[P_MXST];
static int p_sfb[P_MXST];
static int p_sfc[P_MXST];
static int p_ns;
static char p_fdn[36864];
static int p_fdt[P_MXFD];
static int p_fdo[P_MXFD];
static int p_fda[P_MXFD];
static int p_nfd;
#define P_MXTD 256
static char p_tdn[12288];
static int p_tdt[P_MXTD];
static int p_ntd;
#define P_MXDEF 512
#define P_DNBUF 12288
static char p_dn[P_DNBUF];
static int p_dv[P_MXDEF];
static int p_nd;

#define P_MXLBL 512
#define P_LBNBUF 24576
static char p_lbn[P_LBNBUF];
static int p_lbid[P_MXLBL];
static int p_lbdef[P_MXLBL];
static int p_nlb;

#define PP_MXIF 8
static int pp_skip;
static int pp_stk[PP_MXIF];
static int pp_dep;

static char p_sdir[128];

static int p_spos;
static int p_sline;
static int p_scol;
static int p_stok;
static int p_sval;
static char p_sstr[256];
static int p_sslen;

static void p_err(char *m){fputs("s32cc:",stderr);fput_uint(stderr,lex_line);fputs(": error: ",stderr);fputs(m,stderr);fputc(10,stderr);}
static int p_seq(char *a,char *b){int i;i=0;while(1){if(a[i]!=b[i])return 0;if(a[i]==0)return 1;i=i+1;}}
static int p_scpy(char *d,char *s,int m){int i;i=0;while(s[i]!=0&&i<m-1){d[i]=s[i];i=i+1;}d[i]=0;return i;}
static void p_sset(char *b,int i,char *n){int x;int j;x=i*P_NSZ;j=0;while(n[j]!=0&&j<P_NSZ-1){b[x+j]=n[j];j=j+1;}b[x+j]=0;}
static int p_sneq(char *b,int i,char *n){int x;int j;x=i*P_NSZ;j=0;while(1){if(b[x+j]!=n[j])return 0;if(n[j]==0)return 1;j=j+1;}}

static int p_floc(char *n){int i;i=0;while(i<p_nl){if(p_sneq(p_ln,i,n))return i;i=i+1;}return -1;}
static void p_aloc(char *n,int ty,int ac){int sz;int esz;int si;if(p_nl>=P_MXLOC){p_err("LOC");return;}if(ac>0){esz=4;if((ty&255)==TY_CHAR&&((ty>>8)&255)==0)esz=1;if((ty&255)==TY_STRUCT&&((ty>>8)&255)==0){si=(ty>>16)&127;esz=p_ss[si];}sz=ac*esz;sz=((sz+3)/4)*4;}else if((ty&255)==TY_STRUCT&&((ty>>8)&255)==0){si=(ty>>16)&127;sz=p_ss[si];sz=((sz+3)/4)*4;}else{sz=4;}p_loff=p_loff-sz;p_sset(p_ln,p_nl,n);p_lo[p_nl]=p_loff;p_lt[p_nl]=ty;p_ls[p_nl]=ac;p_nl=p_nl+1;}
static void p_apar(char *n,int i,int ty){int o;if(p_nl>=P_MXLOC){p_err("LOC");return;}o=-12-i*4;p_sset(p_ln,p_nl,n);p_lo[p_nl]=o;p_lt[p_nl]=ty;p_ls[p_nl]=0;p_nl=p_nl+1;}

static int p_ffn(char *n){int i;i=0;while(i<p_nf){if(p_sneq(p_fn,i,n))return i;i=i+1;}return -1;}
static void p_afn(char *n,int np,int rt){if(p_nf>=P_MXFN){p_err("FN");return;}p_sset(p_fn,p_nf,n);p_fp[p_nf]=np;p_fr[p_nf]=rt;p_nf=p_nf+1;}

static int p_fgl(char *n){int i;i=0;while(i<p_ng){if(p_sneq(p_gn,i,n))return i;i=i+1;}return -1;}
static void p_agl(char *n,int ty,int ac,int bsz){if(p_ng>=P_MXGLOB){p_err("GL");return;}p_sset(p_gn,p_ng,n);p_gty[p_ng]=ty;p_gsz[p_ng]=ac;p_gby[p_ng]=bsz;p_giv[p_ng]=0;p_ng=p_ng+1;}

static int p_fst(char *n){int i;i=0;while(i<p_ns){if(p_sneq(p_sn,i,n))return i;i=i+1;}return -1;}
static int p_ast(char *n){int i;if(p_ns>=P_MXST){p_err("ST");return 0;}i=p_ns;p_sset(p_sn,i,n);p_ss[i]=0;p_sfb[i]=p_nfd;p_sfc[i]=0;p_ns=p_ns+1;return i;}
static int p_ffd(int si,char *n){int b;int c;int i;b=p_sfb[si];c=p_sfc[si];i=0;while(i<c){if(p_sneq(p_fdn,b+i,n))return b+i;i=i+1;}return -1;}
static int p_ftd(char *n){int i;i=0;while(i<p_ntd){if(p_sneq(p_tdn,i,n))return i;i=i+1;}return -1;}
static void p_atd(char *n,int ty){if(p_ntd>=P_MXTD){p_err("TD");return;}p_sset(p_tdn,p_ntd,n);p_tdt[p_ntd]=ty;p_ntd=p_ntd+1;}
static int p_fdf(char *n){int i;i=0;while(i<p_nd){if(p_sneq(p_dn,i,n))return i;i=i+1;}return -1;}
static void p_adf(char *n,int v){if(p_nd>=P_MXDEF){p_err("DF");return;}p_sset(p_dn,p_nd,n);p_dv[p_nd]=v;p_nd=p_nd+1;}

static int p_flbl(char *n){int i;i=0;while(i<p_nlb){if(p_sneq(p_lbn,i,n))return i;i=i+1;}return -1;}
static int p_albl(char *n){int i;i=p_flbl(n);if(i>=0)return i;if(p_nlb>=P_MXLBL){p_err("LBL");return 0;}i=p_nlb;p_sset(p_lbn,i,n);p_lbid[i]=p_nl2();p_lbdef[i]=0;p_nlb=p_nlb+1;return i;}

/* Preprocessor */
static void pp_sl(void){while(lex_pos<lex_len&&lex_src[lex_pos]!=10)lex_pos=lex_pos+1;}
static int pp_int(void){int v;int n;int c;n=0;v=0;while(lex_pos<lex_len&&(lex_src[lex_pos]==32||lex_src[lex_pos]==9))lex_pos=lex_pos+1;if(lex_pos<lex_len&&lex_src[lex_pos]==45){n=1;lex_pos=lex_pos+1;}if(lex_pos+1<lex_len&&lex_src[lex_pos]==48&&lex_src[lex_pos+1]==120){lex_pos=lex_pos+2;while(lex_pos<lex_len){c=lex_src[lex_pos]&255;if(c>=48&&c<=57){v=v*16+(c-48);lex_pos=lex_pos+1;}else if(c>=97&&c<=102){v=v*16+(c-87);lex_pos=lex_pos+1;}else if(c>=65&&c<=70){v=v*16+(c-55);lex_pos=lex_pos+1;}else break;}}else{while(lex_pos<lex_len&&lex_src[lex_pos]>=48&&lex_src[lex_pos]<=57){v=v*10+(lex_src[lex_pos]-48);lex_pos=lex_pos+1;}}while(lex_pos<lex_len&&(lex_src[lex_pos]==85||lex_src[lex_pos]==117||lex_src[lex_pos]==76||lex_src[lex_pos]==108))lex_pos=lex_pos+1;if(n)v=0-v;return v;}

static void pp_def(void){char nm[128];int i;int v;while(lex_pos<lex_len&&(lex_src[lex_pos]==32||lex_src[lex_pos]==9))lex_pos=lex_pos+1;i=0;while(lex_pos<lex_len&&(lex_is_alpha(lex_src[lex_pos]&255)||lex_is_digit(lex_src[lex_pos]&255))){if(i<126){nm[i]=lex_src[lex_pos];i=i+1;}lex_pos=lex_pos+1;}nm[i]=0;if(lex_pos<lex_len&&lex_src[lex_pos]==40){pp_sl();return;}v=pp_int();p_adf(nm,v);pp_sl();}

static void pp_nm(char *nm){int i;while(lex_pos<lex_len&&(lex_src[lex_pos]==32||lex_src[lex_pos]==9))lex_pos=lex_pos+1;i=0;while(lex_pos<lex_len&&(lex_is_alpha(lex_src[lex_pos]&255)||lex_is_digit(lex_src[lex_pos]&255))){if(i<126){nm[i]=lex_src[lex_pos];i=i+1;}lex_pos=lex_pos+1;}nm[i]=0;}

static void pp_include(void){char fn[256];int i;int f;int ch;int n;int d;while(lex_pos<lex_len&&(lex_src[lex_pos]==32||lex_src[lex_pos]==9))lex_pos=lex_pos+1;if(lex_pos>=lex_len||lex_src[lex_pos]!=34){pp_sl();return;}lex_pos=lex_pos+1;d=0;while(p_sdir[d]!=0)d=d+1;i=0;while(i<d){fn[i]=p_sdir[i];i=i+1;}while(lex_pos<lex_len&&lex_src[lex_pos]!=34&&lex_src[lex_pos]!=10){if(d<254){fn[d]=lex_src[lex_pos];d=d+1;}lex_pos=lex_pos+1;}if(lex_pos<lex_len&&lex_src[lex_pos]==34)lex_pos=lex_pos+1;fn[d]=0;pp_sl();f=fopen(fn,"rb");if(!f){p_err("INC");return;}n=0;while(1){ch=fgetc(f);if(ch<0)break;n=n+1;}fclose(f);if(n==0||n>LEX_SRC_SZ-lex_len-1)return;i=lex_len-1;while(i>=lex_pos){lex_src[i+n]=lex_src[i];i=i-1;}f=fopen(fn,"rb");if(!f)return;i=0;while(i<n){ch=fgetc(f);if(ch<0)break;lex_src[lex_pos+i]=ch;i=i+1;}fclose(f);lex_len=lex_len+n;}

static void pp_dir(void){char d[16];char nm[128];int i;while(lex_pos<lex_len&&(lex_src[lex_pos]==32||lex_src[lex_pos]==9))lex_pos=lex_pos+1;i=0;while(lex_pos<lex_len&&lex_is_alpha(lex_src[lex_pos]&255)){if(i<14){d[i]=lex_src[lex_pos];i=i+1;}lex_pos=lex_pos+1;}d[i]=0;if(pp_skip){if(p_seq(d,"ifdef")||p_seq(d,"ifndef")){pp_dep=pp_dep+1;pp_stk[pp_dep]=1;pp_sl();return;}if(p_seq(d,"endif")){if(pp_dep>0){pp_skip=pp_stk[pp_dep];pp_dep=pp_dep-1;if(pp_dep==0)pp_skip=0;else pp_skip=pp_stk[pp_dep];}pp_sl();return;}if(p_seq(d,"else")){if(pp_dep>0&&(pp_dep<=1||pp_stk[pp_dep-1]==0)){pp_skip=1-pp_skip;pp_stk[pp_dep]=pp_skip;}pp_sl();return;}pp_sl();return;}if(p_seq(d,"define")){pp_def();return;}if(p_seq(d,"include")){pp_include();return;}if(p_seq(d,"ifdef")){pp_nm(nm);pp_dep=pp_dep+1;if(p_fdf(nm)>=0)pp_skip=0;else pp_skip=1;pp_stk[pp_dep]=pp_skip;pp_sl();return;}if(p_seq(d,"ifndef")){pp_nm(nm);pp_dep=pp_dep+1;if(p_fdf(nm)>=0)pp_skip=1;else pp_skip=0;pp_stk[pp_dep]=pp_skip;pp_sl();return;}if(p_seq(d,"endif")){if(pp_dep>0)pp_dep=pp_dep-1;if(pp_dep==0)pp_skip=0;else pp_skip=pp_stk[pp_dep];pp_sl();return;}if(p_seq(d,"else")){if(pp_dep>0){pp_skip=1-pp_skip;pp_stk[pp_dep]=pp_skip;}pp_sl();return;}pp_sl();}

static void next(void){int di;while(1){lex_next();if(lex_tok==TK_HASH){pp_dir();continue;}if(pp_skip){continue;}if(lex_tok==TK_IDENT){di=p_fdf(lex_str);if(di>=0){lex_tok=TK_NUM;lex_val=p_dv[di];return;}}return;}}
static int p_expect(int t){if(lex_tok!=t){p_err("TOK");return 0;}next();return 1;}
static void p_lsave(void){int i;p_spos=lex_pos;p_sline=lex_line;p_scol=lex_col;p_stok=lex_tok;p_sval=lex_val;p_sslen=lex_slen;i=0;while(i<=lex_slen){p_sstr[i]=lex_str[i];i=i+1;}}
static void p_lrest(void){int i;lex_pos=p_spos;lex_line=p_sline;lex_col=p_scol;lex_tok=p_stok;lex_val=p_sval;lex_slen=p_sslen;i=0;while(i<=p_sslen){lex_str[i]=p_sstr[i];i=i+1;}}

/* Emit helpers */
static int p_nl2(void){int l;l=p_lbl;p_lbl=p_lbl+1;return l;}
static void pe(char *s){int i;i=0;while(s[i]!=0){if(p_femit){if(p_flen<P_FORBUF-1){p_fbuf[p_flen]=s[i];p_flen=p_flen+1;}}else{if(p_olen<P_MAX_OUT-1){p_out[p_olen]=s[i];p_olen=p_olen+1;}}i=i+1;}}
static void pc(int c){if(p_femit){if(p_flen<P_FORBUF-1){p_fbuf[p_flen]=c;p_flen=p_flen+1;}}else{if(p_olen<P_MAX_OUT-1){p_out[p_olen]=c;p_olen=p_olen+1;}}}
static void pn(int v){char b[12];int i;int ng;if(v==-2147483647-1){pe("-2147483648");return;}ng=0;if(v<0){ng=1;v=0-v;}i=0;if(v==0){b[0]=48;i=1;}while(v>0){b[i]=48+(v%10);i=i+1;v=v/10;}if(ng)pc(45);while(i>0){i=i-1;pc(b[i]);}}
static void p_ld(int id){pe(".L");pn(id);pe(":\n");}
static void p_lr(int id){pe(".L");pn(id);}
static void p_push(void){pe("    addi r29, r29, -4\n    stw r29, r1, 0\n");}
static void p_pop(void){pe("    ldw r2, r29, 0\n    addi r29, r29, 4\n");}

static void p_li(int v){int hi;int lo;if(v>=-2048&&v<=2047){pe("    addi r1, r0, ");pn(v);pc(10);}else{hi=(v+2048)>>12;hi=hi&1048575;lo=v&4095;if(lo>=2048)lo=lo-4096;pe("    lui r1, ");pn(hi);pc(10);pe("    addi r1, r1, ");pn(lo);pc(10);}}
static void p_bz(int l){int s;s=p_nl2();pe("    bne r1, r0, ");p_lr(s);pc(10);pe("    jal r0, ");p_lr(l);pc(10);p_ld(s);}
static void p_bnz(int l){int s;s=p_nl2();pe("    beq r1, r0, ");p_lr(s);pc(10);pe("    jal r0, ");p_lr(l);pc(10);p_ld(s);}
static void p_jmp(int l){pe("    jal r0, ");p_lr(l);pc(10);}

static void p_prolog_ph(void){int i;p_ppos=p_olen;i=0;while(i<128){if(p_olen<P_MAX_OUT-1){p_out[p_olen]=32;p_olen=p_olen+1;}i=i+1;}pc(10);}
static void p_prolog_fin(int fs){int sv;int s31;int s30;sv=p_olen;p_olen=p_ppos;s31=fs-4;s30=fs-8;pe("    addi r29, r29, -");pn(fs);pe("\n    stw r29, r31, ");pn(s31);pe("\n    stw r29, r30, ");pn(s30);pe("\n    addi r30, r29, ");pn(fs);pc(10);while(p_olen<p_ppos+128){p_out[p_olen]=32;p_olen=p_olen+1;}p_olen=sv;}
static void p_epilog(int rl,int fs){int s31;int s30;s31=fs-4;s30=fs-8;p_ld(rl);pe("    ldw r31, r29, ");pn(s31);pe("\n    ldw r30, r29, ");pn(s30);pe("\n    addi r29, r29, ");pn(fs);pe("\n    jalr r0, r31, 0\n");}
static void p_call(char *n){pe("    jal r31, ");pe(n);pc(10);}
static void p_esn(char *b,int i){int x;int j;x=i*P_NSZ;j=0;while(b[x+j]!=0){pc(b[x+j]);j=j+1;}}

static void p_ega(int gi){pe("    lui r1, %hi(");p_esn(p_gn,gi);pe(")\n    addi r1, r1, %lo(");p_esn(p_gn,gi);pe(")\n");}
static void p_l2r(void){if(!p_lval)return;if((p_ety&255)==TY_STRUCT&&((p_ety>>8)&255)==0){p_lval=0;return;}if((p_ety&255)==TY_CHAR&&((p_ety>>8)&255)==0)pe("    ldb r1, r1, 0\n");else pe("    ldw r1, r1, 0\n");p_lval=0;}
static void p_sti(int t){if((t&255)==TY_CHAR&&((t>>8)&255)==0)pe("    stb r2, r1, 0\n");else pe("    stw r2, r1, 0\n");}

/* Type system */
static int p_tysz(int t){int b;int p;int si;b=t&255;p=(t>>8)&255;if(p>0)return 4;if(b==TY_CHAR)return 1;if(b==TY_STRUCT){si=(t>>16)&127;return p_ss[si];}return 4;}
static int p_isty(void){if(lex_tok==TK_INT||lex_tok==TK_CHAR||lex_tok==TK_VOID||lex_tok==TK_STRUCT)return 1;if(lex_tok==TK_UNSIGNED||lex_tok==TK_LONG||lex_tok==TK_ENUM||lex_tok==TK_CONST)return 1;if(lex_tok==TK_STATIC||lex_tok==TK_SIGNED||lex_tok==TK_SHORT)return 1;if(lex_tok==TK_VOLATILE||lex_tok==TK_REGISTER||lex_tok==TK_INLINE)return 1;if(lex_tok==TK_IDENT&&p_ftd(lex_str)>=0)return 1;return 0;}
static int p_psc(int t){int pt;int bt;int si;pt=(t>>8)&255;if(pt==0)return 0;bt=t&255;if(bt==TY_CHAR&&pt==1)return 1;if(bt==TY_STRUCT&&pt==1){si=(t>>16)&127;return p_ss[si];}return 4;}
static void p_scr1(int sc){if(sc==4)pe("    slli r1, r1, 2\n");else if(sc>1){pe("    addi r11, r0, ");pn(sc);pc(10);pe("    mul r1, r1, r11\n");}}

/* Forward declarations */
static int p_btype(void);
static int p_pstars(void);
static void p_expr(void);
static void p_assign(void);
static void p_stmt(void);

/* Struct parsing */
static void p_stbody(int si){int bt;int pt;int ft;int off;int ar;int fsz;off=0;p_sfb[si]=p_nfd;while(lex_tok!=TK_RBRACE&&lex_tok!=TK_EOF){bt=p_btype();pt=p_pstars();ft=bt|(pt<<8);if(lex_tok!=TK_IDENT){p_err("EF");return;}if(!((ft&255)==TY_CHAR&&((ft>>8)&255)==0))off=((off+3)/4)*4;if(p_nfd<P_MXFD){p_sset(p_fdn,p_nfd,lex_str);p_fdt[p_nfd]=ft;p_fdo[p_nfd]=off;p_fda[p_nfd]=0;p_nfd=p_nfd+1;}next();if(lex_tok==TK_LBRACK){next();ar=lex_val;p_expect(TK_NUM);p_expect(TK_RBRACK);if(p_nfd>0)p_fda[p_nfd-1]=1;fsz=4;if((ft&255)==TY_CHAR&&((ft>>8)&255)==0)fsz=1;if((ft&255)==TY_STRUCT&&((ft>>8)&255)==0)fsz=p_ss[(ft>>16)&127];off=off+ar*fsz;}else{if((ft&255)==TY_CHAR&&((ft>>8)&255)==0)off=off+1;else if((ft&255)==TY_STRUCT&&((ft>>8)&255)==0)off=off+p_ss[(ft>>16)&127];else off=off+4;}p_expect(TK_SEMI);}off=((off+3)/4)*4;p_sfc[si]=p_nfd-p_sfb[si];p_ss[si]=off;p_expect(TK_RBRACE);}

static int p_stref(void){char s[128];int si;int nm;next();nm=0;s[0]=0;if(lex_tok==TK_IDENT){p_scpy(s,lex_str,128);next();nm=1;}if(lex_tok==TK_LBRACE){if(nm){si=p_fst(s);if(si<0)si=p_ast(s);}else{si=p_ast("");}next();p_stbody(si);return TY_STRUCT|(si<<16);}if(!nm){p_err("ST");return TY_INT;}si=p_fst(s);if(si<0)si=p_ast(s);return TY_STRUCT|(si<<16);}

static int p_btype(void){int bt;int ti;while(lex_tok==TK_CONST||lex_tok==TK_VOLATILE||lex_tok==TK_REGISTER||lex_tok==TK_INLINE||lex_tok==TK_STATIC)next();if(lex_tok==TK_INT){bt=TY_INT;next();}else if(lex_tok==TK_CHAR){bt=TY_CHAR;next();}else if(lex_tok==TK_VOID){bt=TY_VOID;next();}else if(lex_tok==TK_STRUCT){bt=p_stref();}else if(lex_tok==TK_UNSIGNED){next();if(lex_tok==TK_INT||lex_tok==TK_LONG)next();else if(lex_tok==TK_CHAR){next();return TY_CHAR;}else if(lex_tok==TK_SHORT)next();bt=TY_INT;}else if(lex_tok==TK_SIGNED){next();if(lex_tok==TK_INT||lex_tok==TK_LONG)next();else if(lex_tok==TK_CHAR){next();return TY_CHAR;}else if(lex_tok==TK_SHORT)next();bt=TY_INT;}else if(lex_tok==TK_LONG){next();if(lex_tok==TK_INT)next();bt=TY_INT;}else if(lex_tok==TK_SHORT){next();if(lex_tok==TK_INT)next();bt=TY_INT;}else if(lex_tok==TK_ENUM){next();if(lex_tok==TK_IDENT)next();bt=TY_INT;}else if(lex_tok==TK_IDENT){ti=p_ftd(lex_str);if(ti>=0){bt=p_tdt[ti];next();}else bt=TY_INT;}else bt=TY_INT;return bt;}
static int p_pstars(void){int p;p=0;while(lex_tok==TK_STAR){p=p+1;next();}return p;}

/* Pointer arithmetic */
static int p_padd(int lt,int rt){int ls;int rs;ls=p_psc(lt);rs=p_psc(rt);if(ls>0&&rs==0){if(ls==4)pe("    slli r1, r1, 2\n");else if(ls>1){pe("    addi r11, r0, ");pn(ls);pc(10);pe("    mul r1, r1, r11\n");}pe("    add r1, r2, r1\n");return lt;}if(ls==0&&rs>0){if(rs==4)pe("    slli r2, r2, 2\n");else if(rs>1){pe("    addi r11, r0, ");pn(rs);pc(10);pe("    mul r2, r2, r11\n");}pe("    add r1, r2, r1\n");return rt;}pe("    add r1, r2, r1\n");return TY_INT;}
static int p_psub(int lt,int rt){int ls;int rs;ls=p_psc(lt);rs=p_psc(rt);if(ls>0&&rs==0){if(ls==4)pe("    slli r1, r1, 2\n");else if(ls>1){pe("    addi r11, r0, ");pn(ls);pc(10);pe("    mul r1, r1, r11\n");}pe("    sub r1, r2, r1\n");return lt;}if(ls>0&&rs>0){pe("    sub r1, r2, r1\n");if(ls==4)pe("    addi r2, r0, 2\n    sra r1, r1, r2\n");else if(ls>1){pe("    addi r2, r0, ");pn(ls);pc(10);pe("    div r1, r1, r2\n");}return TY_INT;}pe("    sub r1, r2, r1\n");return TY_INT;}

static void p_ebop(int t){if(t==TK_STAR)pe("    mul r1, r2, r1\n");else if(t==TK_SLASH)pe("    div r1, r2, r1\n");else if(t==TK_PERCENT)pe("    rem r1, r2, r1\n");else if(t==TK_PLUS)pe("    add r1, r2, r1\n");else if(t==TK_MINUS)pe("    sub r1, r2, r1\n");else if(t==TK_LSHIFT)pe("    sll r1, r2, r1\n");else if(t==TK_RSHIFT)pe("    sra r1, r2, r1\n");else if(t==TK_LT)pe("    slt r1, r2, r1\n");else if(t==TK_GT)pe("    sgt r1, r2, r1\n");else if(t==TK_LE)pe("    sle r1, r2, r1\n");else if(t==TK_GE)pe("    sge r1, r2, r1\n");else if(t==TK_EQ)pe("    seq r1, r2, r1\n");else if(t==TK_NE)pe("    sne r1, r2, r1\n");else if(t==TK_AMP)pe("    and r1, r2, r1\n");else if(t==TK_CARET)pe("    xor r1, r2, r1\n");else if(t==TK_PIPE)pe("    or r1, r2, r1\n");}

/* Expression parser */
static void p_primary(void) {
    char sn[128];
    int na;
    int k;
    int li;
    int gi;
    int fi;
    int bt;
    int pt;
    int sid;
    if (lex_tok == TK_NUM) {
        p_li(lex_val); p_lval = 0; p_ety = TY_INT; next(); return;
    }
    if (lex_tok == TK_CHARLIT) {
        p_li(lex_val); p_lval = 0; p_ety = TY_INT; next(); return;
    }
    if (lex_tok == TK_STRING) {
        sid = lex_val;
        pe("    lui r1, %hi(.Lstr_"); pn(sid);
        pe(")\n    addi r1, r1, %lo(.Lstr_"); pn(sid); pe(")\n");
        p_lval = 0; p_ety = TY_CHAR | (1 << 8); next(); return;
    }
    if (lex_tok == TK_LPAREN) {
        next(); p_expr(); p_expect(TK_RPAREN); return;
    }
    if (lex_tok == TK_IDENT) {
        p_scpy(sn, lex_str, 128); next();
        /* Check locals/globals first (enables fn ptr calls via postfix) */
        li = p_floc(sn);
        if (li >= 0) {
            pe("    addi r1, r30, "); pn(p_lo[li]); pc(10);
            if (p_ls[li] > 0) { bt = p_lt[li] & 255; pt = (p_lt[li] >> 8) & 255; p_lval = 0; p_ety = bt | ((pt + 1) << 8); }
            else { p_lval = 1; p_ety = p_lt[li]; }
            return;
        }
        gi = p_fgl(sn);
        if (gi >= 0) {
            p_ega(gi);
            if (p_gsz[gi] > 0) { bt = p_gty[gi] & 255; pt = (p_gty[gi] >> 8) & 255; p_lval = 0; p_ety = bt | ((pt + 1) << 8); }
            else { p_lval = 1; p_ety = p_gty[gi]; }
            return;
        }
        /* Not a variable: direct function call or function address */
        if (lex_tok == TK_LPAREN) {
            next(); na = 0;
            if (lex_tok != TK_RPAREN) {
                p_assign(); p_l2r(); p_push(); na = 1;
                while (lex_tok == TK_COMMA) { next(); p_assign(); p_l2r(); p_push(); na = na + 1; }
            }
            p_expect(TK_RPAREN);
            k = na;
            while (k > 0) { k = k - 1; pe("    ldw r"); pn(3 + k); pe(", r29, 0\n    addi r29, r29, 4\n"); }
            p_call(sn); p_lval = 0;
            fi = p_ffn(sn);
            if (fi >= 0) p_ety = p_fr[fi]; else p_ety = TY_INT;
            return;
        }
        /* Function address (for fn ptrs) */
        pe("    lui r1, %hi("); pe(sn); pe(")\n    addi r1, r1, %lo("); pe(sn); pe(")\n");
        p_lval = 0; p_ety = TY_INT; return;
    }
    p_err("EXP");
}

static void p_postfix(void) {
    int bty;
    int ebt;
    int ept;
    int esz;
    int si;
    int fi;
    int fo;
    int ft;
    int sc;
    int ty;
    int na;
    int k;
    p_primary();
    while (1) {
        if (lex_tok == TK_LBRACK) {
            bty = p_ety; p_l2r(); p_push(); next();
            p_expr(); p_l2r();
            ept = (bty >> 8) & 255; ebt = bty & 255;
            if (ept > 0) ept = ept - 1;
            esz = 4;
            if (ebt == TY_CHAR && ept == 0) esz = 1;
            if (ebt == TY_STRUCT && ept == 0) { si = (bty >> 16) & 127; esz = p_ss[si]; }
            if (esz == 4) pe("    slli r1, r1, 2\n");
            else if (esz != 1) { pe("    addi r11, r0, "); pn(esz); pc(10); pe("    mul r1, r1, r11\n"); }
            p_pop(); pe("    add r1, r2, r1\n");
            p_lval = 1; p_ety = ebt | (ept << 8);
            if (ebt == TY_STRUCT) p_ety = p_ety | (((bty >> 16) & 127) << 16);
            p_expect(TK_RBRACK);
        } else if (lex_tok == TK_DOT) {
            next();
            if (lex_tok != TK_IDENT) { p_err("EF"); return; }
            ebt = p_ety & 255;
            if (ebt != TY_STRUCT) { p_err("DOT"); return; }
            si = (p_ety >> 16) & 127;
            fi = p_ffd(si, lex_str);
            if (fi < 0) { p_err("UF"); next(); return; }
            fo = p_fdo[fi]; ft = p_fdt[fi];
            if (fo != 0) { pe("    addi r1, r1, "); pn(fo); pc(10); }
            if (p_fda[fi]) { p_ety = (ft & 255) | (((ft >> 8 & 255) + 1) << 8) | (ft & 0x7F0000); p_lval = 0; }
            else { p_ety = ft; p_lval = 1; }
            next();
        } else if (lex_tok == TK_ARROW) {
            p_l2r(); next();
            if (lex_tok != TK_IDENT) { p_err("EF"); return; }
            ept = (p_ety >> 8) & 255; ebt = p_ety & 255;
            si = (p_ety >> 16) & 127;
            fi = p_ffd(si, lex_str);
            if (fi < 0) { p_err("UF"); next(); return; }
            fo = p_fdo[fi]; ft = p_fdt[fi];
            if (fo != 0) { pe("    addi r1, r1, "); pn(fo); pc(10); }
            if (p_fda[fi]) { p_ety = (ft & 255) | (((ft >> 8 & 255) + 1) << 8) | (ft & 0x7F0000); p_lval = 0; }
            else { p_ety = ft; p_lval = 1; }
            next();
        } else if (lex_tok == TK_INC) {
            sc = p_psc(p_ety); if (sc == 0) sc = 1;
            if (p_lval) {
                ty = p_ety;
                pe("    addi r11, r1, 0\n"); p_l2r();
                pe("    addi r2, r1, "); pn(sc); pc(10);
                if ((ty & 255) == TY_CHAR && ((ty >> 8) & 255) == 0) pe("    stb r11, r2, 0\n");
                else pe("    stw r11, r2, 0\n");
            }
            p_lval = 0; next();
        } else if (lex_tok == TK_DEC) {
            sc = p_psc(p_ety); if (sc == 0) sc = 1;
            if (p_lval) {
                ty = p_ety;
                pe("    addi r11, r1, 0\n"); p_l2r();
                pe("    addi r2, r1, -"); pn(sc); pc(10);
                if ((ty & 255) == TY_CHAR && ((ty >> 8) & 255) == 0) pe("    stb r11, r2, 0\n");
                else pe("    stw r11, r2, 0\n");
            }
            p_lval = 0; next();
        } else if (lex_tok == TK_LPAREN) {
            p_l2r();
            pe("    addi r11, r1, 0\n"); p_push(); next();
            na = 0;
            if (lex_tok != TK_RPAREN) {
                p_assign(); p_l2r(); p_push(); na = 1;
                while (lex_tok == TK_COMMA) { next(); p_assign(); p_l2r(); p_push(); na = na + 1; }
            }
            p_expect(TK_RPAREN);
            k = na;
            while (k > 0) { k = k - 1; pe("    ldw r"); pn(3 + k); pe(", r29, 0\n    addi r29, r29, 4\n"); }
            pe("    ldw r11, r29, 0\n    addi r29, r29, 4\n");
            pe("    jalr r31, r11, 0\n");
            p_lval = 0; p_ety = TY_INT;
        } else {
            break;
        }
    }
}

static void p_unary(void) {
    int ty;
    int bt;
    int pt;
    int sc;
    int tp;
    if (lex_tok == TK_SIZEOF) {
        next();
        if (lex_tok == TK_LPAREN) {
            next();
            if (p_isty()) { bt = p_btype(); pt = p_pstars(); tp = bt | (pt << 8); p_expect(TK_RPAREN); p_li(p_tysz(tp)); }
            else { p_expr(); tp = p_ety; p_expect(TK_RPAREN); p_li(p_tysz(tp)); }
        } else { p_unary(); tp = p_ety; p_li(p_tysz(tp)); }
        p_lval = 0; p_ety = TY_INT; return;
    }
    if (lex_tok == TK_BANG) { next(); p_unary(); p_l2r(); pe("    seq r1, r1, r0\n"); p_lval = 0; p_ety = TY_INT; return; }
    if (lex_tok == TK_MINUS) { next(); p_unary(); p_l2r(); pe("    sub r1, r0, r1\n"); p_lval = 0; p_ety = TY_INT; return; }
    if (lex_tok == TK_TILDE) { next(); p_unary(); p_l2r(); pe("    addi r2, r0, -1\n    xor r1, r1, r2\n"); p_lval = 0; p_ety = TY_INT; return; }
    if (lex_tok == TK_STAR) {
        next(); p_unary(); p_l2r();
        pt = (p_ety >> 8) & 255; bt = p_ety & 255;
        if (pt > 0) p_ety = bt | ((pt - 1) << 8);
        if (bt == TY_STRUCT) p_ety = p_ety | (((p_ety >> 16) & 127) << 16);
        p_lval = 1; return;
    }
    if (lex_tok == TK_AMP) {
        next(); p_unary();
        if (!p_lval) { p_err("AMP"); return; }
        p_lval = 0; bt = p_ety & 255; pt = (p_ety >> 8) & 255;
        p_ety = bt | ((pt + 1) << 8);
        if (bt == TY_STRUCT) p_ety = p_ety | (((p_ety >> 16) & 127) << 16);
        return;
    }
    if (lex_tok == TK_INC) {
        next(); p_unary(); sc = p_psc(p_ety); if (sc == 0) sc = 1;
        if (p_lval) { ty = p_ety; pe("    addi r2, r1, 0\n"); p_l2r(); pe("    addi r1, r1, "); pn(sc); pc(10); p_sti(ty); p_lval = 0; }
        else { pe("    addi r1, r1, "); pn(sc); pc(10); }
        return;
    }
    if (lex_tok == TK_DEC) {
        next(); p_unary(); sc = p_psc(p_ety); if (sc == 0) sc = 1;
        if (p_lval) { ty = p_ety; pe("    addi r2, r1, 0\n"); p_l2r(); pe("    addi r1, r1, -"); pn(sc); pc(10); p_sti(ty); p_lval = 0; }
        else { pe("    addi r1, r1, -"); pn(sc); pc(10); }
        return;
    }
    if (lex_tok == TK_LPAREN) {
        p_lsave(); next();
        if (p_isty()) { bt = p_btype(); pt = p_pstars(); tp = bt | (pt << 8); p_expect(TK_RPAREN); p_unary(); p_l2r(); if ((tp & 255) == TY_CHAR && ((tp >> 8) & 255) == 0) pe("    addi r2, r0, 255\n    and r1, r1, r2\n"); p_ety = tp; p_lval = 0; return; }
        p_lrest();
    }
    p_postfix();
}

static int p_bprec(int t){if(t==TK_STAR||t==TK_SLASH||t==TK_PERCENT)return 10;if(t==TK_PLUS||t==TK_MINUS)return 9;if(t==TK_LSHIFT||t==TK_RSHIFT)return 8;if(t==TK_LT||t==TK_GT||t==TK_LE||t==TK_GE)return 7;if(t==TK_EQ||t==TK_NE)return 6;if(t==TK_AMP)return 5;if(t==TK_CARET)return 4;if(t==TK_PIPE)return 3;if(t==TK_LAND)return 2;if(t==TK_LOR)return 1;return 0;}

static void p_binop(int mp) {
    int op;
    int p;
    int lt;
    int rt;
    int sl;
    p_unary();
    while (1) {
        p = p_bprec(lex_tok);
        if (p < mp) break;
        op = lex_tok; lt = p_ety; p_l2r(); next();
        if (op == TK_LAND || op == TK_LOR) {
            pe("    sne r1, r1, r0\n");
            sl = p_nl2();
            if (op == TK_LAND) p_bz(sl); else p_bnz(sl);
            p_binop(p + 1); p_l2r();
            pe("    sne r1, r1, r0\n");
            p_ld(sl); p_lval = 0; p_ety = TY_INT; continue;
        }
        p_push(); p_binop(p + 1); p_l2r(); rt = p_ety; p_pop();
        if (op == TK_PLUS) p_ety = p_padd(lt, rt);
        else if (op == TK_MINUS) p_ety = p_psub(lt, rt);
        else { p_ebop(op); p_ety = TY_INT; }
        p_lval = 0;
    }
}

static void p_ternary(void) {
    int le;
    int lend;
    p_binop(1);
    if (lex_tok == TK_QMARK) {
        p_l2r(); next(); le = p_nl2(); lend = p_nl2();
        p_bz(le); p_expr(); p_l2r(); p_expect(TK_COLON);
        p_jmp(lend); p_ld(le); p_ternary(); p_l2r(); p_ld(lend);
    }
}

static void p_assign(void) {
    int sty;
    int sc;
    int op;
    p_ternary();
    if (lex_tok == TK_ASSIGN) {
        if (!p_lval) p_err("LV");
        sty = p_ety; p_push(); next(); p_assign(); p_l2r(); p_pop(); p_sti(sty); p_lval = 0; return;
    }
    if (lex_tok == TK_PLUSEQ || lex_tok == TK_MINUSEQ) {
        op = lex_tok;
        if (!p_lval) p_err("LV");
        sty = p_ety; sc = p_psc(sty);
        pe("    addi r2, r1, 0\n"); p_push(); p_l2r(); p_push();
        next(); p_assign(); p_l2r();
        if (sc > 0) p_scr1(sc);
        p_pop();
        if (op == TK_PLUSEQ) pe("    add r1, r2, r1\n"); else pe("    sub r1, r2, r1\n");
        p_pop(); p_sti(sty); p_lval = 0; return;
    }
    if (lex_tok == TK_STAREQ || lex_tok == TK_SLASHEQ || lex_tok == TK_PERCENTEQ ||
        lex_tok == TK_AMPEQ || lex_tok == TK_PIPEEQ || lex_tok == TK_CARETEQ ||
        lex_tok == TK_LSHIFTEQ || lex_tok == TK_RSHIFTEQ) {
        op = lex_tok;
        if (!p_lval) p_err("LV");
        sty = p_ety;
        pe("    addi r2, r1, 0\n"); p_push(); p_l2r(); p_push();
        next(); p_assign(); p_l2r(); p_pop();
        if (op == TK_STAREQ) pe("    mul r1, r2, r1\n");
        else if (op == TK_SLASHEQ) pe("    div r1, r2, r1\n");
        else if (op == TK_PERCENTEQ) pe("    rem r1, r2, r1\n");
        else if (op == TK_AMPEQ) pe("    and r1, r2, r1\n");
        else if (op == TK_PIPEEQ) pe("    or r1, r2, r1\n");
        else if (op == TK_CARETEQ) pe("    xor r1, r2, r1\n");
        else if (op == TK_LSHIFTEQ) pe("    sll r1, r2, r1\n");
        else if (op == TK_RSHIFTEQ) pe("    sra r1, r2, r1\n");
        p_pop(); p_sti(sty); p_lval = 0; return;
    }
}

static void p_expr(void) { p_assign(); while (lex_tok == TK_COMMA) { p_l2r(); next(); p_assign(); } }

/* Statement parser */
static void p_compound(void);

static void p_ldecl(void) {
    int bt;
    int pt;
    int ty;
    int ar;
    int off;
    char nb[128];
    bt = p_btype(); pt = p_pstars(); ty = bt | (pt << 8);
    while (1) {
        if (lex_tok == TK_LPAREN) {
            next();
            if (lex_tok == TK_STAR) next();
            if (lex_tok != TK_IDENT) { p_err("VN"); return; }
            p_scpy(nb, lex_str, 128); next();
            p_expect(TK_RPAREN);
            if (lex_tok == TK_LPAREN) { next(); while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next(); p_expect(TK_RPAREN); }
            p_aloc(nb, TY_INT, 0);
            off = p_lo[p_nl - 1];
            if (lex_tok == TK_ASSIGN) { next(); p_expr(); p_l2r(); pe("    stw r30, r1, "); pn(off); pc(10); }
            break;
        }
        if (lex_tok != TK_IDENT) { p_err("VN"); return; }
        p_scpy(nb, lex_str, 128); next();
        if (lex_tok == TK_LBRACK) {
            next(); ar = lex_val; p_expect(TK_NUM); p_expect(TK_RBRACK);
            p_aloc(nb, ty, ar);
        } else {
            p_aloc(nb, ty, 0);
            off = p_lo[p_nl - 1];
            if (lex_tok == TK_ASSIGN) { next(); p_expr(); p_l2r(); pe("    stw r30, r1, "); pn(off); pc(10); }
        }
        if (lex_tok == TK_COMMA) { next(); pt = p_pstars(); ty = bt | (pt << 8); continue; }
        break;
    }
    p_expect(TK_SEMI);
}

static void p_stmt(void) {
    int le;
    int lend;
    int ll;
    int lb;
    int lc;
    int sfl;
    int sef;
    int fi;
    int off;
    int bt;
    int pt;
    int ty;
    char nb[128];
    int snl;
    int swoff;
    int osnc;
    int osdef;
    int osend;
    int osdisp;
    int osoff;
    int oisw;
    int i;

    if (lex_tok == TK_LBRACE) { next(); snl = p_nl; p_compound(); p_nl = snl; return; }
    if (p_isty()) { p_ldecl(); return; }

    if (lex_tok == TK_IF) {
        next(); p_expect(TK_LPAREN); p_expr(); p_l2r(); p_expect(TK_RPAREN);
        le = p_nl2(); lend = p_nl2();
        p_bz(le); p_stmt();
        if (lex_tok == TK_ELSE) { next(); p_jmp(lend); p_ld(le); p_stmt(); p_ld(lend); }
        else p_ld(le);
        return;
    }
    if (lex_tok == TK_WHILE) {
        next(); ll = p_nl2(); lb = p_nl2();
        p_ld(ll);
        p_expect(TK_LPAREN); p_expr(); p_l2r(); p_expect(TK_RPAREN);
        p_bz(lb);
        if (p_ldep < P_MXLP) { p_brk[p_ldep] = lb; p_ctn[p_ldep] = ll; p_ldep = p_ldep + 1; }
        p_stmt();
        p_ldep = p_ldep - 1;
        p_jmp(ll); p_ld(lb); return;
    }
    if (lex_tok == TK_DO) {
        next(); ll = p_nl2(); lb = p_nl2(); lc = p_nl2();
        p_ld(ll);
        if (p_ldep < P_MXLP) { p_brk[p_ldep] = lb; p_ctn[p_ldep] = lc; p_ldep = p_ldep + 1; }
        p_stmt(); p_ldep = p_ldep - 1;
        p_ld(lc);
        p_expect(TK_WHILE); p_expect(TK_LPAREN); p_expr(); p_l2r(); p_expect(TK_RPAREN);
        p_bnz(ll); p_ld(lb); p_expect(TK_SEMI); return;
    }
    if (lex_tok == TK_FOR) {
        next(); p_expect(TK_LPAREN);
        snl = p_nl;
        if (p_isty()) {
            bt = p_btype(); pt = p_pstars(); ty = bt | (pt << 8);
            if (lex_tok == TK_IDENT) {
                p_scpy(nb, lex_str, 128); p_aloc(nb, ty, 0);
                off = p_lo[p_nl - 1]; next();
                if (lex_tok == TK_ASSIGN) { next(); p_expr(); p_l2r(); pe("    stw r30, r1, "); pn(off); pc(10); }
            }
            p_expect(TK_SEMI);
        } else if (lex_tok != TK_SEMI) { p_expr(); p_l2r(); p_expect(TK_SEMI); }
        else next();
        ll = p_nl2(); lb = p_nl2(); lc = p_nl2();
        p_ld(ll);
        if (lex_tok != TK_SEMI) { p_expr(); p_l2r(); p_bz(lb); }
        p_expect(TK_SEMI);
        sfl = p_flen; sef = p_femit; p_femit = 1;
        if (lex_tok != TK_RPAREN) { p_expr(); p_l2r(); }
        p_femit = sef; p_expect(TK_RPAREN);
        if (p_ldep < P_MXLP) { p_brk[p_ldep] = lb; p_ctn[p_ldep] = lc; p_ldep = p_ldep + 1; }
        p_stmt(); p_ldep = p_ldep - 1;
        p_ld(lc);
        fi = sfl; while (fi < p_flen) { pc(p_fbuf[fi]); fi = fi + 1; }
        p_flen = sfl;
        p_jmp(ll); p_ld(lb); p_nl = snl; return;
    }
    if (lex_tok == TK_SWITCH) {
        next(); p_expect(TK_LPAREN); p_expr(); p_l2r(); p_expect(TK_RPAREN);
        p_loff = p_loff - 4; swoff = p_loff;
        pe("    stw r30, r1, "); pn(swoff); pc(10);
        osnc = p_swn; osdef = p_swdef; osend = p_swend; osdisp = p_swdisp; osoff = p_swoff; oisw = p_insw;
        p_swdef = -1; p_swend = p_nl2(); p_swdisp = p_nl2(); p_swoff = swoff; p_insw = 1;
        p_jmp(p_swdisp);
        if (p_ldep < P_MXLP) { p_brk[p_ldep] = p_swend; p_ctn[p_ldep] = -1; p_ldep = p_ldep + 1; }
        p_expect(TK_LBRACE);
        while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
            if (lex_tok == TK_CASE) {
                next();
                if (p_swn < P_MXCASE) {
                    if (lex_tok == TK_MINUS) { next(); p_swv[p_swn] = 0 - lex_val; } else { p_swv[p_swn] = lex_val; }
                    next();
                    p_swl[p_swn] = p_nl2(); p_ld(p_swl[p_swn]); p_swn = p_swn + 1;
                }
                p_expect(TK_COLON);
            } else if (lex_tok == TK_DEFAULT) {
                next(); p_expect(TK_COLON); p_swdef = p_nl2(); p_ld(p_swdef);
            } else { p_stmt(); }
        }
        p_expect(TK_RBRACE);
        p_jmp(p_swend); p_ldep = p_ldep - 1;
        p_ld(p_swdisp);
        pe("    ldw r1, r30, "); pn(p_swoff); pc(10);
        i = osnc;
        while (i < p_swn) {
            pe("    addi r2, r0, "); pn(p_swv[i]); pc(10);
            pe("    beq r1, r2, "); p_lr(p_swl[i]); pc(10);
            i = i + 1;
        }
        if (p_swdef >= 0) p_jmp(p_swdef); else p_jmp(p_swend);
        p_ld(p_swend);
        p_swn = osnc; p_swdef = osdef; p_swend = osend; p_swdisp = osdisp; p_swoff = osoff; p_insw = oisw;
        return;
    }
    if (lex_tok == TK_RETURN) {
        next(); if (lex_tok != TK_SEMI) { p_expr(); p_l2r(); }
        p_jmp(p_retl); p_expect(TK_SEMI); return;
    }
    if (lex_tok == TK_BREAK) {
        next();
        if (p_ldep > 0) p_jmp(p_brk[p_ldep - 1]); else p_err("BRK");
        p_expect(TK_SEMI); return;
    }
    if (lex_tok == TK_CONTINUE) {
        next();
        if (p_ldep > 0 && p_ctn[p_ldep - 1] >= 0) p_jmp(p_ctn[p_ldep - 1]); else p_err("CTN");
        p_expect(TK_SEMI); return;
    }
    if (lex_tok == TK_GOTO) {
        next();
        if (lex_tok == TK_IDENT) { i = p_albl(lex_str); next(); p_jmp(p_lbid[i]); }
        else p_err("GOTO");
        p_expect(TK_SEMI); return;
    }
    if (lex_tok == TK_IDENT) {
        p_lsave(); next();
        if (lex_tok == TK_COLON) {
            next(); i = p_albl(p_sstr); p_lbdef[i] = 1; p_ld(p_lbid[i]);
            p_stmt(); return;
        }
        p_lrest();
    }
    if (lex_tok == TK_SEMI) { next(); return; }
    p_expr(); p_l2r(); p_expect(TK_SEMI);
}

static void p_compound(void) { while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) p_stmt(); p_expect(TK_RBRACE); }

/* Top-level */
static void p_penum(void) {
    char nm[128]; int v;
    next(); if (lex_tok == TK_IDENT) next(); p_expect(TK_LBRACE);
    v = 0;
    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
        if (lex_tok != TK_IDENT) { p_err("EN"); return; }
        p_scpy(nm, lex_str, 128); next();
        if (lex_tok == TK_ASSIGN) { next(); if (lex_tok == TK_MINUS) { next(); v = 0 - lex_val; next(); } else { v = lex_val; next(); } }
        p_adf(nm, v); v = v + 1;
        if (lex_tok == TK_COMMA) next();
    }
    p_expect(TK_RBRACE); p_expect(TK_SEMI);
}

static void p_ptypedef(void) {
    int bt; int pt; int ty;
    next(); bt = p_btype(); pt = p_pstars(); ty = bt | (pt << 8);
    if (lex_tok == TK_LPAREN) {
        next();
        if (lex_tok == TK_STAR) { next(); if (lex_tok == TK_IDENT) { p_atd(lex_str, TY_INT); next(); } p_expect(TK_RPAREN); if (lex_tok == TK_LPAREN) { next(); while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next(); p_expect(TK_RPAREN); } p_expect(TK_SEMI); return; }
        while (lex_tok != TK_SEMI && lex_tok != TK_EOF) next(); p_expect(TK_SEMI); return;
    }
    if (lex_tok == TK_IDENT) { p_atd(lex_str, ty); next(); }
    p_expect(TK_SEMI);
}

static void p_pfunc(char *fn, int rt) {
    int np; int i; int bt; int pt; int pty; int fs;
    p_expect(TK_LPAREN);
    p_nl = 0; p_loff = -12; p_ldep = 0; p_nlb = 0; p_retl = p_nl2(); np = 0;
    if (lex_tok != TK_RPAREN) {
        if (lex_tok == TK_VOID) { next(); }
        else {
            while (1) {
                bt = p_btype(); pt = p_pstars(); pty = bt | (pt << 8);
                if (lex_tok != TK_IDENT) {
                    if (lex_tok == TK_COMMA || lex_tok == TK_RPAREN) { np = np + 1; if (lex_tok == TK_COMMA) { next(); continue; } break; }
                    p_err("PN"); break;
                }
                p_apar(lex_str, np, pty); np = np + 1; next();
                if (lex_tok == TK_LBRACK) { next(); if (lex_tok != TK_RBRACK) next(); p_expect(TK_RBRACK); }
                if (lex_tok != TK_COMMA) break; next();
            }
        }
    }
    if (lex_tok == TK_COMMA) { next(); if (lex_tok == TK_ELLIPSIS) next(); }
    p_expect(TK_RPAREN);
    p_afn(fn, np, rt);
    if (lex_tok == TK_SEMI) { next(); return; }
    pe(fn); pe(":\n    .global "); pe(fn); pc(10);
    p_loff = -12 - np * 4;
    p_prolog_ph();
    i = 0;
    while (i < np && i < 8) { pe("    stw r30, r"); pn(3 + i); pe(", "); pn(-12 - i * 4); pc(10); i = i + 1; }
    p_expect(TK_LBRACE); p_compound();
    fs = 0 - p_loff; fs = ((fs + 15) / 16) * 16; if (fs < 32) fs = 32;
    p_prolog_fin(fs); p_epilog(p_retl, fs); pc(10);
}

static void p_pgdecl(char *nm, int ty) {
    int ar; int esz; int bsz; int si; int iv; int neg; int gi;
    if (lex_tok == TK_LBRACK) {
        next(); ar = lex_val; p_expect(TK_NUM); p_expect(TK_RBRACK);
        esz = 4;
        if ((ty & 255) == TY_CHAR && ((ty >> 8) & 255) == 0) esz = 1;
        if ((ty & 255) == TY_STRUCT && ((ty >> 8) & 255) == 0) { si = (ty >> 16) & 127; esz = p_ss[si]; }
        bsz = ar * esz; bsz = ((bsz + 3) / 4) * 4; p_agl(nm, ty, ar, bsz);
    } else {
        bsz = 4;
        if ((ty & 255) == TY_STRUCT && ((ty >> 8) & 255) == 0) { si = (ty >> 16) & 127; bsz = p_ss[si]; bsz = ((bsz + 3) / 4) * 4; }
        p_agl(nm, ty, 0, bsz);
        if (lex_tok == TK_ASSIGN) {
            next(); neg = 0; iv = 0;
            if (lex_tok == TK_MINUS) { neg = 1; next(); }
            if (lex_tok == TK_NUM) { iv = lex_val; next(); }
            if (neg) iv = 0 - iv;
            gi = p_ng - 1; p_giv[gi] = iv;
        }
    }
    while (lex_tok == TK_COMMA) {
        next(); if (lex_tok == TK_STAR) { while (lex_tok == TK_STAR) next(); }
        if (lex_tok != TK_IDENT) break;
        p_scpy(nm, lex_str, 128); next();
        if (lex_tok == TK_LBRACK) {
            next(); ar = lex_val; p_expect(TK_NUM); p_expect(TK_RBRACK);
            esz = 4; if ((ty & 255) == TY_CHAR && ((ty >> 8) & 255) == 0) esz = 1;
            bsz = ar * esz; bsz = ((bsz + 3) / 4) * 4; p_agl(nm, ty, ar, bsz);
        } else { p_agl(nm, ty, 0, 4); }
    }
    p_expect(TK_SEMI);
}

static void p_estr(void) {
    int i; int j; int off; int len; int bv;
    if (lex_str_count == 0) return;
    pe(".data\n");
    i = 0;
    while (i < lex_str_count) {
        pe(".align 4\n.Lstr_"); pn(i); pe(":\n");
        off = lex_str_off[i]; len = lex_str_len[i]; j = 0;
        while (j < len) { pe(".byte "); bv = lex_strpool[off + j]; if (bv < 0) bv = bv + 256; pn(bv); pc(10); j = j + 1; }
        pe(".byte 0\n");
        i = i + 1;
    }
}

static void p_egbss(void) {
    int i; int hd; int hb;
    if (p_ng == 0) return;
    hd = 0; hb = 0;
    i = 0;
    while (i < p_ng) {
        if (p_giv[i] != 0) {
            if (!hd) { pe(".data\n"); hd = 1; }
            pe(".align 4\n"); p_esn(p_gn, i); pe(":\n    .global "); p_esn(p_gn, i); pe("\n    .word "); pn(p_giv[i]); pc(10);
        }
        i = i + 1;
    }
    i = 0;
    while (i < p_ng) {
        if (p_giv[i] == 0) {
            if (!hb) { pe(".bss\n"); hb = 1; }
            pe(".align 4\n"); p_esn(p_gn, i); pe(":\n    .global "); p_esn(p_gn, i); pe("\n    .space "); pn(p_gby[i]); pc(10);
        }
        i = i + 1;
    }
}

static void p_program(void) {
    int bt; int pt; int rt; char fn[128]; int cv; int cn;
    pe("# Generated by s32-cc\n.text\n");
    while (lex_tok != TK_EOF) {
        if (lex_tok == TK_TYPEDEF) { p_ptypedef(); continue; }
        if (lex_tok == TK_ENUM) {
            p_lsave(); next();
            if (lex_tok == TK_LBRACE) { p_lrest(); p_penum(); continue; }
            if (lex_tok == TK_IDENT) { next(); if (lex_tok == TK_LBRACE) { p_lrest(); p_penum(); continue; } }
            p_lrest();
        }
        if (lex_tok == TK_STATIC) {
            p_lsave(); next();
            if (lex_tok == TK_CONST) {
                next();
                if (lex_tok == TK_INT || lex_tok == TK_UNSIGNED || lex_tok == TK_LONG || lex_tok == TK_CHAR) {
                    next();
                    if (lex_tok == TK_IDENT) {
                        p_scpy(fn, lex_str, 128); next();
                        if (lex_tok == TK_ASSIGN) {
                            next(); cn = 0; cv = 0;
                            if (lex_tok == TK_MINUS) { cn = 1; next(); }
                            if (lex_tok == TK_NUM) { cv = lex_val; next(); }
                            if (cn) cv = 0 - cv;
                            p_adf(fn, cv);
                            p_expect(TK_SEMI); continue;
                        }
                    }
                }
            }
            p_lrest();
            next();
        }
        if (lex_tok == TK_EXTERN) next();
        bt = p_btype(); pt = p_pstars(); rt = bt | (pt << 8);
        if (lex_tok == TK_SEMI) { next(); continue; }
        if (lex_tok != TK_IDENT) { p_err("NM"); break; }
        p_scpy(fn, lex_str, 128); next();
        if (lex_tok == TK_LPAREN) p_pfunc(fn, rt);
        else p_pgdecl(fn, rt);
    }
    p_estr(); p_egbss();
}

/* Entry point */
int s32cc_compile(char *path) {
    int f; int ch; int i; int last;
    last = -1; i = 0; while (path[i] != 0) { if (path[i] == 47) last = i; i = i + 1; }
    if (last >= 0 && last < 126) { i = 0; while (i <= last) { p_sdir[i] = path[i]; i = i + 1; } p_sdir[i] = 0; }
    else { p_sdir[0] = 0; }
    f = fopen(path, "rb");
    if (!f) { p_err("IO"); return 1; }
    lex_len = 0;
    while (1) { ch = fgetc(f); if (ch < 0) break; if (lex_len >= LEX_SRC_SZ - 1) { fclose(f); p_err("BIG"); return 1; } lex_src[lex_len] = ch; lex_len = lex_len + 1; }
    fclose(f); lex_src[lex_len] = 0; lex_pos = 0; lex_line = 1; lex_col = 1;
    p_olen = 0; p_lbl = 0; p_nf = 0; p_nl = 0; p_ng = 0;
    p_ldep = 0; p_femit = 0; p_flen = 0; p_lval = 0; p_ety = 0;
    p_ns = 0; p_nfd = 0; p_ntd = 0; p_nd = 0; p_insw = 0; p_swn = 0;
    p_nlb = 0; pp_skip = 0; pp_dep = 0;
    lex_strpool_len = 0; lex_str_count = 0;
    next(); p_program(); p_out[p_olen] = 0;
    return 0;
}
