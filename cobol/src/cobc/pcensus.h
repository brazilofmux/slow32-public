/* s32-cobc: the census of PERFORM -- how paragraphs are entered and left.
 * A part of one translation unit, included by s32-cobc.c in order; not a
 * header to include anywhere else. */

/* ====================================================================== */
/* The PERFORM census (S32_CENSUS_DIR; docs/plans/census.md, step 3)       */
/* ====================================================================== */

/* A paragraph is a label.  Whether it may be a procedure -- entered at
 * its top by PERFORM alone, left only at the end of its range -- or a
 * block with a known set of returns, depends on everything that names
 * it: the PERFORMs and GO TOs of the whole unit, and whether the
 * paragraph before it falls into it.  The compiler writes the facts
 * here, one record a line in <dir>/<source>.<hash>.perform, and
 * tests/performs.py draws the verdicts:
 *
 *   U unit nparagraphs
 *   P id name kind(para|section) section-id declarative line ends(fall|jump|stop)
 *   F from-id to-id thru-id kind(once|until|varying|times|exit)   an out-of-line PERFORM
 *   G from-id target-id kind(goto|depending|alter|sql)             a GO TO
 *   D section-id                                                   a declarative USE section
 *
 * from-id is the paragraph the statement is in (-1 outside any). */

static FILE *g_pc_out;
static int g_inline_depth;                 /* loopreg.h's: in-line PERFORM bodies being read */

/* The same facts kept in memory for this unit, whatever S32_CENSUS_DIR
 * says: the lowering asks whether a performed range comes back
 * (lower.h, lw_range_returns).  ids are paragraph ids (1-based). */
static struct { int from, lo, thru, iter; } *g_pcf; static int g_npcf, g_pcf_cap;   /* out-of-line PERFORMs; iter: UNTIL/VARYING/TIMES, or issued inside an in-line loop */
static struct { int from, target; } *g_pcg; static int g_npcg, g_pcg_cap;        /* GO TOs, of every kind */
static int *g_pcl; static int g_npcl, g_pcl_cap;                                 /* paragraphs with a GOBACK or EXIT PROGRAM in them */
static char *g_pce; static int g_pce_cap;                                        /* how each paragraph's code ends: 'f'all, 'j'ump, 's'top */
#define PC_GROW(arr, n, cap) do { if ((n) == (cap)) { (cap) = (cap) ? 2 * (cap) : 64; (arr) = xrealloc((arr), (size_t)(cap) * sizeof *(arr)); } } while (0)
static void pc_rec_leave(void)
{
    int id = g_cur_para ? g_cur_para->id : -1;
    if (id < 0) return;
    PC_GROW(g_pcl, g_npcl, g_pcl_cap); g_pcl[g_npcl++] = id;
}

static void pc_open(void)
{
    if (g_pc_out || !g_cen_dir) return;
    char path[1024]; unsigned h = 2166136261u;
    for (const char *p = g_file; *p; p++) h = (h ^ (unsigned char)*p) * 16777619u;
    const char *base = strrchr(g_file, '/'); base = base ? base + 1 : g_file;
    snprintf(path, sizeof path, "%s/%s.%08x.perform", g_cen_dir, base, h);
    g_pc_out = fopen(path, "w");
    if (!g_pc_out) { fprintf(stderr, "s32-cobc: cannot write the census to %s\n", path); exit(1); }
    fprintf(g_pc_out, "#file\t%s\n", g_file);
}
static int pc_here(void) { return g_cur_para ? g_cur_para->id : -1; }

/* how the paragraph whose code just ended leaves: its last instruction a
 * jump (GO TO, GOBACK, EXIT PROGRAM), STOP RUN, or control falling into
 * what follows */
static void pc_para_end(int id)
{
    if (id < 0) return;
    const char *ends = "fall";
    for (int i = g_nasm - 1; i >= 0; i--) {
        const char *l = g_asm[i];
        if (l[0] == '#' || !l[0]) continue;
        if (l[0] != '\t') { if (strchr(l, ':') && l[0] == '.') break; else continue; }   /* a label: the end is reached by a branch */
        if (!strncmp(l, "\tjal r0, ", 9) || !strncmp(l, "\tjalr r0, ", 10)) ends = "jump";
        else if (!strcmp(l, "\tjal r31, cob_stop_run")) ends = "stop";
        break;
    }
    Para *p = &g_para[id - 1];                 /* ids are 1-based (stmt.h) */
    while (id >= g_pce_cap) { int c = g_pce_cap ? 2 * g_pce_cap : 256; g_pce = xrealloc(g_pce, (size_t)c); memset(g_pce + g_pce_cap, 'f', (size_t)(c - g_pce_cap)); g_pce_cap = c; }
    g_pce[id] = ends[0];
    if (!g_cen_on || !g_cen_dir) return;
    pc_open();
    fprintf(g_pc_out, "P\t%d\t%s\t%s\t%d\t%d\t%d\t%s\n", p->id, p->name, p->is_section ? "section" : "para", p->section, p->in_decl, p->line, ends);
}
static void pc_perform(const Para *from, const Para *thru, const char *kind)
{
    if (!from) return;
    PC_GROW(g_pcf, g_npcf, g_pcf_cap); g_pcf[g_npcf].from = pc_here(); g_pcf[g_npcf].lo = from->id; g_pcf[g_npcf].thru = thru ? thru->id : -1;
    g_pcf[g_npcf].iter = strcmp(kind, "once") != 0 || g_inline_depth > 0; g_npcf++;
    if (!g_cen_on || !g_cen_dir) return;
    pc_open();
    fprintf(g_pc_out, "F\t%d\t%d\t%d\t%s\n", pc_here(), from->id, thru ? thru->id : -1, kind);
}
static void pc_goto(const Para *target, const char *kind)
{
    if (!target) return;
    PC_GROW(g_pcg, g_npcg, g_pcg_cap); g_pcg[g_npcg].from = pc_here(); g_pcg[g_npcg].target = target->id; g_npcg++;
    if (!g_cen_on || !g_cen_dir) return;
    pc_open();
    fprintf(g_pc_out, "G\t%d\t%d\t%s\n", pc_here(), target->id, kind);
}
static void pc_goto_from(const Para *from, const Para *target, const char *kind)
{
    if (!target) return;
    PC_GROW(g_pcg, g_npcg, g_pcg_cap); g_pcg[g_npcg].from = from ? from->id : -1; g_pcg[g_npcg].target = target->id; g_npcg++;
    if (!g_cen_on || !g_cen_dir) return;
    pc_open();
    fprintf(g_pc_out, "G\t%d\t%d\t%s\n", from ? from->id : -1, target->id, kind);
}
/* the unit's PROCEDURE DIVISION is compiled */
static void pc_unit(void)
{
    if (!g_cen_on || !g_cen_dir) return;
    pc_open();
    for (int u = 0; u < g_nuse; u++) if (g_use[u].unit == g_unit) fprintf(g_pc_out, "D\t%d\n", g_use[u].sec);
    fprintf(g_pc_out, "U\t%s\t%d\n", g_progid, g_npara - g_para_base);
    fflush(g_pc_out);
}
