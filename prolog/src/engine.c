#include "engine.h"
#include "term.h"
#include "database.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* Variable bindings */
term_t var_binding[MAX_VARS];
int var_count = 0;

/* Trail */
int trail[TRAIL_SIZE];
int trail_top = 0;

/* Goal stack */
term_t goal_stack[GOAL_STACK_SIZE];
int goal_sp = 0;

/* Choice points */
choice_t choices[MAX_CHOICES];
int choice_top = 0;

/* Cut barrier */
int cut_barrier = 0;

term_t deref(term_t t) {
    while (TAG(t) == TAG_VAR) {
        int id = UN_VAR(t);
        if (id >= MAX_VARS || id >= var_count) return t;
        term_t val = var_binding[id];
        if (val == TERM_NIL) return t;
        t = val;
    }
    return t;
}

void bind(int var_id, term_t val) {
    if (var_id < 0 || var_id >= MAX_VARS || var_id >= var_count) {
        g_error = 1;
        snprintf(g_errmsg, sizeof(g_errmsg), "invalid variable id");
        return;
    }
    if (trail_top >= TRAIL_SIZE) {
        g_error = 1;
        snprintf(g_errmsg, sizeof(g_errmsg), "trail overflow");
        return;
    }
    trail[trail_top++] = var_id;
    var_binding[var_id] = val;
}

void trail_undo(int saved_top) {
    while (trail_top > saved_top) {
        trail_top--;
        var_binding[trail[trail_top]] = TERM_NIL;
    }
}

term_t fresh_var(void) {
    if (var_count >= MAX_VARS) {
        g_error = 1;
        snprintf(g_errmsg, sizeof(g_errmsg), "too many variables");
        return TERM_NIL;
    }
    int id = var_count++;
    var_binding[id] = TERM_NIL;
    return MK_VAR(id);
}

static term_t copy_term_impl(term_t t, int *var_map, int map_size, int follow_bindings) {
    if (follow_bindings) t = deref(t);

    switch (TAG(t)) {
    case TAG_INT:
    case TAG_ATOM:
        return t;

    case TAG_VAR: {
        int id = UN_VAR(t);
        if (id >= map_size) return t;
        if (var_map[id] == -1) {
            if (var_count >= MAX_VARS) {
                g_error = 1;
                return TERM_NIL;
            }
            var_map[id] = var_count++;
            var_binding[var_map[id]] = TERM_NIL;
        }
        return MK_VAR(var_map[id]);
    }

    default: /* PTR */
        if (!IS_PTR(t)) return t;
        {
            int func = compound_functor(t);
            int arity = compound_arity(t);
            if (arity < 0 || arity > PROLOG_MAX_ARITY) {
                g_error = 1;
                snprintf(g_errmsg, sizeof(g_errmsg), "arity overflow");
                return TERM_NIL;
            }
            term_t args[PROLOG_MAX_ARITY];
            int i;
            for (i = 0; i < arity; i++) {
                args[i] = copy_term_impl(compound_arg(t, i), var_map, map_size, follow_bindings);
                if (g_error) return TERM_NIL;
            }
            return make_compound(func, arity, args);
        }
    }
}

term_t copy_term(term_t t, int *var_map, int map_size) {
    return copy_term_impl(t, var_map, map_size, 1);
}

term_t copy_term_code(term_t t, int *var_map, int map_size) {
    return copy_term_impl(t, var_map, map_size, 0);
}

void engine_reset(void) {
    trail_top = 0;
    goal_sp = 0;
    choice_top = 0;
    cut_barrier = 0;
    hp = 1;
    int i;
    for (i = 0; i < var_count && i < MAX_VARS; i++)
        var_binding[i] = TERM_NIL;
    var_count = 0;
}

int engine_snapshot_stacks(term_t **goals_out, int *gsp_out,
                           choice_t **choices_out, int *cp_out) {
    *goals_out = NULL;
    *choices_out = NULL;
    *gsp_out = goal_sp;
    *cp_out = choice_top;

    if (goal_sp > 0) {
        *goals_out = (term_t *)malloc((size_t)goal_sp * sizeof(term_t));
        if (!*goals_out) {
            g_error = 1;
            snprintf(g_errmsg, sizeof(g_errmsg), "out of memory (goal stack snapshot)");
            return 0;
        }
        memcpy(*goals_out, goal_stack, (size_t)goal_sp * sizeof(term_t));
    }
    if (choice_top > 0) {
        *choices_out = (choice_t *)malloc((size_t)choice_top * sizeof(choice_t));
        if (!*choices_out) {
            free(*goals_out);
            *goals_out = NULL;
            g_error = 1;
            snprintf(g_errmsg, sizeof(g_errmsg), "out of memory (choice stack snapshot)");
            return 0;
        }
        memcpy(*choices_out, choices, (size_t)choice_top * sizeof(choice_t));
    }
    return 1;
}

void engine_restore_stacks(term_t *goals, int gsp, choice_t *chs, int cp) {
    if (gsp > 0 && goals) {
        if (gsp > GOAL_STACK_SIZE) {
            gsp = GOAL_STACK_SIZE;
        }
        memcpy(goal_stack, goals, (size_t)gsp * sizeof(term_t));
    }
    if (cp > 0 && chs) {
        if (cp > MAX_CHOICES) {
            cp = MAX_CHOICES;
        }
        memcpy(choices, chs, (size_t)cp * sizeof(choice_t));
    }
    goal_sp = gsp;
    choice_top = cp;
    free(goals);
    free(chs);
}

/* Build conjunction from remaining goal stack (top..bottom order). */
static term_t build_continuation(void) {
    if (goal_sp <= 0) return TERM_NIL;
    term_t cont = goal_stack[0];
    int i;
    for (i = 1; i < goal_sp; i++) {
        term_t args[2];
        args[0] = goal_stack[i];
        args[1] = cont;
        cont = make_compound(ATOM_COMMA, 2, args);
    }
    return cont;
}

/* External builtin dispatcher (defined in builtin.c) */
extern int try_builtin(term_t goal, int functor, int arity);

/* Iterative solve loop */
int solve(term_t goal) {
    goal_sp = 0;
    choice_top = 0;
    cut_barrier = 0;

    if (goal_sp >= GOAL_STACK_SIZE) return 0;
    goal_stack[goal_sp++] = goal;

    for (;;) {
        if (g_error) return 0;

        if (goal_sp <= 0) return 1;

        term_t g = deref(goal_stack[--goal_sp]);

        if (g == TERM_NIL || (TAG(g) == TAG_ATOM && UN_ATOM(g) == ATOM_TRUE))
            continue;

        if (TAG(g) == TAG_ATOM && UN_ATOM(g) == ATOM_FAIL)
            goto backtrack;

        /* Conjunction: (A, B) */
        if (IS_PTR(g) && compound_functor(g) == ATOM_COMMA && compound_arity(g) == 2) {
            term_t left = compound_arg(g, 0);
            term_t right = compound_arg(g, 1);
            if (goal_sp + 2 > GOAL_STACK_SIZE) {
                g_error = 1;
                snprintf(g_errmsg, sizeof(g_errmsg), "goal stack overflow");
                return 0;
            }
            goal_stack[goal_sp++] = right;
            goal_stack[goal_sp++] = left;
            continue;
        }

        /* Disjunction: (A ; B) */
        if (IS_PTR(g) && compound_functor(g) == ATOM_SEMI && compound_arity(g) == 2) {
            term_t left = compound_arg(g, 0);
            term_t right = compound_arg(g, 1);

            /* If-then-else: (Cond -> Then ; Else) */
            term_t dl = deref(left);
            if (IS_PTR(dl) && compound_functor(dl) == ATOM_ARROW && compound_arity(dl) == 2) {
                term_t cond = compound_arg(dl, 0);
                term_t then_goal = compound_arg(dl, 1);

                if (choice_top >= MAX_CHOICES) { g_error = 1; return 0; }
                choice_t *cp = &choices[choice_top++];
                cp->trail_top = trail_top;
                cp->var_count = var_count;
                cp->next_clause = 0;
                cp->goal = right;
                cp->cut_barrier = cut_barrier;
                cp->continuation = build_continuation();
                cp->hp = hp;

                goal_stack[goal_sp++] = then_goal;
                goal_stack[goal_sp++] = MK_ATOM(ATOM_CUT);
                goal_stack[goal_sp++] = cond;
                continue;
            }

            /* Plain disjunction */
            if (choice_top >= MAX_CHOICES) { g_error = 1; return 0; }
            choice_t *cp = &choices[choice_top++];
            cp->trail_top = trail_top;
            cp->var_count = var_count;
            cp->next_clause = 0;
            cp->goal = right;
            cp->cut_barrier = cut_barrier;
            cp->continuation = build_continuation();
            cp->hp = hp;

            goal_stack[goal_sp++] = left;
            continue;
        }

        /* If-then without else */
        if (IS_PTR(g) && compound_functor(g) == ATOM_ARROW && compound_arity(g) == 2) {
            goal_stack[goal_sp++] = compound_arg(g, 1);
            goal_stack[goal_sp++] = compound_arg(g, 0);
            continue;
        }

        /* Negation as failure: \+(Goal)
         * solve() resets goal_sp/choice_top; fully snapshot parent stacks
         * (not a truncated 64-entry window) so remaining conjunction goals
         * and choice points survive. */
        if (IS_PTR(g) && compound_functor(g) == ATOM_NOT && compound_arity(g) == 1) {
            term_t inner = compound_arg(g, 0);
            int saved_trail = trail_top;
            int saved_hp = hp;
            int saved_vc = var_count;
            int saved_cb = cut_barrier;
            term_t *saved_goals = NULL;
            choice_t *saved_choices = NULL;
            int saved_gsp = 0, saved_cp = 0;

            if (!engine_snapshot_stacks(&saved_goals, &saved_gsp,
                                       &saved_choices, &saved_cp)) {
                return 0;
            }

            int inner_result = solve(inner);

            trail_undo(saved_trail);
            hp = saved_hp;
            var_count = saved_vc;
            cut_barrier = saved_cb;
            engine_restore_stacks(saved_goals, saved_gsp, saved_choices, saved_cp);

            if (inner_result)
                goto backtrack;
            continue;
        }

        /* Cut */
        if (TAG(g) == TAG_ATOM && UN_ATOM(g) == ATOM_CUT) {
            if (choice_top > cut_barrier)
                choice_top = cut_barrier;
            continue;
        }

        int functor = term_functor(g);
        int arity = term_arity(g);

        if (functor < 0) goto backtrack;

        /* Try builtin */
        {
            int bres = try_builtin(g, functor, arity);
            if (bres == 1) continue;
            if (bres == -1) goto backtrack;
        }

        /* Database lookup */
        {
            predicate_t *pred = db_lookup(functor, arity);
            if (!pred || !pred->first) goto backtrack;

            clause_t *clause = pred->first;

            if (clause->next) {
                if (choice_top >= MAX_CHOICES) { g_error = 1; return 0; }
                choice_t *cp = &choices[choice_top++];
                cp->trail_top = trail_top;
                cp->var_count = var_count;
                cp->next_clause = clause->next;
                cp->goal = g;
                cp->cut_barrier = cut_barrier;
                cp->continuation = build_continuation();
                cp->hp = hp;
            }

            int var_map[256];
            memset(var_map, -1, sizeof(var_map));
            term_t ch = copy_term_code(clause->head, var_map, 256);
            term_t cb = clause->body != TERM_NIL ?
                        copy_term_code(clause->body, var_map, 256) : TERM_NIL;

            if (g_error) return 0;

            if (!unify(g, ch)) {
                goto backtrack;
            }

            if (cb != TERM_NIL && !(TAG(cb) == TAG_ATOM && UN_ATOM(cb) == ATOM_TRUE))
                goal_stack[goal_sp++] = cb;
        }
        continue;

backtrack:
        if (choice_top <= 0) return 0;

        {
            choice_t *cp = &choices[--choice_top];
            trail_undo(cp->trail_top);
            hp = cp->hp;
            var_count = cp->var_count;
            cut_barrier = cp->cut_barrier;

            goal_sp = 0;
            if (cp->continuation != TERM_NIL) {
                if (goal_sp >= GOAL_STACK_SIZE) {
                    g_error = 1;
                    snprintf(g_errmsg, sizeof(g_errmsg), "goal stack overflow");
                    return 0;
                }
                goal_stack[goal_sp++] = cp->continuation;
            }

            clause_t *next = (clause_t *)cp->next_clause;
            term_t retry_goal = cp->goal;

            if (next == 0) {
                /* Disjunction: push the right branch */
                goal_stack[goal_sp++] = retry_goal;
                continue;
            }

            /* Try next clause */
            if (next->next) {
                if (choice_top >= MAX_CHOICES) { g_error = 1; return 0; }
                choice_t *ncp = &choices[choice_top++];
                ncp->trail_top = trail_top;
                ncp->var_count = var_count;
                ncp->next_clause = next->next;
                ncp->goal = retry_goal;
                ncp->cut_barrier = cp->cut_barrier;
                ncp->continuation = build_continuation();
                ncp->hp = hp;
            }

            int vm2[256];
            memset(vm2, -1, sizeof(vm2));
            term_t ch2 = copy_term_code(next->head, vm2, 256);
            term_t cb2 = next->body != TERM_NIL ?
                         copy_term_code(next->body, vm2, 256) : TERM_NIL;

            if (g_error) return 0;

            if (!unify(retry_goal, ch2)) {
                goto backtrack;
            }

            if (cb2 != TERM_NIL && !(TAG(cb2) == TAG_ATOM && UN_ATOM(cb2) == ATOM_TRUE))
                goal_stack[goal_sp++] = cb2;
        }
        continue;
    }
}
