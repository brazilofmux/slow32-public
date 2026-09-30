       identification division.
       program-id. evalrul.
      * EVALUATE (X3.23-1985 EVALUATE format and syntax rules; 2023
      * 14.9.13.3 rules 2, 4 and Table 15): as many objects as subjects,
      * a THRU range of one class, no literal against a literal, a
      * condition or TRUE only against TRUE, FALSE or a condition, a
      * statement after each WHEN, WHEN OTHER last.
       data division.
       working-storage section.
       01 a pic 9 value 1.
       01 b pic x value "b".
       procedure division.
           evaluate a also b when 1 continue end-evaluate.
           evaluate a when 1 also "b" continue end-evaluate.
           evaluate a when 1 thru "z" continue end-evaluate.
           evaluate 1 when 2 continue end-evaluate.
           evaluate a when a = 1 continue end-evaluate.
           evaluate a when true continue end-evaluate.
           evaluate a when 1 end-evaluate.
           evaluate a when other continue when 1 continue
               end-evaluate.
           stop run.
