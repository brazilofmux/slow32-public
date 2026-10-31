// TBrowse across a window narrower than its columns: a frozen column,
// leftVisible and rightVisible through right(), end(), panEnd(), panLeft(),
// home(), panHome(), left() and panRight(); a footing and the separators
// taking their rows from rowCount.
PROCEDURE Main
   LOCAL a := {}, i, b, n := 1
   FOR i := 1 TO 8
      AADD(a, i)
   NEXT
   b := TBrowseNew(1, 0, 10, 39)
   b:skipBlock := {|k| Skipper(k, @n, LEN(a))}
   b:goTopBlock := {|| n := 1}
   b:goBottomBlock := {|| n := LEN(a)}
   FOR i := 1 TO 6
      b:addColumn(TBColumnNew("Column" + STR(i, 1), MakeBlock(i, @n)))
   NEXT
   b:getColumn(1):width := 8
   b:getColumn(2):width := 10
   b:getColumn(3):width := 12
   b:getColumn(4):width := 9
   b:getColumn(5):width := 11
   b:getColumn(6):width := 7
   b:getColumn(2):footing := "Total"
   b:footSep := "-"
   b:headSep := "="
   b:freeze := 1
   b:forceStable()
   ? "start", b:colPos, b:leftVisible, b:rightVisible, b:rowCount, b:freeze
   b:right(); b:forceStable(); ? "right", b:colPos, b:leftVisible, b:rightVisible
   b:right(); b:forceStable(); ? "right", b:colPos, b:leftVisible, b:rightVisible
   b:right(); b:forceStable(); ? "right", b:colPos, b:leftVisible, b:rightVisible
   b:end(); b:forceStable(); ? "end", b:colPos, b:leftVisible, b:rightVisible
   b:panEnd(); b:forceStable(); ? "panEnd", b:colPos, b:leftVisible, b:rightVisible
   b:panLeft(); b:forceStable(); ? "panLeft", b:colPos, b:leftVisible, b:rightVisible
   b:home(); b:forceStable(); ? "home", b:colPos, b:leftVisible, b:rightVisible
   b:panHome(); b:forceStable(); ? "panHome", b:colPos, b:leftVisible, b:rightVisible
   b:left(); b:forceStable(); ? "left", b:colPos, b:leftVisible, b:rightVisible
   b:panRight(); b:forceStable(); ? "panRight", b:colPos, b:leftVisible, b:rightVisible
   b:panRight(); b:forceStable(); ? "panRight", b:colPos, b:leftVisible, b:rightVisible
   b:freeze := 0; b:panHome(); b:forceStable(); ? "unfrozen", b:colPos, b:leftVisible, b:rightVisible
   RETURN

STATIC FUNCTION MakeBlock(i, n)
   RETURN {|| n * 10 + i}

STATIC FUNCTION Skipper(k, n, m)
   LOCAL s := 0
   IF k > 0
      DO WHILE s < k .AND. n < m
         n++; s++
      ENDDO
   ELSEIF k < 0
      DO WHILE s > k .AND. n > 1
         n--; s--
      ENDDO
   ENDIF
   RETURN s
