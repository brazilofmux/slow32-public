// 6.4.6p3  digraphs <: :> <% %> %: %:%: behave like [ ] { } # ##
%:include <stdio.h>
%:define CAT(a, b) a %:%: b
%:define STR(x) %:x
int main(void) <%
    int arr<:3:> = <% 1, 2, 3 %>;
    int CAT(x, y) = arr<:2:>;
    printf("%d %s\n", xy, STR(digraph));
    return 0;
%>
