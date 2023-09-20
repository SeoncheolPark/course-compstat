a <-
c(TRUE, FALSE, FALSE, TRUE)
A <-
FALSE
b <-
c(13, 7, 8, 2)
B <-
FALSE
four68 <-
c(4, 6, 8)
gender <-
c("M", "M", "F", "F", "F")
has_annotations <-
function (input) 
{
    inputLines <- readLines(input)
    chunkStarts <- grep(knitr::all_patterns$md$chunk.begin, inputLines)
    chunkEnds <- grep(knitr::all_patterns$md$chunk.end, inputLines)
    annotations <- grep(".*\\Q#\\E\\s*<[0-9]+>\\s*", inputLines)
    hasAnnotations <- FALSE
    if (length(chunkStarts) > 0 && length(annotations) > 0) {
        lastLine <- max(max(chunkEnds), max(chunkStarts), max(annotations))
        chunkMap <- rep(FALSE, lastLine)
        for (x in 1:length(chunkStarts)) {
            start <- chunkStarts[x]
            end <- chunkEnds[x]
            for (y in start:end) {
                if (y > start && y < end) {
                  chunkMap[y] = TRUE
                }
            }
        }
        for (a in annotations) {
            if (chunkMap[a] == TRUE) {
                hasAnnotations <- TRUE
                break
            }
        }
    }
    hasAnnotations
}
obesityStudy <-
structure(list(gender = structure(c(2L, 2L, 1L, 1L, 1L), levels = c("Female", 
"M"), class = "factor"), weight = c(73, 68, 52, 69, 64)), row.names = c(NA, 
-5L), class = "data.frame")
sampleID <-
c(11759L, 904L, 776L, 654L, 2803L, 12814L, 713L, 497L)
threeM <-
c(3, 6, 9)
values_1 <-
c(10, 10, 18, 30, 32)
values_2 <-
c(40, 10, 10, 18, 30, 32)
weight <-
c(73, 68, 52, 69, 64)
whereF <-
c(FALSE, FALSE, TRUE, TRUE, TRUE)
wrongWay <-
structure(list(gender = structure(c(2L, 2L, NA, NA, NA), levels = c("F", 
"M"), class = "factor"), weight = c(73, 68, 52, 69, 64)), row.names = c(NA, 
-5L), class = "data.frame")
x <-
c(3, 2, 3)
xm <-
8.75
xy <-
structure(list(x = 1:5, y = 7:3), class = "data.frame", row.names = c(NA, 
-5L))
xynew <-
structure(list(x = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L), 
    y = c(7L, 6L, 5L, 4L, 3L, 7L, 6L, 5L, 4L, 3L), new = 10:1), class = "data.frame", row.names = c(NA, 
-10L))
y <-
c(7, 7)
z <-
list(x = c(3, 2, 3), y = c(7, 7))
