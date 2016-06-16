.onAttach <- function(...) {
	.g_m2con <<- NULL

	# TODO: start M2
}

.onDetach <- function(...) {
	# TODO: kill M2
}
