!<_connector:use_interface>
use Burn_interface, ONLY: Burn


!<_connector:execute>


call Timers_start("sourceTerms")
call Burn(dt)
call Timers_stop("sourceTerms")

